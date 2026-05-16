# %%
import argparse
import os
import autoreject
import matplotlib.pyplot as plt
import mne
import numpy as np
import pandas as pd
from pathlib import Path
import time

# Helper function for printing timestamps
def print_timestamp(message):
    print(time.strftime("%H:%M:%S", time.localtime()))
    print(message)


def read_manual_bad_channels(channels_tsv):
    """Return channel names whose status column is not 'good' in a BIDS
    channels.tsv. Bad-channel annotations on the BDF can get lost, so the
    sidecar is the authoritative source."""
    if channels_tsv is None or not Path(channels_tsv).exists():
        return []
    df = pd.read_csv(channels_tsv, sep='\t')
    if 'status' not in df.columns or 'name' not in df.columns:
        return []
    return [str(name) for name, status in zip(df['name'], df['status'])
            if str(status).strip().lower() != 'good']

def add_preprocessing_summary(report, epochs_original, epochs_final,
                             bad_channels):
    n_total = len(epochs_original)
    n_final = len(epochs_final)

    # Build per-condition retention rows
    condition_rows = ""
    for condition in epochs_original.event_id.keys():
        n_cond_original = len(epochs_original[condition])
        n_cond_final = len(epochs_final[condition]) if condition in epochs_final.event_id else 0
        pct = (n_cond_final / n_cond_original * 100) if n_cond_original > 0 else 0.0
        condition_rows += (
            f"    <tr><td>{condition}</td><td>{n_cond_original}</td>"
            f"<td>{n_cond_final}</td><td>{pct:.1f}%</td></tr>\n"
        )

    # Create detailed summary table
    summary_html = f"""
<h3>Complete Preprocessing Summary</h3>
<table border="1" style="border-collapse: collapse; margin: 10px;">
    <tr><th>Processing Step</th><th>Count</th><th>Percentage of Original</th></tr>
    <tr><td>Original epochs</td><td>{n_total}</td><td>100.0%</td></tr>
    <tr><td>After artifact rejection</td><td>{n_final}</td>
        <td>{(n_final/n_total)*100:.1f}%</td></tr>
    <tr><td><strong>Final clean epochs</strong></td><td><strong>{n_final}</strong></td>
        <td><strong>{(n_final/n_total)*100:.1f}%</strong></td></tr>
</table>

<h4>Retained Epochs per Condition</h4>
<table border="1" style="border-collapse: collapse; margin: 10px;">
    <tr><th>Condition</th><th>Original</th><th>Retained</th><th>Retention</th></tr>
{condition_rows}</table>

<h4>Bad Channels Detected: {len(bad_channels)}</h4>
<p>{', '.join(bad_channels) if bad_channels else 'None'}</p>
    """

    report.add_html(title='Complete Preprocessing Summary', html=summary_html)

    return report

def preprocess_EEG(in_file, out_folder, out_file, report_file, channel_config,
                   channels_tsv=None):
    # Setup the output folders
    os.makedirs(out_folder, exist_ok=True)

    # Loading the raw data
    print_timestamp('Reading raw data')
    raw = mne.io.read_raw_bdf(in_file, preload=True)

    # Set channel types
    channel_mapping = pd.read_csv(channel_config).set_index('ch_name').to_dict()['ch_type']
    raw.set_channel_types(channel_mapping, verbose=False)

    # Apply manually annotated bad channels from BIDS sidecar — bad-channel
    # marks on the BDF can get lost on copy/conversion, so the channels.tsv
    # is treated as authoritative.
    manual_bad_channels = [ch for ch in read_manual_bad_channels(channels_tsv)
                           if ch in raw.ch_names]
    if manual_bad_channels:
        raw.info['bads'] = sorted(set(raw.info['bads']) | set(manual_bad_channels))
        print(f'Marked {len(manual_bad_channels)} channels bad from sidecar: '
              f'{manual_bad_channels}')

    # Get the channel locations
    print_timestamp('Defining channels')
    raw = raw.set_montage('biosemi64', match_case=False)

    # Set the refernce (for now)
    print_timestamp('Setting preliminary referencing')
    raw.set_eeg_reference(ref_channels=['Cz'])

    # Resampling
    print_timestamp('Downsampling')
    raw = raw.resample(250)

    # Saving the report
    report = mne.Report(title="Preprocessing Overview", verbose=False)
    report.add_raw(raw=raw, title='Raw', psd=False)
    report.save(report_file, open_browser=False, overwrite=True)

    # Filter the data
    print_timestamp('Filtering 0.5-40Hz')
    filtered = raw.copy().filter(l_freq=0.5, h_freq=40)

    # compute ICA
    print_timestamp('ICA')
    ica = mne.preprocessing.ICA(
        n_components=25,
        method='fastica',
        random_state=23,
    )

    ica.fit(inst=filtered, decim=3)

    try:
        eog_average = mne.preprocessing.create_eog_epochs(filtered).average()
        eog_components, eog_scores = ica.find_bads_eog(filtered)
        ica.exclude.extend(eog_components)
    except:
        print('EOG detection failed')
        eog_average = None
        eog_components = []
        eog_scores = None
        filtered.info["bads"] = sorted(set(filtered.info["bads"]) |
                                       {'hEOG-01', 'hEOG-02', 'vEOG-01', 'vEOG-02'})

    try: 
        ecg_average = mne.preprocessing.create_ecg_epochs(filtered).average()
        ecg_components, ecg_scores = ica.find_bads_ecg(filtered)
        ica.exclude.extend(ecg_components)
    except:
        print('ECG detection failed')
        ecg_average = None
        ecg_components = []
        ecg_scores = None
        filtered.info["bads"] = sorted(set(filtered.info["bads"]) | {'ECG1', 'ECG2'})

    report.add_ica(
        ica=ica,
        title='ICA cleaning',
        inst=None,
        eog_evoked=eog_average,
        ecg_evoked=ecg_average,
        eog_scores=eog_scores,
        ecg_scores=ecg_scores,
        n_jobs=-1
    )
    report.save(report_file, open_browser=False, overwrite=True)

    ica.apply(filtered)

    # Epoch the data
    print_timestamp('Identifying events')
    events = mne.find_events(filtered, min_duration=0.01, output='onset')

    event_dict = {
        'white/angry/repetition/1': 21,
        'white/angry/repetition/2': 22,
        'white/angry/repetition/3': 23,
        'white/angry/repetition/4': 24,
        'white/angry/repetition/5': 25,
        'white/angry/repetition/6': 26,
        'white/angry/repetition/7': 27,
        'white/happy/repetition/1': 41,
        'white/happy/repetition/2': 42,
        'white/happy/repetition/3': 43,
        'white/happy/repetition/4': 44,
        'white/happy/repetition/5': 45,
        'white/happy/repetition/6': 46,
        'white/happy/repetition/7': 47,
    }

    report.add_events(events=events, title='Events', sfreq=filtered.info["sfreq"])
    report.save(report_file, open_browser=False, overwrite=True)


    print_timestamp('Splitting into epochs')
    epochs = mne.Epochs(filtered, events,
                        tmin=-0.1,
                        tmax=1.0,
                        event_id=event_dict,
                        preload=True)
    epochs = epochs.shift_time(tshift=0.025)
    report.add_epochs(epochs=epochs, title='Epochs')
    report.save(report_file, open_browser=False, overwrite=True)

    # Step 1: Detect bad channels based on peak-to-peak amplitude (very lenient)
    print_timestamp('Step 1: Detecting bad channels with extreme amplitude')
    data = epochs.get_data()
    eeg_picks = mne.pick_types(epochs.info, eeg=True, exclude=[])
    bad_by_amplitude = []
    for i in eeg_picks:
        ch = epochs.ch_names[i]
        # Use median ptp across epochs - more robust to occasional bad epochs
        ch_ptp_median = np.median([np.ptp(data[e, i, :]) for e in range(len(epochs))])
        if ch_ptp_median > 0.005:  # 5mV median threshold (very lenient)
            bad_by_amplitude.append(ch)
            print(f"  {ch} marked as bad (median ptp={ch_ptp_median*1e6:.0f}µV)")
    
    if bad_by_amplitude:
        epochs.info['bads'].extend(bad_by_amplitude)
        print(f"Interpolating {len(bad_by_amplitude)} bad channels: {bad_by_amplitude}")
        epochs.interpolate_bads(reset_bads=False)
    else:
        print("  No bad channels detected")

    # Step 2: Reject bad epochs with remaining channels (very lenient)
    print_timestamp('Step 2: Removing epochs with extreme artifacts')
    n_before = len(epochs)
    reject_criteria_coarse = dict(eeg=500e-6)  # 500µV threshold (very lenient)
    epochs = epochs.drop_bad(reject=reject_criteria_coarse)
    n_rejected_coarse = n_before - len(epochs)
    print(f"Removed {n_rejected_coarse} epochs with extreme artifacts (>{500}µV)")
    
    if len(epochs) == 0:
        raise RuntimeError(
            f"All epochs were rejected in coarse artifact rejection. "
            f"Bad channels detected: {bad_by_amplitude}. "
            f"Please inspect the raw data for this participant."
        )

    # Step 3: Run RANSAC to detect and interpolate remaining bad channels
    print_timestamp('Step 3: Detecting bad channels with RANSAC')
    picks = mne.pick_types(epochs.info, meg=False, eeg=True,
                       stim=False, eog=False, ecg=False,
                       include=[], exclude=['Cz'])
    ransac = autoreject.Ransac(
        picks=picks,
        n_jobs=-1,
        n_resample=50,
        min_corr=0.5,
        unbroken_time=0.2,
        min_channels=0.25,
        verbose=True,
        random_state=23
    )
    epochs_clean = ransac.fit_transform(epochs)
    bad_chs = sorted(set(ransac.bad_chs_) | set(bad_by_amplitude) | set(manual_bad_channels))
    print(f'Total bad channels: {len(bad_chs)} - {bad_chs}')

    # Step 4: AutoReject for fine-grained epoch rejection and channel interpolation
    print_timestamp('Step 4: Running AutoReject for final cleanup')
    report.add_epochs(epochs=epochs_clean, title='Epochs after RANSAC')
    fig = epochs_clean.plot_drop_log(show=False)

    report.add_figure(fig=fig, title="Epoch rejection log", image_format="PNG")
    report.save(report_file, open_browser=False, overwrite=True)

    # Run autoreject to detect bad channels and epochs
    print_timestamp('Detecting bad epochs with Autoreject')
    ar = autoreject.AutoReject(
        n_interpolate=[1, 2, 4, 8, 12, 16],
        consensus=np.linspace(0, 1.0, 11),
        thresh_method='bayesian_optimization',
        cv=5,
        random_state=23,
        n_jobs=-1,
        verbose=True
    )
    ar.fit(epochs_clean)
    epochs_clean, reject_log = ar.transform(epochs_clean, return_log=True)

    fig, ax = plt.subplots(figsize=(3, 10))
    reject_log.plot(orientation='vertical', ax=ax, show_names='auto', show=False)

    # Setting the average reference
    print('Re-referencing after artefact rejection')
    print(time.strftime("%H:%M:%S", time.localtime()))
    epochs_clean, _ = mne.set_eeg_reference(epochs_clean, ref_channels='average')

    # Apply baseline correction
    print('Applying baseline correction')
    print(time.strftime("%H:%M:%S", time.localtime()))
    epochs_clean = epochs_clean.apply_baseline(baseline=(None, 0))

    # Plotting data before and after cleaning
    evoked = epochs['white'].average()
    evoked_clean = epochs_clean['white'].average()

    x = evoked.times
    y_original = evoked.get_data().T*10e5
    y_clean = evoked_clean.get_data().T*10e5

    fig = plt.figure(figsize=(6.13, 3.30))
    plt.plot(x, y_original, color='r', linewidth=0.5);
    plt.plot(x, y_clean, color='k', linewidth=0.5);
    plt.xlabel('Time (s)')
    plt.ylabel('$\mu$V')

    report.add_figure(fig=fig, title='Data cleaning', caption='Comparison of data before and after cleaning', image_format='PNG')
    report.save(report_file, open_browser=False, overwrite=True)
    plt.close(fig)

    # Get information about data quality
    num_epochs = len(epochs)
    num_epochs_after_rejection = len(epochs_clean)
    num_rejected_channels = len(bad_chs)

    epochs_clean.save(out_file, overwrite=True)

    # Add preprocessing summary
    report = add_preprocessing_summary(report, epochs, epochs_clean, bad_chs)
    report.save(report_file, open_browser=False, overwrite=True)

    return num_epochs, num_epochs_after_rejection, num_rejected_channels
# %%

def main(channel_config, in_folder):
    in_folder = Path(in_folder)
    channel_config = Path(channel_config)

    subject_list = sorted([subject for subject in os.listdir(in_folder) if subject.startswith('sub-')])

    for subject in subject_list:
        print(subject)
        in_file = in_folder / f'{subject}/eeg/{subject}_task-RovingOddball_eeg.bdf'
        channels_tsv = in_folder / f'{subject}/eeg/{subject}_task-RovingOddball_channels.tsv'
        out_folder = in_folder / f'derivatives/{subject}/eeg'
        out_file = out_folder / f'{subject}_task-RovingOddball_eeg-epo.fif.gz'
        report_file = out_folder / f'{subject}_task-RovingOddball_eeg.html'

        if not os.path.isfile(out_file):
            try:
                _ = preprocess_EEG(in_file, out_folder, out_file, report_file,
                                   channel_config, channels_tsv=channels_tsv)
            except:
                print(f"Error with {subject}")
        else:
            print(f'Processed data found for {subject}')

        print(f'Processing completed for {subject}')


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Preprocess EEG data for all subjects in a BIDS folder.")
    parser.add_argument('--channel-config', required=True, type=Path,
                        help='Path to the channel configuration CSV (e.g. UvA_DiverseConfig_ChTypes.csv).')
    parser.add_argument('--in-folder', required=True, type=Path,
                        help='Path to the BIDS input folder containing sub-* directories.')
    args = parser.parse_args()
    main(args.channel_config, args.in_folder)

# %%
