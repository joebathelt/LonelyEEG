# %%
import argparse
import os

# Cap BLAS/OpenMP thread pools BEFORE numpy/MNE import so they're honoured by
# this process and inherited by any fork/spawn children. Joblib n_jobs is set
# separately via the CLI; together they bound total CPU use per run.
_DEFAULT_JOBS_PER_RUN = 8
for _var in ('OMP_NUM_THREADS', 'OPENBLAS_NUM_THREADS', 'MKL_NUM_THREADS',
             'NUMEXPR_NUM_THREADS', 'VECLIB_MAXIMUM_THREADS',
             'BLIS_NUM_THREADS'):
    os.environ.setdefault(_var, str(_DEFAULT_JOBS_PER_RUN))

import autoreject
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import mne
import numpy as np
import pandas as pd
from concurrent.futures import ProcessPoolExecutor, as_completed
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

EPOCH_TMIN = -0.1
EPOCH_TMAX = 1.0
RESAMPLE_SFREQ = 250


def preprocess_EEG(in_file, out_folder, out_file, report_file, channel_config,
                   channels_tsv=None, n_jobs=_DEFAULT_JOBS_PER_RUN):
    # Setup the output folders
    os.makedirs(out_folder, exist_ok=True)

    # Loading the raw data
    print_timestamp('Reading raw data')
    raw = mne.io.read_raw_bdf(in_file, preload=True)

    # Crop continuous data to the experiment window: from the start marker
    # (event 111) to the end marker (event 101). Anything outside this range
    # is pre-/post-experiment and shouldn't enter preprocessing.
    print_timestamp('Cropping data to experiment window (events 111 → 101)')
    all_events = mne.find_events(raw, min_duration=0.01, output='onset')
    sfreq = raw.info['sfreq']

    start_events = all_events[all_events[:, 2] == 111]
    end_events = all_events[all_events[:, 2] == 101]
    print(f'  found {len(start_events)} event(s) 111 (task start), '
          f'{len(end_events)} event(s) 101 (task end), '
          f'recording duration = {raw.times[-1]:.1f}s')

    tmin = 0.0
    tmax = None
    if len(start_events) > 0:
        tmin = (start_events[0, 0] - raw.first_samp) / sfreq
    else:
        print('  WARNING: event 111 not found — keeping recording start')
    if len(end_events) > 0:
        tmax = (end_events[0, 0] - raw.first_samp) / sfreq
    else:
        print('  WARNING: event 101 not found — keeping recording end')

    raw.crop(tmin=tmin, tmax=tmax)
    print(f'Cropped to {tmin:.1f}s – {tmax if tmax is None else f"{tmax:.1f}s"} '
          f'(post-crop duration = {raw.times[-1]:.1f}s)')
    print(f'  raw.first_samp / sfreq = {raw.first_samp / sfreq:.3f}s, '
          f'annotations.orig_time = {raw.annotations.orig_time}')

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

    # Detect breaks programmatically — any gap >10s between consecutive
    # triggers is treated as a break. The BAD_break span runs from the end
    # of the previous event's epoch window to 1s before the next event, so
    # the boundary epochs on either side are safely outside it. Existing
    # BAD_break annotations are cleared first so re-running on the same
    # raw object can't compound them.
    print_timestamp('Detecting breaks from event gaps')
    existing = raw.annotations
    keep = [i for i, desc in enumerate(existing.description)
            if desc != 'BAD_break']
    if len(keep) != len(existing):
        raw.set_annotations(existing[keep])
    sfreq = raw.info['sfreq']
    stim_events = mne.find_events(raw, min_duration=0.01, output='onset')
    event_times = (stim_events[:, 0] - raw.first_samp) / sfreq
    gaps = np.diff(event_times)
    break_mask = gaps > 5.0
    if break_mask.any():
        break_starts = event_times[:-1][break_mask] + EPOCH_TMAX
        break_ends = event_times[1:][break_mask] - 1.0
        break_durations = break_ends - break_starts
        valid = break_durations > 0
        if valid.any():
            # Onsets must use the same frame as raw.annotations. With
            # orig_time set (BDF's meas_date), MNE interprets onsets as
            # absolute times from orig_time. After raw.crop, the cropped
            # data starts first_samp/sfreq seconds into the recording —
            # so we add that offset to convert our cropped-data-relative
            # break_starts into the absolute frame. Without it, every
            # BAD_break sits first_samp/sfreq seconds too early in the
            # data, landing inside epochs of the previous block.
            orig_time = raw.annotations.orig_time
            onset_offset = (raw.first_samp / sfreq) if orig_time is not None else 0.0
            break_annotations = mne.Annotations(
                onset=break_starts[valid] + onset_offset,
                duration=break_durations[valid],
                description=['BAD_break'] * int(valid.sum()),
                orig_time=orig_time,
            )
            raw.set_annotations(raw.annotations + break_annotations)
            print(f'Marked {int(valid.sum())} break(s) as BAD_break '
                  f'(total {float(break_durations[valid].sum()):.1f}s)')
            # Diagnostic: where does the first BAD_break actually sit in
            # data-array seconds? Subtract onset_offset to convert the
            # stored (absolute) onset back to data-array time, then
            # compare to what we computed. drift should be ~0.
            expected_first = float(break_starts[valid][0])
            bad_break_onsets = [o for o, d in zip(raw.annotations.onset,
                                                  raw.annotations.description)
                                if d == 'BAD_break']
            stored_first = float(bad_break_onsets[0])
            effective_first = stored_first - onset_offset
            drift = effective_first - expected_first
            print(f'  first BAD_break: expected (data-array)={expected_first:.3f}s, '
                  f'effective (data-array)={effective_first:.3f}s, '
                  f'stored (raw)={stored_first:.3f}s, drift={drift:.3f}s, '
                  f'onset_offset={onset_offset:.3f}s')

    # Get the channel locations
    print_timestamp('Defining channels')
    raw = raw.set_montage('biosemi64', match_case=False)

    # Keep the recording reference (BioSemi CMS/DRL) through cleaning. Passing
    # an empty list tells MNE the data is already referenced, which silences
    # the "no reference" warning on ICA/etc. without subtracting any channel.
    # Average referencing is applied later, after bad-channel interpolation.
    print_timestamp('Setting preliminary referencing')
    raw.set_eeg_reference([])

    # Resampling
    print_timestamp('Downsampling')
    raw = raw.resample(RESAMPLE_SFREQ)

    # Saving the report
    report = mne.Report(title="Preprocessing Overview", verbose=False)
    report.add_raw(raw=raw, title='Raw', psd=False)
    report.save(report_file, open_browser=False, overwrite=True)

    # Filter the data. Skip BAD spans (manual break annotations) so high-
    # amplitude transients during breaks don't ring into adjacent trial data
    # through the 0.5 Hz highpass.
    print_timestamp('Filtering 0.5-40Hz')
    filtered = raw.copy().filter(l_freq=0.5, h_freq=40,
                                 skip_by_annotation=('edge', 'BAD'))

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
        n_jobs=n_jobs
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
                        tmin=EPOCH_TMIN,
                        tmax=EPOCH_TMAX,
                        event_id=event_dict,
                        preload=True)
    epochs = epochs.shift_time(tshift=0.025)
    report.add_epochs(epochs=epochs, title='Epochs')
    report.save(report_file, open_browser=False, overwrite=True)

    # Diagnostic: which events get dropped due to BAD_break overlap?
    # epochs.drop_log has one entry per row in `events`, in order.
    bad_break_drops = []
    f_first_samp = filtered.first_samp
    f_sfreq = filtered.info['sfreq']
    bad_break_spans = [(o, o + d) for o, d, desc in zip(filtered.annotations.onset,
                                                        filtered.annotations.duration,
                                                        filtered.annotations.description)
                       if desc == 'BAD_break']
    orig_t = filtered.annotations.orig_time
    annot_offset = (f_first_samp / f_sfreq) if orig_t is not None else 0.0
    for i, log in enumerate(epochs.drop_log):
        if log and any('BAD_break' in r for r in log):
            evt_time = (events[i, 0] - f_first_samp) / f_sfreq
            evt_code = int(events[i, 2])
            bad_break_drops.append((evt_code, evt_time))
    print(f'Diagnostic: {len(bad_break_drops)} epochs dropped due to BAD_break')
    for code, t in bad_break_drops[:5]:
        matching = [(s - annot_offset, e - annot_offset)
                    for s, e in bad_break_spans
                    if (s - annot_offset) < t + EPOCH_TMAX
                    and (e - annot_offset) > t + EPOCH_TMIN]
        print(f'  event code={code}, time={t:.3f}s, epoch=[{t+EPOCH_TMIN:.3f}, '
              f'{t+EPOCH_TMAX:.3f}], overlapping BAD_break(s)={matching}')

    # Step 1: Detect bad channels by extreme or near-zero amplitude. The high
    # threshold catches saturating channels; the low threshold catches flat/
    # disconnected leads that show up as very low PSD and that RANSAC can miss
    # if they happen to weakly correlate with neighbours.
    print_timestamp('Step 1: Detecting bad channels with extreme amplitude')
    data = epochs.get_data()
    eeg_picks = mne.pick_types(epochs.info, eeg=True, exclude=[])
    bad_by_amplitude = []
    for i in eeg_picks:
        ch = epochs.ch_names[i]
        # Median across epochs — robust to occasional bad epochs
        ch_ptp_median = np.median([np.ptp(data[e, i, :]) for e in range(len(epochs))])
        ch_std_median = np.median([np.std(data[e, i, :]) for e in range(len(epochs))])
        if ch_ptp_median > 0.005:  # 5mV median threshold (very lenient)
            bad_by_amplitude.append(ch)
            print(f"  {ch} marked as bad (median ptp={ch_ptp_median*1e6:.0f}µV)")
        elif ch_std_median < 1e-6:  # 1µV — well below physiological EEG
            bad_by_amplitude.append(ch)
            print(f"  {ch} marked as bad (median std={ch_std_median*1e6:.2f}µV, near-flat)")

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
                       include=[], exclude=[])
    ransac = autoreject.Ransac(
        picks=picks,
        n_jobs=n_jobs,
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
        n_interpolate=[1, 4, 16, 32],
        consensus=np.linspace(0, 1.0, 10),
        thresh_method='bayesian_optimization',
        cv=5,
        random_state=23,
        n_jobs=n_jobs,
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

def _process_subject(subject, in_folder, channel_config, n_jobs):
    in_file = in_folder / f'{subject}/eeg/{subject}_task-RovingOddball_eeg.bdf'
    channels_tsv = in_folder / f'{subject}/eeg/{subject}_task-RovingOddball_channels.tsv'
    out_folder = in_folder / f'derivatives/{subject}/eeg'
    out_file = out_folder / f'{subject}_task-RovingOddball_eeg-epo.fif.gz'
    report_file = out_folder / f'{subject}_task-RovingOddball_eeg.html'

    if os.path.isfile(out_file):
        return subject, 'skipped (already processed)'
    try:
        preprocess_EEG(in_file, out_folder, out_file, report_file,
                       channel_config, channels_tsv=channels_tsv,
                       n_jobs=n_jobs)
        return subject, 'done'
    except Exception as exc:
        return subject, f'error: {exc!r}'


def main(channel_config, in_folder, jobs_per_run, parallel_subjects):
    in_folder = Path(in_folder)
    channel_config = Path(channel_config)

    subject_list = sorted([subject for subject in os.listdir(in_folder) if subject.startswith('sub-')])

    with ProcessPoolExecutor(max_workers=parallel_subjects) as pool:
        futures = {
            pool.submit(_process_subject, subject, in_folder, channel_config,
                        jobs_per_run): subject
            for subject in subject_list
        }
        for future in as_completed(futures):
            subject, status = future.result()
            print(f'{subject}: {status}')


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Preprocess EEG data for all subjects in a BIDS folder.")
    parser.add_argument('--channel-config', required=True, type=Path,
                        help='Path to the channel configuration CSV (e.g. UvA_DiverseConfig_ChTypes.csv).')
    parser.add_argument('--in-folder', required=True, type=Path,
                        help='Path to the BIDS input folder containing sub-* directories.')
    parser.add_argument('--jobs-per-run', type=int, default=_DEFAULT_JOBS_PER_RUN,
                        help='CPU cores used per participant (joblib n_jobs + BLAS thread cap).')
    parser.add_argument('--parallel-subjects', type=int, default=8,
                        help='Number of participants to process in parallel.')
    args = parser.parse_args()

    # Keep BLAS thread caps in sync with whatever the user requested. Child
    # workers inherit os.environ, so this also caps threads inside each
    # subject's worker process.
    for _var in ('OMP_NUM_THREADS', 'OPENBLAS_NUM_THREADS', 'MKL_NUM_THREADS',
                 'NUMEXPR_NUM_THREADS', 'VECLIB_MAXIMUM_THREADS',
                 'BLIS_NUM_THREADS'):
        os.environ[_var] = str(args.jobs_per_run)

    main(args.channel_config, args.in_folder,
         args.jobs_per_run, args.parallel_subjects)

# %%
