# %%
import argparse
import os
import mne
import pandas as pd
import json
from pathlib import Path
from shutil import copyfile


EVENT_DICT = {
    11: 'red/angry/deviant/1',
    12: 'red/angry/standard/2',
    13: 'red/angry/standard/3',
    14: 'red/angry/standard/4',
    15: 'red/angry/standard/5',
    16: 'red/angry/standard/6',
    20: 'cross/white',
    30: 'cross/red',
    21: 'white/angry/deviant/1',
    22: 'white/angry/standard/2',
    23: 'white/angry/standard/3',
    24: 'white/angry/standard/4',
    25: 'white/angry/standard/5',
    26: 'white/angry/standard/6',
    27: 'white/angry/standard/7',
    28: 'white/angry/standard/8',
    31: 'red/happy/deviant/1',
    32: 'red/happy/standard/2',
    33: 'red/happy/standard/3',
    35: 'red/happy/standard/5',
    36: 'red/happy/standard/6',
    41: 'white/happy/deviant/1',
    42: 'white/happy/standard/2',
    43: 'white/happy/standard/3',
    44: 'white/happy/standard/4',
    45: 'white/happy/standard/5',
    46: 'white/happy/standard/6',
    47: 'white/happy/standard/7',
    48: 'white/happy/standard/8',
    52: 'red/neutral/standard/2',
    54: 'red/neutral/standard/4',
    57: 'red/neutral/standard/7',
    61: 'white/neutral/deviant/1',
    62: 'white/neutral/standard/2',
    63: 'white/neutral/standard/3',
    64: 'white/neutral/standard/4',
    65: 'white/neutral/standard/5',
    66: 'white/neutral/standard/6',
    67: 'white/neutral/standard/7',
    68: 'white/neutral/standard/8',
    111: 'button_press',
    200: 'feedback'
}


def write_dataset_description(outfolder):
    target = outfolder / 'dataset_description.json'
    if target.exists():
        print(f'  Skipping (exists): {target.name}')
        return
    dataset_description = {
        "Name": "Loneliness EEG - Roving Oddball Task",
        "BIDSVersion": "1.9.0",
        "DatasetType": "raw",
        "Authors": ["Your Name"],
        "License": "CC0"
    }
    with open(target, 'w') as f:
        json.dump(dataset_description, f, indent=4)


def write_participants_tsv(outfolder, participant_list):
    target = outfolder / 'participants.tsv'
    # Protected: may contain manual annotations
    if target.exists():
        print(f'  Skipping (exists, may contain manual annotations): {target.name}')
        return
    participants_df = pd.DataFrame(participant_list).drop_duplicates()
    participants_df.to_csv(target, index=False, sep='\t')


def process_participant(participant, raw_data_folder, outfolder, experiment_folder):
    bdf_files = [f for f in os.listdir(raw_data_folder / participant) if f.endswith('.bdf')]
    if not bdf_files:
        print(f'  No BDF file found for {participant}, skipping')
        return

    subject_eeg_folder = outfolder / participant / 'eeg'

    bdf_dst = subject_eeg_folder / f'{participant}_task-RovingOddball_eeg.bdf'
    events_dst = subject_eeg_folder / f'{participant}_task-RovingOddball_events.tsv'
    channels_dst = subject_eeg_folder / f'{participant}_task-RovingOddball_channels.tsv'
    json_dst = subject_eeg_folder / f'{participant}_task-RovingOddball_eeg.json'

    # Short-circuit if everything for this participant already exists
    if all(p.exists() for p in (bdf_dst, events_dst, channels_dst, json_dst)):
        print(f'  All BIDS files already present for {participant}, skipping')
        return

    subject_eeg_folder.mkdir(parents=True, exist_ok=True)

    # Copy raw BDF file to BIDS structure
    src = raw_data_folder / participant / bdf_files[0]
    if bdf_dst.exists():
        print(f'  Skipping (exists): {bdf_dst.name}')
    else:
        copyfile(src, bdf_dst)

    # Read raw once if any of events/channels/json need to be written
    needs_raw = not (events_dst.exists() and channels_dst.exists() and json_dst.exists())
    if not needs_raw:
        return

    raw = mne.io.read_raw_bdf(bdf_dst, preload=True)

    # Events
    if events_dst.exists():
        print(f'  Skipping (exists): {events_dst.name}')
    else:
        events_onset = mne.find_events(raw, min_duration=0.01, output='onset')

        # Read experimental CSV to get stimulus durations
        csv_files = [f for f in os.listdir(raw_data_folder / participant)
                     if 'RovingOddball' in f and f.endswith('.csv')]
        durations_dict = {}
        if csv_files:
            exp_data = pd.read_csv(raw_data_folder / participant / csv_files[0])
            if 'face.started' in exp_data.columns and 'face.stopped' in exp_data.columns:
                face_trials = exp_data[exp_data['trigger_code'].notna()].copy()
                face_trials['duration'] = face_trials['face.stopped'] - face_trials['face.started']
                for _, row in face_trials.iterrows():
                    if pd.notna(row['trigger_code']) and pd.notna(row['duration']):
                        durations_dict[int(row['trigger_code'])] = row['duration']

        events = pd.DataFrame({
            'onset': events_onset[:, 0] / raw.info['sfreq'],
            'duration': [durations_dict.get(int(code), 0.0) for code in events_onset[:, 2]],
            'trial_type': events_onset[:, 2]
        })

        block_dfs = [pd.read_csv(experiment_folder / f'block{i}.csv') for i in range(1, 5)]
        experiment_df = pd.concat(block_dfs, ignore_index=True).drop_duplicates()
        experiment_df['stimulus'] = experiment_df['stimulus'].str.split('/').str[1]
        stimulus_dict = experiment_df.set_index('trigger_code')['stimulus'].to_dict()
        events['stim_file'] = events['trial_type'].map(lambda x: stimulus_dict.get(x, 'n/a'))

        events_bids = events[['onset', 'duration', 'trial_type', 'stim_file']]
        events_bids.to_csv(events_dst, index=False, sep='\t')

    # Channels — protected: may contain manual bad-channel annotations
    if channels_dst.exists():
        print(f'  Skipping (exists, may contain manual annotations): {channels_dst.name}')
    else:
        ch_types = raw.get_channel_types()
        channels_df = pd.DataFrame({
            'name': raw.ch_names,
            'type': ch_types,
            'units': ['µV' if ch_type in ['eeg', 'eog', 'ecg', 'emg'] else 'n/a' for ch_type in ch_types],
            'sampling_frequency': raw.info['sfreq'],
            'low_cutoff': 'n/a',
            'high_cutoff': 'n/a',
            'reference': 'n/a',
            'status': 'good'
        })
        channels_df.to_csv(channels_dst, index=False, sep='\t')

    # JSON sidecar
    if json_dst.exists():
        print(f'  Skipping (exists): {json_dst.name}')
    else:
        ch_types = raw.get_channel_types()
        eeg_count = sum(1 for ch_type in ch_types if ch_type == 'eeg')
        eog_count = sum(1 for ch_type in ch_types if ch_type == 'eog')
        eeg_json = {
            "TaskName": "RovingOddball",
            "SamplingFrequency": raw.info['sfreq'],
            "SoftwareFilters": "n/a",
            "EEGChannelCount": eeg_count,
            "EOGChannelCount": eog_count,
            "EEGReference": "n/a",
            "PowerLineFrequency": 50
        }
        with open(json_dst, 'w') as f:
            json.dump(eeg_json, f, indent=4)


# %%
def main(raw_data_folder, out_folder, experiment_folder):
    raw_data_folder = Path(raw_data_folder)
    out_folder = Path(out_folder)
    experiment_folder = Path(experiment_folder)

    out_folder.mkdir(parents=True, exist_ok=True)
    write_dataset_description(out_folder)

    participant_list = sorted([sub for sub in os.listdir(raw_data_folder) if sub.startswith('sub-')])

    counter = 0
    for participant in participant_list:
        print('Processing participant:', participant)
        process_participant(participant, raw_data_folder, out_folder, experiment_folder)
        counter += 1

    write_participants_tsv(out_folder, participant_list)

    print(f"Processed {counter} participants")
    print(f"BIDS dataset created at: {out_folder}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Organise raw EEG recordings into a BIDS dataset. "
                    "Existing files are never overwritten.")
    parser.add_argument('--raw-data-folder', required=True, type=Path,
                        help='Folder containing sub-* directories with raw .bdf and .csv files.')
    parser.add_argument('--out-folder', required=True, type=Path,
                        help='Destination BIDS folder.')
    parser.add_argument('--experiment-folder', required=True, type=Path,
                        help='Folder containing block1.csv … block4.csv for the RovingOddball experiment.')
    args = parser.parse_args()
    main(args.raw_data_folder, args.out_folder, args.experiment_folder)

# %%
