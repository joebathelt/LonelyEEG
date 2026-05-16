# %%
import argparse
import os
import re
import shutil
import mne
import pandas as pd
import json
from pathlib import Path
from shutil import copyfile


ANON_FMT = 'sub-{:02d}'
MAPPING_FILENAME = 'participants_id_map.tsv'
RATING_RE = re.compile(r"SAM_([1-5])")
RATING_COL_CANDIDATES = ("mouse.clicked_name",
                         "dimensions.mouse.clicked_name")


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


def write_image_ratings_sidecar(outfolder):
    target = outfolder / 'task-ImageRatings_beh.json'
    if target.exists():
        print(f'  Skipping (exists): {target.name}')
        return
    sidecar = {
        "TaskName": "ImageRatings",
        "Instructions": "Participants rated face stimuli on three SAM "
                        "(Self-Assessment Manikin) dimensions: valence, "
                        "arousal, and dominance.",
        "stim_file": {
            "Description": "Image presented on the trial."
        },
        "emotion": {
            "Description": "Emotional expression of the face stimulus.",
            "Levels": {"happy": "Happy face", "angry": "Angry face"}
        },
        "dimension": {
            "Description": "SAM dimension rated on this trial.",
            "Levels": {
                "valence": "How happy or unhappy does this image make you feel?",
                "arousal": "How excited or calm does this image make you feel?",
                "dominance": "How controlled or in control does this image make you feel?"
            }
        },
        "rating": {
            "Description": "Selected SAM manikin (1–5 Likert), parsed "
                           "from PsychoPy mouse.clicked_name.",
            "Levels": {"1": "manikin 1", "2": "manikin 2", "3": "manikin 3",
                       "4": "manikin 4", "5": "manikin 5"}
        }
    }
    with open(target, 'w') as f:
        json.dump(sidecar, f, indent=4)


def process_image_ratings(orig_id, anon_id, raw_data_folder, outfolder,
                          sourcedata_folder):
    """Convert the PsychoPy ImageRatings CSV to a BIDS-compliant beh.tsv.

    Looks for *_ImageRatings_*.csv in raw_data/<orig_id>/ first, then in
    sourcedata/<orig_id>/. Skips silently if the target TSV already exists.
    """
    beh_folder = outfolder / anon_id / 'beh'
    target = beh_folder / f'{anon_id}_task-ImageRatings_beh.tsv'
    if target.exists():
        print(f'  Skipping (exists): {target.name}')
        return

    csv_path = None
    for folder in (raw_data_folder / orig_id, sourcedata_folder / orig_id):
        if not folder.exists():
            continue
        matches = sorted(folder.glob('*_ImageRatings_*.csv'))
        if matches:
            csv_path = matches[0]
            break
    if csv_path is None:
        print(f'  No ImageRatings CSV for {orig_id}, skipping ratings')
        return

    df = pd.read_csv(csv_path)
    rating_col = next((c for c in RATING_COL_CANDIDATES if c in df.columns),
                      None)
    if rating_col is None:
        raise ValueError(
            f'{csv_path.name} has no SAM click column; '
            f'expected one of {RATING_COL_CANDIDATES}')

    def parse_rating(v):
        if pd.isna(v):
            return 'n/a'
        m = RATING_RE.search(str(v))
        if m is None:
            raise ValueError(
                f'Unexpected SAM value in {csv_path.name}: {v!r}')
        return int(m.group(1))

    out = pd.DataFrame({
        'stim_file': df['imagefile'],
        'emotion': df['emotion'],
        'dimension': df['dimension'],
        'rating': [parse_rating(v) for v in df[rating_col]],
    })

    beh_folder.mkdir(parents=True, exist_ok=True)
    out.to_csv(target, sep='\t', index=False, na_rep='n/a')
    print(f'  Wrote {target.relative_to(outfolder)}')


def write_participants_tsv(outfolder, participant_list):
    target = outfolder / 'participants.tsv'
    # Protected: may contain manual annotations
    if target.exists():
        print(f'  Skipping (exists, may contain manual annotations): {target.name}')
        return
    participants_df = pd.DataFrame({'participant_id': sorted(set(participant_list))})
    participants_df.to_csv(target, index=False, sep='\t')


def load_or_extend_mapping(sourcedata_folder, raw_participants):
    """Return {original_id: anonymous_id} for all known + new participants.

    Loads existing mapping from sourcedata_folder/MAPPING_FILENAME if present,
    assigns the next free sub-NN to any raw_participants not yet mapped
    (alphabetical order), and writes the updated mapping back atomically.
    """
    map_path = sourcedata_folder / MAPPING_FILENAME
    if map_path.exists():
        existing = pd.read_csv(map_path, sep='\t')
        mapping = dict(zip(existing['original_id'], existing['anonymous_id']))
    else:
        mapping = {}

    used_numbers = {int(anon.split('-')[1]) for anon in mapping.values()}
    next_n = 1
    for orig in sorted(raw_participants):
        if orig in mapping:
            continue
        while next_n in used_numbers:
            next_n += 1
        mapping[orig] = ANON_FMT.format(next_n)
        used_numbers.add(next_n)

    sourcedata_folder.mkdir(parents=True, exist_ok=True)
    map_df = pd.DataFrame(
        sorted(((anon, orig) for orig, anon in mapping.items()),
               key=lambda x: x[0]),
        columns=['anonymous_id', 'original_id'],
    )
    tmp_path = map_path.with_suffix(map_path.suffix + '.tmp')
    map_df.to_csv(tmp_path, index=False, sep='\t')
    tmp_path.replace(map_path)
    return mapping


def process_participant(orig_id, anon_id, raw_data_folder, outfolder,
                        sourcedata_folder, experiment_folder):
    raw_subject_folder = raw_data_folder / orig_id
    subject_eeg_folder = outfolder / anon_id / 'eeg'

    bdf_dst = subject_eeg_folder / f'{anon_id}_task-RovingOddball_eeg.bdf'
    events_dst = subject_eeg_folder / f'{anon_id}_task-RovingOddball_events.tsv'
    channels_dst = subject_eeg_folder / f'{anon_id}_task-RovingOddball_channels.tsv'
    json_dst = subject_eeg_folder / f'{anon_id}_task-RovingOddball_eeg.json'

    all_present = all(p.exists() for p in (bdf_dst, events_dst, channels_dst, json_dst))

    if all_present:
        print(f'  All BIDS files already present for {anon_id} ({orig_id}), skipping conversion')
        archive_raw_to_sourcedata(raw_subject_folder, sourcedata_folder / orig_id)
        return

    if not raw_subject_folder.exists():
        print(f'  No raw folder for {orig_id} and BIDS incomplete; skipping')
        return

    bdf_files = [f for f in os.listdir(raw_subject_folder) if f.endswith('.bdf')]
    if not bdf_files:
        print(f'  No BDF file found for {orig_id}, skipping')
        return

    subject_eeg_folder.mkdir(parents=True, exist_ok=True)

    # Copy raw BDF file to BIDS structure
    src = raw_subject_folder / bdf_files[0]
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
        csv_files = [f for f in os.listdir(raw_subject_folder)
                     if 'RovingOddball' in f and f.endswith('.csv')]
        durations_dict = {}
        if csv_files:
            exp_data = pd.read_csv(raw_subject_folder / csv_files[0])
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

    # All four BIDS files are now in place; archive the raw folder.
    archive_raw_to_sourcedata(raw_subject_folder, sourcedata_folder / orig_id)


def archive_raw_to_sourcedata(src, dst):
    """Move src raw subject folder to dst inside sourcedata. No-op if src
    is gone (already archived) or dst already exists."""
    if not src.exists():
        return
    if dst.exists():
        print(f'  Sourcedata folder already exists, leaving raw in place: {dst}')
        return
    dst.parent.mkdir(parents=True, exist_ok=True)
    shutil.move(str(src), str(dst))
    print(f'  Archived {src.name} -> sourcedata/{dst.name}')


# %%
def main(raw_data_folder, out_folder, experiment_folder):
    raw_data_folder = Path(raw_data_folder)
    out_folder = Path(out_folder)
    experiment_folder = Path(experiment_folder)

    out_folder.mkdir(parents=True, exist_ok=True)
    sourcedata_folder = out_folder / 'sourcedata'
    sourcedata_folder.mkdir(parents=True, exist_ok=True)
    write_dataset_description(out_folder)
    write_image_ratings_sidecar(out_folder)

    if raw_data_folder.exists():
        raw_participants = sorted([sub for sub in os.listdir(raw_data_folder)
                                   if sub.startswith('sub-')
                                   and (raw_data_folder / sub).is_dir()])
    else:
        raw_participants = []
        print(f'No raw_data folder at {raw_data_folder}; '
              'processing archived participants only.')

    mapping = load_or_extend_mapping(sourcedata_folder, raw_participants)

    archived = [d.name for d in sourcedata_folder.iterdir()
                if d.is_dir() and d.name.startswith('sub-')
                and d.name in mapping]
    all_ids = sorted(set(raw_participants) | set(archived))

    if not all_ids:
        print('No participants to process.')
        return

    counter = 0
    for orig_id in all_ids:
        anon_id = mapping[orig_id]
        print(f'Processing participant: {orig_id} -> {anon_id}')
        process_participant(orig_id, anon_id, raw_data_folder, out_folder,
                            sourcedata_folder, experiment_folder)
        process_image_ratings(orig_id, anon_id, raw_data_folder,
                              out_folder, sourcedata_folder)
        counter += 1

    write_participants_tsv(out_folder, sorted(mapping.values()))

    print(f"Processed {counter} participants")
    print(f"BIDS dataset created at: {out_folder}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Organise raw EEG recordings into a BIDS dataset with "
                    "anonymous participant IDs. Existing files are never "
                    "overwritten. Raw subject folders are moved into "
                    "<out-folder>/sourcedata/ after conversion.")
    parser.add_argument('--raw-data-folder', required=True, type=Path,
                        help='Folder containing sub-* directories with raw '
                             '.bdf and .csv files. Subject folders are moved '
                             'into <out-folder>/sourcedata/ after conversion.')
    parser.add_argument('--out-folder', required=True, type=Path,
                        help='Destination BIDS folder.')
    parser.add_argument('--experiment-folder', required=True, type=Path,
                        help='Folder containing block1.csv … block4.csv for the RovingOddball experiment.')
    args = parser.parse_args()
    main(args.raw_data_folder, args.out_folder, args.experiment_folder)

# %%
