# %%
import argparse
from pathlib import Path
import mne
import pandas as pd


EMOTIONS = ['angry', 'happy']
REPETITIONS = [1, 2, 3, 4, 5, 6, 7]


def _condition_count(epochs, condition):
    """Number of epochs matching a slash-tag selection, or 0 if absent."""
    try:
        return len(epochs[condition])
    except KeyError:
        return 0


def collect_quality_metrics(processed_folder):
    rows = []
    for subject_dir in sorted(processed_folder.glob('sub-*')):
        subject = subject_dir.name
        epo_file = subject_dir / 'eeg' / f'{subject}_task-RovingOddball_eeg-epo.edf'
        if not epo_file.exists():
            print(f'  Skipping {subject}: no epochs file at {epo_file}')
            continue

        try:
            epochs = mne.read_epochs(epo_file, verbose=False)
        except Exception as exc:
            print(f'  Skipping {subject}: failed to load epochs ({exc})')
            continue

        bad_channels = list(epochs.info['bads'])
        row = {
            'participant_id': subject,
            'n_epochs_retained': len(epochs),
            'n_channels': epochs.info['nchan'],
            'sampling_frequency': epochs.info['sfreq'],
            'n_bad_channels': len(bad_channels),
            'bad_channels': ';'.join(bad_channels),
        }
        for emotion in EMOTIONS:
            row[f'n_epochs_{emotion}'] = _condition_count(epochs, emotion)
        for rep in REPETITIONS:
            row[f'n_epochs_repetition_{rep}'] = _condition_count(epochs, f'repetition/{rep}')
        rows.append(row)
    return pd.DataFrame(rows)


# %%
def main(processed_folder, out_file):
    processed_folder = Path(processed_folder)
    out_file = Path(out_file)

    print(f'Scanning {processed_folder} for processed subjects')
    df = collect_quality_metrics(processed_folder)
    print(f'Collected metrics for {len(df)} subjects')

    out_file.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(out_file, index=False, sep='\t')
    print(f'Wrote {out_file}')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='Aggregate per-subject preprocessing quality metrics into a single TSV.')
    parser.add_argument('--processed-folder', required=True, type=Path,
                        help='Folder containing per-subject sub-* directories with epoched files.')
    parser.add_argument('--out-file', required=True, type=Path,
                        help='Output TSV path.')
    args = parser.parse_args()
    main(args.processed_folder, args.out_file)

# %%
