# %%
import argparse
import re
from pathlib import Path
import mne
import pandas as pd


EMOTIONS = ['angry', 'happy']
REPETITIONS = [1, 2, 3, 4, 5, 6, 7]

# Matches the section emitted by add_preprocessing_summary in 2_preprocess_eeg.py:
#   <h4>Bad Channels Detected: N</h4>
#   <p>ch1, ch2, ch3</p>
# Bad channels are interpolated during preprocessing, so they're not flagged in
# the saved epoch file — the HTML report is the only record.
BAD_CHANNELS_RE = re.compile(
    r'Bad Channels Detected:\s*(\d+)\s*</h4>\s*<p>(.*?)</p>',
    re.DOTALL,
)


def _condition_count(epochs, condition):
    """Number of epochs matching a slash-tag selection, or 0 if absent."""
    try:
        return len(epochs[condition])
    except KeyError:
        return 0


def _bad_channels_from_report(report_path):
    """Return the list of bad channels parsed from the HTML report, or None.

    None means the report is missing or the section can't be found; an empty
    list means the report explicitly recorded no bad channels.
    """
    if not report_path.exists():
        return None
    try:
        html = report_path.read_text()
    except OSError:
        return None
    match = BAD_CHANNELS_RE.search(html)
    if not match:
        return None
    text = match.group(2).strip()
    if not text or text == 'None':
        return []
    return [n.strip() for n in text.split(',') if n.strip()]


def collect_quality_metrics(derivatives_folder):
    rows = []
    for subject_dir in sorted(derivatives_folder.glob('sub-*')):
        subject = subject_dir.name
        epo_file = subject_dir / 'eeg' / f'{subject}_task-RovingOddball_eeg-epo.fif.gz'
        report_file = subject_dir / 'eeg' / f'{subject}_task-RovingOddball_eeg.html'
        if not epo_file.exists():
            print(f'  Skipping {subject}: no epochs file at {epo_file}')
            continue

        try:
            epochs = mne.read_epochs(epo_file, verbose=False)
        except Exception as exc:
            print(f'  Skipping {subject}: failed to load epochs ({exc})')
            continue

        bad_names = _bad_channels_from_report(report_file)
        if bad_names is None:
            print(f'  {subject}: no bad-channel section in {report_file.name}; '
                  f'falling back to epochs.info["bads"] (likely empty)')
            bad_names = list(epochs.info['bads'])
        n_bad = len(bad_names)

        row = {
            'participant_id': subject,
            'n_epochs_retained': len(epochs),
            'n_channels': epochs.info['nchan'],
            'sampling_frequency': epochs.info['sfreq'],
            'n_bad_channels': n_bad,
            'bad_channels': ';'.join(bad_names),
        }
        for emotion in EMOTIONS:
            for rep in REPETITIONS:
                row[f'n_{emotion}_rep_{rep}'] = _condition_count(
                    epochs, f'{emotion}/repetition/{rep}'
                )
        rows.append(row)
    return pd.DataFrame(rows)


# %%
def main(derivatives_folder, out_file):
    derivatives_folder = Path(derivatives_folder)
    out_file = Path(out_file)

    print(f'Scanning {derivatives_folder} for processed subjects')
    df = collect_quality_metrics(derivatives_folder)
    print(f'Collected metrics for {len(df)} subjects')

    out_file.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(out_file, index=False, sep='\t')
    print(f'Wrote {out_file}')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='Aggregate per-subject preprocessing quality metrics into a single TSV.')
    parser.add_argument('--derivatives-folder', required=True, type=Path,
                        help='Folder containing per-subject sub-* directories with epoched files.')
    parser.add_argument('--out-file', required=True, type=Path,
                        help='Output TSV path.')
    args = parser.parse_args()
    main(args.derivatives_folder, args.out_file)

# %%
