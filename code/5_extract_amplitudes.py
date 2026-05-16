# %%
"""
Extract mean ERP amplitudes for the three pre-registered spatiotemporal
clusters (Hypersensitivity 1 / 2; Hyperalertness). Outputs a single
long-format TSV that the R analysis script (6_main_analysis.R) consumes.

Filtering matches script 4 (`>= 50 epochs` per angry/happy x rep 1/5,
`<= 4 bad channels`, `exclude != TRUE`, valid group label).
"""

import argparse
import importlib.util
import sys
import time
import warnings
from pathlib import Path

import mne
import numpy as np
import pandas as pd


# ---------------------------------------------------------------------------
# Reuse script-4 helpers (filename starts with a digit -> can't normal-import)
# ---------------------------------------------------------------------------

_THIS_DIR = Path(__file__).resolve().parent
_DEMO_SPEC = importlib.util.spec_from_file_location(
    'demographics_table', _THIS_DIR / '4_demographics_table.py'
)
_DEMO = importlib.util.module_from_spec(_DEMO_SPEC)
sys.modules['demographics_table'] = _DEMO
_DEMO_SPEC.loader.exec_module(_DEMO)

load_and_filter = _DEMO.load_and_filter
GROUP_COL = _DEMO.GROUP_COL


# ---------------------------------------------------------------------------
# Cluster definitions
# ---------------------------------------------------------------------------

EMOTIONS = ['angry', 'happy']
REPETITIONS = [1, 5]

CLUSTERS = [
    {
        'name': 'Hypersensitivity 1 (120-170 ms)',
        'tmin': 0.120, 'tmax': 0.170,
        'channels': ['CP5', 'CP3', 'CP1', 'P1', 'P3', 'P5', 'P7', 'PO7', 'O1',
                     'Oz', 'POz', 'Pz', 'CPz', 'CP4', 'CP2', 'P2', 'P4', 'P6',
                     'PO8', 'PO4', 'O2'],
    },
    {
        'name': 'Hypersensitivity 2 (360-470 ms)',
        'tmin': 0.360, 'tmax': 0.470,
        'channels': ['C4', 'TP8', 'CP6', 'CP4', 'P4', 'P6', 'P8', 'PO8'],
    },
    {
        'name': 'Hyperalertness (480-600 ms)',
        'tmin': 0.480, 'tmax': 0.600,
        'channels': ['POz', 'F8', 'FC6', 'C4', 'C6', 'T8', 'TP8', 'CP6', 'CP4',
                     'CP2', 'P2', 'P4', 'P8', 'P10', 'PO8', 'PO4', 'O2'],
    },
]


def print_timestamp(message):
    print(f"[{time.strftime('%H:%M:%S', time.localtime())}] {message}")


def epoch_path(bids_folder, participant_id):
    return (Path(bids_folder) / 'derivatives' / participant_id / 'eeg' /
            f'{participant_id}_task-RovingOddball_eeg-epo.fif.gz')


def cluster_amplitude(evoked, channels, tmin, tmax):
    """Mean amplitude (uV) over a time x channel window of an Evoked."""
    present = [c for c in channels if c in evoked.ch_names]
    if not present:
        return np.nan
    e = evoked.copy().pick(present).crop(tmin=tmin, tmax=tmax,
                                          include_tmax=True)
    return float(e.get_data().mean() * 1e6)


def extract_long_table(filtered_df, bids_folder):
    """Build a long-format DataFrame of mean cluster amplitudes per subject.

    Columns: participant_id, group, emotion, repetition, cluster, amplitude.
    """
    rows = []
    p10_missing = 0
    n_total = 0

    for _, sub_row in filtered_df.iterrows():
        pid = sub_row['participant_id']
        grp = sub_row[GROUP_COL]
        n_total += 1
        f = epoch_path(bids_folder, pid)
        if not f.is_file():
            warnings.warn(f'{pid}: epoch file not found ({f})')
            for cl in CLUSTERS:
                for em in EMOTIONS:
                    for rep in REPETITIONS:
                        rows.append({'participant_id': pid, 'group': grp,
                                     'emotion': em, 'repetition': rep,
                                     'cluster': cl['name'],
                                     'amplitude': np.nan})
            continue

        try:
            epochs = mne.read_epochs(f, preload=True, verbose='ERROR')
        except Exception as exc:
            warnings.warn(f'{pid}: load error: {exc}')
            for cl in CLUSTERS:
                for em in EMOTIONS:
                    for rep in REPETITIONS:
                        rows.append({'participant_id': pid, 'group': grp,
                                     'emotion': em, 'repetition': rep,
                                     'cluster': cl['name'],
                                     'amplitude': np.nan})
            continue

        if epochs.info['sfreq'] != 250.0:
            warnings.warn(f'{pid}: unexpected sfreq={epochs.info["sfreq"]}')
        for cl in CLUSTERS:
            if not (epochs.tmin - 1e-6 <= cl['tmin'] and
                    epochs.tmax + 1e-6 >= cl['tmax']):
                warnings.warn(
                    f"{pid}: cluster '{cl['name']}' window outside epoch "
                    f"range [{epochs.tmin:.3f}, {epochs.tmax:.3f}]"
                )

        if 'P10' not in epochs.ch_names:
            p10_missing += 1

        evokeds = {}
        for em in EMOTIONS:
            for rep in REPETITIONS:
                key = f'white/{em}/repetition/{rep}'
                evokeds[(em, rep)] = (epochs[key].average()
                                       if key in epochs.event_id else None)

        for cl in CLUSTERS:
            missing_chs = [c for c in cl['channels']
                           if c not in epochs.ch_names]
            if missing_chs:
                warnings.warn(
                    f"{pid}: missing channels for '{cl['name']}': "
                    f"{missing_chs}"
                )
            for em in EMOTIONS:
                for rep in REPETITIONS:
                    ev = evokeds[(em, rep)]
                    if ev is None:
                        amp = np.nan
                        warnings.warn(
                            f'{pid}: condition white/{em}/repetition/{rep} '
                            f'missing from event_id'
                        )
                    else:
                        amp = cluster_amplitude(ev, cl['channels'],
                                                cl['tmin'], cl['tmax'])
                    rows.append({'participant_id': pid, 'group': grp,
                                 'emotion': em, 'repetition': rep,
                                 'cluster': cl['name'],
                                 'amplitude': amp})

    if n_total > 0 and p10_missing / n_total > 0.2:
        warnings.warn(
            f'P10 absent in {p10_missing}/{n_total} subjects '
            f'(>20%); Hyperalertness cluster is using a reduced channel set.'
        )

    return pd.DataFrame(rows)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main(participants_tsv, qc_tsv, bids_folder, out_tsv):
    participants_tsv = Path(participants_tsv)
    qc_tsv = Path(qc_tsv)
    bids_folder = Path(bids_folder)
    out_tsv = Path(out_tsv)

    print_timestamp(f'Loading {participants_tsv} and {qc_tsv}')
    df, drop_log, _ = load_and_filter(participants_tsv, qc_tsv)
    print_timestamp(f'Filter trace: {drop_log}')

    print_timestamp(f'Extracting amplitudes for {len(df)} participants')
    long_df = extract_long_table(df, bids_folder)
    print_timestamp(f'Long table: {len(long_df)} rows')

    out_tsv.parent.mkdir(parents=True, exist_ok=True)
    long_df.to_csv(out_tsv, sep='\t', index=False)
    with open(out_tsv, 'a') as f:
        f.write('\n# Methods\n')
        for cl in CLUSTERS:
            f.write(f"# Cluster '{cl['name']}': time {cl['tmin']*1000:.0f}"
                    f"-{cl['tmax']*1000:.0f} ms; channels="
                    f"{', '.join(cl['channels'])}\n")
        f.write(f"# Sample drop log: {drop_log}\n")
    print_timestamp(f'Wrote {out_tsv}')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='Extract mean ERP amplitudes for three pre-registered '
                    'spatiotemporal clusters and save as a long-format TSV.'
    )
    parser.add_argument('--participants-tsv', required=True, type=Path)
    parser.add_argument('--qc-tsv', required=True, type=Path)
    parser.add_argument('--bids-folder', required=True, type=Path)
    parser.add_argument('--out-tsv', required=True, type=Path)
    args = parser.parse_args()
    main(args.participants_tsv, args.qc_tsv, args.bids_folder, args.out_tsv)

# %%
