# %%
"""
Extract mean ERP amplitudes for the three pre-registered spatiotemporal
clusters across repetitions 1..6 per emotion. The 7th repetition is
excluded because it has too few trials and is too noisy to contribute
reliable per-participant means. Outputs a long-format TSV consumed
downstream by `13_repetition_trends_analysis.R`, which fits linear
mixed-effects models with repetition as a continuous predictor.

Reuses the cluster definitions and helpers from `5_extract_amplitudes.py`;
the only behavioural difference is REPETITIONS = list(range(1, 7)). The
analytic-sample filter is identical (passed via `load_and_filter` from
`4_demographics_table.py`).
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
# Reuse helpers from sibling scripts (filenames start with a digit -> can't
# normal-import).
# ---------------------------------------------------------------------------

_THIS_DIR = Path(__file__).resolve().parent


def _load_sibling(module_name, file_name):
    spec = importlib.util.spec_from_file_location(
        module_name, _THIS_DIR / file_name
    )
    mod = importlib.util.module_from_spec(spec)
    sys.modules[module_name] = mod
    spec.loader.exec_module(mod)
    return mod


_DEMO = _load_sibling('demographics_table', '4_demographics_table.py')
_EXTRACT = _load_sibling('extract_amplitudes', '5_extract_amplitudes.py')

load_and_filter = _DEMO.load_and_filter
GROUP_COL = _DEMO.GROUP_COL
CLUSTERS = _EXTRACT.CLUSTERS
cluster_amplitude = _EXTRACT.cluster_amplitude
epoch_path = _EXTRACT.epoch_path
print_timestamp = _EXTRACT.print_timestamp


EMOTIONS = ['angry', 'happy']
REPETITIONS = list(range(1, 7))  # 1..6 (rep 7 excluded: too few trials/too noisy)


def extract_long_table(filtered_df, bids_folder):
    """Build a long-format DataFrame of mean cluster amplitudes per subject
    across repetitions 1..6.

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

    print_timestamp(f'Extracting all-rep amplitudes for {len(df)} participants')
    long_df = extract_long_table(df, bids_folder)
    print_timestamp(f'Long table: {len(long_df)} rows')

    out_tsv.parent.mkdir(parents=True, exist_ok=True)
    long_df.to_csv(out_tsv, sep='\t', index=False)
    with open(out_tsv, 'a') as f:
        f.write('\n# Methods\n')
        f.write(f"# Repetitions extracted: {REPETITIONS}\n")
        for cl in CLUSTERS:
            f.write(f"# Cluster '{cl['name']}': time {cl['tmin']*1000:.0f}"
                    f"-{cl['tmax']*1000:.0f} ms; channels="
                    f"{', '.join(cl['channels'])}\n")
        f.write(f"# Sample drop log: {drop_log}\n")
    print_timestamp(f'Wrote {out_tsv}')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='Extract mean ERP amplitudes for three pre-registered '
                    'spatiotemporal clusters across repetitions 1..6 and '
                    'save as a long-format TSV.'
    )
    parser.add_argument('--participants-tsv', required=True, type=Path)
    parser.add_argument('--qc-tsv', required=True, type=Path)
    parser.add_argument('--bids-folder', required=True, type=Path)
    parser.add_argument('--out-tsv', required=True, type=Path)
    args = parser.parse_args()
    main(args.participants_tsv, args.qc_tsv, args.bids_folder, args.out_tsv)

# %%
