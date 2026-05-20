# %%
"""
Extract per-participant image rating means (arousal, dominance, valence) for
the angry and happy face stimuli. Outputs a long-format TSV that the R
analysis script (10_ratings_analysis.R) consumes.

Filtering matches script 4 (`>= 50 epochs` per angry/happy x rep 1/5,
`<= 4 bad channels`, `exclude != TRUE`, valid group label), so the rating
sample is identical to the ERP analytic sample.
"""

import argparse
import importlib.util
import sys
import time
import warnings
from pathlib import Path

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


EMOTIONS = ['angry', 'happy']
DIMENSIONS = ['arousal', 'dominance', 'valence']


def print_timestamp(message):
    print(f"[{time.strftime('%H:%M:%S', time.localtime())}] {message}")


def ratings_path(bids_folder, participant_id):
    return (Path(bids_folder) / participant_id / 'beh' /
            f'{participant_id}_task-ImageRatings_beh.tsv')


def per_subject_means(ratings_df):
    """Aggregate trial-level ratings to one mean per (emotion, dimension) cell.

    Returns a DataFrame with columns: emotion, dimension, rating_mean, n_trials.
    Cells with no valid trials are omitted (the R-side `complete_subjects`
    helper drops subjects missing any of the six cells).
    """
    df = ratings_df.copy()
    df['rating'] = pd.to_numeric(df['rating'], errors='coerce')
    df = df.dropna(subset=['rating'])
    df = df[df['emotion'].isin(EMOTIONS) & df['dimension'].isin(DIMENSIONS)]
    if df.empty:
        return pd.DataFrame(columns=['emotion', 'dimension',
                                     'rating_mean', 'n_trials'])
    agg = (df.groupby(['emotion', 'dimension'], observed=True)['rating']
             .agg(rating_mean='mean', n_trials='size')
             .reset_index())
    return agg


def extract_long_table(df, bids_folder):
    rows = []
    missing = []
    for _, sub in df.iterrows():
        pid = sub['participant_id']
        path = ratings_path(bids_folder, pid)
        if not path.exists():
            missing.append(pid)
            warnings.warn(f'Missing ratings file for {pid}: {path}')
            continue
        ratings = pd.read_csv(path, sep='\t', na_values=['n/a'])
        agg = per_subject_means(ratings)
        if agg.empty:
            missing.append(pid)
            warnings.warn(f'No valid ratings for {pid} in {path}')
            continue
        agg.insert(0, 'participant_id', pid)
        agg.insert(1, 'group', sub[GROUP_COL])
        rows.append(agg)

    if not rows:
        return pd.DataFrame(columns=['participant_id', 'group', 'emotion',
                                     'dimension', 'rating_mean', 'n_trials']), missing
    return pd.concat(rows, ignore_index=True), missing


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

    print_timestamp(f'Extracting ratings for {len(df)} participants')
    long_df, missing = extract_long_table(df, bids_folder)
    drop_log['dropped_missing_ratings'] = len(missing)
    drop_log['final_with_ratings'] = long_df['participant_id'].nunique()
    print_timestamp(f'Long table: {len(long_df)} rows; '
                    f'{drop_log["final_with_ratings"]} participants with ratings')

    out_tsv.parent.mkdir(parents=True, exist_ok=True)
    long_df.to_csv(out_tsv, sep='\t', index=False)
    with open(out_tsv, 'a') as f:
        f.write('\n# Methods\n')
        f.write('# Per-participant rating means aggregated across trials '
                'within each (emotion, dimension) cell.\n')
        f.write('# Emotions: ' + ', '.join(EMOTIONS) + '\n')
        f.write('# Dimensions: ' + ', '.join(DIMENSIONS) + '\n')
        f.write(f'# Sample drop log: {drop_log}\n')
        if missing:
            f.write(f"# Participants with missing/empty ratings: "
                    f"{', '.join(missing)}\n")
    print_timestamp(f'Wrote {out_tsv}')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='Extract per-participant image rating means for '
                    'angry/happy faces on arousal, dominance, and valence; '
                    'save as a long-format TSV.'
    )
    parser.add_argument('--participants-tsv', required=True, type=Path)
    parser.add_argument('--qc-tsv', required=True, type=Path)
    parser.add_argument('--bids-folder', required=True, type=Path)
    parser.add_argument('--out-tsv', required=True, type=Path)
    args = parser.parse_args()
    main(args.participants_tsv, args.qc_tsv, args.bids_folder, args.out_tsv)

# %%
