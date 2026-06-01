# %%
"""
Extract within-subject DIFFERENCE waves and difference amplitudes for the three
pre-registered spatiotemporal clusters, computed per individual via MNE's
contrast function (`mne.combine_evoked([hi, lo], weights=[1, -1])`).

Each contrast is collapsed to a single difference Evoked PER SUBJECT before any
group averaging, so the figure script (8b_figure_difference_waves.py) only needs
to average these per-subject differences within group. Two contrasts:

  - emotion    (fixed repetition 1): angry - happy
  - repetition (fixed emotion angry): rep 1 - rep 5

Outputs two long-format TSVs:
  - difference waveforms  (one row per subject x contrast x cluster x timepoint)
  - difference amplitudes (one row per subject x contrast x cluster)

Reuses the QC filter, cluster definitions and extraction helpers from
5_extract_amplitudes.py and 7_extract_erp_waveforms.py so the differences
describe the same analytic sample as the scalar amplitudes and per-condition
waveforms.
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
# Reuse helpers from scripts 5 and 7 (filenames start with a digit -> can't
# normal-import). Cluster definitions / sample filter live in script 5.
# ---------------------------------------------------------------------------

_THIS_DIR = Path(__file__).resolve().parent


def _load_sibling(module_name, filename):
    spec = importlib.util.spec_from_file_location(module_name,
                                                  _THIS_DIR / filename)
    module = importlib.util.module_from_spec(spec)
    sys.modules[module_name] = module
    spec.loader.exec_module(module)
    return module


_AMP = _load_sibling('extract_amplitudes', '5_extract_amplitudes.py')
_WAVE = _load_sibling('extract_erp_waveforms', '7_extract_erp_waveforms.py')

CLUSTERS = _AMP.CLUSTERS
EMOTIONS = _AMP.EMOTIONS
REPETITIONS = _AMP.REPETITIONS
cluster_amplitude = _AMP.cluster_amplitude
epoch_path = _AMP.epoch_path
load_and_filter = _AMP.load_and_filter
GROUP_COL = _AMP.GROUP_COL
print_timestamp = _AMP.print_timestamp
cluster_waveform = _WAVE.cluster_waveform


# ---------------------------------------------------------------------------
# Contrast definitions
# Each contrast is (high_level - low_level), matching the dashed-minus-solid
# encoding of the parent figure. Levels are (emotion, repetition) condition keys.
# ---------------------------------------------------------------------------

CONTRASTS = [
    {'name': 'emotion',    'hi': ('angry', 1), 'lo': ('happy', 1)},
    {'name': 'repetition', 'hi': ('angry', 1), 'lo': ('angry', 5)},
]


# ---------------------------------------------------------------------------
# Difference extraction
# ---------------------------------------------------------------------------

def extract_difference_tables(filtered_df, bids_folder):
    """Build long-format difference waveform and amplitude tables.

    Per subject: average epochs into per-condition Evokeds, form each contrast's
    difference Evoked via `mne.combine_evoked(..., weights=[1, -1])`, then read
    cluster-mean waveforms and window amplitudes off the difference Evoked.

    Returns (wave_df, amp_df).
        wave_df columns: participant_id, group, contrast, cluster, time_ms, diff_uv
        amp_df  columns: participant_id, group, contrast, cluster, diff_amplitude
    """
    wave_rows = []
    amp_rows = []

    for _, sub_row in filtered_df.iterrows():
        pid = sub_row['participant_id']
        grp = sub_row[GROUP_COL]
        f = epoch_path(bids_folder, pid)
        if not f.is_file():
            warnings.warn(f'{pid}: epoch file not found ({f})')
            continue

        try:
            epochs = mne.read_epochs(f, preload=True, verbose='ERROR')
        except Exception as exc:
            warnings.warn(f'{pid}: load error: {exc}')
            continue

        evokeds = {}
        for em in EMOTIONS:
            for rep in REPETITIONS:
                key = f'white/{em}/repetition/{rep}'
                evokeds[(em, rep)] = (epochs[key].average()
                                       if key in epochs.event_id else None)

        for con in CONTRASTS:
            ev_hi = evokeds[con['hi']]
            ev_lo = evokeds[con['lo']]
            if ev_hi is None or ev_lo is None:
                warnings.warn(
                    f"{pid}: contrast '{con['name']}' skipped "
                    f"(missing {con['hi'] if ev_hi is None else con['lo']})"
                )
                continue

            diff = mne.combine_evoked([ev_hi, ev_lo], weights=[1, -1])

            for cl in CLUSTERS:
                result = cluster_waveform(diff, cl['channels'])
                if result is None:
                    warnings.warn(
                        f"{pid}: no cluster channels present for "
                        f"'{cl['name']}' (contrast '{con['name']}')"
                    )
                    continue
                wave_uv, times_s = result
                times_ms = np.round(times_s * 1000.0, 1)
                wave_rows.append(pd.DataFrame({
                    'participant_id': pid,
                    'group': grp,
                    'contrast': con['name'],
                    'cluster': cl['name'],
                    'time_ms': times_ms,
                    'diff_uv': wave_uv,
                }))

                amp = cluster_amplitude(diff, cl['channels'],
                                        cl['tmin'], cl['tmax'])
                amp_rows.append({
                    'participant_id': pid,
                    'group': grp,
                    'contrast': con['name'],
                    'cluster': cl['name'],
                    'diff_amplitude': amp,
                })

    wave_cols = ['participant_id', 'group', 'contrast', 'cluster',
                 'time_ms', 'diff_uv']
    amp_cols = ['participant_id', 'group', 'contrast', 'cluster',
                'diff_amplitude']
    wave_df = (pd.concat(wave_rows, ignore_index=True)
               if wave_rows else pd.DataFrame(columns=wave_cols))
    amp_df = (pd.DataFrame(amp_rows)
              if amp_rows else pd.DataFrame(columns=amp_cols))
    return wave_df, amp_df


def _write_methods_footer(path, drop_log):
    with open(path, 'a') as f:
        f.write('\n# Methods\n')
        f.write('# Within-subject difference Evokeds via '
                'mne.combine_evoked(weights=[1, -1]).\n')
        for con in CONTRASTS:
            f.write(f"# Contrast '{con['name']}': {con['hi']} - {con['lo']} "
                    f"(emotion, repetition)\n")
        for cl in CLUSTERS:
            f.write(f"# Cluster '{cl['name']}': time {cl['tmin']*1000:.0f}"
                    f"-{cl['tmax']*1000:.0f} ms; channels="
                    f"{', '.join(cl['channels'])}\n")
        f.write(f"# Sample drop log: {drop_log}\n")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main(participants_tsv, qc_tsv, bids_folder, out_waveforms_tsv,
         out_amplitudes_tsv):
    participants_tsv = Path(participants_tsv)
    qc_tsv = Path(qc_tsv)
    bids_folder = Path(bids_folder)
    out_waveforms_tsv = Path(out_waveforms_tsv)
    out_amplitudes_tsv = Path(out_amplitudes_tsv)

    print_timestamp(f'Loading {participants_tsv} and {qc_tsv}')
    df, drop_log, _ = load_and_filter(participants_tsv, qc_tsv)
    print_timestamp(f'Filter trace: {drop_log}')

    print_timestamp(f'Extracting difference waves for {len(df)} participants')
    wave_df, amp_df = extract_difference_tables(df, bids_folder)
    print_timestamp(f'Difference waveform table: {len(wave_df)} rows; '
                    f'amplitude table: {len(amp_df)} rows')

    out_waveforms_tsv.parent.mkdir(parents=True, exist_ok=True)
    wave_df.to_csv(out_waveforms_tsv, sep='\t', index=False,
                   float_format='%.6f')
    _write_methods_footer(out_waveforms_tsv, drop_log)
    print_timestamp(f'Wrote {out_waveforms_tsv}')

    out_amplitudes_tsv.parent.mkdir(parents=True, exist_ok=True)
    amp_df.to_csv(out_amplitudes_tsv, sep='\t', index=False)
    _write_methods_footer(out_amplitudes_tsv, drop_log)
    print_timestamp(f'Wrote {out_amplitudes_tsv}')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='Extract within-subject difference waves and amplitudes '
                    'for the three pre-registered clusters via MNE contrasts.'
    )
    parser.add_argument('--participants-tsv', required=True, type=Path)
    parser.add_argument('--qc-tsv', required=True, type=Path)
    parser.add_argument('--bids-folder', required=True, type=Path)
    parser.add_argument('--out-waveforms-tsv', required=True, type=Path)
    parser.add_argument('--out-amplitudes-tsv', required=True, type=Path)
    args = parser.parse_args()
    main(args.participants_tsv, args.qc_tsv, args.bids_folder,
         args.out_waveforms_tsv, args.out_amplitudes_tsv)

# %%
