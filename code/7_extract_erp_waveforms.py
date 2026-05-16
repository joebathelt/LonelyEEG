# %%
"""
Extract cluster-mean ERP waveforms (time-resolved, one trace per subject x
condition x cluster) and 2D channel positions, for the figure built by
`8_figure_erps.py`.

Mirrors the QC filter and cluster definitions used by `5_extract_amplitudes.py`
so the waveforms and the scalar amplitudes describe the same analytic sample.
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
# Source of truth: 5_extract_amplitudes.py. Keep in sync if changed there.
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


# ---------------------------------------------------------------------------
# Waveform extraction
# ---------------------------------------------------------------------------

def cluster_waveform(evoked, channels):
    """Channel-averaged 1D ERP (uV) over the cluster's channels."""
    present = [c for c in channels if c in evoked.ch_names]
    if not present:
        return None
    e = evoked.copy().pick(present)
    return e.get_data().mean(axis=0) * 1e6, e.times  # (n_times,), seconds


def extract_waveforms(filtered_df, bids_folder):
    """Return one row per (subject, condition, cluster, timepoint)."""
    rows = []
    n_total = 0
    first_info = None

    for _, sub_row in filtered_df.iterrows():
        pid = sub_row['participant_id']
        grp = sub_row[GROUP_COL]
        n_total += 1
        f = epoch_path(bids_folder, pid)
        if not f.is_file():
            warnings.warn(f'{pid}: epoch file not found ({f})')
            continue

        try:
            epochs = mne.read_epochs(f, preload=True, verbose='ERROR')
        except Exception as exc:
            warnings.warn(f'{pid}: load error: {exc}')
            continue

        if first_info is None:
            first_info = epochs.info.copy()

        evokeds = {}
        for em in EMOTIONS:
            for rep in REPETITIONS:
                key = f'white/{em}/repetition/{rep}'
                evokeds[(em, rep)] = (epochs[key].average()
                                       if key in epochs.event_id else None)

        for cl in CLUSTERS:
            for em in EMOTIONS:
                for rep in REPETITIONS:
                    ev = evokeds[(em, rep)]
                    if ev is None:
                        warnings.warn(
                            f'{pid}: condition white/{em}/repetition/{rep} '
                            f'missing from event_id'
                        )
                        continue
                    result = cluster_waveform(ev, cl['channels'])
                    if result is None:
                        warnings.warn(
                            f"{pid}: no cluster channels present for "
                            f"'{cl['name']}'"
                        )
                        continue
                    wave_uv, times_s = result
                    times_ms = np.round(times_s * 1000.0, 1)
                    rows.append(pd.DataFrame({
                        'participant_id': pid,
                        'group': grp,
                        'emotion': em,
                        'repetition': rep,
                        'cluster': cl['name'],
                        'time_ms': times_ms,
                        'amplitude_uv': wave_uv,
                    }))

    if not rows:
        return pd.DataFrame(columns=['participant_id', 'group', 'emotion',
                                     'repetition', 'cluster', 'time_ms',
                                     'amplitude_uv']), first_info
    return pd.concat(rows, ignore_index=True), first_info


# ---------------------------------------------------------------------------
# Channel positions (for topomap insets in the figure)
# ---------------------------------------------------------------------------

def project_3d_to_2d(xyz):
    """Azimuthal projection of 3D head coordinates to a 2D disk.

    Convention: x=R, y=A, z=S (RAS+, MNE head coords). After projection, the
    nose (anterior) ends up at +y in the figure, +x is the participant's right.
    Channels at the vertex sit near (0, 0); channels at the equator land near
    the unit circle.
    """
    x, y, z = xyz
    r = float(np.linalg.norm([x, y, z]))
    if r == 0:
        return 0.0, 0.0
    theta = float(np.arccos(np.clip(z / r, -1.0, 1.0)))  # polar from vertex
    phi = float(np.arctan2(y, x))                        # azimuth in xy
    rho = theta / (np.pi / 2.0)
    return rho * np.cos(phi), rho * np.sin(phi)


def channel_positions_table(info):
    """Build the channel position TSV from an Epochs info.

    Returns a DataFrame with columns: channel, x, y, and a boolean
    in_cluster_<k> for each cluster (1-indexed in the column name).
    """
    montage = info.get_montage()
    if montage is None:
        warnings.warn('No montage found on info; applying standard_1005 '
                      'as a fallback for topomap positions.')
        montage = mne.channels.make_standard_montage('standard_1005')

    ch_pos = montage.get_positions()['ch_pos']  # name -> (3,) array

    rows = []
    eeg_picks = mne.pick_types(info, eeg=True, exclude=[])
    eeg_names = [info['ch_names'][i] for i in eeg_picks]

    for name in eeg_names:
        if name not in ch_pos:
            continue
        xyz = ch_pos[name]
        if not np.all(np.isfinite(xyz)):
            continue
        x, y = project_3d_to_2d(xyz)
        row = {'channel': name, 'x': x, 'y': y}
        for k, cl in enumerate(CLUSTERS, start=1):
            row[f'in_cluster_{k}'] = name in cl['channels']
        rows.append(row)

    return pd.DataFrame(rows)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main(participants_tsv, qc_tsv, bids_folder, out_waveforms_tsv,
         out_channels_tsv):
    participants_tsv = Path(participants_tsv)
    qc_tsv = Path(qc_tsv)
    bids_folder = Path(bids_folder)
    out_waveforms_tsv = Path(out_waveforms_tsv)
    out_channels_tsv = Path(out_channels_tsv)

    print_timestamp(f'Loading {participants_tsv} and {qc_tsv}')
    df, drop_log, _ = load_and_filter(participants_tsv, qc_tsv)
    print_timestamp(f'Filter trace: {drop_log}')

    print_timestamp(f'Extracting waveforms for {len(df)} participants')
    wave_df, first_info = extract_waveforms(df, bids_folder)
    print_timestamp(f'Waveform table: {len(wave_df)} rows')

    out_waveforms_tsv.parent.mkdir(parents=True, exist_ok=True)
    wave_df.to_csv(out_waveforms_tsv, sep='\t', index=False,
                   float_format='%.6f')
    with open(out_waveforms_tsv, 'a') as f:
        f.write('\n# Methods\n')
        for cl in CLUSTERS:
            f.write(f"# Cluster '{cl['name']}': time {cl['tmin']*1000:.0f}"
                    f"-{cl['tmax']*1000:.0f} ms; channels="
                    f"{', '.join(cl['channels'])}\n")
        f.write(f"# Sample drop log: {drop_log}\n")
    print_timestamp(f'Wrote {out_waveforms_tsv}')

    if first_info is None:
        raise RuntimeError(
            'No epoch file was loaded; cannot build channel positions.'
        )
    chan_df = channel_positions_table(first_info)
    out_channels_tsv.parent.mkdir(parents=True, exist_ok=True)
    chan_df.to_csv(out_channels_tsv, sep='\t', index=False,
                   float_format='%.6f')
    print_timestamp(f'Wrote {out_channels_tsv} ({len(chan_df)} channels)')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='Extract cluster-mean ERP waveforms and 2D channel '
                    'positions for the cluster figure.'
    )
    parser.add_argument('--participants-tsv', required=True, type=Path)
    parser.add_argument('--qc-tsv', required=True, type=Path)
    parser.add_argument('--bids-folder', required=True, type=Path)
    parser.add_argument('--out-waveforms-tsv', required=True, type=Path)
    parser.add_argument('--out-channels-tsv', required=True, type=Path)
    args = parser.parse_args()
    main(args.participants_tsv, args.qc_tsv, args.bids_folder,
         args.out_waveforms_tsv, args.out_channels_tsv)

# %%
