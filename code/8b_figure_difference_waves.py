# %%
"""
Companion to 8_figure_erps.py: a 3 (cluster) x 2 (comparison) grid of
within-subject DIFFERENCE WAVES.

Where 8_figure_erps.py overlays the two contrast conditions per group, this
figure collapses each contrast into a single per-subject difference and plots
one difference trace per group (Lonely vs Non-Lonely) with SE shading:

  - Left column  (1st repetition): angry - happy
  - Right column (angry trials):   1st rep - 5th rep

The differences are computed per individual upstream
(5b_extract_difference_waves.py, via mne.combine_evoked), so this figure only
averages the per-subject differences within group. A separation between the two
group traces directly visualizes the loneliness x condition interaction. Each
panel keeps the same two insets as the parent figure: the cluster topomap (left
column) and a raincloud of the per-subject amplitude differences per group.

Inputs:
  - results/cluster_difference_waveforms.tsv  (from 5b_extract_difference_waves.py)
  - results/cluster_difference_amplitudes.tsv (from 5b_extract_difference_waves.py)
  - results/cluster_channel_positions.tsv     (from 7_extract_erp_waveforms.py)
"""

import argparse
import re
from pathlib import Path

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
from matplotlib.lines import Line2D
from matplotlib.patches import Patch
from scipy.signal import butter, filtfilt


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

GROUP_COLORS = {
    'Non-Lonely': '#A4002A99',   # Lancet red
    'Lonely': '#00468B99',       # Lancet blue
}
WINDOW_FACECOLOR = '#D3D3D3'  # Light grey, alpha applied separately for visibility
WINDOW_ALPHA = 0.55

# Display-only low-pass filter for difference traces (zero-phase Butterworth).
DISPLAY_LOWPASS_HZ = 20.0

# Each column maps to a `contrast` value emitted by 5b_extract_difference_waves.py
# (angry - happy at rep 1; rep 1 - rep 5 for angry). Display metadata only --
# the per-subject differencing happens upstream.
DIFF_BY_COLUMN = {
    'emotion': {
        'header': 'Emotion (1st repetition)',
        'diff_label': 'angry − happy',
    },
    'repetition': {
        'header': 'Repetition (angry trials)',
        'diff_label': 'rep 1 − rep 5',
    },
}

COLUMN_ORDER = ['emotion', 'repetition']
GROUP_ORDER = ['Lonely', 'Non-Lonely']


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def short_cluster_name(full_name):
    return re.sub(r'\s*\(.*\)\s*$', '', full_name)


def cluster_time_window_ms(full_name):
    m = re.search(r'\((\d+)\s*-\s*(\d+)\s*ms\)', full_name)
    if not m:
        raise ValueError(f'Cannot parse time window from: {full_name!r}')
    return int(m.group(1)), int(m.group(2))


def display_lowpass(y, time_ms, cutoff_hz=DISPLAY_LOWPASS_HZ, order=4):
    """Zero-phase Butterworth low-pass for plot smoothing only."""
    if len(y) < 3 * order:
        return y
    dt_ms = float(np.median(np.diff(time_ms)))
    fs = 1000.0 / dt_ms
    nyq = fs / 2.0
    wn = min(cutoff_hz / nyq, 0.99)
    b, a = butter(order, wn, btype='low')
    return filtfilt(b, a, y)


def load_tables(diff_waveforms_tsv, diff_amplitudes_tsv, channels_tsv):
    diff_waves = pd.read_csv(diff_waveforms_tsv, sep='\t', comment='#')
    diff_amps = pd.read_csv(diff_amplitudes_tsv, sep='\t', comment='#')
    chans = pd.read_csv(channels_tsv, sep='\t')
    for df in (diff_waves, diff_amps):
        df['contrast'] = df['contrast'].astype(str)
        df['group'] = df['group'].astype(str)
    return diff_waves, diff_amps, chans


# ---------------------------------------------------------------------------
# Group averaging of the per-subject differences (computed upstream in 5b)
# ---------------------------------------------------------------------------

def difference_wave_stats(diff_waves, cluster_name, contrast):
    """Group-mean +/- SE difference wave per time point."""
    sub = diff_waves[(diff_waves['cluster'] == cluster_name) &
                     (diff_waves['contrast'] == contrast)]
    sub = sub.dropna(subset=['diff_uv'])
    if sub.empty:
        return pd.DataFrame(columns=['group', 'time_ms', 'mean', 'sem'])
    return (sub.groupby(['group', 'time_ms'])['diff_uv']
               .agg(['mean', 'sem']).reset_index())


def difference_amplitudes(diff_amps, cluster_name, contrast):
    """Per-subject amplitude difference (one value per subject) for raincloud."""
    sub = diff_amps[(diff_amps['cluster'] == cluster_name) &
                    (diff_amps['contrast'] == contrast)]
    sub = sub.dropna(subset=['diff_amplitude'])
    if sub.empty:
        return pd.DataFrame(columns=['group', 'diff'])
    return sub[['group', 'diff_amplitude']].rename(
        columns={'diff_amplitude': 'diff'})


# ---------------------------------------------------------------------------
# Plot pieces
# ---------------------------------------------------------------------------

def plot_difference_panel(ax, stats, cluster_name):
    """One difference trace (mean +/- SE) per group."""
    tmin_ms, tmax_ms = cluster_time_window_ms(cluster_name)
    ax.axvspan(tmin_ms, tmax_ms, color=WINDOW_FACECOLOR,
               alpha=WINDOW_ALPHA, zorder=0, lw=0)
    ax.axhline(0, color='black', lw=0.5, zorder=1)
    ax.axvline(0, color='black', lw=0.5, ls=':', zorder=1)

    for grp in GROUP_ORDER:
        cell = stats[stats['group'] == grp]
        if cell.empty:
            continue
        cell = cell.sort_values('time_ms')
        t = cell['time_ms'].to_numpy()
        m = display_lowpass(cell['mean'].to_numpy(), t)
        s = display_lowpass(cell['sem'].to_numpy(), t)
        color = GROUP_COLORS[grp]
        ax.fill_between(t, m - s, m + s, color=color, alpha=0.3,
                        lw=0, zorder=-1)
        ax.plot(t, m, color=color, linestyle='-', lw=1.1, zorder=3)

    ax.set_xlim(-100, 1000)
    ax.xaxis.set_major_locator(mpl.ticker.MultipleLocator(100))
    ax.yaxis.set_major_locator(mpl.ticker.MultipleLocator(1))
    ax.grid(True, which='major', color='0.8', lw=0.4, zorder=0.5)
    ax.tick_params(axis='both', labelsize=7, length=2, pad=1)
    ax.spines[['top', 'right']].set_visible(False)


def plot_topomap_inset(parent_ax, chans, cluster_index, color='#1A1A1A'):
    """Small head plot with this cluster's channels highlighted (1-based)."""
    ax = parent_ax.inset_axes([0.76, 0.66, 0.23, 0.32], zorder=4)
    ax.set_facecolor((1, 1, 1, 0.85))
    ax.set_aspect('equal')

    in_cluster = chans[f'in_cluster_{cluster_index}'].astype(bool)
    xs = chans['x'].to_numpy()
    ys = chans['y'].to_numpy()

    theta = np.linspace(0, 2 * np.pi, 200)
    ax.plot(np.cos(theta), np.sin(theta), color='black', lw=0.6)
    ax.plot([-0.10, 0.0, 0.10], [0.99, 1.13, 0.99], color='black', lw=0.6)
    ear_t = np.linspace(-np.pi / 6, np.pi / 6, 30)
    for sign in (-1, 1):
        ax.plot(sign * (1.0 + 0.08 * np.cos(ear_t)),
                0.08 * np.sin(ear_t) * 3.5, color='black', lw=0.6)

    ax.scatter(xs[~in_cluster], ys[~in_cluster], s=4,
               color='#BBBBBB', edgecolor='none', zorder=3)
    ax.scatter(xs[in_cluster], ys[in_cluster], s=8,
               color=color, edgecolor='white', linewidths=0.3, zorder=4)

    ax.set_xlim(-1.25, 1.25)
    ax.set_ylim(-1.25, 1.30)
    ax.set_xticks([]); ax.set_yticks([])
    for s in ax.spines.values():
        s.set_visible(False)


def plot_diff_raincloud_inset(parent_ax, diff_amps):
    """Per-group violin + strip of the per-subject amplitude differences."""
    ax = parent_ax.inset_axes([0.71, 0.06, 0.28, 0.32], zorder=4)
    ax.set_facecolor('white')

    if diff_amps.empty:
        ax.set_visible(False)
        return

    sns.violinplot(
        data=diff_amps, x='group', y='diff',
        order=GROUP_ORDER, hue='group', hue_order=GROUP_ORDER,
        inner=None, cut=0, density_norm='width',
        palette=GROUP_COLORS, linewidth=0.3, ax=ax, legend=False,
    )
    # Match the SE-band fill opacity used in the difference traces.
    for coll in ax.collections:
        coll.set_alpha(0.18)
    sns.stripplot(
        data=diff_amps, x='group', y='diff',
        order=GROUP_ORDER, hue='group', hue_order=GROUP_ORDER,
        size=1.3, jitter=0.06, palette=GROUP_COLORS,
        edgecolor='white', linewidth=0.1, ax=ax, legend=False,
    )
    ax.axhline(0, color='black', lw=0.4, zorder=0)

    ax.tick_params(axis='x', labelsize=5.5, pad=1, length=1.5)
    ax.tick_params(axis='y', labelsize=5.5, pad=1, length=1.5)
    ax.set_xlabel('')
    ax.set_ylabel('')
    ax.yaxis.set_major_locator(mpl.ticker.MaxNLocator(nbins=3))
    for spine in ('top', 'right'):
        ax.spines[spine].set_visible(False)
    for spine in ('left', 'bottom'):
        ax.spines[spine].set_linewidth(0.4)


# ---------------------------------------------------------------------------
# Figure orchestration
# ---------------------------------------------------------------------------

def build_figure(waves, amps, chans):
    mpl.rcParams.update({
        'font.size': 8,
        'axes.labelsize': 8,
        'axes.titlesize': 8.5,
        'pdf.fonttype': 42,
        'ps.fonttype': 42,
    })

    cluster_names = list(dict.fromkeys(waves['cluster'].tolist()))
    if len(cluster_names) != 3:
        raise ValueError(
            f'Expected 3 clusters, got {len(cluster_names)}: {cluster_names}'
        )

    # Precompute all panel difference waves so we can share a y range.
    wave_stats = {}
    diff_amp_cache = {}
    for cname in cluster_names:
        for col_key in COLUMN_ORDER:
            wave_stats[(cname, col_key)] = difference_wave_stats(
                waves, cname, col_key)
            diff_amp_cache[(cname, col_key)] = difference_amplitudes(
                amps, cname, col_key)

    hi_vals, lo_vals = [], []
    for st in wave_stats.values():
        if st.empty:
            continue
        hi_vals.append((st['mean'] + st['sem']).max())
        lo_vals.append((st['mean'] - st['sem']).min())
    y_hi = np.ceil(max(hi_vals) + 0.5) if hi_vals else 2.0
    y_lo = np.floor(min(lo_vals) - 0.5) if lo_vals else -2.0

    mm = 1 / 25.4
    fig = plt.figure(figsize=(180 * mm, 210 * mm))
    gs = fig.add_gridspec(
        3, 2, left=0.085, right=0.985, top=0.93, bottom=0.10,
        wspace=0.18, hspace=0.32,
    )

    axes_grid = {}
    for r, cname in enumerate(cluster_names):
        for c, col_key in enumerate(COLUMN_ORDER):
            ax = fig.add_subplot(gs[r, c])
            axes_grid[(r, c)] = ax

            plot_difference_panel(ax, wave_stats[(cname, col_key)], cname)
            ax.set_ylim(y_lo, y_hi)

            if c == 0:
                plot_topomap_inset(ax, chans, cluster_index=r + 1)
            plot_diff_raincloud_inset(ax, diff_amp_cache[(cname, col_key)])

            if r == 0:
                ax.set_title(DIFF_BY_COLUMN[col_key]['header'], pad=6)
            if r == len(cluster_names) - 1:
                ax.set_xlabel('Time (ms)')
            else:
                ax.set_xticklabels([])
            if c == 0:
                ax.set_ylabel('Δ Amplitude (µV)')
            else:
                ax.set_yticklabels([])

    # Row labels on the left margin.
    for r, cname in enumerate(cluster_names):
        ax = axes_grid[(r, 0)]
        pos = ax.get_position()
        fig.text(
            0.012, pos.y0 + pos.height / 2,
            short_cluster_name(cname),
            rotation=90, ha='center', va='center',
            fontsize=9, fontweight='bold',
        )

    legend_handles = [
        Patch(facecolor=GROUP_COLORS['Lonely'], edgecolor='none',
              label='Lonely'),
        Patch(facecolor=GROUP_COLORS['Non-Lonely'], edgecolor='none',
              label='Non-Lonely'),
        Line2D([0], [0], color='0.25', lw=1.2, linestyle='-',
               label='difference: %s (col 1) · %s (col 2)' % (
                   DIFF_BY_COLUMN['emotion']['diff_label'],
                   DIFF_BY_COLUMN['repetition']['diff_label'])),
        Patch(facecolor=WINDOW_FACECOLOR, alpha=WINDOW_ALPHA, edgecolor='none',
              label='cluster time window'),
    ]
    fig.legend(handles=legend_handles, loc='lower center',
               bbox_to_anchor=(0.5, 0.01), ncol=2, frameon=False,
               fontsize=7.5, handlelength=2.2, columnspacing=1.4)

    return fig


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main(diff_waveforms_tsv, diff_amplitudes_tsv, channels_tsv, out_figure):
    diff_waveforms_tsv = Path(diff_waveforms_tsv)
    diff_amplitudes_tsv = Path(diff_amplitudes_tsv)
    channels_tsv = Path(channels_tsv)
    out_figure = Path(out_figure)

    waves, amps, chans = load_tables(diff_waveforms_tsv, diff_amplitudes_tsv,
                                     channels_tsv)
    fig = build_figure(waves, amps, chans)

    out_figure.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_figure, dpi=600)
    print(f'Wrote {out_figure}')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='Build the 3x2 cluster difference-wave figure with '
                    'raincloud and topomap insets.'
    )
    parser.add_argument('--diff-waveforms-tsv', required=True, type=Path)
    parser.add_argument('--diff-amplitudes-tsv', required=True, type=Path)
    parser.add_argument('--channels-tsv', required=True, type=Path)
    parser.add_argument('--out-figure', required=True, type=Path)
    args = parser.parse_args()
    main(args.diff_waveforms_tsv, args.diff_amplitudes_tsv, args.channels_tsv,
         args.out_figure)

# %%
