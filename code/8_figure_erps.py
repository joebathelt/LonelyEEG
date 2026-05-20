# %%
"""
Build the manuscript ERP figure: a 3 (cluster) x 2 (comparison) grid where
the left column compares emotion (happy vs angry) at the 1st repetition and
the right column compares repetition (1st vs 5th) for angry trials, with
each panel showing the cluster-mean ERP per group (Lonely vs Non-Lonely)
with SE shading. Every panel carries two insets: a topomap of the cluster's
channels (upper-right) and a half-violin raincloud of the cluster amplitude
distribution for the 4 cells in that panel (upper-left).

Inputs:
  - results/cluster_waveforms.tsv         (from 7_extract_erp_waveforms.py)
  - results/cluster_amplitudes.tsv        (from 5_extract_amplitudes.py)
  - results/cluster_channel_positions.tsv (from 7_extract_erp_waveforms.py)
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
    'Non-Lonely': '#A4002A99',       # Lancet red
    'Lonely': '#00468B99',   # Lancet blue
}
WINDOW_FACECOLOR = '#D3D3D3'  # Light grey, alpha applied separately for visibility
WINDOW_ALPHA = 0.55

# Display-only low-pass filter for ERP traces (zero-phase Butterworth).
# The underlying waveforms in cluster_waveforms.tsv are unchanged; this only
# smooths the plotted mean / SE bands.
DISPLAY_LOWPASS_HZ = 20.0

# Column-A: rep==1, contrast emotion. Column-B: emotion=='angry', contrast rep.
CONDITION_BY_COLUMN = {
    'emotion': {
        'fixed_factor': ('repetition', 1),
        'contrast_factor': 'emotion',
        'contrast_levels': ['happy', 'angry'],
        'linestyles': {'happy': '-', 'angry': '--'},
        'header': 'Emotion (1st repetition)',
        'cell_label': lambda lev: lev,
    },
    'repetition': {
        'fixed_factor': ('emotion', 'angry'),
        'contrast_factor': 'repetition',
        'contrast_levels': [1, 5],
        'linestyles': {1: '-', 5: '--'},
        'header': 'Repetition (angry trials)',
        'cell_label': lambda lev: f'rep {lev}',
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


def load_tables(waveforms_tsv, amplitudes_tsv, channels_tsv):
    waves = pd.read_csv(waveforms_tsv, sep='\t', comment='#')
    amps = pd.read_csv(amplitudes_tsv, sep='\t', comment='#')
    chans = pd.read_csv(channels_tsv, sep='\t')
    # Normalize types for safe filtering / categorical plotting.
    for df in (waves, amps):
        df['repetition'] = df['repetition'].astype(int)
        df['emotion'] = df['emotion'].astype(str)
        df['group'] = df['group'].astype(str)
    return waves, amps, chans


# ---------------------------------------------------------------------------
# Plot pieces
# ---------------------------------------------------------------------------

def plot_erp_panel(ax, waves, cluster_name, column_key):
    """Mean +/- SE ERP traces (group x condition) for one panel."""
    cfg = CONDITION_BY_COLUMN[column_key]
    fixed_col, fixed_val = cfg['fixed_factor']

    sub = waves[
        (waves['cluster'] == cluster_name) & (waves[fixed_col] == fixed_val)
    ].copy()

    tmin_ms, tmax_ms = cluster_time_window_ms(cluster_name)
    ax.axvspan(tmin_ms, tmax_ms, color=WINDOW_FACECOLOR,
               alpha=WINDOW_ALPHA, zorder=0, lw=0)
    ax.axhline(0, color='black', lw=0.5, zorder=1)
    ax.axvline(0, color='black', lw=0.5, ls=':', zorder=1)

    contrast_col = cfg['contrast_factor']
    for level in cfg['contrast_levels']:
        for grp in GROUP_ORDER:
            cell = sub[(sub[contrast_col] == level) & (sub['group'] == grp)]
            if cell.empty:
                continue
            stats = (cell.groupby('time_ms')['amplitude_uv']
                         .agg(['mean', 'sem'])
                         .reset_index())
            t = stats['time_ms'].to_numpy()
            m = display_lowpass(stats['mean'].to_numpy(), t)
            s = display_lowpass(stats['sem'].to_numpy(), t)
            color = GROUP_COLORS[grp]
            ls = cfg['linestyles'][level]
            ax.fill_between(t, m - s, m + s, color=color, alpha=0.3,
                            lw=0, zorder=-1)
            ax.plot(t, m, color=color, linestyle=ls, lw=1.1, zorder=3)

    ax.set_xlim(-100, 1000)
    ax.xaxis.set_major_locator(mpl.ticker.MultipleLocator(100))
    ax.yaxis.set_major_locator(mpl.ticker.MultipleLocator(1))
    ax.grid(True, which='major', color='0.8', lw=0.4, zorder=0.5)
    ax.tick_params(axis='both', labelsize=7, length=2, pad=1)
    ax.spines[['top', 'right']].set_visible(False)


def plot_topomap_inset(parent_ax, chans, cluster_index, color='#1A1A1A'):
    """Small head plot with this cluster's channels highlighted.

    cluster_index is 1-based to match the column names in the channels TSV.
    """
    ax = parent_ax.inset_axes([0.76, 0.66, 0.23, 0.32], zorder=4)
    ax.set_facecolor((1, 1, 1, 0.85))
    ax.set_aspect('equal')

    in_cluster = chans[f'in_cluster_{cluster_index}'].astype(bool)
    xs = chans['x'].to_numpy()
    ys = chans['y'].to_numpy()

    # Head outline (unit circle), nose triangle, ears.
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


def plot_raincloud_inset(parent_ax, amps, cluster_name, column_key):
    """4-cell split-violin raincloud for this panel's conditions."""
    ax = parent_ax.inset_axes([0.71, 0.06, 0.28, 0.32], zorder=4)
    ax.set_facecolor('white')

    cfg = CONDITION_BY_COLUMN[column_key]
    fixed_col, fixed_val = cfg['fixed_factor']
    contrast_col = cfg['contrast_factor']

    sub = amps[
        (amps['cluster'] == cluster_name) & (amps[fixed_col] == fixed_val)
    ].dropna(subset=['amplitude']).copy()
    if sub.empty:
        ax.set_visible(False)
        return

    # Use strings so seaborn lays out a clean categorical x axis.
    sub[contrast_col] = sub[contrast_col].astype(str)
    contrast_order = [str(x) for x in cfg['contrast_levels']]

    sns.violinplot(
        data=sub, x=contrast_col, y='amplitude', hue='group',
        order=contrast_order, hue_order=GROUP_ORDER,
        split=True, inner=None, cut=0, density_norm='width',
        palette=GROUP_COLORS, linewidth=0.3, ax=ax, legend=False,
    )
    # Match the SE-band fill opacity used in the ERP traces (see plot_erp_panel).
    for coll in ax.collections:
        coll.set_alpha(0.18)
    sns.stripplot(
        data=sub, x=contrast_col, y='amplitude', hue='group',
        order=contrast_order, hue_order=GROUP_ORDER,
        dodge=True, size=1.3, jitter=0.06, palette=GROUP_COLORS,
        edgecolor='white', linewidth=0.1, ax=ax, legend=False,
    )
    ax.axhline(0, color='black', lw=0.4, zorder=0)

    ax.tick_params(axis='x', labelsize=5.5, pad=1, length=1.5)
    ax.tick_params(axis='y', labelsize=5.5, pad=1, length=1.5)
    ax.set_xlabel('')
    ax.set_ylabel('')
    ax.set_xticks(range(len(cfg['contrast_levels'])))
    ax.set_xticklabels(
        [cfg['cell_label'](lev) for lev in cfg['contrast_levels']]
    )
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

    mm = 1 / 25.4
    fig = plt.figure(figsize=(180 * mm, 210 * mm))
    gs = fig.add_gridspec(
        3, 2, left=0.085, right=0.985, top=0.93, bottom=0.10,
        wspace=0.18, hspace=0.32,
    )

    # Determine a shared y range so panels are visually comparable.
    erp_summary = (
        waves.groupby(['cluster', 'emotion', 'repetition', 'group', 'time_ms'])
              ['amplitude_uv'].agg(['mean', 'sem']).reset_index()
    )
    erp_summary['hi'] = erp_summary['mean'] + erp_summary['sem']
    erp_summary['lo'] = erp_summary['mean'] - erp_summary['sem']
    y_lo = np.floor(erp_summary['lo'].min() - 0.5)
    y_hi = np.ceil(erp_summary['hi'].max() + 0.5)

    axes_grid = {}
    for r, cname in enumerate(cluster_names):
        for c, col_key in enumerate(COLUMN_ORDER):
            ax = fig.add_subplot(gs[r, c])
            axes_grid[(r, c)] = ax

            plot_erp_panel(ax, waves, cname, col_key)
            ax.set_ylim(y_lo, y_hi)

            if c == 0:
                plot_topomap_inset(ax, chans, cluster_index=r + 1)
            plot_raincloud_inset(ax, amps, cname, col_key)

            if r == 0:
                ax.set_title(CONDITION_BY_COLUMN[col_key]['header'],
                             pad=6)
            if r == len(cluster_names) - 1:
                ax.set_xlabel('Time (ms)')
            else:
                ax.set_xticklabels([])
            if c == 0:
                ax.set_ylabel('Amplitude (µV)')
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

    # Shared legend at bottom: group color + linetype encoding.
    legend_handles = [
        Patch(facecolor=GROUP_COLORS['Lonely'], edgecolor='none',
              label='Lonely'),
        Patch(facecolor=GROUP_COLORS['Non-Lonely'], edgecolor='none',
              label='Non-Lonely'),
        Line2D([0], [0], color='0.25', lw=1.2, linestyle='-',
               label='solid: happy (col 1) · 1st rep (col 2)'),
        Line2D([0], [0], color='0.25', lw=1.2, linestyle='--',
               label='dashed: angry (col 1) · 5th rep (col 2)'),
        Patch(facecolor=WINDOW_FACECOLOR, alpha=WINDOW_ALPHA, edgecolor='none',
              label='cluster time window'),
    ]
    fig.legend(handles=legend_handles, loc='lower center',
               bbox_to_anchor=(0.5, 0.01), ncol=3, frameon=False,
               fontsize=7.5, handlelength=2.2, columnspacing=1.4)

    return fig


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main(waveforms_tsv, amplitudes_tsv, channels_tsv, out_figure):
    waveforms_tsv = Path(waveforms_tsv)
    amplitudes_tsv = Path(amplitudes_tsv)
    channels_tsv = Path(channels_tsv)
    out_figure = Path(out_figure)

    waves, amps, chans = load_tables(waveforms_tsv, amplitudes_tsv, channels_tsv)
    fig = build_figure(waves, amps, chans)

    out_figure.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_figure, dpi=600)
    print(f'Wrote {out_figure}')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='Build the 3x2 cluster ERP figure with raincloud and '
                    'topomap insets.'
    )
    parser.add_argument('--waveforms-tsv', required=True, type=Path)
    parser.add_argument('--amplitudes-tsv', required=True, type=Path)
    parser.add_argument('--channels-tsv', required=True, type=Path)
    parser.add_argument('--out-figure', required=True, type=Path)
    args = parser.parse_args()
    main(args.waveforms_tsv, args.amplitudes_tsv, args.channels_tsv,
         args.out_figure)

# %%
