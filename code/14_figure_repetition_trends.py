# %%
"""
Repetition-trend figure: 3 (cluster) x 2 (emotion) grid of panels. Each
panel shows per-group (Lonely vs Non-Lonely) mean amplitude across
repetitions 1..6 (rep 7 excluded upstream as too noisy) with SE error
bars, overlaid with the LMM fixed-effects predictions for linear and
logarithmic forms. The model recommended (lower AIC) in that cell is
drawn solid; the alternative is dashed.

Inputs:
  - results/cluster_amplitudes_all_reps.tsv (from 11_extract_amplitudes_all_reps.py)
  - results/repetition_model_predictions.tsv (from 13_repetition_trends_analysis.R)
"""

import argparse
import re
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib.lines import Line2D


GROUP_COLORS = {
    'Non-Lonely': '#A4002A99',
    'Lonely': '#00468B99',
}
GROUP_ORDER = ['Lonely', 'Non-Lonely']
EMOTIONS = ['angry', 'happy']
MODELS = ('linear', 'log')
REPS = np.arange(1, 7)  # 1..6 (rep 7 excluded as too noisy)


def short_cluster_name(full_name):
    return re.sub(r'\s*\(.*\)\s*$', '', full_name)


def cluster_window_label(full_name):
    m = re.search(r'\(([^)]+)\)\s*$', full_name)
    return m.group(1) if m else ''


def load_tables(amplitudes_tsv, predictions_tsv):
    amps = pd.read_csv(amplitudes_tsv, sep='\t', comment='#')
    preds = pd.read_csv(predictions_tsv, sep='\t')
    amps['repetition'] = amps['repetition'].astype(int)
    amps['emotion'] = amps['emotion'].astype(str)
    amps['group'] = amps['group'].astype(str)
    preds['repetition'] = preds['repetition'].astype(int)
    preds['emotion'] = preds['emotion'].astype(str)
    preds['group'] = preds['group'].astype(str)
    preds['model'] = preds['model'].astype(str)
    if 'is_recommended' in preds.columns:
        preds['is_recommended'] = preds['is_recommended'].astype(bool)
    return amps, preds


def group_mean_se_per_rep(amps_cell):
    """Return (reps, mean, se) per group for a cluster x emotion sub-frame."""
    out = {}
    for grp in GROUP_ORDER:
        sub = amps_cell[amps_cell['group'] == grp]
        agg = (sub.groupby('repetition')['amplitude']
                  .agg(['mean', 'std', 'count'])
                  .reindex(REPS))
        mean = agg['mean'].to_numpy()
        se = (agg['std'] / np.sqrt(agg['count'])).to_numpy()
        out[grp] = (REPS, mean, se)
    return out


def plot_panel(ax, amps_cell, preds_cell, cluster_name, emotion):
    means = group_mean_se_per_rep(amps_cell)

    # Observed means with SE error bars per group.
    for grp in GROUP_ORDER:
        x, m, se = means[grp]
        ax.errorbar(x, m, yerr=se,
                    fmt='o', markersize=4,
                    color=GROUP_COLORS[grp], capsize=2,
                    linewidth=0, elinewidth=1, alpha=0.95,
                    zorder=3, label=None)

    # LMM population-level predictions: recommended model solid, alt dashed.
    for grp in GROUP_ORDER:
        color = GROUP_COLORS[grp]
        for model in MODELS:
            sub = preds_cell[(preds_cell['group'] == grp) &
                             (preds_cell['model'] == model)]
            if sub.empty:
                continue
            sub = sub.sort_values('repetition')
            is_rec = bool(sub['is_recommended'].iloc[0]) \
                if 'is_recommended' in sub.columns else False
            linestyle = '-' if is_rec else '--'
            ax.plot(sub['repetition'].to_numpy(),
                    sub['predicted'].to_numpy(),
                    linestyle=linestyle, color=color,
                    linewidth=1.5, alpha=0.85, zorder=2)

    ax.set_xticks(REPS)
    ax.set_xlim(0.7, 6.3)
    ax.set_title(f'{short_cluster_name(cluster_name)} -- {emotion}\n'
                 f'{cluster_window_label(cluster_name)}',
                 fontsize=9)
    ax.grid(True, axis='y', linestyle=':', alpha=0.4)


def build_figure(amps, preds, out_pdf):
    clusters = list(amps['cluster'].drop_duplicates())
    n_rows = len(clusters)
    n_cols = len(EMOTIONS)

    fig, axes = plt.subplots(
        n_rows, n_cols,
        figsize=(7.08, 2.4 * n_rows),  # ~180 mm wide
        sharex=True
    )
    if n_rows == 1:
        axes = axes[np.newaxis, :]

    for i, cluster_name in enumerate(clusters):
        for j, emotion in enumerate(EMOTIONS):
            amps_cell = amps[(amps['cluster'] == cluster_name) &
                             (amps['emotion'] == emotion)]
            preds_cell = preds[(preds['cluster'] == cluster_name) &
                               (preds['emotion'] == emotion)]
            ax = axes[i, j]
            plot_panel(ax, amps_cell, preds_cell, cluster_name, emotion)

    axes[-1, 0].set_xlabel('Repetition')
    axes[-1, 0].set_ylabel('Mean amplitude (uV)')

    legend_handles = [
        Line2D([0], [0], color=GROUP_COLORS['Lonely'], marker='o',
               linewidth=1.5, label='Lonely'),
        Line2D([0], [0], color=GROUP_COLORS['Non-Lonely'], marker='o',
               linewidth=1.5, label='Non-Lonely'),
        Line2D([0], [0], color='#444444', linestyle='-', linewidth=1.5,
               label='LMM (recommended)'),
        Line2D([0], [0], color='#444444', linestyle='--', linewidth=1.5,
               label='LMM (alternative)'),
    ]
    fig.legend(handles=legend_handles, loc='lower center',
               ncol=4, bbox_to_anchor=(0.5, 0.00), frameon=False,
               fontsize=8)

    fig.tight_layout(rect=[0, 0.04, 1, 1])
    out_pdf.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_pdf, dpi=300, bbox_inches='tight')
    plt.close(fig)


def main(amplitudes_tsv, predictions_tsv, out_pdf):
    amplitudes_tsv = Path(amplitudes_tsv)
    predictions_tsv = Path(predictions_tsv)
    out_pdf = Path(out_pdf)

    amps, preds = load_tables(amplitudes_tsv, predictions_tsv)
    build_figure(amps, preds, out_pdf)
    print(f'Wrote {out_pdf}')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='Render the repetition-trend figure (3 cluster x 2 '
                    'emotion grid; reps 1..6) with observed group means, '
                    'SE bars and LMM fixed-effects predictions for linear '
                    'and log models.'
    )
    parser.add_argument('--amplitudes-tsv', required=True, type=Path)
    parser.add_argument('--predictions-tsv', required=True, type=Path)
    parser.add_argument('--out-figure', required=True, type=Path)
    args = parser.parse_args()
    main(args.amplitudes_tsv, args.predictions_tsv, args.out_figure)

# %%
