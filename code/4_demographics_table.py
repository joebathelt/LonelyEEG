# %%
import argparse
from pathlib import Path

import numpy as np
import pandas as pd
from scipy import stats


GROUP_COL = 'group'
GROUP_ORDER = ['Lonely', 'Non-Lonely']

QC_TRIAL_COLS = ['n_angry_rep_1', 'n_angry_rep_5', 'n_happy_rep_1', 'n_happy_rep_5']
QC_BAD_CHAN_COL = 'n_bad_channels'
MIN_TRIALS = 50
MAX_BAD_CHANNELS = 4

CONTINUOUS_DEMO = ['age', 'handedness']
CATEGORICAL_DEMO = ['gender', 'ses', 'ethnicity']
CONTINUOUS_QC = [QC_BAD_CHAN_COL] + QC_TRIAL_COLS
UCLA_COL = 'ucla-ls'
CONTINUOUS_MH = [
    'gad7_total', 'pss10_total', 'phq9_total',
    'lsns6_total',
    'bsi53_gsi',
    'bsi53_somatisation', 'bsi53_obsessive_compulsive',
    'bsi53_interpersonal_sensitivity', 'bsi53_depression',
    'bsi53_anxiety', 'bsi53_hostility',
    'bsi53_phobic_anxiety', 'bsi53_paranoid_ideation',
    'bsi53_psychoticism',
]

LABELS = {
    'age': 'Age (years)',
    'handedness': 'Handedness',
    'gender': 'Gender',
    'ses': 'Education',
    'ethnicity': 'Ethnicity',
    'n_bad_channels': 'Bad channels (n)',
    'n_angry_rep_1': 'Angry, rep 1 (epochs)',
    'n_angry_rep_5': 'Angry, rep 5 (epochs)',
    'n_happy_rep_1': 'Happy, rep 1 (epochs)',
    'n_happy_rep_5': 'Happy, rep 5 (epochs)',
    'ucla-ls': 'UCLA-LS total',
    'gad7_total': 'GAD-7 total',
    'pss10_total': 'PSS-10 total',
    'phq9_total': 'PHQ-9 total',
    'lsns6_total': 'LSNS-6 total',
    'bsi53_gsi': 'BSI-53 GSI',
    'bsi53_somatisation': 'BSI-53 somatisation',
    'bsi53_obsessive_compulsive': 'BSI-53 obsessive-compulsive',
    'bsi53_interpersonal_sensitivity': 'BSI-53 interpersonal sensitivity',
    'bsi53_depression': 'BSI-53 depression',
    'bsi53_anxiety': 'BSI-53 anxiety',
    'bsi53_hostility': 'BSI-53 hostility',
    'bsi53_phobic_anxiety': 'BSI-53 phobic anxiety',
    'bsi53_paranoid_ideation': 'BSI-53 paranoid ideation',
    'bsi53_psychoticism': 'BSI-53 psychoticism',
}


# ---------------------------------------------------------------------------
# Load + filter
# ---------------------------------------------------------------------------

def load_and_filter(participants_tsv, qc_tsv):
    """Merge participants + QC, apply exclusions.

    Returns
    -------
    df : DataFrame
        The analytic sample (post-filter).
    drop_log : dict
        Per-step drop counts for the footnote.
    exclusion_df : DataFrame
        Participants with a valid group label and their excluded flag (computed
        pre-filter). Used to compare exclusion rates between groups.
    """
    participants = pd.read_csv(participants_tsv, sep='\t', na_values=['n/a'])
    qc = pd.read_csv(qc_tsv, sep='\t', na_values=['n/a'])

    numeric_cols = (
        CONTINUOUS_DEMO + CONTINUOUS_QC + CONTINUOUS_MH + ['ucla-ls']
    )
    for col in numeric_cols:
        if col in participants.columns:
            participants[col] = pd.to_numeric(participants[col], errors='coerce')
        if col in qc.columns:
            qc[col] = pd.to_numeric(qc[col], errors='coerce')

    df = participants.merge(qc, on='participant_id', how='inner')
    n_initial = len(df)

    exclude_flag = df['exclude'].astype(str).str.upper() == 'TRUE'
    too_many_bad = df[QC_BAD_CHAN_COL] > MAX_BAD_CHANNELS
    too_few_trials = (df[QC_TRIAL_COLS] < MIN_TRIALS).any(axis=1)
    is_excluded = exclude_flag | too_many_bad | too_few_trials

    has_group = df[GROUP_COL].isin(GROUP_ORDER)
    exclusion_df = (
        df.loc[has_group, ['participant_id', GROUP_COL]]
        .assign(excluded=is_excluded[has_group].values)
        .reset_index(drop=True)
    )

    n_after_exclude = int((~exclude_flag).sum())
    df = df[~exclude_flag].copy()

    qc_fail = (df[QC_BAD_CHAN_COL] > MAX_BAD_CHANNELS) | (df[QC_TRIAL_COLS] < MIN_TRIALS).any(axis=1)
    df = df[~qc_fail].copy()
    n_after_qc = len(df)

    missing_group = df[GROUP_COL].isna() | ~df[GROUP_COL].isin(GROUP_ORDER)
    df = df[~missing_group].copy()
    n_final = len(df)

    drop_log = {
        'initial': n_initial,
        'dropped_exclude_flag': n_initial - n_after_exclude,
        'dropped_qc': n_after_exclude - n_after_qc,
        'dropped_missing_group': n_after_qc - n_final,
        'final': n_final,
    }
    return df, drop_log, exclusion_df


# ---------------------------------------------------------------------------
# Stats
# ---------------------------------------------------------------------------

def welch_df(a, b):
    n1, n2 = len(a), len(b)
    if n1 < 2 or n2 < 2:
        return np.nan
    v1, v2 = a.var(ddof=1), b.var(ddof=1)
    num = (v1 / n1 + v2 / n2) ** 2
    den = (v1 / n1) ** 2 / (n1 - 1) + (v2 / n2) ** 2 / (n2 - 1)
    if den == 0:
        return np.nan
    return num / den


def cohens_d(a, b):
    """Cohen's d for two independent samples with pooled SD."""
    n1, n2 = len(a), len(b)
    if n1 < 2 or n2 < 2:
        return np.nan
    v1, v2 = a.var(ddof=1), b.var(ddof=1)
    s_pool = np.sqrt(((n1 - 1) * v1 + (n2 - 1) * v2) / (n1 + n2 - 2))
    if s_pool == 0:
        return np.nan
    return float((a.mean() - b.mean()) / s_pool)


def cramers_v(table_array):
    """Cramér's V from a 2D contingency table."""
    n = table_array.sum()
    if n == 0:
        return np.nan
    r, c = table_array.shape
    k = min(r, c) - 1
    if k <= 0:
        return np.nan
    try:
        chi2_stat = stats.chi2_contingency(table_array, correction=False).statistic
    except Exception:
        return np.nan
    return float(np.sqrt(chi2_stat / (n * k)))


def compare_continuous(df, var):
    """Return per-group summary dicts and the Welch test result."""
    all_vals = df[var].dropna()
    is_integer = (
        len(all_vals) > 0
        and np.issubdtype(all_vals.dtype, np.number)
        and bool((all_vals == all_vals.round()).all())
    )

    per_group = {}
    samples = {}
    for grp in GROUP_ORDER:
        values = df.loc[df[GROUP_COL] == grp, var].dropna()
        samples[grp] = values
        n = len(values)
        if n == 0:
            per_group[grp] = {
                'mean': np.nan, 'se': np.nan, 'n': 0,
                'n_missing': int((df[GROUP_COL] == grp).sum()),
                'min': np.nan, 'max': np.nan, 'integer': is_integer,
            }
            continue
        sd = values.std(ddof=1) if n > 1 else 0.0
        se = sd / np.sqrt(n) if n > 0 else np.nan
        per_group[grp] = {
            'mean': float(values.mean()),
            'se': float(se),
            'n': int(n),
            'n_missing': int((df[GROUP_COL] == grp).sum() - n),
            'min': float(values.min()),
            'max': float(values.max()),
            'integer': is_integer,
        }

    a = samples[GROUP_ORDER[0]].to_numpy()
    b = samples[GROUP_ORDER[1]].to_numpy()
    if len(a) >= 2 and len(b) >= 2:
        res = stats.ttest_ind(a, b, equal_var=False, nan_policy='omit')
        t_stat = float(res.statistic)
        p = float(res.pvalue)
        df_val = float(welch_df(pd.Series(a), pd.Series(b)))
        d_val = cohens_d(pd.Series(a), pd.Series(b))
    else:
        t_stat, df_val, p, d_val = np.nan, np.nan, np.nan, np.nan
    return per_group, {
        'test': 't', 'statistic': t_stat, 'df': df_val, 'p': p,
        'effect': d_val, 'effect_label': 'd',
    }


def _test_contingency(table_array):
    """Run chi-square (or Fisher's exact if any cell < 5) on a 2D count array.

    Always reports Cramér's V (computed from the chi-square statistic on the
    same table) as the effect size, regardless of which p-value test was used.
    """
    test_info = {'test': None, 'statistic': np.nan, 'df': np.nan, 'p': np.nan,
                 'effect': np.nan, 'effect_label': 'V'}
    if table_array.shape[1] < 2 or table_array.sum() == 0:
        return test_info
    v = cramers_v(table_array)
    any_small = (table_array < 5).any()
    if any_small:
        try:
            res = stats.fisher_exact(table_array)
            return {'test': 'Fisher', 'statistic': np.nan, 'df': np.nan,
                    'p': float(res.pvalue),
                    'effect': v, 'effect_label': 'V'}
        except Exception as exc:
            chi = stats.chi2_contingency(table_array)
            return {'test': 'chi2 (small cells)',
                    'statistic': float(chi.statistic), 'df': int(chi.dof),
                    'p': float(chi.pvalue),
                    'effect': v, 'effect_label': 'V',
                    'warning': f'Fisher failed ({exc}); used chi-square'}
    chi = stats.chi2_contingency(table_array)
    return {'test': 'chi2', 'statistic': float(chi.statistic),
            'df': int(chi.dof), 'p': float(chi.pvalue),
            'effect': v, 'effect_label': 'V'}


def compare_categorical(df, var):
    """Build group × level contingency, run chi-square or Fisher's exact."""
    sub = df[[GROUP_COL, var]].dropna(subset=[var]).copy()
    sub[var] = sub[var].astype(str)
    levels = sorted(sub[var].unique().tolist())

    contingency = pd.DataFrame(
        0, index=GROUP_ORDER, columns=levels, dtype=int,
    )
    for grp in GROUP_ORDER:
        counts = sub.loc[sub[GROUP_COL] == grp, var].value_counts()
        for level, count in counts.items():
            contingency.loc[grp, level] = int(count)

    per_group = {}
    for grp in GROUP_ORDER:
        total = int(contingency.loc[grp].sum())
        per_group[grp] = {}
        for level in levels:
            n = int(contingency.loc[grp, level])
            pct = (n / total * 100) if total > 0 else 0.0
            per_group[grp][level] = {'n': n, 'pct': pct, 'group_total': total}

    table = contingency.loc[:, (contingency.sum(axis=0) > 0)]
    test_info = _test_contingency(table.to_numpy())
    return per_group, test_info, levels


def compare_ethnicity(df):
    """Split comma-separated ethnicity reports into continents and count each.

    Treats ethnicity as multi-label: a participant reporting "East Asia,
    South-East Asia" contributes one count to each continent. The denominator
    for percentages is the analytic group n; the column sum across continents
    can therefore exceed the group n.
    """
    sub = df[[GROUP_COL, 'ethnicity']].dropna(subset=['ethnicity']).copy()
    sub['continents'] = sub['ethnicity'].apply(
        lambda s: [c.strip() for c in str(s).split(',') if c.strip()]
    )
    continents = sorted({c for cs in sub['continents'] for c in cs})

    contingency = pd.DataFrame(0, index=GROUP_ORDER, columns=continents, dtype=int)
    group_totals = {}
    for grp in GROUP_ORDER:
        grp_df = sub[sub[GROUP_COL] == grp]
        group_totals[grp] = len(grp_df)
        for cs in grp_df['continents']:
            for c in cs:
                contingency.loc[grp, c] += 1

    per_group = {}
    for grp in GROUP_ORDER:
        per_group[grp] = {}
        for c in continents:
            n = int(contingency.loc[grp, c])
            pct = (n / group_totals[grp] * 100) if group_totals[grp] > 0 else 0.0
            per_group[grp][c] = {'n': n, 'pct': pct, 'group_total': group_totals[grp]}

    table = contingency.loc[:, (contingency.sum(axis=0) > 0)]
    test_info = _test_contingency(table.to_numpy())
    return per_group, test_info, continents


def compare_exclusion(exclusion_df):
    """2x2 comparison of (excluded vs retained) by group."""
    rows = []
    per_group = {}
    for grp in GROUP_ORDER:
        grp_df = exclusion_df[exclusion_df[GROUP_COL] == grp]
        n_total = len(grp_df)
        n_excl = int(grp_df['excluded'].sum())
        n_ret = n_total - n_excl
        pct = (n_excl / n_total * 100) if n_total > 0 else 0.0
        per_group[grp] = {'n_excluded': n_excl, 'n_total': n_total, 'pct': pct}
        rows.append([n_excl, n_ret])
    test_info = _test_contingency(np.array(rows))
    return per_group, test_info


# ---------------------------------------------------------------------------
# Formatting
# ---------------------------------------------------------------------------

def fmt_p_plain(p):
    if not np.isfinite(p):
        return ''
    if p >= 0.001:
        return f'{p:.3f}'
    return f'{p:.2e}'


def fmt_p_latex_math(p):
    """Return the math-mode content (no surrounding $) for a p-value."""
    if not np.isfinite(p):
        return ''
    if p >= 0.001:
        return f'{p:.3f}'
    mantissa, exponent = f'{p:.2e}'.split('e')
    return f'{mantissa} \\times 10^{{{int(exponent)}}}'


def _effect_suffix_plain(t):
    eff = t.get('effect')
    if eff is None or not np.isfinite(eff):
        return ''
    return f", {t['effect_label']}={eff:.2f}"


def _effect_latex_fragment(t):
    """Standalone math fragment for the effect size (or '')."""
    eff = t.get('effect')
    if eff is None or not np.isfinite(eff):
        return ''
    return f"${t['effect_label']} = {eff:.2f}$"


def fmt_test_plain(t):
    test = t.get('test')
    if test is None:
        return ''
    p_str = fmt_p_plain(t['p'])
    eff = _effect_suffix_plain(t)
    if test == 't':
        return f"t({t['df']:.1f})={t['statistic']:.2f}, p={p_str}{eff}"
    if test == 'Fisher':
        return f"Fisher's exact, p={p_str}{eff}"
    if test.startswith('chi2'):
        return f"chi2({int(t['df'])})={t['statistic']:.2f}, p={p_str}{eff}"
    return ''


def fmt_test_latex(t):
    """Render the test cell as a sequence of small math fragments so that the
    line-break-allergic X column has natural break points (the commas between
    `$...$` groups) instead of one long unbreakable math expression."""
    test = t.get('test')
    if test is None or not np.isfinite(t['p']):
        return ''
    p_frag = f"$p = {fmt_p_latex_math(t['p'])}$"
    eff_frag = _effect_latex_fragment(t)
    if test == 't':
        stat_frag = f"$t({t['df']:.1f}) = {t['statistic']:.2f}$"
        parts = [stat_frag, p_frag]
    elif test == 'Fisher':
        parts = ["Fisher's exact", p_frag]
    elif test.startswith('chi2'):
        stat_frag = f"$\\chi^{{2}}({int(t['df'])}) = {t['statistic']:.2f}$"
        parts = [stat_frag, p_frag]
    else:
        return ''
    if eff_frag:
        parts.append(eff_frag)
    return ', '.join(parts)


def _fmt_range(lo, hi, integer, dash):
    if not (np.isfinite(lo) and np.isfinite(hi)):
        return ''
    if integer:
        return f"{int(round(lo))}{dash}{int(round(hi))}"
    return f"{lo:.2f}{dash}{hi:.2f}"


def fmt_cont_cell(s, *, latex=False):
    if s['n'] == 0:
        return '--'
    dash = '--' if latex else '-'
    base = f"{s['mean']:.2f} ({s['se']:.2f})"
    rng = _fmt_range(s.get('min', np.nan), s.get('max', np.nan),
                     s.get('integer', False), dash)
    extras = []
    if rng:
        extras.append(f"[{rng}]")
    if s['n_missing'] > 0:
        extras.append(f"[n={s['n']}]")
    if extras:
        # In LaTeX `p{}` columns, drop the range onto the next line; in plain
        # text keep everything on one line for the TSV.
        sep = r' \newline ' if latex else ' '
        base += sep + ' '.join(extras)
    return base


def fmt_cat_cell(d):
    if d['group_total'] == 0:
        return '--'
    return f"{d['n']} ({d['pct']:.1f}\\%)"


def fmt_cat_cell_plain(d):
    if d['group_total'] == 0:
        return '--'
    return f"{d['n']} ({d['pct']:.1f}%)"


# ---------------------------------------------------------------------------
# Build rows
# ---------------------------------------------------------------------------

def build_rows(df, exclusion_df):
    rows = []
    footnotes = {}

    def section(name):
        rows.append({'kind': 'section', 'label': name})

    def add_continuous(var, footnote_marker=None):
        per_group, test = compare_continuous(df, var)
        if footnote_marker:
            label_latex = f"{LABELS[var]}\\textsuperscript{{{footnote_marker}}}"
            label_plain = f"{LABELS[var]} ({footnote_marker})"
        else:
            label_latex = LABELS[var]
            label_plain = LABELS[var]
        rows.append({
            'kind': 'continuous',
            'label': label_latex,
            'label_plain': label_plain,
            'lonely_plain': fmt_cont_cell(per_group['Lonely']),
            'nonlonely_plain': fmt_cont_cell(per_group['Non-Lonely']),
            'lonely_latex': fmt_cont_cell(per_group['Lonely'], latex=True),
            'nonlonely_latex': fmt_cont_cell(per_group['Non-Lonely'], latex=True),
            'test_plain': fmt_test_plain(test),
            'test_latex': fmt_test_latex(test),
        })
        return per_group

    def add_categorical(var):
        per_group, test, levels = compare_categorical(df, var)
        rows.append({
            'kind': 'cat_header',
            'label': f'{LABELS[var]}, n (\\%)',
            'label_plain': f'{LABELS[var]}, n (%)',
            'lonely_plain': '',
            'nonlonely_plain': '',
            'test_plain': fmt_test_plain(test),
            'test_latex': fmt_test_latex(test),
        })
        for level in levels:
            rows.append({
                'kind': 'cat_level',
                'label': level,
                'lonely_plain': fmt_cat_cell_plain(per_group['Lonely'][level]),
                'nonlonely_plain': fmt_cat_cell_plain(per_group['Non-Lonely'][level]),
                'lonely_latex': fmt_cat_cell(per_group['Lonely'][level]),
                'nonlonely_latex': fmt_cat_cell(per_group['Non-Lonely'][level]),
            })

    def add_ethnicity():
        per_group, test, continents = compare_ethnicity(df)
        rows.append({
            'kind': 'cat_header',
            'label': r'Ethnicity\textsuperscript{a}, n (\%)',
            'label_plain': 'Ethnicity (a), n (%)',
            'lonely_plain': '',
            'nonlonely_plain': '',
            'test_plain': fmt_test_plain(test),
            'test_latex': fmt_test_latex(test),
        })
        for c in continents:
            rows.append({
                'kind': 'cat_level',
                'label': c,
                'lonely_plain': fmt_cat_cell_plain(per_group['Lonely'][c]),
                'nonlonely_plain': fmt_cat_cell_plain(per_group['Non-Lonely'][c]),
                'lonely_latex': fmt_cat_cell(per_group['Lonely'][c]),
                'nonlonely_latex': fmt_cat_cell(per_group['Non-Lonely'][c]),
            })
        footnotes['a'] = True

    def add_exclusion():
        per_group, test = compare_exclusion(exclusion_df)
        l, nl = per_group['Lonely'], per_group['Non-Lonely']
        rows.append({
            'kind': 'continuous',
            'label': 'Excluded from analysis, n/N (\\%)',
            'label_plain': 'Excluded from analysis, n/N (%)',
            'lonely_plain': f"{l['n_excluded']}/{l['n_total']} ({l['pct']:.1f}%)",
            'nonlonely_plain': f"{nl['n_excluded']}/{nl['n_total']} ({nl['pct']:.1f}%)",
            'lonely_latex': f"{l['n_excluded']}/{l['n_total']} ({l['pct']:.1f}\\%)",
            'nonlonely_latex': f"{nl['n_excluded']}/{nl['n_total']} ({nl['pct']:.1f}\\%)",
            'test_plain': fmt_test_plain(test),
            'test_latex': fmt_test_latex(test),
        })

    section('Demographics')
    add_continuous('age')
    add_categorical('gender')
    add_categorical('ses')
    add_ethnicity()
    add_continuous('handedness')

    section('EEG data quality')
    add_exclusion()
    for var in CONTINUOUS_QC:
        add_continuous(var)

    section('Mental-health questionnaires')
    ucla_per_group = add_continuous(UCLA_COL, footnote_marker='b')
    footnotes['b'] = {
        'lonely_n': ucla_per_group['Lonely']['n'],
        'nonlonely_n': ucla_per_group['Non-Lonely']['n'],
    }
    for var in CONTINUOUS_MH:
        add_continuous(var)

    return rows, footnotes


MH_SECTION_LABEL = 'Mental-health questionnaires'


def split_rows_by_table(rows, footnotes):
    """Partition rows + footnotes into the demographics and mental-health tables.

    The mental-health section header is dropped: the standalone MH table needs
    no internal divider.
    """
    demo_rows, mh_rows = [], []
    target = demo_rows
    for row in rows:
        if row['kind'] == 'section' and row['label'] == MH_SECTION_LABEL:
            target = mh_rows
            continue
        target.append(row)

    demo_footnotes = {k: v for k, v in footnotes.items() if k == 'a'}
    mh_footnotes = {k: v for k, v in footnotes.items() if k == 'b'}
    return demo_rows, demo_footnotes, mh_rows, mh_footnotes


# ---------------------------------------------------------------------------
# Renderers
# ---------------------------------------------------------------------------

def render_tsv(rows, df, drop_log, footnotes, out_path):
    n_lonely = int((df[GROUP_COL] == 'Lonely').sum())
    n_nonlonely = int((df[GROUP_COL] == 'Non-Lonely').sum())
    records = []
    for row in rows:
        if row['kind'] == 'section':
            records.append({
                'variable': f'== {row["label"]} ==',
                f'Lonely (n={n_lonely})': '',
                f'Non-Lonely (n={n_nonlonely})': '',
                'test': '',
            })
        elif row['kind'] == 'continuous':
            label = row.get('label_plain', row['label'])
            records.append({
                'variable': label,
                f'Lonely (n={n_lonely})': row['lonely_plain'],
                f'Non-Lonely (n={n_nonlonely})': row['nonlonely_plain'],
                'test': row['test_plain'],
            })
        elif row['kind'] == 'cat_header':
            records.append({
                'variable': row['label_plain'],
                f'Lonely (n={n_lonely})': '',
                f'Non-Lonely (n={n_nonlonely})': '',
                'test': row['test_plain'],
            })
        elif row['kind'] == 'cat_level':
            records.append({
                'variable': f'  {row["label"]}',
                f'Lonely (n={n_lonely})': row['lonely_plain'],
                f'Non-Lonely (n={n_nonlonely})': row['nonlonely_plain'],
                'test': '',
            })
    out = pd.DataFrame.from_records(records)
    out.to_csv(out_path, sep='\t', index=False)
    with open(out_path, 'a') as f:
        f.write(f"# Sample sizes: initial={drop_log['initial']}; "
                f"dropped (exclude flag)={drop_log['dropped_exclude_flag']}; "
                f"dropped (QC: >{MAX_BAD_CHANNELS} bad channels or <{MIN_TRIALS} trials in any of "
                f"{', '.join(QC_TRIAL_COLS)})={drop_log['dropped_qc']}; "
                f"dropped (missing group)={drop_log['dropped_missing_group']}; "
                f"final={drop_log['final']}\n")
        if 'a' in footnotes:
            f.write("# Note (a): Ethnicity reflects the continent(s) of birth of "
                    "participants' parents and grandparents. Participants could report "
                    "multiple continents, so column sums can exceed the group n.\n")
        if 'b' in footnotes:
            fn = footnotes['b']
            f.write(
                f"# Note (b): UCLA-LS scores were missing for part of the sample; "
                f"the comparison is based on n={fn['lonely_n']} Lonely and "
                f"n={fn['nonlonely_n']} Non-Lonely participants.\n"
            )


def render_latex(rows, df, drop_log, footnotes, caption, label, out_path):
    n_lonely = int((df[GROUP_COL] == 'Lonely').sum())
    n_nonlonely = int((df[GROUP_COL] == 'Non-Lonely').sum())

    header_row = (
        f'\\textbf{{Variable}} & \\textbf{{Lonely}} ($n={n_lonely}$) '
        f'& \\textbf{{Non-Lonely}} ($n={n_nonlonely}$) & \\textbf{{Test}} \\\\'
    )

    lines = []
    lines.append(r'% Required packages: booktabs, xltabular, array.')
    lines.append(r'% Add to your preamble:')
    lines.append(r'%   \usepackage{booktabs}')
    lines.append(r'%   \usepackage{xltabular}  % longtable + tabularx; allows page breaks')
    lines.append(r'%   \usepackage{array}')
    lines.append(r'\begingroup')
    lines.append(r'\scriptsize')
    lines.append(r'\setlength{\tabcolsep}{3pt}')
    lines.append(r'\renewcommand{\arraystretch}{1.1}')
    lines.append(
        r'\begin{xltabular}{\textwidth}{@{}'
        r'>{\raggedright\arraybackslash}p{4cm}'
        r'>{\raggedright\arraybackslash}p{3.6cm}'
        r'>{\raggedright\arraybackslash}p{3.6cm}'
        r'>{\raggedright\arraybackslash}X@{}}'
    )
    lines.append(r'\caption{' + caption + r'}')
    lines.append(r'\label{' + label + r'} \\')
    lines.append(r'\toprule')
    lines.append(header_row)
    lines.append(r'\midrule')
    lines.append(r'\endfirsthead')
    lines.append(r'\multicolumn{4}{@{}l}{\textit{Table \ref{' + label + r'} (continued)}} \\')
    lines.append(r'\toprule')
    lines.append(header_row)
    lines.append(r'\midrule')
    lines.append(r'\endhead')
    lines.append(r'\midrule')
    lines.append(r'\multicolumn{4}{r@{}}{\textit{continued on next page}} \\')
    lines.append(r'\endfoot')
    lines.append(r'\bottomrule')
    lines.append(r'\endlastfoot')

    first_section = True
    for row in rows:
        if row['kind'] == 'section':
            if not first_section:
                lines.append(r'\midrule')
            first_section = False
            lines.append(f"\\multicolumn{{4}}{{@{{}}l}}{{\\textbf{{{row['label']}}}}} \\\\")
        elif row['kind'] == 'continuous':
            l_cell = row.get('lonely_latex', row['lonely_plain'])
            nl_cell = row.get('nonlonely_latex', row['nonlonely_plain'])
            lines.append(
                f"{row['label']} & {l_cell} & {nl_cell} & {row['test_latex']} \\\\"
            )
        elif row['kind'] == 'cat_header':
            lines.append(
                f"{row['label']} & & & {row['test_latex']} \\\\"
            )
        elif row['kind'] == 'cat_level':
            lines.append(
                f"\\hspace{{1em}}{escape_latex(row['label'])} & {row['lonely_latex']} & {row['nonlonely_latex']} & \\\\"
            )

    lines.append(r'\end{xltabular}')
    note = (
        f"\\par\\vspace{{0.5ex}}\\noindent\\scriptsize Initial $n={drop_log['initial']}$; "
        f"dropped: exclude flag={drop_log['dropped_exclude_flag']}, "
        f"QC ($>${MAX_BAD_CHANNELS} bad channels or $<${MIN_TRIALS} trials in any of "
        f"{escape_latex(', '.join(QC_TRIAL_COLS))})={drop_log['dropped_qc']}, "
        f"missing group={drop_log['dropped_missing_group']}; "
        f"final $n={drop_log['final']}$."
    )
    lines.append(note)
    if 'a' in footnotes:
        eth_note = (
            r"\par\vspace{0.5ex}\noindent\scriptsize \textsuperscript{a}Ethnicity reflects "
            r"the continent(s) of birth of participants' parents and grandparents. "
            r"Participants could report multiple continents, so column sums can "
            r"exceed the group $n$."
        )
        lines.append(eth_note)
    if 'b' in footnotes:
        fn = footnotes['b']
        ucla_note = (
            r"\par\vspace{0.5ex}\noindent\scriptsize \textsuperscript{b}UCLA-LS "
            r"scores were missing for part of the sample; the comparison is "
            f"based on $n={fn['lonely_n']}$ Lonely and "
            f"$n={fn['nonlonely_n']}$ Non-Lonely participants."
        )
        lines.append(ucla_note)
    lines.append(r'\endgroup')

    out_path.write_text('\n'.join(lines) + '\n')


DEMO_CAPTION = (
    r'Demographics and EEG data quality by loneliness group. '
    r'Continuous variables: mean (SE) [min--max]. '
    r'Categorical variables: $n$ (\%). '
    r'Continuous comparisons use Welch-corrected independent $t$-tests, with '
    r"Cohen's $d$ (pooled SD) as the effect size; "
    r"categorical comparisons use Pearson $\chi^2$ tests, or Fisher's exact "
    r"when any observed cell count is below 5, with Cram\'er's $V$ as the "
    r'effect size.'
)

MH_CAPTION = (
    r'Mental-health questionnaire scores by loneliness group. '
    r'Values are mean (SE) [min--max]. '
    r"Group comparisons use Welch-corrected independent $t$-tests, with "
    r"Cohen's $d$ (pooled SD) as the effect size."
)


def escape_latex(text):
    if text is None:
        return ''
    replacements = [
        ('\\', r'\textbackslash{}'),
        ('&', r'\&'),
        ('%', r'\%'),
        ('$', r'\$'),
        ('#', r'\#'),
        ('_', r'\_'),
        ('{', r'\{'),
        ('}', r'\}'),
        ('~', r'\textasciitilde{}'),
        ('^', r'\textasciicircum{}'),
    ]
    out = str(text)
    for src, dst in replacements:
        out = out.replace(src, dst)
    return out


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main(participants_tsv, qc_tsv, out_tex, out_tsv, out_mh_tex, out_mh_tsv):
    participants_tsv = Path(participants_tsv)
    qc_tsv = Path(qc_tsv)
    out_tex = Path(out_tex)
    out_tsv = Path(out_tsv)
    out_mh_tex = Path(out_mh_tex)
    out_mh_tsv = Path(out_mh_tsv)

    print(f'Loading {participants_tsv} and {qc_tsv}')
    df, drop_log, exclusion_df = load_and_filter(participants_tsv, qc_tsv)
    print(f'Filter trace: {drop_log}')

    rows, footnotes = build_rows(df, exclusion_df)
    demo_rows, demo_footnotes, mh_rows, mh_footnotes = split_rows_by_table(
        rows, footnotes,
    )

    for path in (out_tex, out_tsv, out_mh_tex, out_mh_tsv):
        path.parent.mkdir(parents=True, exist_ok=True)

    render_tsv(demo_rows, df, drop_log, demo_footnotes, out_tsv)
    print(f'Wrote {out_tsv}')
    render_latex(
        demo_rows, df, drop_log, demo_footnotes,
        DEMO_CAPTION, 'tab:demographics', out_tex,
    )
    print(f'Wrote {out_tex}')

    render_tsv(mh_rows, df, drop_log, mh_footnotes, out_mh_tsv)
    print(f'Wrote {out_mh_tsv}')
    render_latex(
        mh_rows, df, drop_log, mh_footnotes,
        MH_CAPTION, 'tab:mental_health', out_mh_tex,
    )
    print(f'Wrote {out_mh_tex}')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='Build summary tables (demographics + QC, mental-health) by loneliness group.')
    parser.add_argument('--participants-tsv', required=True, type=Path,
                        help='Path to BIDS participants.tsv.')
    parser.add_argument('--qc-tsv', required=True, type=Path,
                        help='Path to preprocessing QC TSV produced by 3_quality_overview.py.')
    parser.add_argument('--out-tex', required=True, type=Path,
                        help='Output LaTeX path for the demographics + EEG-QC table.')
    parser.add_argument('--out-tsv', required=True, type=Path,
                        help='Output TSV path for the demographics + EEG-QC table.')
    parser.add_argument('--out-mh-tex', required=True, type=Path,
                        help='Output LaTeX path for the mental-health questionnaires table.')
    parser.add_argument('--out-mh-tsv', required=True, type=Path,
                        help='Output TSV path for the mental-health questionnaires table.')
    args = parser.parse_args()
    main(args.participants_tsv, args.qc_tsv,
         args.out_tex, args.out_tsv,
         args.out_mh_tex, args.out_mh_tsv)

# %%
