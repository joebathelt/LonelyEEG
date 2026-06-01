# Loneliness in the Brain: Hypersensitivity vs. Hyperalertness

EEG analysis pipeline for a study investigating whether subjective loneliness is
associated with altered automatic processing of social-emotional information.
Young adults, grouped by their score on the **UCLA Loneliness Scale (Version 3)**,
performed a **roving oddball task** with happy and angry emotional face stimuli
while 64-channel EEG was recorded. The analysis tests whether loneliness modulates
the event-related potential (ERP) response to emotional faces, and dissociates two
candidate accounts — **hypersensitivity** (heightened early/mid-latency responses
to emotional content) and **hyperalertness** (heightened later sustained responses).

- **Institution:** Department of Psychology, University of Amsterdam
- **Acquisition:** BioSemi ActiveTwo, 72 channels @ 2048 Hz
- **Authors:** Joe Bathelt, Corine van Dijk, Marte Otten

> Bathelt, J., van Dijk, C., & Otten, M. (forthcoming).
> *Loneliness in the Brain: Distinguishing Between Hypersensitivity and Hyperalertness.*
> University of Amsterdam.

## Overview

The repository contains a fully automated, [Snakemake](https://snakemake.github.io/)-driven
pipeline that goes from raw BioSemi recordings to publication-ready figures,
LaTeX tables, and APA-style prose. The data themselves are organised following the
[BIDS](https://bids.org/) standard.

The analysis proceeds through the following stages:

1. **Organise** raw recordings into a BIDS dataset (with anonymised participant IDs).
2. **Preprocess** the EEG (filtering, bad-channel detection, ICA, AutoReject,
   epoching) → cleaned epochs + per-subject QC reports.
3. **Extract** mean ERP amplitudes for three pre-registered spatiotemporal clusters.
4. **Analyse** group differences, sensitivity analyses, repetition trends, and
   behavioural ratings.
5. **Report** results as figures, tables, and prose.

### Pre-registered clusters

| Cluster | Window | Interpretation |
|---|---|---|
| Hypersensitivity 1 | 120–170 ms | early visual / facial response |
| Hypersensitivity 2 | 360–470 ms | mid-latency emotional evaluation |
| Hyperalertness | 480–600 ms | later sustained / arousal response |

## Repository layout

```
.
├── Snakefile                 # End-to-end pipeline definition
├── config.yaml               # Paths to raw data, BIDS folder, channel config
├── UvA_DiverseConfig_ChTypes.csv  # Channel type configuration
├── code/                     # Analysis scripts (numbered by pipeline order)
│   ├── 1_organise_bids.py
│   ├── 2_preprocess_eeg.py
│   ├── 3_quality_overview.py
│   ├── 4_demographics_table.py
│   ├── 5_extract_amplitudes.py
│   ├── 5b_extract_difference_waves.py
│   ├── 6_main_analysis.R          # 3-way mixed ANOVA + Bayes Factors
│   ├── 7_extract_erp_waveforms.py
│   ├── 8_figure_erps.py
│   ├── 8b_figure_difference_waves.py
│   ├── 9_extract_ratings.py
│   ├── 10_ratings_analysis.R
│   ├── 11_extract_amplitudes_all_reps.py
│   ├── 13_repetition_trends_analysis.R
│   ├── 14_figure_repetition_trends.py
│   ├── 15_control_group_effects.R  # Positive-control manipulation check
│   ├── tests/                # Unit tests for preprocessing + main analysis
│   └── log.md                # Chronological development log
├── experiment/               # PsychoPy / MATLAB experiment presentation code
├── data/                     # BIDS dataset + derivatives (not tracked in git)
└── results/                  # Generated figures, tables, reports (not tracked)
```

> **Note:** `data/`, `results/`, and the movie-task stimuli are git-ignored. The
> repository ships the analysis code; the EEG dataset is distributed separately
> (the face stimuli are from a restricted-access database and cannot be redistributed).

## The task

A roving oddball implemented in the visual domain: trains of a repeated face
identity are interrupted by a change to a new identity (the "deviant" that begins
the next train). Faces display a **happy** or **angry** expression. Each face
carries a trigger code indexing its (colour × emotion) combination and its serial
position within the train (1 = deviant, 2–8 = standards). After the EEG session,
participants rated each face on valence, arousal, and dominance
(Self-Assessment Manikin, 1–5 scale).

The full trigger-code mapping is defined in
[code/1_organise_bids.py](code/1_organise_bids.py) (`EVENT_DICT`).

## Requirements

**Python** (≥ 3.12)
- `mne`, `autoreject`, `numpy`, `scipy`, `pandas`, `matplotlib`, `seaborn`

**R**
- `afex`, `lme4`, `lmerTest`, `BayesFactor`, `bayestestR`, `MuMIn`,
  `dplyr`, `tidyr`, `readr`, `stringr`

**Workflow**
- [Snakemake](https://snakemake.github.io/)

> On Linux, `afex`/`lme4` build chain requires the `libnlopt-dev` system package.

## Usage

Configure the data paths in [config.yaml](config.yaml):

```yaml
raw_data_folder: data/raw_data
bids_folder: data/bids
channel_config: UvA_DiverseConfig_ChTypes.csv
experiment_folder: experiment/rovingOddball_Sep24
```

Run the entire pipeline:

```bash
snakemake --cores 8
```

Run (or re-run) a single stage:

```bash
snakemake --cores 8 main_analysis      # confirmatory ANOVA + Bayes Factors
snakemake --cores 8 figure_erps         # cluster ERP figure
snakemake -R organise_bids              # force re-organising after adding raw data
```

Clean up:

```bash
snakemake clean           # remove derivatives + preprocessing sentinel
snakemake clean_analysis  # remove analysis outputs but keep preprocessed data
```

Run the test suites (preprocessing helpers + main R analysis):

```bash
snakemake --cores 1 tests
```

## Analyses

| Analysis | Script(s) | Output |
|---|---|---|
| **Main analysis** — 2×2×2 (group × emotion × repetition) mixed ANOVA per cluster, with per-term inclusion Bayes Factors and pre-registered post-hoc Welch t-tests | [5_extract_amplitudes.py](code/5_extract_amplitudes.py), [6_main_analysis.R](code/6_main_analysis.R) | `main_analysis_*` |
| **Sensitivity analyses** — ethnicity subset/covariate; BSI-53 GSI (mental-health) covariate | [6_main_analysis.R](code/6_main_analysis.R) | `main_analysis_sensitivity_*` |
| **Control / manipulation check** — canonical emotion + repetition-suppression effects within the Non-Lonely group | [15_control_group_effects.R](code/15_control_group_effects.R) | `control_group_effects_*` |
| **Repetition trends** — exploratory linear/log mixed-effects trends across repetitions 1–6 | [11_extract_amplitudes_all_reps.py](code/11_extract_amplitudes_all_reps.py), [13_repetition_trends_analysis.R](code/13_repetition_trends_analysis.R) | `repetition_trends_*` |
| **Behavioural ratings** — valence/arousal/dominance ratings of the face stimuli | [9_extract_ratings.py](code/9_extract_ratings.py), [10_ratings_analysis.R](code/10_ratings_analysis.R) | `ratings_analysis_*` |
| **Figures** — group ERPs, difference waves, repetition trends | [8_figure_erps.py](code/8_figure_erps.py), [8b_figure_difference_waves.py](code/8b_figure_difference_waves.py), [14_figure_repetition_trends.py](code/14_figure_repetition_trends.py) | `figure_*.pdf` |
| **Demographics** — sample description + data-quality table with group comparisons | [4_demographics_table.py](code/4_demographics_table.py) | `demographics_table.*`, `mental_health_table.*` |

Each statistical analysis emits three artefacts: a full numerical Markdown report,
an APA-style prose `.tex` drop-in for the manuscript, and a LaTeX table.

## Statistical approach

The confirmatory analysis is a 3-way mixed ANOVA (`afex::aov_ez`, Type III SS,
partial η²) per cluster, evaluated at α < 0.02 per term. Pre-registered post-hoc
Welch t-tests decompose significant effects involving group. Every frequentist
test is accompanied by a Bayes Factor (BayesFactor / bayestestR, default JZS
priors) quantifying evidence for the alternative (BF₁₀) and the null (BF₀₁),
interpreted by Jeffreys' conventions (BF > 3 substantial, > 10 strong, > 30 very strong).

## License

Code is released under the terms described by the maintainer; the accompanying
BIDS dataset is licensed CC0. The FACES stimulus images are **not** redistributable
— see the [FACES database](https://faces.mpdl.mpg.de/imeji/).

## Contact

Joe Bathelt — Department of Psychology, University of Amsterdam — j.m.c.bathelt@uva.nl
