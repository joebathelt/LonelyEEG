2026-06-01:
- Added a control analysis (positive control / manipulation check) that confirms the canonical ERP effects are present in the comparison (Non-Lonely) group. `15_control_group_effects.R` reuses `results/cluster_amplitudes.tsv` (no new extraction) and runs, within the Non-Lonely group only, three within-subject paired t-tests: an emotion effect (angry vs happy at first presentation) at Hypersensitivity 1 and Hypersensitivity 2, and a repetition-suppression effect (rep 1 vs rep 5 for angry faces) at Hyperalertness. Tests are one-tailed in the pre-specified direction (angry > happy; first > fifth presentation, matching the difference-wave contrasts in `5b_extract_difference_waves.py`), with Cohen's d_z and directional Bayes Factors (`BayesFactor::ttestBF`, mu = 0, nullInterval = c(0, Inf)). Threshold alpha = 0.02 (no within-cluster correction; one test per cluster). Writes `results/control_group_effects_report.md`, `results/control_group_effects_results.tex` (prose), and `results/control_group_effects_table.tex`. Snakefile rule: `control_group_effects`.

2026-05-20:
- Added Bayes Factors alongside every frequentist test in `6_main_analysis.R`. Per-term inclusion Bayes Factors for the 3-way mixed ANOVA,
- Added an exploratory repetition-trend analysis using repetitions 1..6 per emotion.

2026-05-18:
- Added an analysis of the image ratings.
- Added sensitivity analyses to evaluate the influence of ethnicity confounds.

2026-05-17:
- Removed original IDs from the info field of the raw EEG files.
- Adjusted the preprocessing to cut out time before the experiment start, after the end, and mark break segments. I also added a lower bound amplitude threshold to reject flat channels.
- Changed reference in the preprocessing from Cz to the BioSemi CMS/DRL, which is the original recording reference. This avoids biases in the flat channel detection. Average referencing is still applied later after bad-channel interpolation.

2026-05-16:
- Added the cluster ERP figure pipeline. `7_extract_erp_waveforms.py` reuses the QC filter and cluster definitions from `5_extract_amplitudes.py`, loads each subject's epochs, and writes time-resolved cluster-averaged ERPs (one row per subject × emotion × repetition × cluster × timepoint) to `results/cluster_waveforms.tsv`, alongside a 2D projection of channel positions (with per-cluster booleans) to `results/cluster_channel_positions.tsv`. `8_figure_erps.py` consumes those two TSVs plus `cluster_amplitudes.tsv` and renders `results/figure_erps.pdf` (180 mm wide): a 3 (cluster) × 2 (comparison) grid showing the group ERPs (mean ± SE) with the cluster time window shaded in the background; the left column contrasts happy vs angry at rep 1, the right column contrasts rep 1 vs rep 5 within angry trials. Each panel carries a topomap inset (cluster channels highlighted) and a split-violin raincloud inset of the 4-cell amplitude distribution. Colour encodes group (Lonely vs Non-Lonely; Okabe–Ito palette), linetype encodes condition. Snakefile rules: `extract_erp_waveforms` and `figure_erps`.

2026-05-15:
- Added a new script, 4_demographics_table.py, to generate a LaTeX table summarizing participant demographics and data quality metrics, with appropriate statistical tests for group comparisons.
- Added the confirmatory main-analysis pipeline, split across Python (extraction) and R (stats). `5_extract_amplitudes.py` filters to the QC-passing analytic sample and extracts mean ERP amplitudes for three pre-registered spatiotemporal clusters (Hypersensitivity 1: 120-170 ms over CP5..O2; Hypersensitivity 2: 360-470 ms over right central-parietal; Hyperalertness: 480-600 ms over right hemisphere), writing a long-format TSV (`results/cluster_amplitudes.tsv`). `6_main_analysis.R` consumes that TSV and runs a 2x2x2 mixed ANOVA per cluster via `afex::aov_ez` (Type III SS, classical multi-stratum error terms, partial eta-squared). Post-hoc Welch t-tests are run **only to interpret a significant ANOVA effect**: a significant 3-way interaction is decomposed into 4 cell-level Lonely vs Non-Lonely comparisons; a significant 2-way interaction with group is decomposed into between-group comparisons at each level of the relevant within factor; a significant group main effect (without a group-involving interaction) is reported as one overall comparison on subject-mean amplitudes. Bonferroni: alpha < 0.02 across clusters (ANOVA) and 0.05/k within cluster for the simple-effect family. Outputs: `results/main_analysis_report.md` (full numerical report), `results/main_analysis_results.tex` (APA-style prose paragraph drop-in for the manuscript), and `results/main_analysis_table.tex` (LaTeX longtable). Snakefile rules: `extract_amplitudes` and `main_analysis`. R user-library packages: afex, lme4, car, pbkrtest, lmerTest, nloptr, emmeans, dplyr, tidyr, readr, stringr, knitr, xtable (libnlopt-dev was installed as a system dependency).

2026-05-14:
- Updated the reporting of data quality in 2_preprocess_eeg.py
- Made scripts callable from the command line with argparse for better usability.
- Created snake file to automate the entire pipeline from data organization to preprocessing.
- Added anonymisation script to rename participant IDs in the BIDS dataset for sharing without revealing identifying metadata.

2025-11-25:
- Revised code in 1_organise_bids.py to use *.bdf files instead of *.edf files for EEG data.
- Updated the 2_preprocess_eeg.py script to adjust epoch time windows and improve artefact rejection criteria.