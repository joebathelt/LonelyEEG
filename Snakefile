configfile: "config.yaml"

BIDS = config["bids_folder"]
QC_TSV = BIDS + "/derivatives/task-RovingOddball_desc-preprocessing_qc.tsv"
PARTICIPANTS_TSV = BIDS + "/participants.tsv"
DEMOGRAPHICS_TEX = "results/demographics_table.tex"
DEMOGRAPHICS_TSV = "results/demographics_table.tsv"
MENTAL_HEALTH_TEX = "results/mental_health_table.tex"
MENTAL_HEALTH_TSV = "results/mental_health_table.tsv"
BSI_SUBSCALES_TEX = "results/bsi_subscales_table.tex"
BSI_SUBSCALES_TSV = "results/bsi_subscales_table.tsv"
CLUSTER_AMPLITUDES_TSV = "results/cluster_amplitudes.tsv"
CLUSTER_WAVEFORMS_TSV = "results/cluster_waveforms.tsv"
CLUSTER_CHANNELS_TSV = "results/cluster_channel_positions.tsv"
CLUSTER_DIFF_WAVEFORMS_TSV = "results/cluster_difference_waveforms.tsv"
CLUSTER_DIFF_AMPLITUDES_TSV = "results/cluster_difference_amplitudes.tsv"
MAIN_REPORT = "results/main_analysis_report.md"
MAIN_PROSE_TEX = "results/main_analysis_results.tex"
MAIN_TABLE_TEX = "results/main_analysis_table.tex"
MAIN_SENS_EUROPE_REPORT = "results/main_analysis_sensitivity_europe_report.md"
MAIN_SENS_EUROPE_PROSE_TEX = "results/main_analysis_sensitivity_europe_results.tex"
MAIN_SENS_EUROPE_TABLE_TEX = "results/main_analysis_sensitivity_europe_table.tex"
MAIN_SENS_COV_REPORT = "results/main_analysis_sensitivity_ethnicity_covariate_report.md"
MAIN_SENS_COV_PROSE_TEX = "results/main_analysis_sensitivity_ethnicity_covariate_results.tex"
MAIN_SENS_COV_TABLE_TEX = "results/main_analysis_sensitivity_ethnicity_covariate_table.tex"
MAIN_SENS_BSI_REPORT = "results/main_analysis_sensitivity_bsi_gsi_covariate_report.md"
MAIN_SENS_BSI_PROSE_TEX = "results/main_analysis_sensitivity_bsi_gsi_covariate_results.tex"
MAIN_SENS_BSI_TABLE_TEX = "results/main_analysis_sensitivity_bsi_gsi_covariate_table.tex"
IMAGE_RATINGS_TSV = "results/image_ratings.tsv"
RATINGS_REPORT = "results/ratings_analysis_report.md"
RATINGS_PROSE_TEX = "results/ratings_analysis_results.tex"
RATINGS_TABLE_TEX = "results/ratings_analysis_table.tex"
FIGURE_ERPS = "results/figure_erps.pdf"
FIGURE_DIFF_WAVES = "results/figure_difference_waves.pdf"
CLUSTER_AMPLITUDES_ALL_REPS_TSV = "results/cluster_amplitudes_all_reps.tsv"
REPETITION_PREDICTIONS_TSV = "results/repetition_model_predictions.tsv"
REP_TRENDS_REPORT = "results/repetition_trends_report.md"
REP_TRENDS_PROSE_TEX = "results/repetition_trends_results.tex"
REP_TRENDS_TABLE_TEX = "results/repetition_trends_table.tex"
FIGURE_REP_TRENDS = "results/figure_repetition_trends.pdf"
CONTROL_GROUP_REPORT = "results/control_group_effects_report.md"
CONTROL_GROUP_PROSE_TEX = "results/control_group_effects_results.tex"
CONTROL_GROUP_TABLE_TEX = "results/control_group_effects_table.tex"
TESTS_REPORT = "results/tests_report.md"


rule all:
    input:
        QC_TSV,
        DEMOGRAPHICS_TEX,
        MENTAL_HEALTH_TEX,
        BSI_SUBSCALES_TEX,
        CLUSTER_AMPLITUDES_TSV,
        MAIN_REPORT,
        MAIN_PROSE_TEX,
        MAIN_TABLE_TEX,
        MAIN_SENS_EUROPE_REPORT,
        MAIN_SENS_EUROPE_PROSE_TEX,
        MAIN_SENS_EUROPE_TABLE_TEX,
        MAIN_SENS_COV_REPORT,
        MAIN_SENS_COV_PROSE_TEX,
        MAIN_SENS_COV_TABLE_TEX,
        MAIN_SENS_BSI_REPORT,
        MAIN_SENS_BSI_PROSE_TEX,
        MAIN_SENS_BSI_TABLE_TEX,
        IMAGE_RATINGS_TSV,
        RATINGS_REPORT,
        RATINGS_PROSE_TEX,
        RATINGS_TABLE_TEX,
        FIGURE_ERPS,
        FIGURE_DIFF_WAVES,
        CLUSTER_AMPLITUDES_ALL_REPS_TSV,
        REPETITION_PREDICTIONS_TSV,
        REP_TRENDS_REPORT,
        REP_TRENDS_PROSE_TEX,
        REP_TRENDS_TABLE_TEX,
        FIGURE_REP_TRENDS,
        CONTROL_GROUP_REPORT,
        CONTROL_GROUP_PROSE_TEX,
        CONTROL_GROUP_TABLE_TEX,
        TESTS_REPORT


# raw_data_folder is consumed by the script (subject folders are moved into
# {BIDS}/sourcedata/ keyed by their original IDs), so it's declared as a
# params rather than an input. The script is idempotent: re-runs handle a
# missing/empty raw_data and only allocate new anonymous IDs for any new
# subject folders dropped in. Force a re-run with `snakemake -R organise_bids`
# after adding new raw recordings.
rule organise_bids:
    input:
        experiment=config["experiment_folder"],
    output:
        touch(".snakemake_sentinels/organise_bids.done"),
    params:
        raw=config["raw_data_folder"],
    shell:
        "python code/1_organise_bids.py "
        "--raw-data-folder {params.raw} "
        f"--out-folder {BIDS} "
        "--experiment-folder {input.experiment}"


rule preprocess:
    input:
        bids_done=".snakemake_sentinels/organise_bids.done",
        channel_config=config["channel_config"],
    output:
        touch(".snakemake_sentinels/preprocess.done"),
    shell:
        "python code/2_preprocess_eeg.py "
        "--channel-config {input.channel_config} "
        f"--in-folder {BIDS}"


rule quality_overview:
    input:
        preprocess_done=".snakemake_sentinels/preprocess.done",
    output:
        QC_TSV,
    shell:
        "python code/3_quality_overview.py "
        f"--derivatives-folder {BIDS}/derivatives "
        "--out-file {output}"


rule demographics_table:
    input:
        participants=PARTICIPANTS_TSV,
        qc=QC_TSV,
    output:
        tex=DEMOGRAPHICS_TEX,
        tsv=DEMOGRAPHICS_TSV,
        mh_tex=MENTAL_HEALTH_TEX,
        mh_tsv=MENTAL_HEALTH_TSV,
        bsi_tex=BSI_SUBSCALES_TEX,
        bsi_tsv=BSI_SUBSCALES_TSV,
    shell:
        "python code/4_demographics_table.py "
        "--participants-tsv {input.participants} "
        "--qc-tsv {input.qc} "
        "--out-tex {output.tex} "
        "--out-tsv {output.tsv} "
        "--out-mh-tex {output.mh_tex} "
        "--out-mh-tsv {output.mh_tsv} "
        "--out-bsi-tex {output.bsi_tex} "
        "--out-bsi-tsv {output.bsi_tsv}"


rule extract_amplitudes:
    input:
        participants=PARTICIPANTS_TSV,
        qc=QC_TSV,
        preprocess_done=".snakemake_sentinels/preprocess.done",
    output:
        tsv=CLUSTER_AMPLITUDES_TSV,
    shell:
        "python code/5_extract_amplitudes.py "
        "--participants-tsv {input.participants} "
        "--qc-tsv {input.qc} "
        f"--bids-folder {BIDS} "
        "--out-tsv {output.tsv}"


rule main_analysis:
    input:
        amplitudes=CLUSTER_AMPLITUDES_TSV,
    output:
        report=MAIN_REPORT,
        prose=MAIN_PROSE_TEX,
        table=MAIN_TABLE_TEX,
    shell:
        "Rscript code/6_main_analysis.R "
        "--amplitudes-tsv {input.amplitudes} "
        "--out-report {output.report} "
        "--out-prose-tex {output.prose} "
        "--out-table-tex {output.table}"


# Sensitivity analysis: re-runs the main 3-way mixed ANOVA on the subset of
# participants who report at least one grandparent born in Europe. Restriction
# is applied inside 6_main_analysis.R via --ethnicity-filter, which token-
# matches the comma-separated `ethnicity` column in participants.tsv.
rule sensitivity_analysis_ethnicity_europe:
    input:
        amplitudes=CLUSTER_AMPLITUDES_TSV,
        participants=PARTICIPANTS_TSV,
    output:
        report=MAIN_SENS_EUROPE_REPORT,
        prose=MAIN_SENS_EUROPE_PROSE_TEX,
        table=MAIN_SENS_EUROPE_TABLE_TEX,
    shell:
        "Rscript code/6_main_analysis.R "
        "--amplitudes-tsv {input.amplitudes} "
        "--participants-tsv {input.participants} "
        "--ethnicity-filter Europe "
        "--out-report {output.report} "
        "--out-prose-tex {output.prose} "
        "--out-table-tex {output.table}"


# Sensitivity analysis (ANCOVA): re-runs the main 3-way mixed ANOVA on the
# full post-QC sample (NA-ethnicity participants dropped) with a binary
# European-descent indicator (1 if `ethnicity` lists "Europe" as a parent/
# grandparent continent of birth, 0 otherwise) included as a between-subjects
# nuisance covariate via afex::aov_ez(covariate = ...). Mutually exclusive
# with --ethnicity-filter.
rule sensitivity_analysis_ethnicity_covariate:
    input:
        amplitudes=CLUSTER_AMPLITUDES_TSV,
        participants=PARTICIPANTS_TSV,
    output:
        report=MAIN_SENS_COV_REPORT,
        prose=MAIN_SENS_COV_PROSE_TEX,
        table=MAIN_SENS_COV_TABLE_TEX,
    shell:
        "Rscript code/6_main_analysis.R "
        "--amplitudes-tsv {input.amplitudes} "
        "--participants-tsv {input.participants} "
        "--ethnicity-covariate Europe "
        "--out-report {output.report} "
        "--out-prose-tex {output.prose} "
        "--out-table-tex {output.table}"


# Sensitivity analysis (ANCOVA): re-runs the main 3-way mixed ANOVA on the
# full post-QC sample (participants with NA bsi53_gsi dropped) with the BSI-53
# General Severity Index (overall mental-health symptom load) included as a
# continuous between-subjects nuisance covariate via
# afex::aov_ez(covariate = ...). Controls for between-group differences in
# co-occurring mental-health difficulties confounded with loneliness. Mutually
# exclusive with --ethnicity-filter and --ethnicity-covariate.
rule sensitivity_analysis_bsi_gsi_covariate:
    input:
        amplitudes=CLUSTER_AMPLITUDES_TSV,
        participants=PARTICIPANTS_TSV,
    output:
        report=MAIN_SENS_BSI_REPORT,
        prose=MAIN_SENS_BSI_PROSE_TEX,
        table=MAIN_SENS_BSI_TABLE_TEX,
    shell:
        "Rscript code/6_main_analysis.R "
        "--amplitudes-tsv {input.amplitudes} "
        "--participants-tsv {input.participants} "
        "--bsi-covariate bsi53_gsi "
        "--out-report {output.report} "
        "--out-prose-tex {output.prose} "
        "--out-table-tex {output.table}"


rule extract_ratings:
    input:
        participants=PARTICIPANTS_TSV,
        qc=QC_TSV,
        preprocess_done=".snakemake_sentinels/preprocess.done",
    output:
        tsv=IMAGE_RATINGS_TSV,
    shell:
        "python code/9_extract_ratings.py "
        "--participants-tsv {input.participants} "
        "--qc-tsv {input.qc} "
        f"--bids-folder {BIDS} "
        "--out-tsv {output.tsv}"


rule ratings_analysis:
    input:
        ratings=IMAGE_RATINGS_TSV,
    output:
        report=RATINGS_REPORT,
        prose=RATINGS_PROSE_TEX,
        table=RATINGS_TABLE_TEX,
    shell:
        "Rscript code/10_ratings_analysis.R "
        "--ratings-tsv {input.ratings} "
        "--out-report {output.report} "
        "--out-prose-tex {output.prose} "
        "--out-table-tex {output.table}"


rule extract_erp_waveforms:
    input:
        participants=PARTICIPANTS_TSV,
        qc=QC_TSV,
        preprocess_done=".snakemake_sentinels/preprocess.done",
    output:
        waveforms=CLUSTER_WAVEFORMS_TSV,
        channels=CLUSTER_CHANNELS_TSV,
    shell:
        "python code/7_extract_erp_waveforms.py "
        "--participants-tsv {input.participants} "
        "--qc-tsv {input.qc} "
        f"--bids-folder {BIDS} "
        "--out-waveforms-tsv {output.waveforms} "
        "--out-channels-tsv {output.channels}"


rule figure_erps:
    input:
        waveforms=CLUSTER_WAVEFORMS_TSV,
        amplitudes=CLUSTER_AMPLITUDES_TSV,
        channels=CLUSTER_CHANNELS_TSV,
    output:
        figure=FIGURE_ERPS,
    shell:
        "python code/8_figure_erps.py "
        "--waveforms-tsv {input.waveforms} "
        "--amplitudes-tsv {input.amplitudes} "
        "--channels-tsv {input.channels} "
        "--out-figure {output.figure}"


# Within-subject difference Evokeds per individual via MNE contrasts
# (mne.combine_evoked weights=[1, -1]): angry - happy at rep 1, and rep 5 -
# rep 1 for angry. Emits per-subject difference waveforms and amplitudes that
# the difference-wave figure only needs to average within group. Reuses the
# cluster definitions and analytic-sample filter from 5_extract_amplitudes.py.
rule extract_difference_waves:
    input:
        participants=PARTICIPANTS_TSV,
        qc=QC_TSV,
        preprocess_done=".snakemake_sentinels/preprocess.done",
    output:
        waveforms=CLUSTER_DIFF_WAVEFORMS_TSV,
        amplitudes=CLUSTER_DIFF_AMPLITUDES_TSV,
    shell:
        "python code/5b_extract_difference_waves.py "
        "--participants-tsv {input.participants} "
        "--qc-tsv {input.qc} "
        f"--bids-folder {BIDS} "
        "--out-waveforms-tsv {output.waveforms} "
        "--out-amplitudes-tsv {output.amplitudes}"


# Companion to figure_erps: 3 (cluster) x 2 (comparison) grid of within-
# subject difference waves (angry - happy at rep 1; rep 1 - rep 5 for angry),
# one trace per group with SE shading, plus topomap and difference-amplitude
# raincloud insets. Differences are computed per individual upstream
# (extract_difference_waves); this rule only group-averages them.
rule figure_difference_waves:
    input:
        diff_waveforms=CLUSTER_DIFF_WAVEFORMS_TSV,
        diff_amplitudes=CLUSTER_DIFF_AMPLITUDES_TSV,
        channels=CLUSTER_CHANNELS_TSV,
    output:
        figure=FIGURE_DIFF_WAVES,
    shell:
        "python code/8b_figure_difference_waves.py "
        "--diff-waveforms-tsv {input.diff_waveforms} "
        "--diff-amplitudes-tsv {input.diff_amplitudes} "
        "--channels-tsv {input.channels} "
        "--out-figure {output.figure}"


# Per-cluster mean ERP amplitudes across repetitions 1..6 per emotion,
# feeding the exploratory linear/log repetition-trend analysis. Reuses the
# cluster definitions and analytic-sample filter from 5_extract_amplitudes.py.
# Rep 7 is excluded upstream as too noisy (too few trials per participant).
rule extract_amplitudes_all_reps:
    input:
        participants=PARTICIPANTS_TSV,
        qc=QC_TSV,
        preprocess_done=".snakemake_sentinels/preprocess.done",
    output:
        tsv=CLUSTER_AMPLITUDES_ALL_REPS_TSV,
    shell:
        "python code/11_extract_amplitudes_all_reps.py "
        "--participants-tsv {input.participants} "
        "--qc-tsv {input.qc} "
        f"--bids-folder {BIDS} "
        "--out-tsv {output.tsv}"


# Exploratory: per cluster x emotion, fit a linear mixed-effects model with
# repetition as a continuous predictor (linear and logarithmic forms);
# Bonferroni-corrected fixed-effect tests of the group main effect (rep-1
# intercept difference) and group:rep interaction (slope difference);
# AIC + marginal/conditional R^2 to recommend a model per cell. Writes the
# population-level predictions per (cluster, emotion, model, group, rep)
# for the figure script.
rule repetition_trends_analysis:
    input:
        amplitudes=CLUSTER_AMPLITUDES_ALL_REPS_TSV,
    output:
        report=REP_TRENDS_REPORT,
        prose=REP_TRENDS_PROSE_TEX,
        table=REP_TRENDS_TABLE_TEX,
        predictions=REPETITION_PREDICTIONS_TSV,
    shell:
        "Rscript code/13_repetition_trends_analysis.R "
        "--amplitudes-tsv {input.amplitudes} "
        "--out-report {output.report} "
        "--out-prose-tex {output.prose} "
        "--out-table-tex {output.table} "
        "--out-predictions-tsv {output.predictions}"


# Figure: 3 (cluster) x 2 (emotion) grid showing per-group mean amplitude
# trajectories across reps 1..6 with SE bars, overlaid with the LMM
# population-level predictions for linear and log models (recommended
# model solid, alternative dashed).
rule figure_repetition_trends:
    input:
        amplitudes=CLUSTER_AMPLITUDES_ALL_REPS_TSV,
        predictions=REPETITION_PREDICTIONS_TSV,
    output:
        figure=FIGURE_REP_TRENDS,
    shell:
        "python code/14_figure_repetition_trends.py "
        "--amplitudes-tsv {input.amplitudes} "
        "--predictions-tsv {input.predictions} "
        "--out-figure {output.figure}"


# Control analysis (positive control / manipulation check): within the
# comparison (Non-Lonely) group only, three within-subject paired t-tests on
# the pre-extracted cluster amplitudes confirm the canonical effects -- an
# emotion effect (angry vs happy at first presentation) at Hypersensitivity 1
# and 2, and a repetition-suppression effect (rep 1 vs rep 5 for angry faces)
# at Hyperalertness. One-tailed in the pre-specified direction, with directional
# Bayes Factors. Reuses results/cluster_amplitudes.tsv from extract_amplitudes.
rule control_group_effects:
    input:
        amplitudes=CLUSTER_AMPLITUDES_TSV,
    output:
        report=CONTROL_GROUP_REPORT,
        prose=CONTROL_GROUP_PROSE_TEX,
        table=CONTROL_GROUP_TABLE_TEX,
    shell:
        "Rscript code/15_control_group_effects.R "
        "--amplitudes-tsv {input.amplitudes} "
        "--out-report {output.report} "
        "--out-prose-tex {output.prose} "
        "--out-table-tex {output.table}"


# Unit-test suites for the two scripts whose helpers we own end-to-end
# (preprocessing helpers and the main R analysis). The renderer drives both
# suites and emits a single markdown report; it returns non-zero if any test
# fails, so this rule fails loudly when something regresses.
rule tests:
    input:
        py_test="code/tests/test_preprocess_eeg.py",
        r_test="code/tests/test_main_analysis.R",
        renderer="code/tests/render_report.py",
        py_src="code/2_preprocess_eeg.py",
        r_src="code/6_main_analysis.R",
    output:
        report=TESTS_REPORT,
    shell:
        "python code/tests/render_report.py --out-md {output.report}"


rule clean:
    shell:
        f"rm -rf {BIDS}/derivatives logs .snakemake_sentinels/preprocess.done"


# Removes analysis outputs downstream of preprocessing but keeps the
# preprocessed derivatives and preprocess sentinel intact, so re-runs skip
# the expensive preprocessing step.
rule clean_analysis:
    shell:
        "rm -f "
        f"{QC_TSV} "
        f"{DEMOGRAPHICS_TEX} {DEMOGRAPHICS_TSV} "
        f"{MENTAL_HEALTH_TEX} {MENTAL_HEALTH_TSV} "
        f"{CLUSTER_AMPLITUDES_TSV} "
        f"{CLUSTER_WAVEFORMS_TSV} {CLUSTER_CHANNELS_TSV} "
        f"{MAIN_REPORT} {MAIN_PROSE_TEX} {MAIN_TABLE_TEX} "
        f"{MAIN_SENS_EUROPE_REPORT} {MAIN_SENS_EUROPE_PROSE_TEX} {MAIN_SENS_EUROPE_TABLE_TEX} "
        f"{MAIN_SENS_COV_REPORT} {MAIN_SENS_COV_PROSE_TEX} {MAIN_SENS_COV_TABLE_TEX} "
        f"{MAIN_SENS_BSI_REPORT} {MAIN_SENS_BSI_PROSE_TEX} {MAIN_SENS_BSI_TABLE_TEX} "
        f"{CLUSTER_AMPLITUDES_ALL_REPS_TSV} {REPETITION_PREDICTIONS_TSV} "
        f"{REP_TRENDS_REPORT} {REP_TRENDS_PROSE_TEX} {REP_TRENDS_TABLE_TEX} "
        f"{FIGURE_REP_TRENDS} "
        f"{CONTROL_GROUP_REPORT} {CONTROL_GROUP_PROSE_TEX} {CONTROL_GROUP_TABLE_TEX} "
        f"{TESTS_REPORT} "
        f"{FIGURE_ERPS} "
        f"{FIGURE_DIFF_WAVES}"
