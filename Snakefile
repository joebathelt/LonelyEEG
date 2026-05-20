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
MAIN_REPORT = "results/main_analysis_report.md"
MAIN_PROSE_TEX = "results/main_analysis_results.tex"
MAIN_TABLE_TEX = "results/main_analysis_table.tex"
MAIN_SENS_EUROPE_REPORT = "results/main_analysis_sensitivity_europe_report.md"
MAIN_SENS_EUROPE_PROSE_TEX = "results/main_analysis_sensitivity_europe_results.tex"
MAIN_SENS_EUROPE_TABLE_TEX = "results/main_analysis_sensitivity_europe_table.tex"
MAIN_SENS_COV_REPORT = "results/main_analysis_sensitivity_ethnicity_covariate_report.md"
MAIN_SENS_COV_PROSE_TEX = "results/main_analysis_sensitivity_ethnicity_covariate_results.tex"
MAIN_SENS_COV_TABLE_TEX = "results/main_analysis_sensitivity_ethnicity_covariate_table.tex"
IMAGE_RATINGS_TSV = "results/image_ratings.tsv"
RATINGS_REPORT = "results/ratings_analysis_report.md"
RATINGS_PROSE_TEX = "results/ratings_analysis_results.tex"
RATINGS_TABLE_TEX = "results/ratings_analysis_table.tex"
FIGURE_ERPS = "results/figure_erps.pdf"
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
        IMAGE_RATINGS_TSV,
        RATINGS_REPORT,
        RATINGS_PROSE_TEX,
        RATINGS_TABLE_TEX,
        FIGURE_ERPS,
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
        f"{TESTS_REPORT} "
        f"{FIGURE_ERPS}"
