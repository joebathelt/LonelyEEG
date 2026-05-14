configfile: "config.yaml"

BIDS = config["bids_folder"]
QC_TSV = BIDS + "/processed/task-RovingOddball_desc-preprocessing_qc.tsv"


rule all:
    input:
        QC_TSV


rule organise_bids:
    input:
        raw=config["raw_data_folder"],
        experiment=config["experiment_folder"],
    output:
        touch(".snakemake_sentinels/organise_bids.done"),
    shell:
        "python code/1_organise_bids.py "
        "--raw-data-folder {input.raw} "
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
        f"--processed-folder {BIDS}/processed "
        "--out-file {output}"


rule clean:
    shell:
        f"rm -rf {BIDS}/processed .snakemake_sentinels/preprocess.done"
