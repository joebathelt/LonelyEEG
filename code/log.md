2026-05-15:
- Added a new script, 4_demographics_table.py, to generate a LaTeX table summarizing participant demographics and data quality metrics, with appropriate statistical tests for group comparisons.
2026-05-14:
- Updated the reporting of data quality in 2_preprocess_eeg.py
- Made scripts callable from the command line with argparse for better usability.
- Created snake file to automate the entire pipeline from data organization to preprocessing.
- Added anonymisation script to rename participant IDs in the BIDS dataset for sharing without revealing identifying metadata.
2025-11-25:
- Revised code in 1_organise_bids.py to use *.bdf files instead of *.edf files for EEG data.
- Updated the 2_preprocess_eeg.py script to adjust epoch time windows and improve artefact rejection criteria.