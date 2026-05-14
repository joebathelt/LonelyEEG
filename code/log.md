2026-05-14:
- Updated the reporting of data quality in 2_preprocess_eeg.py
- Made scripts callable from the command line with argparse for better usability.
- Created snake file to automate the entire pipeline from data organization to preprocessing.
2025-11-25:
- Revised code in 1_organise_bids.py to use *.bdf files instead of *.edf files for EEG data.
- Updated the 2_preprocess_eeg.py script to adjust epoch time windows and improve artefact rejection criteria.