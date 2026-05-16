"""Unit tests for code/2_preprocess_eeg.py using simulated data.

Run from the repo root with:
    python code/tests/test_preprocess_eeg.py
or:
    python -m unittest discover -s code/tests -p 'test_*.py'

Pass --report-tsv PATH to emit a machine-readable TSV with one row per test
(columns: name, status, duration_ms, message). Used by the Snakemake `tests`
rule together with the R sibling suite.

Covers the pure helpers (read_manual_bad_channels, add_preprocessing_summary).
The full preprocess_EEG pipeline depends on a real BDF, ICA, and AutoReject
and is not exercised here; it should be smoke-tested on a real recording.
"""
import argparse
import csv
import importlib.util
import os
import sys
import tempfile
import time
import unittest
from pathlib import Path

import numpy as np
import pandas as pd
import mne

mne.set_log_level("ERROR")

SCRIPT_PATH = Path(__file__).resolve().parent.parent / "2_preprocess_eeg.py"
_spec = importlib.util.spec_from_file_location("preprocess_eeg", SCRIPT_PATH)
preprocess_eeg = importlib.util.module_from_spec(_spec)
sys.modules["preprocess_eeg"] = preprocess_eeg
_spec.loader.exec_module(preprocess_eeg)


def _write_tsv(rows, columns):
    fd, path = tempfile.mkstemp(suffix=".tsv")
    os.close(fd)
    pd.DataFrame(rows, columns=columns).to_csv(path, sep="\t", index=False)
    return path


def _make_epochs(counts_per_condition, n_channels=2, sfreq=250, n_samples=300,
                 seed=0):
    """Build a tiny EpochsArray with the hierarchical event names used by the
    pipeline ('white/angry/repetition/1', 'white/happy/repetition/1')."""
    event_id = {
        "white/angry/repetition/1": 21,
        "white/happy/repetition/1": 41,
    }
    ch_names = [f"EEG {i+1:03d}" for i in range(n_channels)]
    info = mne.create_info(ch_names, sfreq=sfreq, ch_types="eeg")
    codes = []
    for cond, n in counts_per_condition.items():
        codes.extend([event_id[cond]] * n)
    n_total = len(codes)
    rng = np.random.default_rng(seed)
    data = rng.standard_normal((n_total, n_channels, n_samples)) * 1e-6
    events = np.column_stack([
        np.arange(n_total) * (n_samples + 50),
        np.zeros(n_total, dtype=int),
        np.array(codes, dtype=int),
    ])
    used_ids = {k: v for k, v in event_id.items() if v in set(codes)}
    return mne.EpochsArray(data, info, events=events, event_id=used_ids,
                           tmin=-0.1, verbose=False)


class TestReadManualBadChannels(unittest.TestCase):
    def test_none_path_returns_empty(self):
        self.assertEqual(preprocess_eeg.read_manual_bad_channels(None), [])

    def test_missing_file_returns_empty(self):
        self.assertEqual(
            preprocess_eeg.read_manual_bad_channels("/tmp/does_not_exist.tsv"),
            [],
        )

    def test_only_non_good_channels_returned(self):
        path = _write_tsv(
            [("Fp1", "good"), ("Fz", "bad"), ("Cz", "good"), ("Oz", "bad")],
            ["name", "status"],
        )
        try:
            result = preprocess_eeg.read_manual_bad_channels(path)
            self.assertEqual(sorted(result), ["Fz", "Oz"])
        finally:
            os.remove(path)

    def test_status_case_and_whitespace_insensitive(self):
        path = _write_tsv(
            [("Fp1", "GOOD"), ("Fz", " good "), ("Cz", "Bad"), ("Oz", "BAD")],
            ["name", "status"],
        )
        try:
            result = preprocess_eeg.read_manual_bad_channels(path)
            self.assertEqual(sorted(result), ["Cz", "Oz"])
        finally:
            os.remove(path)

    def test_missing_status_column_returns_empty(self):
        path = _write_tsv([("Fp1", "x")], ["name", "type"])
        try:
            self.assertEqual(preprocess_eeg.read_manual_bad_channels(path), [])
        finally:
            os.remove(path)

    def test_missing_name_column_returns_empty(self):
        path = _write_tsv([("good",), ("bad",)], ["status"])
        try:
            self.assertEqual(preprocess_eeg.read_manual_bad_channels(path), [])
        finally:
            os.remove(path)

    def test_channel_names_coerced_to_string(self):
        path = _write_tsv([(1, "bad"), (2, "good")], ["name", "status"])
        try:
            result = preprocess_eeg.read_manual_bad_channels(path)
            self.assertEqual(result, ["1"])
        finally:
            os.remove(path)


class TestAddPreprocessingSummary(unittest.TestCase):
    def _html_text(self, report):
        return "\n".join(str(chunk) for chunk in report.html)

    def test_returns_report_with_added_html_section(self):
        epochs = _make_epochs({"white/angry/repetition/1": 4,
                               "white/happy/repetition/1": 4})
        report = mne.Report(verbose=False)
        n_before = len(report.html)
        result = preprocess_eeg.add_preprocessing_summary(
            report, epochs, epochs, bad_channels=[])
        self.assertIs(result, report)
        self.assertGreater(len(result.html), n_before)
        self.assertIn("Complete Preprocessing Summary",
                      self._html_text(result))

    def test_overall_counts_and_percentages(self):
        orig = _make_epochs({"white/angry/repetition/1": 10,
                             "white/happy/repetition/1": 10})
        final = _make_epochs({"white/angry/repetition/1": 8,
                              "white/happy/repetition/1": 7})
        report = mne.Report(verbose=False)
        preprocess_eeg.add_preprocessing_summary(report, orig, final,
                                                 bad_channels=[])
        html = self._html_text(report)
        # 20 original, 15 retained -> 75.0% overall
        self.assertIn(">20<", html)
        self.assertIn(">15<", html)
        self.assertIn("75.0%", html)

    def test_per_condition_retention_rows(self):
        orig = _make_epochs({"white/angry/repetition/1": 10,
                             "white/happy/repetition/1": 10})
        final = _make_epochs({"white/angry/repetition/1": 8,
                              "white/happy/repetition/1": 7})
        report = mne.Report(verbose=False)
        preprocess_eeg.add_preprocessing_summary(report, orig, final,
                                                 bad_channels=[])
        html = self._html_text(report)
        self.assertIn("white/angry/repetition/1", html)
        self.assertIn("white/happy/repetition/1", html)
        # Per-condition retention percentages
        self.assertIn("80.0%", html)  # angry: 8/10
        self.assertIn("70.0%", html)  # happy: 7/10

    def test_handles_condition_absent_from_final(self):
        """drop_bad can remove all epochs of a condition, deleting it from
        event_id. The summary must still tabulate it as 0 retained, not crash."""
        orig = _make_epochs({"white/angry/repetition/1": 6,
                             "white/happy/repetition/1": 4})
        final = _make_epochs({"white/angry/repetition/1": 6})
        report = mne.Report(verbose=False)
        preprocess_eeg.add_preprocessing_summary(report, orig, final,
                                                 bad_channels=[])
        html = self._html_text(report)
        self.assertIn("white/happy/repetition/1", html)
        self.assertIn("0.0%", html)  # 0 retained out of 4 -> 0.0%

    def test_bad_channel_list_rendered(self):
        epochs = _make_epochs({"white/angry/repetition/1": 4,
                               "white/happy/repetition/1": 4})
        report = mne.Report(verbose=False)
        preprocess_eeg.add_preprocessing_summary(
            report, epochs, epochs, bad_channels=["Fz", "Cz", "Oz"])
        html = self._html_text(report)
        self.assertIn("Bad Channels Detected: 3", html)
        for ch in ("Fz", "Cz", "Oz"):
            self.assertIn(ch, html)

    def test_empty_bad_channel_list_says_none(self):
        epochs = _make_epochs({"white/angry/repetition/1": 4,
                               "white/happy/repetition/1": 4})
        report = mne.Report(verbose=False)
        preprocess_eeg.add_preprocessing_summary(report, epochs, epochs,
                                                 bad_channels=[])
        html = self._html_text(report)
        self.assertIn("Bad Channels Detected: 0", html)
        self.assertIn("None", html)


class TsvResult(unittest.TextTestResult):
    """Captures per-test outcomes for TSV export. Records on success, failure,
    error, and skip; the only path that doesn't terminate a test is the
    expected-failure/unexpected-success pair, which the current suite doesn't
    use."""

    def __init__(self, *a, **kw):
        super().__init__(*a, **kw)
        self.records = []
        self._t0 = {}

    def startTest(self, test):
        super().startTest(test)
        self._t0[test.id()] = time.perf_counter()

    def _duration_ms(self, test):
        return (time.perf_counter() - self._t0.get(test.id(),
                                                   time.perf_counter())) * 1000.0

    def addSuccess(self, test):
        super().addSuccess(test)
        self.records.append({"name": test.id().removeprefix("__main__."), "status": "pass",
                             "duration_ms": self._duration_ms(test),
                             "message": ""})

    def addFailure(self, test, err):
        super().addFailure(test, err)
        self.records.append({"name": test.id().removeprefix("__main__."), "status": "fail",
                             "duration_ms": self._duration_ms(test),
                             "message": self._exc_info_to_string(err, test)})

    def addError(self, test, err):
        super().addError(test, err)
        self.records.append({"name": test.id().removeprefix("__main__."), "status": "fail",
                             "duration_ms": self._duration_ms(test),
                             "message": self._exc_info_to_string(err, test)})

    def addSkip(self, test, reason):
        super().addSkip(test, reason)
        self.records.append({"name": test.id().removeprefix("__main__."), "status": "skip",
                             "duration_ms": self._duration_ms(test),
                             "message": reason})


def _write_report(records, path):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=["name", "status", "duration_ms",
                                          "message"],
                           delimiter="\t", quoting=csv.QUOTE_MINIMAL)
        w.writeheader()
        for r in records:
            r = dict(r)
            r["duration_ms"] = f"{r['duration_ms']:.2f}"
            r["message"] = r["message"].replace("\n", " ").replace("\t", " ")
            w.writerow(r)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--report-tsv", type=str, default=None,
                        help="If given, write per-test results to this TSV.")
    args = parser.parse_args()

    loader = unittest.TestLoader()
    suite = loader.loadTestsFromModule(sys.modules[__name__])
    runner = unittest.TextTestRunner(verbosity=2, resultclass=TsvResult)
    result = runner.run(suite)

    if args.report_tsv:
        _write_report(result.records, args.report_tsv)

    sys.exit(0 if result.wasSuccessful() else 1)
