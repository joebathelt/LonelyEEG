"""Run both test suites and render a single markdown report.

Drives:
    code/tests/test_preprocess_eeg.py  (python -> unittest)
    code/tests/test_main_analysis.R    (Rscript)

Each suite writes a per-test TSV; this script aggregates them into one
markdown file and exits non-zero if any test failed (so Snakemake fails loudly).

Usage:
    python code/tests/render_report.py --out-md results/tests_report.md
"""
import argparse
import csv
import datetime
import shutil
import subprocess
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
TESTS_DIR = REPO / "code" / "tests"


def read_tsv(path):
    with Path(path).open(newline="") as f:
        return list(csv.DictReader(f, delimiter="\t"))


def md_escape(s):
    return (s or "").replace("|", "\\|").replace("\n", " ").replace("\r", " ")


def truncate(s, n=200):
    s = s or ""
    return s if len(s) <= n else s[: n - 3] + "..."


def render(suites, out_path, py_rc, r_rc):
    total = sum(len(rows) for _, rows in suites)
    passed = sum(sum(1 for r in rows if r["status"] == "pass")
                 for _, rows in suites)
    failed = sum(sum(1 for r in rows if r["status"] == "fail")
                 for _, rows in suites)
    skipped = total - passed - failed
    ts = datetime.datetime.now().isoformat(timespec="seconds")

    lines = [
        "# Test Report",
        "",
        f"_Generated: {ts}_",
        "",
        "## Summary",
        "",
        f"- **Total:** {total}",
        f"- **Passed:** {passed}",
        f"- **Failed:** {failed}",
    ]
    if skipped:
        lines.append(f"- **Skipped:** {skipped}")
    lines += [
        "",
        ("**All tests passed.**" if failed == 0
         else f"**{failed} test(s) failed — see details below.**"),
        "",
    ]

    for label, rows in suites:
        n = len(rows)
        n_pass = sum(1 for r in rows if r["status"] == "pass")
        n_fail = sum(1 for r in rows if r["status"] == "fail")
        lines.append(f"## {label} ({n_pass}/{n} passed"
                     + (f", {n_fail} failed" if n_fail else "") + ")")
        lines.append("")
        lines.append("| Test | Status | Duration (ms) | Message |")
        lines.append("| --- | --- | --- | --- |")
        for r in rows:
            status_md = {"pass": "ok", "fail": "**FAIL**",
                         "skip": "skip"}.get(r["status"], r["status"])
            try:
                dur = f"{float(r.get('duration_ms') or 0):.1f}"
            except ValueError:
                dur = r.get("duration_ms", "")
            msg = md_escape(truncate(r.get("message", "")))
            lines.append(f"| {md_escape(r['name'])} | {status_md} | {dur} | {msg} |")
        lines.append("")

    lines += [
        "## Provenance",
        "",
        f"- `code/tests/test_preprocess_eeg.py` exit status: `{py_rc}`",
        f"- `code/tests/test_main_analysis.R` exit status: `{r_rc}`",
        "",
    ]

    out_path = Path(out_path)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_text("\n".join(lines))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--out-md", required=True, type=Path)
    parser.add_argument("--python", default=sys.executable,
                        help="Python interpreter (defaults to the one running this script).")
    parser.add_argument("--rscript", default=shutil.which("Rscript") or "Rscript",
                        help="Rscript executable.")
    args = parser.parse_args()

    py_tsv = TESTS_DIR / "_preprocess_eeg_results.tsv"
    r_tsv = TESTS_DIR / "_main_analysis_results.tsv"
    for p in (py_tsv, r_tsv):
        if p.exists():
            p.unlink()

    print(f"[tests] Running Python suite ({TESTS_DIR / 'test_preprocess_eeg.py'})")
    py_rc = subprocess.run(
        [args.python, str(TESTS_DIR / "test_preprocess_eeg.py"),
         "--report-tsv", str(py_tsv)],
        cwd=REPO,
    ).returncode

    print(f"\n[tests] Running R suite ({TESTS_DIR / 'test_main_analysis.R'})")
    r_rc = subprocess.run(
        [args.rscript, str(TESTS_DIR / "test_main_analysis.R"),
         "--report-tsv", str(r_tsv)],
        cwd=REPO,
    ).returncode

    suites = []
    if py_tsv.exists():
        suites.append(("Python — `code/2_preprocess_eeg.py`", read_tsv(py_tsv)))
    else:
        print(f"[tests] WARNING: {py_tsv} not produced", file=sys.stderr)
    if r_tsv.exists():
        suites.append(("R — `code/6_main_analysis.R`", read_tsv(r_tsv)))
    else:
        print(f"[tests] WARNING: {r_tsv} not produced", file=sys.stderr)

    render(suites, args.out_md, py_rc, r_rc)
    print(f"\n[tests] Wrote {args.out_md}")

    sys.exit(0 if (py_rc == 0 and r_rc == 0) else 1)


if __name__ == "__main__":
    main()
