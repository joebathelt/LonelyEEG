#!/usr/bin/env python3
"""Interactive raw-EEG viewer.

Opens an MNE-readable file in the scrolling raw plot so bad-channel marks
and annotations made in the GUI are saved back to the file on close.
"""
import argparse
from pathlib import Path

import mne


def read_raw(path: Path) -> mne.io.BaseRaw:
    suffix = path.suffix.lower()
    if suffix == ".bdf":
        return mne.io.read_raw_bdf(path, preload=True)
    if suffix == ".edf":
        return mne.io.read_raw_edf(path, preload=True)
    if suffix == ".fif":
        return mne.io.read_raw_fif(path, preload=True)
    if suffix == ".set":
        return mne.io.read_raw_eeglab(path, preload=True)
    if suffix == ".vhdr":
        return mne.io.read_raw_brainvision(path, preload=True)
    return mne.io.read_raw(path, preload=True)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("filename", type=Path, help="Path to a raw EEG file")
    parser.add_argument("--duration", type=float, default=10.0,
                        help="Window length in seconds (default: 10)")
    parser.add_argument("--n-channels", type=int, default=32,
                        help="Channels visible at once (default: 32)")
    parser.add_argument("--scaling", type=float, default=50e-6,
                        help="EEG scaling in volts, e.g. 100e-6 for noisy data "
                             "(default: 50e-6)")
    parser.add_argument("--highpass", type=float, default=0.1,
                        help="Display-only high-pass in Hz (default: 0.1)")
    parser.add_argument("--lowpass", type=float, default=40.0,
                        help="Display-only low-pass in Hz (default: 40)")
    parser.add_argument("--save", action="store_true",
                        help="Overwrite the input with bad-channel/annotation "
                             "edits when the window closes (FIF only)")
    args = parser.parse_args()

    if not args.filename.exists():
        parser.error(f"File not found: {args.filename}")

    raw = read_raw(args.filename)
    events = mne.find_events(raw, min_duration=0.01, output='onset')

    raw.plot(
        duration=args.duration,
        n_channels=args.n_channels,
        scalings=dict(eeg=args.scaling),
        highpass=args.highpass,
        lowpass=args.lowpass,
        events=events,
        filtorder=4,
        remove_dc=True,
        show_scrollbars=True,
        show_scalebars=True,
        block=True,
    )

    interactive_annot = raw.annotations
    raw.set_annotations(interactive_annot)

    if args.save:
        if args.filename.suffix.lower() != ".fif":
            raise SystemExit("--save only supported for .fif files; export "
                             "manually for other formats.")
        raw.save(args.filename, overwrite=True)


if __name__ == "__main__":
    main()
