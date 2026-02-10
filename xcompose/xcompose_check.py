#!/usr/bin/env python3
"""
XCompose conflict checker.

Detects two kinds of conflicts in an XCompose file:
  1. Exact duplicates  – the same key sequence is defined more than once.
  2. Prefix conflicts  – one sequence is a prefix of another, meaning the
                         shorter sequence would shadow the longer one (or vice
                         versa, depending on the compose implementation).
"""

import re
import sys
from collections import defaultdict
from pathlib import Path


# ---------------------------------------------------------------------------
# Parsing
# ---------------------------------------------------------------------------

# A single key token inside angle brackets, e.g. <Multi_key> or <a>
_KEY_RE = re.compile(r"<([^>]+)>")
# The value / result part after the colon, e.g. "é" or "é" U00E9 # comment
_VALUE_RE = re.compile(r":\s*(.*)")


def parse_xcompose(path: str) -> list[tuple[tuple[str, ...], str, int]]:
    """
    Parse an XCompose file and return a list of
        (key_sequence, raw_result_string, line_number)

    Lines starting with '#' and blank lines are skipped.
    Include directives (%include) are noted but not followed.
    """
    entries: list[tuple[tuple[str, ...], str, int]] = []
    filepath = Path(path)

    with filepath.open(encoding="utf-8", errors="replace") as fh:
        for lineno, raw_line in enumerate(fh, start=1):
            line = raw_line.strip()

            # Skip blank lines and comments
            if not line or line.startswith("#"):
                continue

            # Skip include directives (not followed here)
            if line.startswith("%include"):
                continue

            keys = tuple(_KEY_RE.findall(line))
            if not keys:
                continue  # Not a compose rule line

            value_match = _VALUE_RE.search(line)
            result = value_match.group(1).strip() if value_match else ""

            entries.append((keys, result, lineno))

    return entries


# ---------------------------------------------------------------------------
# Conflict detection
# ---------------------------------------------------------------------------

def find_conflicts(
    entries: list[tuple[tuple[str, ...], str, int]],
) -> tuple[list, list]:
    """
    Returns:
      duplicates  – list of groups where the same sequence appears > once
      prefix_conflicts – list of (shorter_entry, longer_entry) pairs
    """
    # --- 1. Exact duplicates ---
    by_sequence: dict[tuple[str, ...], list] = defaultdict(list)
    for seq, result, lineno in entries:
        by_sequence[seq].append((result, lineno))

    duplicates = [
        (seq, occurrences)
        for seq, occurrences in by_sequence.items()
        if len(occurrences) > 1
    ]

    # --- 2. Prefix conflicts ---
    # Build a trie-like set for quick prefix look-ups.
    all_sequences = set(by_sequence.keys())

    prefix_conflicts = []
    seen_pairs: set[frozenset] = set()

    for seq in all_sequences:
        # Check every strict prefix of this sequence
        for length in range(1, len(seq)):
            prefix = seq[:length]
            if prefix in all_sequences:
                pair_key = frozenset({seq, prefix})
                if pair_key not in seen_pairs:
                    seen_pairs.add(pair_key)
                    shorter = (prefix, by_sequence[prefix])
                    longer = (seq, by_sequence[seq])
                    prefix_conflicts.append((shorter, longer))

    # Sort for deterministic output
    duplicates.sort(key=lambda x: x[1][0][1])          # by first line number
    prefix_conflicts.sort(key=lambda x: x[0][1][0][1]) # by shorter seq's first line

    return duplicates, prefix_conflicts


# ---------------------------------------------------------------------------
# Reporting
# ---------------------------------------------------------------------------

def format_sequence(seq: tuple[str, ...]) -> str:
    return " ".join(f"<{k}>" for k in seq)


def report(duplicates: list, prefix_conflicts: list) -> int:
    """Print a human-readable report. Returns exit code (0 = clean)."""
    total = len(duplicates) + len(prefix_conflicts)

    if total == 0:
        print("✓ No conflicts found.")
        return 0

    if duplicates:
        print(f"{'='*60}")
        print(f"DUPLICATE SEQUENCES ({len(duplicates)} found)")
        print(f"{'='*60}")
        for seq, occurrences in duplicates:
            print(f"\n  Sequence: {format_sequence(seq)}")
            for result, lineno in occurrences:
                print(f"    line {lineno:>5}: {result}")

    if prefix_conflicts:
        print(f"\n{'='*60}")
        print(f"PREFIX CONFLICTS ({len(prefix_conflicts)} found)")
        print(f"  (a shorter sequence is a prefix of a longer one)")
        print(f"{'='*60}")
        for (short_seq, short_occ), (long_seq, long_occ) in prefix_conflicts:
            short_line = short_occ[0][1]
            long_line = long_occ[0][1]
            print(
                f"\n  Shorter (line {short_line:>5}): {format_sequence(short_seq)}"
                f"  →  {short_occ[0][0]}"
            )
            print(
                f"  Longer  (line {long_line:>5}): {format_sequence(long_seq)}"
                f"  →  {long_occ[0][0]}"
            )

    print(f"\n{total} conflict(s) found.")
    return 1


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

def main() -> None:
    if len(sys.argv) < 2:
        # Default to ~/.XCompose if no argument given
        default = Path.home() / ".XCompose"
        if default.exists():
            path = str(default)
            print(f"No file specified – using {path}\n")
        else:
            print("Usage: xcompose_check.py [path/to/.XCompose]")
            sys.exit(1)
    else:
        path = sys.argv[1]

    if not Path(path).exists():
        print(f"Error: file not found: {path}")
        sys.exit(2)

    entries = parse_xcompose(path)
    print(f"Parsed {len(entries)} compose rule(s) from {path}\n")

    duplicates, prefix_conflicts = find_conflicts(entries)
    sys.exit(report(duplicates, prefix_conflicts))


if __name__ == "__main__":
    main()
