#!/usr/bin/env python3
"""Lightweight complexity report/ratchet for typestate/protocol hot paths.

Usage:
  python3 scripts/complexity/typestate_protocol_ratchet.py
  python3 scripts/complexity/typestate_protocol_ratchet.py --enforce

Without --enforce, threshold violations are reported as warnings.
With --enforce, the script exits non-zero if any threshold is exceeded.
"""

from __future__ import annotations

import argparse
import pathlib
import re
import sys
from dataclasses import dataclass


ROOT = pathlib.Path(__file__).resolve().parents[2]

# File-level guardrails (line count upper bounds).
FILE_LIMITS: dict[str, int] = {
    "src/core/typestate/mod.rs": 900,
    "src/core/typestate/managed_api.rs": 600,
    "src/core/typestate/support_types.rs": 400,
    "src/core/typestate/handlers.rs": 450,
    "src/core/protocol/index.rs": 450,
    "src/core/typecheck/validate/protocol.rs": 820,
    "runtime/machine/runtime.c": 1500,
    "runtime/machine/runtime.h": 750,
    "runtime/machine/emit.c": 260,
    "runtime/machine/bridge.c": 400,
}

# Function-level guardrails for known hotspots.
FUNCTION_LIMITS: dict[tuple[str, str], int] = {
    ("src/core/typestate/managed_api.rs", "machine_handle_method_block"): 50,
    ("src/core/typestate/managed_api.rs", "lower_spawn_func"): 240,
    ("src/core/typestate/mod.rs", "desugar_typestate"): 240,
    ("src/core/protocol/index.rs", "build_protocol_fact"): 120,
    ("src/core/typecheck/validate/protocol.rs", "check_protocol_shape_conformance"): 100,
}

FUNC_RE = re.compile(r"^\s*(?:pub\([^)]*\)\s+)?(?:pub\s+)?fn\s+([A-Za-z0-9_]+)\b")


@dataclass(frozen=True)
class FnSpan:
    name: str
    start: int
    end: int

    @property
    def lines(self) -> int:
        return self.end - self.start + 1


def read_lines(path: pathlib.Path) -> list[str]:
    return path.read_text(encoding="utf-8").splitlines()


def extract_fn_spans(path: pathlib.Path) -> dict[str, FnSpan]:
    lines = read_lines(path)
    out: dict[str, FnSpan] = {}
    i = 0
    while i < len(lines):
        m = FUNC_RE.match(lines[i])
        if not m:
            i += 1
            continue

        name = m.group(1)
        depth = 0
        started = False
        j = i
        while j < len(lines):
            for ch in lines[j]:
                if ch == "{":
                    depth += 1
                    started = True
                elif ch == "}":
                    depth -= 1
            if started and depth == 0:
                break
            j += 1

        out[name] = FnSpan(name=name, start=i + 1, end=j + 1)
        i = j + 1

    return out


def format_row(cols: list[str], widths: list[int]) -> str:
    return " | ".join(col.ljust(width) for col, width in zip(cols, widths))


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--enforce", action="store_true", help="exit non-zero on threshold violations")
    args = parser.parse_args()

    violations: list[str] = []

    print("[typestate/protocol complexity] file metrics")
    file_rows: list[list[str]] = [["path", "lines", "limit", "status"]]
    for rel, limit in FILE_LIMITS.items():
        path = ROOT / rel
        lines = len(read_lines(path))
        ok = lines <= limit
        status = "ok" if ok else "OVER"
        if not ok:
            violations.append(f"file {rel}: {lines} > {limit}")
        file_rows.append([rel, str(lines), str(limit), status])

    file_widths = [max(len(row[i]) for row in file_rows) for i in range(4)]
    for idx, row in enumerate(file_rows):
        print(format_row(row, file_widths))
        if idx == 0:
            print("-" * (sum(file_widths) + 3 * (len(file_widths) - 1)))

    print("\n[typestate/protocol complexity] function metrics")
    fn_rows: list[list[str]] = [["file", "fn", "lines", "limit", "status"]]
    fn_cache: dict[str, dict[str, FnSpan]] = {}
    for (rel, fn_name), limit in FUNCTION_LIMITS.items():
        spans = fn_cache.setdefault(rel, extract_fn_spans(ROOT / rel))
        span = spans.get(fn_name)
        if span is None:
            violations.append(f"function {rel}::{fn_name}: missing")
            fn_rows.append([rel, fn_name, "<missing>", str(limit), "MISSING"])
            continue
        ok = span.lines <= limit
        status = "ok" if ok else "OVER"
        if not ok:
            violations.append(f"function {rel}::{fn_name}: {span.lines} > {limit}")
        fn_rows.append([rel, fn_name, str(span.lines), str(limit), status])

    fn_widths = [max(len(row[i]) for row in fn_rows) for i in range(5)]
    for idx, row in enumerate(fn_rows):
        print(format_row(row, fn_widths))
        if idx == 0:
            print("-" * (sum(fn_widths) + 3 * (len(fn_widths) - 1)))

    if violations:
        print("\n[typestate/protocol complexity] violations")
        for v in violations:
            print(f"- {v}")
        if args.enforce:
            print("\nResult: FAIL (enforced)")
            return 1
        print("\nResult: WARN (non-enforced)")
        return 0

    print("\nResult: OK")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
