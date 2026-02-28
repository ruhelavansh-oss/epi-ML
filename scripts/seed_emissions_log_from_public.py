#!/usr/bin/env python3
"""Seed local CodeCarbon module log from previously published emissions events."""

from __future__ import annotations

import argparse
import csv
import io
import sys
import urllib.error
import urllib.request
from pathlib import Path


FULL_FIELDS = [
    "timestamp_utc",
    "run_id",
    "run_source",
    "module",
    "status",
    "duration_seconds",
    "emissions_kg",
    "energy_kwh",
    "cpu_energy_kwh",
    "gpu_energy_kwh",
    "ram_energy_kwh",
    "cpu_power_w",
    "gpu_power_w",
    "ram_power_w",
    "country_iso_code",
    "country_name",
    "region",
    "cloud_provider",
    "tracking_mode",
    "command",
]

KEY_FIELDS = (
    "timestamp_utc",
    "run_id",
    "module",
    "status",
    "duration_seconds",
    "emissions_kg",
    "energy_kwh",
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Seed logs/emissions/module_emissions_log.csv from public events CSV."
    )
    src = parser.add_mutually_exclusive_group(required=True)
    src.add_argument("--source-url", help="URL for emissions_module_events.csv")
    src.add_argument("--source-file", type=Path, help="Local path for emissions_module_events.csv")
    parser.add_argument(
        "--log-path",
        type=Path,
        default=Path("logs/emissions/module_emissions_log.csv"),
        help="Target module emissions log path.",
    )
    parser.add_argument(
        "--require-data",
        action="store_true",
        help="Exit non-zero when the source cannot provide usable rows.",
    )
    return parser.parse_args()


def read_csv_rows_from_text(text: str) -> list[dict[str, str]]:
    reader = csv.DictReader(io.StringIO(text))
    return [dict(row) for row in reader]


def fetch_source_rows(args: argparse.Namespace) -> list[dict[str, str]]:
    if args.source_file is not None:
        if not args.source_file.exists():
            print(f"[seed_emissions] source file not found: {args.source_file}")
            return []
        return read_csv_rows_from_text(args.source_file.read_text(encoding="utf-8"))

    try:
        with urllib.request.urlopen(args.source_url, timeout=30) as resp:
            if resp.status != 200:
                print(f"[seed_emissions] source URL returned HTTP {resp.status}; skipping seed.")
                return []
            payload = resp.read().decode("utf-8")
            return read_csv_rows_from_text(payload)
    except urllib.error.HTTPError as exc:
        print(f"[seed_emissions] source URL returned HTTP {exc.code}; skipping seed.")
        return []
    except Exception as exc:  # pragma: no cover - defensive network handling
        print(f"[seed_emissions] could not fetch source URL: {exc}; skipping seed.")
        return []


def as_text(value: object) -> str:
    if value is None:
        return ""
    return str(value)


def normalize_event_row(row: dict[str, str]) -> dict[str, str]:
    out = {key: "" for key in FULL_FIELDS}
    out["timestamp_utc"] = as_text(row.get("timestamp_utc", "")).strip()
    out["run_id"] = as_text(row.get("run_id", "")).strip()
    source = as_text(row.get("run_source", "")).strip().lower()
    if source not in {"local", "cloud"}:
        provider = as_text(row.get("cloud_provider", "")).strip()
        source = "cloud" if provider else "local"
    out["run_source"] = source
    out["module"] = as_text(row.get("module", "")).strip()
    out["status"] = as_text(row.get("status", "")).strip() or "ok"
    out["duration_seconds"] = as_text(row.get("duration_seconds", "")).strip() or "0"
    out["emissions_kg"] = as_text(row.get("emissions_kg", "")).strip() or "0"
    out["energy_kwh"] = as_text(row.get("energy_kwh", "")).strip() or "0"
    out["country_iso_code"] = as_text(row.get("country_iso_code", "")).strip()
    out["cloud_provider"] = as_text(row.get("cloud_provider", "")).strip()
    out["region"] = as_text(row.get("region", "")).strip()
    out["tracking_mode"] = "restored"
    out["command"] = "restored-from-published-emissions-module-events"
    return out


def normalize_existing_row(row: dict[str, str]) -> dict[str, str]:
    out = {key: as_text(row.get(key, "")).strip() for key in FULL_FIELDS}
    if not out["status"]:
        out["status"] = "ok"
    if not out["duration_seconds"]:
        out["duration_seconds"] = "0"
    if not out["emissions_kg"]:
        out["emissions_kg"] = "0"
    if not out["energy_kwh"]:
        out["energy_kwh"] = "0"
    return out


def dedupe_rows(rows: list[dict[str, str]]) -> list[dict[str, str]]:
    seen: set[tuple[str, ...]] = set()
    out: list[dict[str, str]] = []
    for row in rows:
        key = tuple(as_text(row.get(k, "")).strip() for k in KEY_FIELDS)
        if key in seen:
            continue
        seen.add(key)
        out.append(row)
    return out


def read_existing_rows(path: Path) -> list[dict[str, str]]:
    if not path.exists():
        return []
    with path.open("r", encoding="utf-8", newline="") as handle:
        return [normalize_existing_row(dict(row)) for row in csv.DictReader(handle)]


def write_rows(path: Path, rows: list[dict[str, str]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=FULL_FIELDS)
        writer.writeheader()
        writer.writerows(rows)


def main() -> int:
    args = parse_args()
    source_rows = fetch_source_rows(args)
    if not source_rows:
        print("[seed_emissions] no source rows available; skipping.")
        return 1 if args.require_data else 0

    normalized_source = [normalize_event_row(row) for row in source_rows]
    normalized_source = [
        row for row in normalized_source if row["timestamp_utc"] and row["run_id"] and row["module"]
    ]
    if not normalized_source:
        print("[seed_emissions] no usable rows in source; skipping.")
        return 1 if args.require_data else 0

    existing = read_existing_rows(args.log_path)
    merged = dedupe_rows(existing + normalized_source)
    write_rows(args.log_path, merged)
    print(
        "[seed_emissions] wrote {total} rows to {path} ({seeded} sourced, {existing} existing).".format(
            total=len(merged),
            path=args.log_path,
            seeded=len(normalized_source),
            existing=len(existing),
        )
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
