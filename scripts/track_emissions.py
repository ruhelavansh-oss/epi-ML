#!/usr/bin/env python3
"""Track per-command emissions with CodeCarbon and append module run metadata."""

from __future__ import annotations

import argparse
import csv
import datetime as dt
import os
import subprocess
import sys
import time
from pathlib import Path
from typing import Iterable, Mapping


TRUTHY = {"1", "true", "yes", "y", "on"}


def is_truthy(value: str | None) -> bool:
    if value is None:
        return False
    return value.strip().lower() in TRUTHY


def detect_run_source(explicit_source: str | None, cloud_provider: str | None) -> str:
    source = (explicit_source or "").strip().lower()
    if source in {"local", "cloud"}:
        return source
    if is_truthy(os.getenv("GITHUB_ACTIONS")) or os.getenv("GITHUB_RUN_ID"):
        return "cloud"
    if (cloud_provider or "").strip():
        return "cloud"
    return "local"


def sanitize_module_name(module: str) -> str:
    cleaned = "".join(ch if ch.isalnum() or ch in "._-" else "_" for ch in module.strip())
    return cleaned or "module"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Run a command under CodeCarbon and append a module-level emissions log."
    )
    parser.add_argument("--module", required=True, help="Module label for summary output.")
    parser.add_argument("--run-id", default=os.getenv("EPI_ML_EMISSIONS_RUN_ID", ""))
    parser.add_argument(
        "--log-dir",
        type=Path,
        default=Path(os.getenv("CODECARBON_LOG_DIR", "logs/emissions")),
        help="Directory where raw CodeCarbon CSV and module logs are written.",
    )
    parser.add_argument(
        "--output-file",
        default=os.getenv("CODECARBON_OUTPUT_FILE", "codecarbon_emissions.csv"),
        help="Raw CodeCarbon CSV filename.",
    )
    parser.add_argument(
        "--module-log-file",
        default=os.getenv("EPI_ML_MODULE_EMISSIONS_FILE", "module_emissions_log.csv"),
        help="Sanitized module-level emissions CSV filename.",
    )
    parser.add_argument(
        "--project-name",
        default=os.getenv("CODECARBON_PROJECT_NAME", "epi-ML"),
        help="CodeCarbon project name.",
    )
    parser.add_argument(
        "--measure-power-secs",
        type=float,
        default=float(os.getenv("CODECARBON_MEASURE_POWER_SECS", "15")),
    )
    parser.add_argument(
        "--save-to-api",
        action="store_true",
        default=is_truthy(os.getenv("CODECARBON_SAVE_TO_API")),
        help="Forward emissions to the CodeCarbon API.",
    )
    parser.add_argument(
        "--experiment-id",
        default=os.getenv("CODECARBON_EXPERIMENT_ID", ""),
        help="CodeCarbon experiment UUID for dashboard API uploads.",
    )
    parser.add_argument(
        "--offline",
        action="store_true",
        default=is_truthy(os.getenv("CODECARBON_OFFLINE")),
        help="Use CodeCarbon offline tracking mode.",
    )
    parser.add_argument(
        "--country-iso-code",
        default=os.getenv("CODECARBON_COUNTRY_ISO_CODE", ""),
        help="Required by CodeCarbon when offline mode is enabled (e.g., CAN, USA).",
    )
    parser.add_argument(
        "command",
        nargs=argparse.REMAINDER,
        help="Command to run, prefixed by `--` (example: -- Rscript script.R).",
    )
    args = parser.parse_args()
    if args.command and args.command[0] == "--":
        args.command = args.command[1:]
    if not args.command:
        parser.error("Missing command to execute. Pass it after `--`.")
    return args


def as_float(value: object, fallback: float = 0.0) -> float:
    try:
        if value is None:
            return fallback
        return float(value)
    except (TypeError, ValueError):
        return fallback


def get_attr(data: object, key: str, default: object = "") -> object:
    return getattr(data, key, default)


def append_csv_row(path: Path, fieldnames: Iterable[str], row: Mapping[str, object]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    exists = path.exists()
    with path.open("a", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(fieldnames))
        if not exists:
            writer.writeheader()
        writer.writerow(row)


def build_tracker(args: argparse.Namespace):
    try:
        from codecarbon import EmissionsTracker, OfflineEmissionsTracker
    except ImportError as exc:
        raise RuntimeError(
            "CodeCarbon is not installed. Install it with: python3 -m pip install codecarbon"
        ) from exc

    tracker_kwargs = {
        "project_name": args.project_name,
        "measure_power_secs": args.measure_power_secs,
        "output_dir": str(args.log_dir),
        "output_file": args.output_file,
        "save_to_file": True,
        "allow_multiple_runs": True,
    }

    if args.save_to_api:
        tracker_kwargs["save_to_api"] = True
    if args.experiment_id:
        tracker_kwargs["experiment_id"] = args.experiment_id

    if args.offline:
        if not args.country_iso_code:
            raise RuntimeError(
                "Offline tracking requires --country-iso-code (for example CAN or USA)."
            )
        tracker_kwargs["country_iso_code"] = args.country_iso_code
        return OfflineEmissionsTracker(**tracker_kwargs)

    return EmissionsTracker(**tracker_kwargs)


def main() -> int:
    args = parse_args()
    args.log_dir.mkdir(parents=True, exist_ok=True)

    run_id = args.run_id or dt.datetime.now(dt.timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    module = sanitize_module_name(args.module)
    command_str = " ".join(args.command)
    started_at = dt.datetime.now(dt.timezone.utc).replace(microsecond=0).isoformat()

    try:
        tracker = build_tracker(args)
    except RuntimeError as exc:
        print(f"[track_emissions] {exc}", file=sys.stderr)
        return 2

    wall_start = time.monotonic()
    try:
        tracker.start()
    except Exception as exc:  # pragma: no cover - defensive handling for tracker startup errors
        print(f"[track_emissions] tracker start failed: {exc}", file=sys.stderr)
        return 2
    proc = subprocess.run(args.command, check=False)
    stop_error = ""
    emissions_value = None
    final = None
    try:
        emissions_value = tracker.stop()
        final = getattr(tracker, "final_emissions_data", None)
    except Exception as exc:  # pragma: no cover - defensive handling for tracker runtime errors
        stop_error = str(exc)

    wall_duration = time.monotonic() - wall_start
    emissions_kg = as_float(get_attr(final, "emissions", emissions_value), 0.0)
    duration_seconds = as_float(get_attr(final, "duration", wall_duration), wall_duration)
    energy_kwh = as_float(get_attr(final, "energy_consumed", 0.0), 0.0)
    cloud_provider = str(get_attr(final, "cloud_provider", ""))
    tracking_mode = str(get_attr(final, "tracking_mode", ""))
    run_source = detect_run_source(os.getenv("EPI_ML_EMISSIONS_SOURCE"), cloud_provider)

    row = {
        "timestamp_utc": started_at,
        "run_id": run_id,
        "module": module,
        "run_source": run_source,
        "status": "ok" if proc.returncode == 0 else f"exit_{proc.returncode}",
        "duration_seconds": round(duration_seconds, 6),
        "emissions_kg": round(emissions_kg, 12),
        "energy_kwh": round(energy_kwh, 12),
        "cpu_energy_kwh": round(as_float(get_attr(final, "cpu_energy", 0.0), 0.0), 12),
        "gpu_energy_kwh": round(as_float(get_attr(final, "gpu_energy", 0.0), 0.0), 12),
        "ram_energy_kwh": round(as_float(get_attr(final, "ram_energy", 0.0), 0.0), 12),
        "cpu_power_w": round(as_float(get_attr(final, "cpu_power", 0.0), 0.0), 6),
        "gpu_power_w": round(as_float(get_attr(final, "gpu_power", 0.0), 0.0), 6),
        "ram_power_w": round(as_float(get_attr(final, "ram_power", 0.0), 0.0), 6),
        "country_iso_code": str(get_attr(final, "country_iso_code", "")),
        "country_name": str(get_attr(final, "country_name", "")),
        "region": str(get_attr(final, "region", "")),
        "cloud_provider": cloud_provider,
        "tracking_mode": tracking_mode,
        "command": command_str,
    }

    append_csv_row(
        args.log_dir / args.module_log_file,
        fieldnames=row.keys(),
        row=row,
    )

    if stop_error:
        print(f"[track_emissions] tracker stop warning: {stop_error}", file=sys.stderr)

    print(
        "[track_emissions] module={module} run_id={run_id} status={status} emissions_kg={em:.10f} "
        "energy_kwh={energy:.10f} duration_s={dur:.2f}".format(
            module=module,
            run_id=run_id,
            status=row["status"],
            em=emissions_kg,
            energy=energy_kwh,
            dur=duration_seconds,
        )
    )
    return proc.returncode


if __name__ == "__main__":
    sys.exit(main())
