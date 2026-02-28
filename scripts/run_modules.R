#!/usr/bin/env Rscript
source("surveillance/lib/config_paths.R")
paths <- get_paths()
init_paths(paths)
setwd(paths$project_root)

args <- commandArgs(trailingOnly = TRUE)

is_truthy <- function(value) {
  if (length(value) == 0 || is.na(value) || !nzchar(trimws(value))) return(FALSE)
  tolower(trimws(value)) %in% c("1", "true", "yes", "y", "on")
}

has_flag <- function(flag, env_name = NULL) {
  if (flag %in% args) return(TRUE)
  if (!is.null(env_name)) return(is_truthy(Sys.getenv(env_name, "")))
  FALSE
}

get_arg_value <- function(prefix, default = "") {
  hit <- args[startsWith(args, prefix)]
  if (length(hit) == 0) return(default)
  sub(prefix, "", hit[[length(hit)]], fixed = TRUE)
}

tracking_enabled <- has_flag("--track-emissions", env_name = "TRACK_EMISSIONS")
tracking_python_bin <- get_arg_value("--python-bin=", Sys.getenv("PYTHON_BIN", "python3"))
tracking_project_name <- get_arg_value(
  "--emissions-project-name=",
  Sys.getenv("CODECARBON_PROJECT_NAME", "epi-ML")
)
tracking_measure_power_secs <- get_arg_value(
  "--emissions-measure-power-secs=",
  Sys.getenv("CODECARBON_MEASURE_POWER_SECS", "1")
)
tracking_experiment_id <- get_arg_value(
  "--emissions-experiment-id=",
  Sys.getenv("CODECARBON_EXPERIMENT_ID", "")
)
tracking_run_source <- get_arg_value(
  "--emissions-run-source=",
  Sys.getenv("EPI_ML_EMISSIONS_SOURCE", "")
)
tracking_country_iso_code <- get_arg_value(
  "--emissions-country-iso-code=",
  Sys.getenv("CODECARBON_COUNTRY_ISO_CODE", "")
)
tracking_save_to_api <- has_flag("--emissions-save-to-api", env_name = "CODECARBON_SAVE_TO_API")
tracking_offline <- has_flag("--emissions-offline", env_name = "CODECARBON_OFFLINE")

emissions_tracker_script <- file.path(paths$project_root, "scripts", "track_emissions.py")
emissions_log_dir <- file.path(paths$logs_dir, "emissions")
emissions_run_id <- paste0(
  format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y%m%dT%H%M%SZ"),
  "-",
  Sys.getpid()
)

power_script <- "surveillance/investigation/05_power_design.R"
bootstrap_script <- "surveillance/investigation/03_data_wrangling.R"
wrangled_required <- file.path(paths$wrangled_dir, "data_wrangled.rds")

module_scripts <- list(
  list(path = "surveillance/investigation/04_descriptive_stats.R", optional = FALSE),
  list(path = "surveillance/investigation/04_distributions.R", optional = FALSE),
  list(path = "surveillance/investigation/05_frequentist.R", optional = FALSE),
  list(path = "surveillance/investigation/05_bayesian.R", optional = FALSE),
  list(path = "surveillance/investigation/06_logistic.R", optional = FALSE),
  list(path = "surveillance/investigation/06_model_comparison.R", optional = FALSE),
  list(path = "surveillance/investigation/06_regression.R", optional = FALSE),
  list(path = "surveillance/investigation/07_propensity.R", optional = FALSE),
  list(path = "surveillance/investigation/07_causal_estimators.R", optional = FALSE),
  list(path = "surveillance/investigation/07_treatment_effects.R", optional = FALSE),
  list(path = "surveillance/investigation/07_dag.R", optional = FALSE),
  list(path = "surveillance/investigation/07_meta_synthesis.R", optional = TRUE),
  list(path = "surveillance/ebac/07_ebac.R", optional = FALSE),
  list(path = "surveillance/ebac/07_ebac_ipw.R", optional = FALSE),
  list(path = "surveillance/ebac/07_ebac_gender_smote_sensitivity.R", optional = FALSE),
  list(path = "surveillance/ebac/07_ebac_integrations.R", optional = FALSE),
  list(path = "surveillance/investigation/08_figures.R", optional = FALSE),
  list(path = "surveillance/investigation/08_tables.R", optional = FALSE),
  list(path = "surveillance/investigation/09_report.R", optional = FALSE)
)

rscript_bin <- file.path(R.home("bin"), "Rscript")

module_label_from_script <- function(script) {
  rel <- gsub("\\\\", "/", script)
  rel <- sub("^surveillance/", "", rel)
  rel <- sub("\\.R$", "", rel)
  rel <- gsub("[^A-Za-z0-9_.-]+", "_", rel)
  if (!nzchar(rel)) {
    rel <- gsub("[^A-Za-z0-9_.-]+", "_", basename(script))
  }
  rel
}

run_with_tracker <- function(script, env_vars = character()) {
  source_hint <- tolower(trimws(tracking_run_source))
  if (!nzchar(source_hint)) {
    source_hint <- if (is_truthy(Sys.getenv("GITHUB_ACTIONS", "")) || nzchar(Sys.getenv("GITHUB_RUN_ID", ""))) {
      "cloud"
    } else {
      "local"
    }
  }
  tracker_env <- c(
    env_vars,
    sprintf("EPI_ML_EMISSIONS_RUN_ID=%s", emissions_run_id),
    sprintf("EPI_ML_EMISSIONS_SOURCE=%s", source_hint)
  )
  tracker_args <- c(
    emissions_tracker_script,
    "--module", module_label_from_script(script),
    "--run-id", emissions_run_id,
    "--log-dir", emissions_log_dir,
    "--project-name", tracking_project_name,
    "--measure-power-secs", tracking_measure_power_secs
  )

  if (tracking_save_to_api) {
    tracker_args <- c(tracker_args, "--save-to-api")
  }
  if (nzchar(tracking_experiment_id)) {
    tracker_args <- c(tracker_args, "--experiment-id", tracking_experiment_id)
  }
  if (tracking_offline) {
    tracker_args <- c(tracker_args, "--offline")
    if (nzchar(tracking_country_iso_code)) {
      tracker_args <- c(tracker_args, "--country-iso-code", tracking_country_iso_code)
    }
  }

  tracker_args <- c(tracker_args, "--", rscript_bin, script)
  system2(tracking_python_bin, tracker_args, env = tracker_env)
}

run_script <- function(script, optional = FALSE, env_vars = character()) {
  cat("Running:", script, if (optional) "(optional)" else "", "\n")
  code <- if (tracking_enabled) run_with_tracker(script, env_vars = env_vars) else system2(rscript_bin, script, env = env_vars)
  if (!identical(code, 0L)) {
    if (optional) {
      warning("Optional script failed and was skipped: ", script, " (exit code ", code, ")")
      return(invisible(FALSE))
    }
    stop("Execution failed at ", script, " (exit code ", code, ")")
  }
  invisible(TRUE)
}

file_stamp <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  info <- file.info(path)
  paste0(as.numeric(info$size[[1]]), ":", as.numeric(info$mtime[[1]]))
}

build_power_env <- function(paths) {
  out <- character()
  power_csv <- file.path(paths$output_private_dir, "power_interaction_sample_size_targets.csv")
  if (!file.exists(power_csv)) return(out)

  p <- tryCatch(read.csv(power_csv, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(p) || nrow(p) == 0) return(out)

  req <- subset(
    p,
    scenario == "pilot_observed" &
      allocation_strategy == "equal_strata" &
      abs(target_power - 0.80) < 1e-9 &
      status == "reached"
  )
  if (nrow(req) == 0) return(out)

  heavy <- req$required_n[req$endpoint == "heavy_drinking_30d"][1]
  ebac <- req$required_n[req$endpoint == "ebac_legal"][1]
  if (!is.na(heavy) && length(heavy) == 1) {
    out <- c(out, sprintf("POWER_TARGET_N_HEAVY=%s", as.integer(heavy)))
  }
  if (!is.na(ebac) && length(ebac) == 1) {
    out <- c(out, sprintf("POWER_TARGET_N_EBAC=%s", as.integer(ebac)))
  }

  out
}

if (tracking_enabled) {
  if (!file.exists(emissions_tracker_script)) {
    stop("CodeCarbon tracker helper not found: ", emissions_tracker_script)
  }
  dir.create(emissions_log_dir, recursive = TRUE, showWarnings = FALSE)
  cat("CodeCarbon tracking enabled. Run ID: ", emissions_run_id, "\n", sep = "")
  cat("Raw logs: ", safe_label_path(emissions_log_dir, paths), "\n", sep = "")
}

# -----------------------------------------------------------------------------
# Power-first orchestration
# -----------------------------------------------------------------------------
if (!file.exists(paths$data_file)) {
  stop("Power-first bootstrap failed: source data file not found: ", paths$data_file)
}
source_stamp_before <- file_stamp(paths$data_file)

cat("Power-first bootstrap: recomputing", bootstrap_script, "to guarantee fresh inputs.\n")
run_script(bootstrap_script, optional = FALSE)

if (!file.exists(wrangled_required)) {
  stop(
    "Power-first bootstrap failed: missing required wrangled artifact ",
    wrangled_required
  )
}
source_stamp_after <- file_stamp(paths$data_file)
if (!identical(source_stamp_before, source_stamp_after)) {
  stop(
    "Power-first bootstrap aborted: source data changed during wrangling. ",
    "Re-run scripts/run_modules.R to ensure a consistent snapshot."
  )
}

cat("Power-first execution: running", power_script, "before all other modules.\n")
run_script(power_script, optional = FALSE)

power_targets <- file.path(paths$output_private_dir, "power_interaction_sample_size_targets.csv")
if (file.exists(power_targets) && file.exists(wrangled_required)) {
  if (as.numeric(file.info(power_targets)$mtime) < as.numeric(file.info(wrangled_required)$mtime)) {
    stop(
      "Power-first gating failed: power targets are older than wrangled inputs (stale power output)."
    )
  }
}

power_env <- build_power_env(paths)
required_power_keys <- c("POWER_TARGET_N_HEAVY", "POWER_TARGET_N_EBAC")
present_power_keys <- sub("=.*$", "", power_env)
missing_power_keys <- setdiff(required_power_keys, present_power_keys)

if (length(missing_power_keys) > 0) {
  stop(
    "Power-first gating failed: missing required runtime targets from ",
    basename(power_script),
    " -> ",
    paste(missing_power_keys, collapse = ", "),
    ". Aborting downstream modules."
  )
}

cat("Power runtime targets exported for downstream modules:\n")
for (e in power_env) cat("  ", e, "\n", sep = "")

for (entry in module_scripts) {
  script <- entry$path
  optional <- isTRUE(entry$optional)
  run_script(script, optional = optional, env_vars = power_env)
}

publish_script <- file.path(paths$project_root, "scripts", "publish_public_artifacts.R")
if (file.exists(publish_script)) {
  cat("Publishing public artifacts...\n")
  pub_code <- system2(rscript_bin, publish_script)
  if (!identical(pub_code, 0L)) {
    warning("Public artifact publish step failed (exit code ", pub_code, ").")
  }
}

cat("All modules completed successfully.\n")
