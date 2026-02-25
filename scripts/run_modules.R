#!/usr/bin/env Rscript
source("surveillance/lib/config_paths.R")
paths <- get_paths()
init_paths(paths)
setwd(paths$project_root)

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

run_script <- function(script, optional = FALSE, env_vars = character()) {
  cat("Running:", script, if (optional) "(optional)" else "", "\n")
  code <- system2(rscript_bin, script, env = env_vars)
  if (!identical(code, 0L)) {
    if (optional) {
      warning("Optional script failed and was skipped: ", script, " (exit code ", code, ")")
      return(invisible(FALSE))
    }
    stop("Execution failed at ", script, " (exit code ", code, ")")
  }
  invisible(TRUE)
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

# -----------------------------------------------------------------------------
# Power-first orchestration
# -----------------------------------------------------------------------------
cat("Power-first bootstrap: recomputing", bootstrap_script, "to guarantee fresh inputs.\n")
run_script(bootstrap_script, optional = FALSE)

if (!file.exists(wrangled_required)) {
  stop(
    "Power-first bootstrap failed: missing required wrangled artifact ",
    wrangled_required
  )
}

cat("Power-first execution: running", power_script, "before all other modules.\n")
run_script(power_script, optional = FALSE)
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
