#!/usr/bin/env Rscript
source("surveillance/lib/config_paths.R")
paths <- get_paths()
init_paths(paths)
setwd(paths$project_root)

module_scripts <- list(
  list(path = "surveillance/investigation/03_data_wrangling.R", optional = FALSE),
  list(path = "surveillance/investigation/04_descriptive_stats.R", optional = FALSE),
  list(path = "surveillance/investigation/04_distributions.R", optional = FALSE),
  list(path = "surveillance/investigation/05_frequentist.R", optional = FALSE),
  list(path = "surveillance/investigation/05_bayesian.R", optional = FALSE),
  list(path = "surveillance/investigation/05_power_design.R", optional = FALSE),
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
for (entry in module_scripts) {
  script <- entry$path
  optional <- isTRUE(entry$optional)
  cat("Running:", script, if (optional) "(optional)" else "", "\n")
  code <- system2(rscript_bin, script)
  if (!identical(code, 0L)) {
    if (optional) {
      warning("Optional script failed and was skipped: ", script, " (exit code ", code, ")")
      next
    }
    stop("Execution failed at ", script, " (exit code ", code, ")")
  }
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
