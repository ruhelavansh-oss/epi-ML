#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
install_full <- "--full" %in% args
install_connect <- "--connect" %in% args

core_required <- c(
  "yaml",
  "knitr",
  "rmarkdown"
)

connect_required <- c("rsconnect")

analysis_required <- c(
  "tidyverse",
  "survey",
  "janitor",
  "MASS",
  "BayesFactor",
  "pwr",
  "MatchIt",
  "cobalt",
  "dagitty",
  "ggdag",
  "car",
  "gtsummary",
  "gt",
  "meta",
  "smotefamily",
  "DoubleML",
  "mlr3",
  "mlr3learners",
  "AIPW",
  "SuperLearner",
  "broom"
)

# Optional package referenced in code paths with internal fallbacks.
optional_pkgs <- c("sl3")

required <- unique(c(
  core_required,
  if (install_full) analysis_required,
  if (install_connect) connect_required
))
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing) > 0) {
  cat("Installing missing packages:\n")
  cat("  ", paste(missing, collapse = ", "), "\n", sep = "")
  install.packages(missing, repos = "https://cloud.r-project.org")
}

still_missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(still_missing) > 0) {
  stop("Failed to install required packages: ", paste(still_missing, collapse = ", "))
}

missing_optional <- optional_pkgs[!vapply(optional_pkgs, requireNamespace, logical(1), quietly = TRUE)]

mode <- if (install_full) "full pipeline" else "core (render/deploy)"
if (install_connect) {
  mode <- paste(mode, "+ connect")
}
cat("Dependency bootstrap complete for ", mode, " mode.\n", sep = "")
cat("Installed/available required packages (n=", length(required), ").\n", sep = "")
if (length(missing_optional) > 0) {
  cat("Optional packages not installed: ", paste(missing_optional, collapse = ", "), "\n", sep = "")
}
