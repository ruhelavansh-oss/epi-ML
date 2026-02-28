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
  "AIPW",
  "BayesFactor",
  "broom",
  "car",
  "cobalt",
  "dagitty",
  "DoubleML",
  "dplyr",
  "forcats",
  "ggplot2",
  "gt",
  "gtsummary",
  "janitor",
  "MASS",
  "MatchIt",
  "meta",
  "mlr3",
  "mlr3learners",
  "purrr",
  "pwr",
  "readr",
  "smotefamily",
  "stringr",
  "SuperLearner",
  "survey",
  "tibble",
  "tidyr",
  "tidyverse",
  "yaml",
  "ranger",
  "knitr",
  "rmarkdown"
)

optional_pkgs <- c("sl3", "ggdag", "effectsize")

required <- unique(c(
  core_required,
  if (install_full) analysis_required,
  if (install_connect) connect_required
))
repos <- Sys.getenv("RSPM", "https://cloud.r-project.org")
ncpus <- max(1L, as.integer(Sys.getenv("NCPUS", "2")))
namespace_ok <- function(pkg) {
  isTRUE(suppressWarnings(requireNamespace(pkg, quietly = TRUE)))
}

missing <- required[!vapply(required, namespace_ok, logical(1))]

if (length(missing) > 0) {
  cat("Installing missing packages:\n")
  cat("  ", paste(missing, collapse = ", "), "\n", sep = "")
  cat("Using CRAN mirror: ", repos, "\n", sep = "")
  install.packages(missing, repos = repos, Ncpus = ncpus)
}

still_missing <- required[!vapply(required, namespace_ok, logical(1))]
if (length(still_missing) > 0) {
  cat("Retrying required packages serially:\n")
  cat("  ", paste(still_missing, collapse = ", "), "\n", sep = "")
  install.packages(still_missing, repos = repos, Ncpus = 1)
}

still_missing <- required[!vapply(required, namespace_ok, logical(1))]
if (length(still_missing) > 0) {
  cat("Retrying required packages from source:\n")
  cat("  ", paste(still_missing, collapse = ", "), "\n", sep = "")
  for (pkg in still_missing) {
    install.packages(pkg, repos = repos, type = "source", dependencies = TRUE, Ncpus = 1)
  }
}

still_missing <- required[!vapply(required, namespace_ok, logical(1))]
if (length(still_missing) > 0) {
  diagnostics <- vapply(still_missing, function(pkg) {
    ip <- rownames(installed.packages())
    installed <- pkg %in% ip
    err <- tryCatch({
      loadNamespace(pkg)
      ""
    }, error = function(e) conditionMessage(e))
    paste0(pkg, " | installed=", installed, " | load_error=", ifelse(nzchar(err), err, "unknown"))
  }, character(1))
  cat("Package diagnostics:\n")
  cat("  ", paste(diagnostics, collapse = "\n  "), "\n", sep = "")
  stop("Failed to install required packages: ", paste(still_missing, collapse = ", "))
}

missing_optional <- optional_pkgs[!vapply(optional_pkgs, namespace_ok, logical(1))]
if (length(missing_optional) > 0) {
  cat("Installing optional packages:\n")
  cat("  ", paste(missing_optional, collapse = ", "), "\n", sep = "")
  try(install.packages(missing_optional, repos = repos, Ncpus = 1), silent = TRUE)
}

missing_optional <- optional_pkgs[!vapply(optional_pkgs, namespace_ok, logical(1))]

mode <- if (install_full) "full pipeline" else "core (render/deploy)"
if (install_connect) {
  mode <- paste(mode, "+ connect")
}
cat("Dependency bootstrap complete for ", mode, " mode.\n", sep = "")
cat("Installed/available required packages (n=", length(required), ").\n", sep = "")
if (length(missing_optional) > 0) {
  cat("Optional packages not installed: ", paste(missing_optional, collapse = ", "), "\n", sep = "")
}
