#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
project_root <- if (length(args) > 0) args[[1]] else "."
project_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)

page_path <- file.path(project_root, "_site", "code", "power-design.html")
if (!file.exists(page_path)) {
  stop("Rendered power page not found: ", page_path, call. = FALSE)
}

page_html <- paste(readLines(page_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

required_outputs <- c(
  "frequentist_heavy_drinking_prevalence_ci.csv",
  "official_doc_alignment_checklist.csv",
  "power_ebac_endpoint_anchors.csv",
  "power_gpower_reference_two_group.csv",
  "power_interaction_assumptions.csv",
  "power_interaction_feasibility_flags.csv",
  "power_interaction_group_allocations.csv",
  "power_interaction_imbalance_penalty.csv",
  "power_interaction_pairwise_details.csv",
  "power_interaction_sample_size_targets.csv",
  "power_one_proportion_grid.csv",
  "power_summary.csv",
  "power_two_proportion_gender.csv",
  "randomization_block_blueprints.csv"
)

legacy_outputs <- c(
  "power_analysis_summary.csv",
  "power_curves.pdf"
)

required_phrases <- c(
  "Power Focus",
  "Female-versus-Male interaction power"
)

contains_token <- function(token) grepl(token, page_html, fixed = TRUE)

missing_outputs <- required_outputs[!vapply(required_outputs, contains_token, logical(1))]
present_legacy <- legacy_outputs[vapply(legacy_outputs, contains_token, logical(1))]
missing_phrases <- required_phrases[!vapply(required_phrases, contains_token, logical(1))]

if (length(missing_outputs) > 0 || length(present_legacy) > 0 || length(missing_phrases) > 0) {
  if (length(missing_outputs) > 0) {
    cat("Missing required power outputs in rendered page:\n")
    cat(paste0(" - ", missing_outputs), sep = "\n")
    cat("\n")
  }
  if (length(present_legacy) > 0) {
    cat("Found legacy outputs that should not appear in rendered page:\n")
    cat(paste0(" - ", present_legacy), sep = "\n")
    cat("\n")
  }
  if (length(missing_phrases) > 0) {
    cat("Missing required page text:\n")
    cat(paste0(" - ", missing_phrases), sep = "\n")
    cat("\n")
  }
  stop("Power Design page validation failed.", call. = FALSE)
}

cat("Power Design page validation passed.\n")
