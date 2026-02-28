#!/usr/bin/env Rscript
# =============================================================================
# 07_meta_synthesis.R — Phase 7: Meta-Analysis of CADS Aggregate Data
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(tibble)
  library(stringr)
  library(forcats)
  library(purrr)
  library(meta)
})

options(scipen = 999)
set.seed(42)
source("surveillance/lib/config_paths.R")
paths <- get_paths()
init_paths(paths)


wrangled_dir <- paths$wrangled_dir
output_dir <- paths$output_private_dir
fig_dir <- paths$figures_dir
cat("=== PHASE 7: META-ANALYSIS ===\n\n")

cads_path <- file.path(wrangled_dir, "cads_alcohol.rds")
if (!file.exists(cads_path)) {
  cat("Skipping meta-analysis: missing file ", cads_path, "\n", sep = "")
  quit(save = "no", status = 0)
}

cads_alc <- readRDS(cads_path)

# Convert character columns to numeric
cads_alc <- cads_alc %>%
  dplyr::mutate(
    colpercent = as.numeric(colpercent),
    collowercl = as.numeric(collowercl),
    coluppercl = as.numeric(coluppercl),
    numerator  = as.numeric(numerator),
    denominator = as.numeric(denominator)
  )

# Filter: overall Year estimates (not disaggregated by demographics)
overall <- cads_alc %>%
  dplyr::filter(disagg_var1 == "Year", !is.na(numerator), !is.na(denominator),
         denominator > 0, numerator > 0)

cat("Overall alcohol estimates:", nrow(overall), "rows\n")
cat("Questions:\n")
for (q in unique(overall$question)) cat("  -", q, "\n")

# --- Meta Synthesis of proportions ---
cat("\n--- Meta-Analysis of Alcohol Prevalence ---\n")

# Use metaprop for proportions
tryCatch({
  ma <- metaprop(
    event = round(overall$numerator),
    n     = round(overall$denominator),
    studlab = overall$question,
    sm    = "PLOGIT",
    method = "Inverse",
    random = TRUE,
    fixed  = FALSE
  )

  cat("\nMeta Synthesis summary:\n")
  print(summary(ma))

  cat(sprintf("\nPooled prevalence (random): %.3f (95%% CI: %.3f, %.3f)\n",
              ma$TE.random, ma$lower.random, ma$upper.random))
  cat(sprintf("I-squared: %.1f%%\n", ma$I2 * 100))
  cat(sprintf("Q statistic: %.2f (p = %s)\n",
              ma$Q, formatC(ma$pval.Q, format = "e", digits = 3)))
  cat(sprintf("Tau-squared: %.4f\n", ma$tau2))

  # Forest plot
  pdf(file.path(fig_dir, "forest_plot.pdf"), width = 12, height = 8)
  forest(ma,
         sortvar = TE,
         print.tau2 = TRUE,
         print.I2 = TRUE,
         leftcols = c("studlab"),
         leftlabs = c("Alcohol Measure"),
         rightcols = c("effect", "ci"),
         rightlabs = c("Proportion", "95% CI"),
         col.diamond = "blue",
         col.square = "darkblue",
         smlab = "Prevalence (logit scale)")
  dev.off()
  cat("\nSaved: figures/forest_plot.pdf\n")

  # Funnel plot
  pdf(file.path(fig_dir, "funnel_plot.pdf"), width = 8, height = 6)
  funnel(ma, main = "Funnel Plot: CADS Alcohol Estimates")
  dev.off()
  cat("Saved: figures/funnel_plot.pdf\n")

}, error = function(e) {
  cat("Meta Synthesis error:", e$message, "\n")
  cat("Attempting simpler approach...\n")

  # Fallback: manual random-effects meta-analysis
  overall <- overall %>%
    dplyr::mutate(
      prop = numerator / denominator,
      se_prop = sqrt(prop * (1 - prop) / denominator),
      logit_p = log(prop / (1 - prop)),
      se_logit = 1 / sqrt(numerator) + 1 / sqrt(denominator - numerator)
    ) %>%
    dplyr::filter(is.finite(logit_p), is.finite(se_logit))

  # Fixed-effect pooled
  w <- 1 / overall$se_logit^2
  pooled_logit <- sum(w * overall$logit_p) / sum(w)
  pooled_se <- 1 / sqrt(sum(w))
  pooled_prop <- 1 / (1 + exp(-pooled_logit))

  cat(sprintf("Pooled prevalence (FE): %.3f\n", pooled_prop))
  cat(sprintf("Q statistic: %.2f\n", sum(w * (overall$logit_p - pooled_logit)^2)))
})

# --- Save results ---
meta_results <- overall %>%
  dplyr::select(question, colpercent, collowercl, coluppercl, numerator, denominator)
write_csv(meta_results, file.path(output_dir, "meta_analysis_inputs.csv"))

cat("\n=== META-ANALYSIS COMPLETE ===\n")
