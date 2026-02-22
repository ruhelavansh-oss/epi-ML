#!/usr/bin/env Rscript
# =============================================================================
# 06_regression.R — Phase 6: Regression Analysis (Seeing Theory Ch.6)
# =============================================================================
# OLS baseline (unweighted), survey-weighted GLM, correlation matrix,
# model diagnostics, naive vs adjusted comparison.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(survey)
  library(car)
})

options(scipen = 999)
set.seed(42)
source("surveillance/lib/config_paths.R")
paths <- get_paths()
init_paths(paths)


wrangled_dir <- paths$wrangled_dir
output_dir <- paths$output_private_dir
fig_dir <- paths$figures_dir
cat("=== PHASE 6: REGRESSION ANALYSIS (Seeing Theory Ch.6) ===\n\n")

# --- Load wrangled data ---
pumf <- readRDS(file.path(wrangled_dir, "cpads_pumf_wrangled.rds"))
cat("Loaded CPADS PUMF:", nrow(pumf), "observations,", ncol(pumf), "variables\n\n")

# --- Prepare analysis subset (complete cases for model variables) ---
model_vars <- c("heavy_drinking_30d", "age_group", "gender", "province_region",
                 "mental_health", "wtpumf")

pumf_cc <- pumf %>%
  dplyr::select(dplyr::all_of(model_vars), alc03, alc06) %>%
  dplyr::filter(complete.cases(dplyr::across(dplyr::all_of(model_vars))))

cat("Complete cases for regression:", nrow(pumf_cc), "of", nrow(pumf),
    sprintf("(%.1f%%)\n\n", 100 * nrow(pumf_cc) / nrow(pumf)))

# =============================================================================
# 6.1 — Correlation Matrix for Numeric Alcohol Variables
# =============================================================================
cat(paste(rep("=", 62), collapse = ""), "\n")
cat("6.1 CORRELATION MATRIX: NUMERIC ALCOHOL VARIABLES\n")
cat(paste(rep("=", 62), collapse = ""), "\n\n")

# Filter to valid ranges: alc03 <= 7 (frequency code), alc06 <= 30 (drinks)
pumf_corr <- pumf %>%
  dplyr::mutate(
    alc03_valid = dplyr::if_else(alc03 <= 7, alc03, NA_real_),
    alc06_valid = dplyr::if_else(alc06 <= 30, alc06, NA_real_)
  ) %>%
  dplyr::select(heavy_drinking_30d, alc03_valid, alc06_valid) %>%
  dplyr::filter(complete.cases(.))

cat("Observations with valid alc03 (<= 7) and alc06 (<= 30) and heavy_drinking_30d:",
    nrow(pumf_corr), "\n\n")

if (nrow(pumf_corr) > 2) {
  var_sd <- vapply(pumf_corr, stats::sd, numeric(1), na.rm = TRUE)
  corr_vars <- names(var_sd)[is.finite(var_sd) & var_sd > 0]

  if (length(corr_vars) < 2) {
    cat("Insufficient variable variability for correlation matrix.\n")
    corr_matrix <- NULL
  } else {
    corr_matrix <- stats::cor(pumf_corr[, corr_vars, drop = FALSE], use = "complete.obs")
  cat("Pearson Correlation Matrix:\n")
  print(round(corr_matrix, 4))

  cat("\nCorrelation tests (pairwise):\n")
  pairs <- list(
    c("heavy_drinking_30d", "alc03_valid"),
    c("heavy_drinking_30d", "alc06_valid"),
    c("alc03_valid", "alc06_valid")
  )
  for (pr in pairs) {
    if (!(pr[1] %in% corr_vars) || !(pr[2] %in% corr_vars)) next
    ct <- stats::cor.test(pumf_corr[[pr[1]]], pumf_corr[[pr[2]]])
    cat(sprintf("  %s vs %s: r = %.4f, p = %s\n",
                pr[1], pr[2], ct$estimate,
                formatC(ct$p.value, format = "e", digits = 3)))
  }
  }
} else {
  cat("Insufficient complete observations for correlation matrix.\n")
  corr_matrix <- NULL
}

# =============================================================================
# 6.2 — OLS Baseline (Unweighted Linear Probability Model)
# =============================================================================
cat("\n")
cat(paste(rep("=", 62), collapse = ""), "\n")
cat("6.2 OLS BASELINE: UNWEIGHTED LINEAR PROBABILITY MODEL\n")
cat(paste(rep("=", 62), collapse = ""), "\n\n")

cat("Model: heavy_drinking_30d ~ age_group + gender + province_region + mental_health\n")
cat("Note: OLS on binary outcome = linear probability model (LPM) for baseline\n\n")

ols_model <- lm(heavy_drinking_30d ~ age_group + gender + province_region + mental_health,
                data = pumf_cc)

cat("--- OLS Summary ---\n")
print(summary(ols_model))

cat("\n--- OLS VIF (Variance Inflation Factors) ---\n")
vif_ols <- car::vif(ols_model)
print(vif_ols)
cat("\nInterpretation: VIF > 5 suggests moderate multicollinearity;",
    "VIF > 10 is severe.\n")

max_vif <- if (is.matrix(vif_ols)) {
  if ("GVIF^(1/(2*Df))" %in% colnames(vif_ols)) {
    max(vif_ols[, "GVIF^(1/(2*Df))"])
  } else {
    max(vif_ols[, ncol(vif_ols)])
  }
} else {
  max(vif_ols)
}
cat(sprintf("Maximum GVIF^(1/(2*Df)): %.3f — %s\n\n",
            max_vif,
            if (max_vif < 2) "No multicollinearity concern"
            else if (max_vif < 5) "Moderate — acceptable"
            else "Elevated — investigate"))

# =============================================================================
# 6.3 — Survey-Weighted GLM (Quasibinomial)
# =============================================================================
cat(paste(rep("=", 62), collapse = ""), "\n")
cat("6.3 SURVEY-WEIGHTED GLM (QUASIBINOMIAL)\n")
cat(paste(rep("=", 62), collapse = ""), "\n\n")

cat("Model: svyglm(heavy_drinking_30d ~ age_group + gender + province_region + mental_health,\n")
cat("              family = quasibinomial(), design = svy_design)\n\n")

# Create survey design on complete-case data
svy_design <- svydesign(ids = ~1, weights = ~wtpumf, data = pumf_cc)

svy_model <- svyglm(
  heavy_drinking_30d ~ age_group + gender + province_region + mental_health,
  design = svy_design,
  family = quasibinomial()
)

cat("--- Survey-Weighted GLM Summary ---\n")
print(summary(svy_model))

cat("\n--- Survey GLM VIF ---\n")
# VIF from the svyglm object (car::vif works on glm-like objects)
vif_svy <- car::vif(svy_model)
print(vif_svy)

# =============================================================================
# 6.4 — Naive (Intercept-Only) vs Adjusted Model Comparison
# =============================================================================
cat("\n")
cat(paste(rep("=", 62), collapse = ""), "\n")
cat("6.4 NAIVE vs ADJUSTED MODEL COMPARISON\n")
cat(paste(rep("=", 62), collapse = ""), "\n\n")

# Intercept-only (null) model
svy_null <- svyglm(
  heavy_drinking_30d ~ 1,
  design = svy_design,
  family = quasibinomial()
)

cat("--- Null Model (Intercept Only) ---\n")
cat(sprintf("Intercept: %.4f (SE = %.4f)\n",
            coef(svy_null), SE(svy_null)))
cat(sprintf("Implied prevalence: expit(%.4f) = %.4f\n",
            coef(svy_null),
            plogis(coef(svy_null))))
cat(sprintf("Null deviance: %.2f on %d df\n",
            svy_null$deviance, svy_null$df.residual))

cat("\n--- Adjusted Model ---\n")
cat(sprintf("Residual deviance: %.2f on %d df\n",
            svy_model$deviance, svy_model$df.residual))

# Deviance comparison
dev_reduction <- svy_null$deviance - svy_model$deviance
pct_reduction <- 100 * dev_reduction / svy_null$deviance
cat(sprintf("\nDeviance reduction: %.2f (%.2f%%)\n", dev_reduction, pct_reduction))

# ANOVA-style comparison (F-test for survey models)
cat("\n--- ANOVA: Null vs Adjusted ---\n")
anova_result <- anova(svy_null, svy_model, method = "Wald")
print(anova_result)

# regTermTest for each predictor
cat("\n--- Wald Tests for Individual Predictors ---\n")
for (pred in c("age_group", "gender", "province_region", "mental_health")) {
  rtt <- regTermTest(svy_model, pred, method = "Wald")
  cat(sprintf("  %s: F = %.3f, df = (%.0f, %.0f), p = %s\n",
              pred, rtt$Ftest, rtt$df, rtt$ddf,
              formatC(rtt$p, format = "e", digits = 3)))
}

# AIC approximation
aic_null <- extractAIC(svy_null)
aic_adj  <- extractAIC(svy_model)
cat(sprintf("\nAIC approximation:  Null = %.2f,  Adjusted = %.2f,  Delta = %.2f\n",
            aic_null[2], aic_adj[2], aic_null[2] - aic_adj[2]))

# =============================================================================
# 6.5 — Save Initial Results
# =============================================================================
cat("\n")
cat(paste(rep("=", 62), collapse = ""), "\n")
cat("6.5 SAVING RESULTS\n")
cat(paste(rep("=", 62), collapse = ""), "\n\n")

# --- OLS coefficients ---
ols_coefs <- broom::tidy(ols_model, conf.int = TRUE) %>%
  dplyr::mutate(model = "OLS_unweighted")

# --- Survey GLM coefficients (with ORs) ---
svy_coefs <- broom::tidy(svy_model, conf.int = TRUE) %>%
  dplyr::mutate(
    model = "svyglm_quasibinomial",
    OR    = exp(estimate),
    OR_lower = exp(conf.low),
    OR_upper = exp(conf.high)
  )

# Combine
all_coefs <- bind_rows(ols_coefs, svy_coefs)

write_csv(all_coefs, file.path(output_dir, "regression_coefficients.csv"))
cat("Saved: regression_coefficients.csv\n")

# --- Correlation matrix ---
if (!is.null(corr_matrix)) {
  corr_df <- as.data.frame(corr_matrix) %>%
    tibble::rownames_to_column("variable")
  write_csv(corr_df, file.path(output_dir, "alcohol_correlation_matrix.csv"))
  cat("Saved: alcohol_correlation_matrix.csv\n")
}

# --- Model comparison summary ---
comparison_df <- tibble(
  model = c("Null (intercept-only)", "Adjusted (4 predictors)"),
  deviance = c(svy_null$deviance, svy_model$deviance),
  df_residual = c(svy_null$df.residual, svy_model$df.residual),
  AIC_approx = c(aic_null[2], aic_adj[2]),
  n_parameters = c(length(coef(svy_null)), length(coef(svy_model)))
)

write_csv(comparison_df, file.path(output_dir, "regression_model_comparison.csv"))
cat("Saved: regression_model_comparison.csv\n")

cat(sprintf("\nComplete cases used: %d of %d (%.1f%%)\n",
            nrow(pumf_cc), nrow(pumf), 100 * nrow(pumf_cc) / nrow(pumf)))
cat("\n=== REGRESSION ANALYSIS COMPLETE ===\n")
