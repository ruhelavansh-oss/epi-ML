#!/usr/bin/env Rscript
# =============================================================================
# 06_model_comparison.R — Phase 6: Nested Model Comparison
# =============================================================================
# Compares nested survey-weighted logistic models for heavy_drinking_30d:
#   Model 0: ~1 (null)
#   Model 1: ~age_group + gender
#   Model 2: Model 1 + province_region + mental_health
#   Model 3: Model 2 + cannabis_any_use + physical_health
# Uses anova(), regTermTest(), and AIC approximation.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(survey)
})

options(scipen = 999)
set.seed(42)
source("surveillance/lib/config_paths.R")
paths <- get_paths()
init_paths(paths)


wrangled_dir <- paths$wrangled_dir
output_dir <- paths$output_private_dir
cat("=== PHASE 6: NESTED MODEL COMPARISON ===\n\n")

# --- Load wrangled data ---
pumf <- readRDS(file.path(wrangled_dir, "cpads_pumf_wrangled.rds"))
cat("Loaded CPADS PUMF:", nrow(pumf), "observations\n\n")

# --- Prepare complete-case subset (all variables across all models) ---
model_vars <- c("heavy_drinking_30d", "age_group", "gender", "province_region",
                 "mental_health", "cannabis_any_use", "physical_health", "wtpumf")

pumf_cc <- pumf %>%
  dplyr::select(dplyr::all_of(model_vars)) %>%
  dplyr::filter(complete.cases(.))

cat("Complete cases (all model variables):", nrow(pumf_cc), "of", nrow(pumf),
    sprintf("(%.1f%%)\n", 100 * nrow(pumf_cc) / nrow(pumf)))
cat("Note: Using same complete-case sample for all models to ensure comparability.\n\n")

# --- Outcome distribution ---
cat("Outcome distribution (heavy_drinking_30d):\n")
tab <- table(pumf_cc$heavy_drinking_30d, useNA = "ifany")
cat(sprintf("  0 (No heavy drinking):  %d (%.1f%%)\n",
            tab["0"], 100 * tab["0"] / sum(tab)))
cat(sprintf("  1 (Heavy drinking):     %d (%.1f%%)\n\n",
            tab["1"], 100 * tab["1"] / sum(tab)))

# --- Survey design ---
svy_design <- svydesign(ids = ~1, weights = ~wtpumf, data = pumf_cc)

# =============================================================================
# Fit Nested Models
# =============================================================================
cat(paste(rep("=", 62), collapse = ""), "\n")
cat("FITTING NESTED MODELS\n")
cat(paste(rep("=", 62), collapse = ""), "\n\n")

# Model 0: Null (intercept only)
cat("--- Model 0: heavy_drinking_30d ~ 1 ---\n")
m0 <- svyglm(heavy_drinking_30d ~ 1, design = svy_design, family = quasibinomial())
cat(sprintf("  Intercept = %.4f, implied prevalence = %.4f\n",
            coef(m0), plogis(coef(m0))))
cat(sprintf("  Deviance = %.2f, df = %d\n\n", m0$deviance, m0$df.residual))

# Model 1: Demographics
cat("--- Model 1: heavy_drinking_30d ~ age_group + gender ---\n")
m1 <- svyglm(heavy_drinking_30d ~ age_group + gender,
              design = svy_design, family = quasibinomial())
print(summary(m1))
cat(sprintf("  Deviance = %.2f, df = %d\n\n", m1$deviance, m1$df.residual))

# Model 2: Demographics + Region + Mental Health
cat("--- Model 2: Model 1 + province_region + mental_health ---\n")
m2 <- svyglm(heavy_drinking_30d ~ age_group + gender + province_region + mental_health,
              design = svy_design, family = quasibinomial())
print(summary(m2))
cat(sprintf("  Deviance = %.2f, df = %d\n\n", m2$deviance, m2$df.residual))

# Model 3: Full model
cat("--- Model 3: Model 2 + cannabis_any_use + physical_health ---\n")
m3 <- svyglm(heavy_drinking_30d ~ age_group + gender + province_region + mental_health +
                cannabis_any_use + physical_health,
              design = svy_design, family = quasibinomial())
print(summary(m3))
cat(sprintf("  Deviance = %.2f, df = %d\n\n", m3$deviance, m3$df.residual))

# Model 4: Interaction model
cat("--- Model 4: Model 3 + cannabis_any_use:gender ---\n")
m4 <- svyglm(heavy_drinking_30d ~ age_group + gender + province_region + mental_health +
               cannabis_any_use + physical_health + cannabis_any_use:gender,
             design = svy_design, family = quasibinomial())
print(summary(m4))
cat(sprintf("  Deviance = %.2f, df = %d\n\n", m4$deviance, m4$df.residual))

# =============================================================================
# ANOVA: Sequential Model Comparisons
# =============================================================================
cat(paste(rep("=", 62), collapse = ""), "\n")
cat("ANOVA: SEQUENTIAL MODEL COMPARISONS\n")
cat(paste(rep("=", 62), collapse = ""), "\n\n")

cat("--- Model 0 vs Model 1 (adding age_group + gender) ---\n")
anova_01 <- anova(m0, m1, method = "Wald")
print(anova_01)

cat("\n--- Model 1 vs Model 2 (adding province_region + mental_health) ---\n")
anova_12 <- anova(m1, m2, method = "Wald")
print(anova_12)

cat("\n--- Model 2 vs Model 3 (adding cannabis_any_use + physical_health) ---\n")
anova_23 <- anova(m2, m3, method = "Wald")
print(anova_23)

cat("\n--- Model 0 vs Model 3 (null vs full) ---\n")
anova_03 <- anova(m0, m3, method = "Wald")
print(anova_03)

cat("\n--- Model 3 vs Model 4 (adding cannabis_any_use:gender) ---\n")
anova_34 <- anova(m3, m4, method = "Wald")
print(anova_34)

# =============================================================================
# regTermTest for Added Predictors
# =============================================================================
cat("\n")
cat(paste(rep("=", 62), collapse = ""), "\n")
cat("WALD TESTS FOR INDIVIDUAL PREDICTORS (regTermTest)\n")
cat(paste(rep("=", 62), collapse = ""), "\n\n")

cat("Testing each predictor's contribution in the full model (Model 3):\n\n")

predictors <- c("age_group", "gender", "province_region", "mental_health",
                 "cannabis_any_use", "physical_health")

rtt_results <- tibble(
  predictor = character(),
  F_statistic = numeric(),
  df_num = numeric(),
  df_denom = numeric(),
  p_value = numeric(),
  significant = character()
)

for (pred in predictors) {
  rtt <- regTermTest(m3, pred, method = "Wald")
  cat(sprintf("  %s:\n    F = %.3f, df = (%.0f, %.0f), p = %s %s\n\n",
              pred, rtt$Ftest, rtt$df, rtt$ddf,
              formatC(rtt$p, format = "e", digits = 3),
              if (rtt$p < 0.05) "*" else ""))

  rtt_results <- rtt_results %>% add_row(
    predictor   = pred,
    F_statistic = round(as.numeric(rtt$Ftest), 4),
    df_num      = rtt$df,
    df_denom    = rtt$ddf,
    p_value     = rtt$p,
    significant = if (rtt$p < 0.05) "Yes" else "No"
  )
}

# =============================================================================
# AIC Approximation
# =============================================================================
cat(paste(rep("=", 62), collapse = ""), "\n")
cat("AIC APPROXIMATION (extractAIC)\n")
cat(paste(rep("=", 62), collapse = ""), "\n\n")

cat("Note: AIC for survey-weighted models is an approximation.\n")
cat("Lower AIC indicates better fit-complexity trade-off.\n\n")

aic_m0 <- extractAIC(m0)
aic_m1 <- extractAIC(m1)
aic_m2 <- extractAIC(m2)
aic_m3 <- extractAIC(m3)
aic_m4 <- extractAIC(m4)

aic_table <- tibble(
  model = c("Model 0 (null)", "Model 1 (demographics)",
            "Model 2 (+ region, MH)", "Model 3 (full)", "Model 4 (+ cannabis x gender)"),
  formula = c("~ 1",
              "~ age_group + gender",
              "~ age_group + gender + province_region + mental_health",
              "~ age_group + gender + province_region + mental_health + cannabis_any_use + physical_health",
              "~ age_group + gender + province_region + mental_health + cannabis_any_use + physical_health + cannabis_any_use:gender"),
  n_params = c(aic_m0[1], aic_m1[1], aic_m2[1], aic_m3[1], aic_m4[1]),
  AIC = c(aic_m0[2], aic_m1[2], aic_m2[2], aic_m3[2], aic_m4[2])
)

aic_table <- aic_table %>%
  dplyr::mutate(
    delta_AIC = AIC - min(AIC),
    best = dplyr::if_else(delta_AIC == 0, "<-- Best", "")
  )

cat("AIC Comparison Table:\n")
print(aic_table, n = Inf, width = Inf)

# =============================================================================
# Comprehensive Comparison Table
# =============================================================================
cat("\n")
cat(paste(rep("=", 62), collapse = ""), "\n")
cat("COMPREHENSIVE MODEL COMPARISON\n")
cat(paste(rep("=", 62), collapse = ""), "\n\n")

comparison_table <- tibble(
  model = c("Model 0", "Model 1", "Model 2", "Model 3", "Model 4"),
  description = c("Null (intercept only)",
                   "Demographics (age + gender)",
                   "+ Region + Mental Health",
                   "+ Cannabis + Physical Health",
                   "+ Cannabis x Gender interaction"),
  n_parameters = c(length(coef(m0)), length(coef(m1)),
                    length(coef(m2)), length(coef(m3)), length(coef(m4))),
  deviance = round(c(m0$deviance, m1$deviance, m2$deviance, m3$deviance, m4$deviance), 2),
  df_residual = c(m0$df.residual, m1$df.residual,
                   m2$df.residual, m3$df.residual, m4$df.residual),
  AIC_approx = round(c(aic_m0[2], aic_m1[2], aic_m2[2], aic_m3[2], aic_m4[2]), 2),
  pseudo_R2 = round(c(
    0,
    1 - m1$deviance / m0$deviance,
    1 - m2$deviance / m0$deviance,
    1 - m3$deviance / m0$deviance,
    1 - m4$deviance / m0$deviance
  ), 6)
)

cat("Summary Table:\n")
print(comparison_table, n = Inf, width = Inf)

# Deviance reductions
cat("\nDeviance Reduction (vs null):\n")
for (i in 2:5) {
  dev_red <- comparison_table$deviance[1] - comparison_table$deviance[i]
  pct_red <- 100 * dev_red / comparison_table$deviance[1]
  cat(sprintf("  %s: %.2f reduction (%.2f%% of null deviance)\n",
              comparison_table$model[i], dev_red, pct_red))
}

# Incremental deviance reductions
cat("\nIncremental Deviance Reduction (each step):\n")
for (i in 2:5) {
  dev_inc <- comparison_table$deviance[i - 1] - comparison_table$deviance[i]
  cat(sprintf("  %s -> %s: %.2f\n",
              comparison_table$model[i - 1],
              comparison_table$model[i],
              dev_inc))
}

# =============================================================================
# Save Initial Results
# =============================================================================
cat("\n")
cat(paste(rep("=", 62), collapse = ""), "\n")
cat("SAVING RESULTS\n")
cat(paste(rep("=", 62), collapse = ""), "\n\n")

# Full comparison table
write_csv(comparison_table, file.path(output_dir, "model_comparison_summary.csv"))
cat("Saved: model_comparison_summary.csv\n")

# Wald test results
rtt_results_flat <- rtt_results %>%
  dplyr::mutate(dplyr::across(dplyr::where(is.matrix), ~as.numeric(.)),
         dplyr::across(dplyr::where(~is.list(.) || !is.atomic(.)), ~as.numeric(.)))
write_csv(rtt_results_flat, file.path(output_dir, "model_comparison_wald_tests.csv"))
cat("Saved: model_comparison_wald_tests.csv\n")

# Full model coefficients (Model 3) with ORs
m3_summary <- summary(m3)
m3_ci <- confint(m3)

m3_coefs <- tibble(
  term       = names(coef(m3)),
  log_odds   = round(coef(m3), 4),
  SE         = round(SE(m3), 4),
  OR         = round(exp(coef(m3)), 4),
  OR_lower95 = round(exp(m3_ci[, 1]), 4),
  OR_upper95 = round(exp(m3_ci[, 2]), 4),
  p_value    = round(m3_summary$coefficients[, "Pr(>|t|)"], 6)
)

write_csv(m3_coefs, file.path(output_dir, "model_comparison_full_coefs.csv"))
cat("Saved: model_comparison_full_coefs.csv\n")

interaction_cmp <- tibble(
  model_base = "Model 3",
  model_interaction = "Model 4",
  delta_deviance = m3$deviance - m4$deviance,
  delta_aic = aic_m4[2] - aic_m3[2],
  interaction_test_F = as.numeric(regTermTest(m4, ~ cannabis_any_use:gender, method = "Wald")$Ftest),
  interaction_df_num = as.numeric(regTermTest(m4, ~ cannabis_any_use:gender, method = "Wald")$df),
  interaction_df_den = as.numeric(regTermTest(m4, ~ cannabis_any_use:gender, method = "Wald")$ddf),
  interaction_p_value = as.numeric(regTermTest(m4, ~ cannabis_any_use:gender, method = "Wald")$p)
)
write_csv(interaction_cmp, file.path(output_dir, "model_comparison_interaction.csv"))
cat("Saved: model_comparison_interaction.csv\n")

cat(sprintf("\nAnalysis sample: %d complete cases of %d total (%.1f%%)\n",
            nrow(pumf_cc), nrow(pumf), 100 * nrow(pumf_cc) / nrow(pumf)))

cat("\n=== MODEL COMPARISON COMPLETE ===\n")
