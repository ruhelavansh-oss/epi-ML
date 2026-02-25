#!/usr/bin/env Rscript
# =============================================================================
# 06_logistic.R — Phase 6: Survey-Weighted Logistic Regression
# =============================================================================
# Full logistic model with odds ratios, confidence intervals, and diagnostics.
# Outcome: heavy_drinking_30d (binary)
# Predictors: age_group, gender, province_region, mental_health, cannabis_any_use
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
cat("=== PHASE 6: SURVEY-WEIGHTED LOGISTIC REGRESSION ===\n\n")

# --- Load wrangled data ---
pumf <- readRDS(file.path(wrangled_dir, "cpads_pumf_wrangled.rds"))
cat("Loaded CPADS PUMF:", nrow(pumf), "observations\n\n")

# --- Prepare complete-case subset ---
model_vars <- c("heavy_drinking_30d", "age_group", "gender", "province_region",
                 "mental_health", "cannabis_any_use", "wtpumf")

pumf_cc <- pumf %>%
  dplyr::select(dplyr::all_of(model_vars)) %>%
  dplyr::filter(complete.cases(.))

cat("Complete cases for logistic model:", nrow(pumf_cc), "of", nrow(pumf),
    sprintf("(%.1f%%)\n", 100 * nrow(pumf_cc) / nrow(pumf)))

# --- Outcome distribution ---
cat("\nOutcome distribution (heavy_drinking_30d) in analysis sample:\n")
tab_binge <- table(pumf_cc$heavy_drinking_30d, useNA = "ifany")
cat(sprintf("  0 (No heavy drinking):  %d (%.1f%%)\n",
            tab_binge["0"], 100 * tab_binge["0"] / sum(tab_binge)))
cat(sprintf("  1 (Heavy drinking):     %d (%.1f%%)\n",
            tab_binge["1"], 100 * tab_binge["1"] / sum(tab_binge)))

# --- Predictor distributions ---
cat("\nPredictor distributions:\n")
for (v in c("age_group", "gender", "province_region", "mental_health", "cannabis_any_use")) {
  cat(sprintf("\n  %s:\n", v))
  if (is.factor(pumf_cc[[v]])) {
    tab <- table(pumf_cc[[v]])
    pct <- round(100 * prop.table(tab), 1)
    for (i in seq_along(tab)) {
      cat(sprintf("    %s: %d (%.1f%%)\n", names(tab)[i], tab[i], pct[i]))
    }
  } else {
    cat(sprintf("    0: %d (%.1f%%), 1: %d (%.1f%%)\n",
                sum(pumf_cc[[v]] == 0), 100 * mean(pumf_cc[[v]] == 0),
                sum(pumf_cc[[v]] == 1), 100 * mean(pumf_cc[[v]] == 1)))
  }
}

# =============================================================================
# Survey-Weighted Logistic Regression
# =============================================================================
cat("\n")
cat(paste(rep("=", 62), collapse = ""), "\n")
cat("SURVEY-WEIGHTED LOGISTIC REGRESSION\n")
cat(paste(rep("=", 62), collapse = ""), "\n\n")

cat("Model: svyglm(heavy_drinking_30d ~ age_group + gender + province_region\n")
cat("                              + mental_health + cannabis_any_use,\n")
cat("              family = quasibinomial(), design = svy_design)\n\n")

# Create survey design
svy_design <- svydesign(ids = ~1, weights = ~wtpumf, data = pumf_cc)

# Fit model
logit_model <- svyglm(
  heavy_drinking_30d ~ age_group + gender + province_region + mental_health + cannabis_any_use,
  design = svy_design,
  family = quasibinomial()
)

# Interaction model (official-doc alignment: primary interaction inference)
logit_model_int <- svyglm(
  heavy_drinking_30d ~ age_group + gender + province_region + mental_health +
    cannabis_any_use + cannabis_any_use:gender,
  design = svy_design,
  family = quasibinomial()
)

# =============================================================================
# Model Summary
# =============================================================================
cat(paste(rep("-", 62), collapse = ""), "\n")
cat("MODEL SUMMARY\n")
cat(paste(rep("-", 62), collapse = ""), "\n\n")

model_summary <- summary(logit_model)
print(model_summary)

# =============================================================================
# Odds Ratios with 95% Confidence Intervals
# =============================================================================
cat("\n")
cat(paste(rep("=", 62), collapse = ""), "\n")
cat("ODDS RATIOS WITH 95% CONFIDENCE INTERVALS\n")
cat(paste(rep("=", 62), collapse = ""), "\n\n")

# Extract coefficients (log-odds)
coefs <- coef(logit_model)
se_vals <- SE(logit_model)

# Confidence intervals on log-odds scale
ci_logit <- confint(logit_model)

# Odds ratios
or_vals <- exp(coefs)
or_lower <- exp(ci_logit[, 1])
or_upper <- exp(ci_logit[, 2])

# Build OR table
or_table <- tibble(
  term       = names(coefs),
  log_odds   = round(coefs, 4),
  SE         = round(se_vals, 4),
  OR         = round(or_vals, 4),
  OR_lower95 = round(or_lower, 4),
  OR_upper95 = round(or_upper, 4),
  p_value    = round(model_summary$coefficients[, "Pr(>|t|)"], 6),
  significant = ifelse(model_summary$coefficients[, "Pr(>|t|)"] < 0.05, "*", "")
)

cat("Odds Ratio Table:\n")
print(or_table, n = Inf, width = Inf)

# --- Interpretation ---
cat("\n")
cat(paste(rep("-", 62), collapse = ""), "\n")
cat("INTERPRETATION OF KEY ODDS RATIOS\n")
cat(paste(rep("-", 62), collapse = ""), "\n\n")

# Reference categories
cat("Reference categories:\n")
cat("  age_group:       16-19\n")
cat("  gender:          Woman\n")
cat("  province_region: Atlantic\n")
cat("  mental_health:   Excellent\n")
cat("  cannabis_any_use: 0 (non-user)\n\n")

# Print notable ORs (excluding intercept)
or_no_int <- or_table %>% dplyr::filter(term != "(Intercept)")

for (i in seq_len(nrow(or_no_int))) {
  row <- or_no_int[i, ]
  direction <- if (row$OR > 1) "higher" else "lower"
  cat(sprintf("  %s: OR = %.3f (95%% CI: %.3f-%.3f) %s\n",
              row$term, row$OR, row$OR_lower95, row$OR_upper95,
              if (row$significant == "*")
                paste0("-> Significantly ", direction, " odds of heavy drinking")
              else
                "-> Not statistically significant"))
}

# --- Cannabis use effect ---
cat("\n--- Cannabis Use Effect ---\n")
can_row <- or_table %>% dplyr::filter(term == "cannabis_any_use")
if (nrow(can_row) == 1) {
  cat(sprintf("Cannabis users vs non-users: OR = %.3f (95%% CI: %.3f-%.3f)\n",
              can_row$OR, can_row$OR_lower95, can_row$OR_upper95))
  if (can_row$OR > 1 && can_row$significant == "*") {
    cat("Cannabis users have significantly higher odds of heavy drinking,\n")
    cat("adjusting for age, gender, region, and mental health.\n")
  } else if (can_row$OR < 1 && can_row$significant == "*") {
    cat("Cannabis users have significantly lower odds of heavy drinking,\n")
    cat("adjusting for age, gender, region, and mental health.\n")
  } else {
    cat("The association is not statistically significant at alpha = 0.05.\n")
  }
}

# --- Model fit ---
cat("\n--- Model Fit Statistics ---\n")
cat(sprintf("Null deviance:     %.2f on %d df\n",
            logit_model$null.deviance, logit_model$df.null))
cat(sprintf("Residual deviance: %.2f on %d df\n",
            logit_model$deviance, logit_model$df.residual))
cat(sprintf("Deviance reduction: %.2f (%.2f%%)\n",
            logit_model$null.deviance - logit_model$deviance,
            100 * (logit_model$null.deviance - logit_model$deviance) /
              logit_model$null.deviance))

# Pseudo R-squared (McFadden approximation)
pseudo_r2 <- 1 - (logit_model$deviance / logit_model$null.deviance)
cat(sprintf("Pseudo R-squared (McFadden): %.4f\n", pseudo_r2))

# =============================================================================
# Save Initial Results
# =============================================================================
cat("\n")
cat(paste(rep("=", 62), collapse = ""), "\n")
cat("SAVING RESULTS\n")
cat(paste(rep("=", 62), collapse = ""), "\n\n")

write_csv(or_table, file.path(output_dir, "logistic_odds_ratios.csv"))
cat("Saved: logistic_odds_ratios.csv\n")

int_summary <- summary(logit_model_int)
int_ci <- confint(logit_model_int)
int_or <- tibble(
  model = "svy_interaction_cannabis_by_gender",
  term = names(coef(logit_model_int)),
  log_odds = as.numeric(coef(logit_model_int)),
  se = as.numeric(SE(logit_model_int)),
  or = exp(log_odds),
  or_lower95 = exp(int_ci[, 1]),
  or_upper95 = exp(int_ci[, 2]),
  p_value = as.numeric(int_summary$coefficients[, "Pr(>|t|)"]),
  significant = ifelse(p_value < 0.05, "*", "")
)
write_csv(int_or, file.path(output_dir, "logistic_interaction_odds_ratios.csv"))
cat("Saved: logistic_interaction_odds_ratios.csv\n")

int_test <- regTermTest(logit_model_int, ~ cannabis_any_use:gender, method = "Wald")
int_test_tbl <- tibble(
  test = "cannabis_any_use:gender (joint Wald)",
  F_stat = as.numeric(int_test$Ftest),
  df_num = as.numeric(int_test$df),
  df_den = as.numeric(int_test$ddf),
  p_value = as.numeric(int_test$p),
  analysis_mode = "observational",
  model = "heavy_drinking_interaction"
)
write_csv(int_test_tbl, file.path(output_dir, "logistic_interaction_tests.csv"))
cat("Saved: logistic_interaction_tests.csv\n")

cat(sprintf("\nAnalysis sample: %d complete cases of %d total (%.1f%%)\n",
            nrow(pumf_cc), nrow(pumf), 100 * nrow(pumf_cc) / nrow(pumf)))

# =============================================================================
# SMOTE Sensitivity Analysis for Class Imbalance
# =============================================================================
cat("\n")
cat(paste(rep("=", 62), collapse = ""), "\n")
cat("SMOTE SENSITIVITY ANALYSIS (CLASS IMBALANCE)\n")
cat(paste(rep("=", 62), collapse = ""), "\n\n")

class_pct_1 <- 100 * mean(pumf_cc$heavy_drinking_30d == 1)
class_pct_0 <- 100 * mean(pumf_cc$heavy_drinking_30d == 0)
cat(sprintf("Class distribution: heavy=1: %.1f%%, heavy=0: %.1f%%\n",
            class_pct_1, class_pct_0))
minority_class <- if (class_pct_1 <= class_pct_0) 1L else 0L
cat(sprintf("Minority class detected: %d\n\n", minority_class))

strip_backticks <- function(x) gsub("`", "", x, fixed = TRUE)

extract_glm_or <- function(model_obj, model_name) {
  sm <- summary(model_obj)$coefficients
  ci <- confint.default(model_obj)
  tibble(
    model = model_name,
    term = strip_backticks(rownames(sm)),
    OR_smote = exp(as.numeric(sm[, 1])),
    OR_lower95 = exp(ci[, 1]),
    OR_upper95 = exp(ci[, 2]),
    p_value = as.numeric(sm[, "Pr(>|z|)"])
  )
}

build_smote_matrix <- function(data) {
  x_mm <- model.matrix(
    ~ age_group + gender + province_region + mental_health + cannabis_any_use,
    data = data
  )[, -1, drop = FALSE]
  x_df <- as.data.frame(x_mm, check.names = FALSE)
  y <- factor(
    ifelse(data$heavy_drinking_30d == 1, "Yes", "No"),
    levels = c("No", "Yes")
  )
  list(x = x_df, y = y)
}

smote_status <- tibble(
  method = NA_character_,
  package_available = requireNamespace("smotefamily", quietly = TRUE),
  imbalance_ratio = NA_real_,
  run_completed = FALSE,
  warning_count = 0L,
  note = NA_character_
)

smote_or_table <- tibble()
comparison <- tibble()

tryCatch({
  design_mat <- build_smote_matrix(pumf_cc)
  base_data <- design_mat$x %>% mutate(heavy_drinking_30d = design_mat$y)

  class_counts <- table(base_data$heavy_drinking_30d)
  minority_label <- names(which.min(class_counts))
  majority_n <- as.integer(max(class_counts))
  minority_n <- as.integer(min(class_counts))
  imbalance_ratio <- majority_n / minority_n
  smote_status$imbalance_ratio <- imbalance_ratio

  cat(sprintf("Class imbalance ratio (majority/minority): %.3f\n", imbalance_ratio))

  if (imbalance_ratio < 1.25) {
    smote_status$method <- "skipped_near_balanced"
    smote_status$note <- "Outcome is near-balanced (<1.25 ratio); SMOTE skipped by design."
    cat("Outcome is near-balanced; skipping SMOTE and fitting unweighted baseline sensitivity model.\n")
    smote_data <- base_data
  } else if (smote_status$package_available) {
    smote_status$method <- "smotefamily"
    cat("Using smotefamily::SMOTE on one-hot design matrix...\n")
    smote_warnings <- character()
    smote_result <- withCallingHandlers(
      smotefamily::SMOTE(
        X = design_mat$x,
        target = design_mat$y,
        K = 5,
        dup_size = 0
      ),
      warning = function(w) {
        smote_warnings <<- c(smote_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    smote_status$warning_count <- length(smote_warnings)

    if (length(smote_warnings) > 0) {
      cat("smotefamily emitted warnings; falling back to deterministic random oversampling.\n")
      smote_status$method <- "random_oversampling_fallback"
      smote_status$note <- paste0(
        "smotefamily emitted ", length(smote_warnings),
        " warning(s); fallback oversampling used."
      )
      minority_rows <- base_data %>% filter(heavy_drinking_30d == minority_label)
      n_to_add <- majority_n - nrow(minority_rows)
      if (n_to_add > 0) {
        add_rows <- minority_rows[sample(nrow(minority_rows), n_to_add, replace = TRUE), ]
        smote_data <- bind_rows(base_data, add_rows)
      } else {
        smote_data <- base_data
      }
    } else {
      smote_data <- as.data.frame(smote_result$data, check.names = FALSE)
      names(smote_data) <- c(names(design_mat$x), "heavy_drinking_30d")
      smote_data$heavy_drinking_30d <- factor(smote_data$heavy_drinking_30d, levels = levels(design_mat$y))
      smote_status$note <- "smotefamily completed without warnings."
    }
  } else {
    smote_status$method <- "random_oversampling"
    smote_status$note <- "smotefamily unavailable; deterministic random oversampling used."
    cat("smotefamily not available. Using random oversampling of minority class...\n")
    minority_rows <- base_data %>% filter(heavy_drinking_30d == minority_label)
    n_to_add <- majority_n - nrow(minority_rows)
    if (n_to_add > 0) {
      add_rows <- minority_rows[sample(nrow(minority_rows), n_to_add, replace = TRUE), ]
      smote_data <- bind_rows(base_data, add_rows)
    } else {
      smote_data <- base_data
    }
  }

  smote_status$run_completed <- TRUE

  cat(sprintf("After sensitivity resampling: n=%d, heavy=Yes: %.1f%%, heavy=No: %.1f%%\n",
              nrow(smote_data),
              100 * mean(smote_data$heavy_drinking_30d == "Yes"),
              100 * mean(smote_data$heavy_drinking_30d == "No")))

  smote_model <- glm(heavy_drinking_30d ~ ., data = smote_data, family = binomial())
  smote_or_table <- extract_glm_or(smote_model, smote_status$method) %>%
    mutate(
      OR_smote = round(OR_smote, 4),
      OR_lower95 = round(OR_lower95, 4),
      OR_upper95 = round(OR_upper95, 4),
      p_value = round(p_value, 6)
    )

  cat("\nSMOTE/Oversampled Logistic Regression — Odds Ratios:\n")
  print(smote_or_table, n = Inf, width = Inf)

  # Compare with original weighted model ORs using normalized term labels
  cat("\n--- Comparison: Original vs SMOTE-adjusted ORs ---\n")
  comparison <- or_table %>%
    mutate(term_key = strip_backticks(term)) %>%
    select(term, term_key, OR_original = OR) %>%
    left_join(
      smote_or_table %>% mutate(term_key = strip_backticks(term)) %>%
        select(term_key, OR_smote),
      by = "term_key"
    ) %>%
    mutate(pct_change = round(100 * (OR_smote - OR_original) / OR_original, 1)) %>%
    select(term, OR_original, OR_smote, pct_change)

  print(comparison, n = Inf, width = Inf)

  write_csv(smote_or_table, file.path(output_dir, "logistic_smote_odds_ratios.csv"))
  write_csv(smote_status, file.path(output_dir, "logistic_smote_status.csv"))
  cat("\nSaved: logistic_smote_odds_ratios.csv\n")
  cat("Saved: logistic_smote_status.csv\n")

}, error = function(e) {
  cat("SMOTE analysis error:", e$message, "\n")
  smote_status$method <- ifelse(is.na(smote_status$method), "failed", smote_status$method)
  smote_status$note <- paste("SMOTE sensitivity failed:", e$message)
  write_csv(smote_status, file.path(output_dir, "logistic_smote_status.csv"))
  cat("Saved: logistic_smote_status.csv\n")
  cat("Proceeding without SMOTE sensitivity analysis.\n")
})

cat("\n=== LOGISTIC REGRESSION ANALYSIS COMPLETE ===\n")
