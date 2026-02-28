#!/usr/bin/env Rscript
# =============================================================================
# 07_ebac.R — Phase 7b: eBAC Analysis Using dataset-Measured Variables
# =============================================================================
# Purpose:
#   1) Use provided eBAC variables (ebac_tot, ebac_legal) directly.
#   2) Do NOT recompute eBAC from anthropometrics (demq3/demq4 are absent in dataset).
#   3) Run survey-weighted descriptive + regression analyses.
#   4) Run critical QA checks so code can be syntactically correct but still
#      flagged if design/procedure assumptions are weak.
#
# Input:
#   PROJECT_ROOT/data/private/outputs/wrangled/data_wrangled.rds
#
# Outputs (new files; no old script is modified):
#   PROJECT_ROOT/data/private/outputs/ebac_data_quality_checks.csv
#   PROJECT_ROOT/data/private/outputs/ebac_model_samples.csv
#   PROJECT_ROOT/data/private/outputs/ebac_weighted_summaries.csv
#   PROJECT_ROOT/data/private/outputs/ebac_distribution_unweighted.csv
#   PROJECT_ROOT/data/private/outputs/ebac_logistic_or_primary.csv
#   PROJECT_ROOT/data/private/outputs/ebac_logistic_or_sensitivity_with_heavy.csv
#   PROJECT_ROOT/data/private/outputs/ebac_linear_coefficients_primary.csv
#   PROJECT_ROOT/data/private/outputs/ebac_linear_coefficients_sensitivity_with_heavy.csv
#   PROJECT_ROOT/data/private/outputs/ebac_missingness_weighted.csv
#   PROJECT_ROOT/data/private/outputs/ebac_missingness_or.csv
#   PROJECT_ROOT/data/private/outputs/ebac_missingness_or_eligible_drinkers.csv
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
  library(survey)
})

options(scipen = 999)
set.seed(42)
source("surveillance/lib/config_paths.R")
paths <- get_paths()
init_paths(paths)

# ---- Runtime paths ----
output_dir <- paths$output_private_dir
wrangled_dir <- paths$wrangled_dir
cat("=== PHASE 7b: eBAC ANALYSIS (dataset-MEASURED VARIABLES) ===\n\n")

# ---- Load data ----
df_path <- file.path(wrangled_dir, "data_wrangled.rds")
if (!file.exists(df_path)) {
  stop("Missing file: ", df_path)
}
df <- readRDS(df_path)

# ---- Required variables ----
required <- c(
  "weight",
  "ebac_tot",
  "ebac_legal",
  "alc13_lrdg",
  "cannabis_any_use",
  "heavy_drinking_30d",
  "alcohol_past12m",
  "age_group",
  "gender",
  "province_region",
  "mental_health",
  "physical_health"
)
missing <- setdiff(required, names(df))
if (length(missing) > 0) {
  stop("Missing required variables in data_wrangled.rds: ",
       paste(missing, collapse = ", "))
}

# ---- Helpers ----
safe_confint <- function(model_obj) {
  ci <- tryCatch(confint(model_obj), error = function(e) NULL)
  if (is.null(ci)) {
    b <- coef(model_obj)
    se <- SE(model_obj)
    ci <- cbind(b - 1.96 * se, b + 1.96 * se)
    rownames(ci) <- names(b)
  }
  ci
}

extract_logistic_or <- function(model_obj, model_name) {
  sm <- summary(model_obj)$coefficients
  ci <- safe_confint(model_obj)
  tibble(
    model = model_name,
    term = rownames(sm),
    log_odds = as.numeric(sm[, 1]),
    se = as.numeric(sm[, 2]),
    or = exp(log_odds),
    or_lower95 = exp(ci[, 1]),
    or_upper95 = exp(ci[, 2]),
    p_value = as.numeric(sm[, ncol(sm)]),
    significant = ifelse(p_value < 0.05, "*", "")
  )
}

extract_linear <- function(model_obj, model_name) {
  sm <- summary(model_obj)$coefficients
  ci <- safe_confint(model_obj)
  tibble(
    model = model_name,
    term = rownames(sm),
    estimate = as.numeric(sm[, 1]),
    se = as.numeric(sm[, 2]),
    ci_lower95 = ci[, 1],
    ci_upper95 = ci[, 2],
    p_value = as.numeric(sm[, ncol(sm)]),
    significant = ifelse(p_value < 0.05, "*", "")
  )
}

weighted_mean_overall <- function(data, outcome, metric_name) {
  d <- data %>% filter(!is.na(.data[[outcome]]), !is.na(weight))
  if (nrow(d) == 0) {
    return(tibble())
  }
  des <- svydesign(ids = ~1, weights = ~weight, data = d)
  est <- svymean(as.formula(paste0("~", outcome)), des, na.rm = TRUE)
  ci <- confint(est)
  tibble(
    metric = metric_name,
    group_var = "Overall",
    group_level = "All",
    estimate = as.numeric(coef(est)[1]),
    se = as.numeric(SE(est)[1]),
    ci_lower95 = as.numeric(ci[1, 1]),
    ci_upper95 = as.numeric(ci[1, 2]),
    n_unweighted_nonmissing = nrow(d)
  )
}

weighted_mean_by_group <- function(data, outcome, group_var, metric_name) {
  d <- data %>%
    filter(!is.na(.data[[outcome]]), !is.na(.data[[group_var]]), !is.na(weight))

  if (nrow(d) == 0) {
    return(tibble())
  }

  des <- svydesign(ids = ~1, weights = ~weight, data = d)
  by_obj <- svyby(
    as.formula(paste0("~", outcome)),
    as.formula(paste0("~", group_var)),
    des,
    svymean,
    na.rm = TRUE,
    vartype = c("se", "ci")
  )

  by_df <- as.data.frame(by_obj)
  counts_df <- d %>%
    count(.data[[group_var]], name = "n_unweighted_nonmissing") %>%
    mutate(group_level = as.character(.data[[group_var]])) %>%
    select(group_level, n_unweighted_nonmissing)

  out <- tibble(
    metric = metric_name,
    group_var = group_var,
    group_level = as.character(by_df[[group_var]]),
    estimate = as.numeric(by_df[[outcome]]),
    se = as.numeric(by_df$se),
    ci_lower95 = as.numeric(by_df$ci_l),
    ci_upper95 = as.numeric(by_df$ci_u)
  ) %>%
    left_join(counts_df, by = "group_level")

  out
}

# =============================================================================
# 1) Critical QA checks
# =============================================================================
cat("--- 1) Data quality and design QA checks ---\n")

qa <- tibble(
  check_name = character(),
  value = character(),
  pass = logical(),
  note = character()
)

add_qa <- function(name, value, pass, note) {
  qa <<- bind_rows(
    qa,
    tibble(
      check_name = name,
      value = as.character(value),
      pass = as.logical(pass),
      note = as.character(note)
    )
  )
}

# Check A: anthropometric variables absent in dataset (expected limitation)
has_demq3 <- "demq3" %in% names(df)
has_demq4 <- "demq4" %in% names(df)
add_qa(
  "anthropometric_vars_present",
  paste0("demq3=", has_demq3, "; demq4=", has_demq4),
  !(has_demq3 || has_demq4),
  "Expected in public dataset: unavailable; do not recompute eBAC from Widmark inputs."
)

# Check B: ebac_tot numeric and plausible range
ebac_nonmiss <- df$ebac_tot[!is.na(df$ebac_tot)]
range_ok <- all(ebac_nonmiss >= 0 & ebac_nonmiss <= 0.8)
add_qa(
  "ebac_tot_range_plausible",
  sprintf("[%.3f, %.3f]", min(ebac_nonmiss), max(ebac_nonmiss)),
  range_ok,
  "Expected eBAC totals in plausible human range; out-of-range values indicate data coding issues."
)

# Check C: ebac_legal consistency with ebac_tot threshold rule
# IMPORTANT: ebac_tot appears rounded (e.g., two decimals), so exact 0.08 values
# can be boundary-ambiguous if ebac_legal was defined from higher-precision values.
rule_df <- df %>% filter(!is.na(ebac_tot), !is.na(ebac_legal))
rule_expected <- as.integer(rule_df$ebac_tot > 0.08)
rule_agreement <- mean(rule_expected == rule_df$ebac_legal)
rule_mismatch <- sum(rule_expected != rule_df$ebac_legal)
rule_boundary_n <- sum(rule_df$ebac_tot == 0.08)
rule_off_boundary_df <- rule_df %>% filter(ebac_tot != 0.08)
rule_off_boundary_agreement <- mean(
  as.integer(rule_off_boundary_df$ebac_tot > 0.08) == rule_off_boundary_df$ebac_legal
)
add_qa(
  "ebac_legal_consistency_with_threshold_strict",
  sprintf(
    "agreement=%.5f; mismatches=%d; exact_0.08_n=%d",
    rule_agreement, rule_mismatch, rule_boundary_n
  ),
  rule_agreement >= 0.98,
  "Strict check may disagree at exactly 0.08 due to rounding/precision boundary."
)
add_qa(
  "ebac_legal_consistency_with_threshold_off_boundary",
  sprintf("agreement=%.5f; n=%d", rule_off_boundary_agreement, nrow(rule_off_boundary_df)),
  rule_off_boundary_agreement >= 0.999,
  "Primary correctness check excluding exact 0.08 boundary values."
)

# Check D: ebac_legal concordance with alc13_lrdg (related but not guaranteed identical)
lrdg_df <- df %>% filter(!is.na(ebac_legal), !is.na(alc13_lrdg))
lrdg_agreement <- mean(lrdg_df$ebac_legal == lrdg_df$alc13_lrdg)
lrdg_mismatch <- sum(lrdg_df$ebac_legal != lrdg_df$alc13_lrdg)
add_qa(
  "ebac_legal_vs_alc13_lrdg_concordance",
  sprintf("agreement=%.5f; mismatches=%d", lrdg_agreement, lrdg_mismatch),
  lrdg_agreement >= 0.95,
  "Concordance check only; these indicators can differ by internal derivation rules."
)

# Check E: missingness burden
miss_rate <- mean(is.na(df$ebac_tot))
add_qa(
  "ebac_tot_missingness_rate",
  sprintf("%.4f", miss_rate),
  miss_rate <= 0.40,
  "If high, eBAC analyses are a selected-domain analysis, not full-cohort inference."
)

# Check E2: structural missingness by alcohol eligibility
miss_by_alc <- df %>%
  filter(!is.na(alcohol_past12m)) %>%
  group_by(alcohol_past12m) %>%
  summarise(miss_rate = mean(is.na(ebac_tot)), n = n(), .groups = "drop")

miss_rate_no_alc <- miss_by_alc$miss_rate[miss_by_alc$alcohol_past12m == 0]
miss_rate_yes_alc <- miss_by_alc$miss_rate[miss_by_alc$alcohol_past12m == 1]
add_qa(
  "structural_missingness_check_alcohol_past12m",
  sprintf(
    "P(miss|alc12m=0)=%.4f; P(miss|alc12m=1)=%.4f",
    miss_rate_no_alc, miss_rate_yes_alc
  ),
  (length(miss_rate_no_alc) == 1 && miss_rate_no_alc > 0.99),
  "Expected skip-pattern behavior: almost all non-drinkers should have missing eBAC."
)

# Check F: positivity for cannabis in eBAC observed domain
dom <- df %>% filter(!is.na(ebac_tot), !is.na(cannabis_any_use))
p_treated <- mean(dom$cannabis_any_use == 1)
p_control <- mean(dom$cannabis_any_use == 0)
add_qa(
  "cannabis_overlap_in_ebac_domain",
  sprintf("treated=%.3f; control=%.3f", p_treated, p_control),
  p_treated > 0.05 & p_control > 0.05,
  "Both exposure groups should be represented in eBAC-observed domain for stable comparisons."
)

# Check G: weights positive
pos_w <- all(df$weight[!is.na(df$weight)] > 0)
add_qa(
  "survey_weights_positive",
  as.character(pos_w),
  pos_w,
  "Non-positive weights invalidate standard weighted estimation."
)

print(qa, n = Inf, width = Inf)
write_csv(qa, file.path(output_dir, "ebac_data_quality_checks.csv"))
cat("Saved: ebac_data_quality_checks.csv\n\n")

# Sample accounting table (for reproducibility and model comparability)
sample_accounting <- tibble(
  sample_name = c(
    "Total wrangled cohort",
    "eBAC total non-missing",
    "eBAC legal non-missing",
    "Past-12m alcohol users",
    "Past-12m alcohol users with eBAC non-missing"
  ),
  n = c(
    nrow(df),
    sum(!is.na(df$ebac_tot)),
    sum(!is.na(df$ebac_legal)),
    sum(df$alcohol_past12m == 1, na.rm = TRUE),
    sum(df$alcohol_past12m == 1 & !is.na(df$ebac_tot), na.rm = TRUE)
  )
)
print(sample_accounting, n = Inf)
write_csv(sample_accounting, file.path(output_dir, "ebac_model_samples.csv"))
cat("Saved: ebac_model_samples.csv\n\n")

# =============================================================================
# 2) Weighted descriptive summaries (eBAC as measured variable)
# =============================================================================
cat("--- 2) Weighted descriptive summaries ---\n")

group_vars <- c(
  "cannabis_any_use",
  "gender",
  "age_group",
  "province_region",
  "mental_health",
  "physical_health",
  "heavy_drinking_30d"
)

# ebac_legal prevalence
ebac_legal_summary <- bind_rows(
  weighted_mean_overall(df, "ebac_legal", "ebac_legal_prevalence"),
  map_dfr(group_vars, ~ weighted_mean_by_group(
    df, "ebac_legal", .x, "ebac_legal_prevalence"
  ))
)

# ebac_tot mean
ebac_tot_summary <- bind_rows(
  weighted_mean_overall(df, "ebac_tot", "ebac_tot_mean"),
  map_dfr(group_vars, ~ weighted_mean_by_group(
    df, "ebac_tot", .x, "ebac_tot_mean"
  ))
)

weighted_summaries <- bind_rows(ebac_legal_summary, ebac_tot_summary)
print(weighted_summaries %>% arrange(metric, group_var, group_level), n = 40)
write_csv(weighted_summaries, file.path(output_dir, "ebac_weighted_summaries.csv"))
cat("Saved: ebac_weighted_summaries.csv\n\n")

# Unweighted distribution checkpoints for ebac_tot
dist_tbl <- tibble(
  n_nonmissing = sum(!is.na(df$ebac_tot)),
  mean = mean(df$ebac_tot, na.rm = TRUE),
  sd = sd(df$ebac_tot, na.rm = TRUE),
  min = min(df$ebac_tot, na.rm = TRUE),
  p25 = quantile(df$ebac_tot, 0.25, na.rm = TRUE),
  median = median(df$ebac_tot, na.rm = TRUE),
  p75 = quantile(df$ebac_tot, 0.75, na.rm = TRUE),
  p90 = quantile(df$ebac_tot, 0.90, na.rm = TRUE),
  p95 = quantile(df$ebac_tot, 0.95, na.rm = TRUE),
  max = max(df$ebac_tot, na.rm = TRUE)
)
print(dist_tbl)
write_csv(dist_tbl, file.path(output_dir, "ebac_distribution_unweighted.csv"))
cat("Saved: ebac_distribution_unweighted.csv\n\n")

# =============================================================================
# 3) Regression models (survey-weighted)
# =============================================================================
cat("--- 3) Survey-weighted regression models ---\n")

covars <- c(
  "cannabis_any_use",
  "age_group",
  "gender",
  "province_region",
  "mental_health",
  "physical_health"
)

# 3a) Primary model: ebac_legal (binary outcome)
df_bin <- df %>%
  select(ebac_legal, weight, all_of(covars)) %>%
  drop_na()
des_bin <- svydesign(ids = ~1, weights = ~weight, data = df_bin)

fit_bin <- svyglm(
  ebac_legal ~ cannabis_any_use + age_group + gender +
    province_region + mental_health + physical_health,
  design = des_bin,
  family = quasibinomial()
)

or_primary <- extract_logistic_or(fit_bin, "ebac_legal_primary")
print(or_primary, n = Inf, width = Inf)
write_csv(or_primary, file.path(output_dir, "ebac_logistic_or_primary.csv"))
cat("Saved: ebac_logistic_or_primary.csv\n")

# 3b) Sensitivity model: add heavy_drinking_30d
# NOTE: This may over-adjust because heavy_drinking_30d overlaps with alcohol intensity.
df_bin_sens <- df %>%
  select(ebac_legal, heavy_drinking_30d, weight, all_of(covars)) %>%
  drop_na()
des_bin_sens <- svydesign(ids = ~1, weights = ~weight, data = df_bin_sens)

fit_bin_sens <- svyglm(
  ebac_legal ~ cannabis_any_use + age_group + gender +
    province_region + mental_health + physical_health +
    heavy_drinking_30d,
  design = des_bin_sens,
  family = quasibinomial()
)

or_sens <- extract_logistic_or(
  fit_bin_sens,
  "ebac_legal_sensitivity_with_heavy_drinking"
)
print(or_sens, n = Inf, width = Inf)
write_csv(or_sens, file.path(output_dir, "ebac_logistic_or_sensitivity_with_heavy.csv"))
cat("Saved: ebac_logistic_or_sensitivity_with_heavy.csv\n\n")

# 3c) Primary model: ebac_tot (continuous outcome)
df_lin <- df %>%
  select(ebac_tot, weight, all_of(covars)) %>%
  drop_na()
des_lin <- svydesign(ids = ~1, weights = ~weight, data = df_lin)

fit_lin <- svyglm(
  ebac_tot ~ cannabis_any_use + age_group + gender +
    province_region + mental_health + physical_health,
  design = des_lin
)

lin_primary <- extract_linear(fit_lin, "ebac_tot_primary")
print(lin_primary, n = Inf, width = Inf)
write_csv(lin_primary, file.path(output_dir, "ebac_linear_coefficients_primary.csv"))
cat("Saved: ebac_linear_coefficients_primary.csv\n")

# 3d) Sensitivity linear model: add heavy_drinking_30d
df_lin_sens <- df %>%
  select(ebac_tot, heavy_drinking_30d, weight, all_of(covars)) %>%
  drop_na()
des_lin_sens <- svydesign(ids = ~1, weights = ~weight, data = df_lin_sens)

fit_lin_sens <- svyglm(
  ebac_tot ~ cannabis_any_use + age_group + gender +
    province_region + mental_health + physical_health +
    heavy_drinking_30d,
  design = des_lin_sens
)

lin_sens <- extract_linear(
  fit_lin_sens,
  "ebac_tot_sensitivity_with_heavy_drinking"
)
print(lin_sens, n = Inf, width = Inf)
write_csv(
  lin_sens,
  file.path(output_dir, "ebac_linear_coefficients_sensitivity_with_heavy.csv")
)
cat("Saved: ebac_linear_coefficients_sensitivity_with_heavy.csv\n\n")

# =============================================================================
# 4) Missingness diagnostics (critical design check)
# =============================================================================
cat("--- 4) Missingness diagnostics for eBAC domain selection ---\n")

df_miss <- df %>%
  mutate(ebac_missing = as.integer(is.na(ebac_tot)))

miss_summary <- bind_rows(
  weighted_mean_overall(df_miss, "ebac_missing", "ebac_missing_rate"),
  weighted_mean_by_group(df_miss, "ebac_missing", "alcohol_past12m", "ebac_missing_rate"),
  weighted_mean_by_group(df_miss, "ebac_missing", "cannabis_any_use", "ebac_missing_rate"),
  weighted_mean_by_group(df_miss, "ebac_missing", "heavy_drinking_30d", "ebac_missing_rate")
)
print(miss_summary, n = Inf, width = Inf)
write_csv(miss_summary, file.path(output_dir, "ebac_missingness_weighted.csv"))
cat("Saved: ebac_missingness_weighted.csv\n")

df_miss_model <- df_miss %>%
  select(
    ebac_missing, weight, alcohol_past12m, cannabis_any_use,
    age_group, gender, province_region, mental_health, physical_health
  ) %>%
  drop_na()
des_miss <- svydesign(ids = ~1, weights = ~weight, data = df_miss_model)

fit_miss <- svyglm(
  ebac_missing ~ alcohol_past12m + cannabis_any_use + age_group + gender +
    province_region + mental_health + physical_health,
  design = des_miss,
  family = quasibinomial()
)

miss_or <- extract_logistic_or(fit_miss, "ebac_missingness_model")
print(miss_or, n = Inf, width = Inf)
write_csv(miss_or, file.path(output_dir, "ebac_missingness_or.csv"))
cat("Saved: ebac_missingness_or.csv\n\n")

# Missingness among eligible drinkers only (removes structural skip-pattern effect)
eligible <- df_miss %>%
  filter(alcohol_past12m == 1) %>%
  select(
    ebac_missing, weight, cannabis_any_use,
    age_group, gender, province_region, mental_health, physical_health
  ) %>%
  drop_na()
des_miss_eligible <- svydesign(ids = ~1, weights = ~weight, data = eligible)
fit_miss_eligible <- svyglm(
  ebac_missing ~ cannabis_any_use + age_group + gender +
    province_region + mental_health + physical_health,
  design = des_miss_eligible,
  family = quasibinomial()
)
miss_or_eligible <- extract_logistic_or(
  fit_miss_eligible,
  "ebac_missingness_model_eligible_drinkers"
)
print(miss_or_eligible, n = Inf, width = Inf)
write_csv(
  miss_or_eligible,
  file.path(output_dir, "ebac_missingness_or_eligible_drinkers.csv")
)
cat("Saved: ebac_missingness_or_eligible_drinkers.csv\n\n")

# =============================================================================
# 5) Console interpretation checkpoints
# =============================================================================
cat("--- 5) Interpretation checkpoints ---\n")

overall_legal <- ebac_legal_summary %>%
  filter(group_var == "Overall", group_level == "All") %>%
  pull(estimate)
overall_tot <- ebac_tot_summary %>%
  filter(group_var == "Overall", group_level == "All") %>%
  pull(estimate)

can_or <- or_primary %>%
  filter(term == "cannabis_any_use") %>%
  slice(1)

cat(sprintf(
  "Weighted prevalence of eBAC > 0.08: %.4f\n",
  overall_legal
))
cat(sprintf(
  "Weighted mean eBAC (g/dL): %.4f\n",
  overall_tot
))
if (nrow(can_or) == 1) {
  cat(sprintf(
    "Adjusted OR (cannabis -> eBAC_legal): %.3f [%.3f, %.3f], p=%.3g\n",
    can_or$or, can_or$or_lower95, can_or$or_upper95, can_or$p_value
  ))
}

if (miss_rate > 0.40) {
  cat("WARNING: eBAC missingness is high; treat results as domain-selected estimates.\n")
}
if (rule_off_boundary_agreement < 0.999) {
  cat("WARNING: Off-boundary mismatch detected between ebac_legal and ebac_tot threshold.\n")
} else if (rule_agreement < 1) {
  cat("NOTE: Strict mismatch occurs only at ebac_tot = 0.08 boundary (rounding/precision issue).\n")
}
if (!qa$pass[qa$check_name == "cannabis_overlap_in_ebac_domain"]) {
  cat("WARNING: exposure overlap in eBAC domain is weak.\n")
}

cat("\n=== eBAC ANALYSIS COMPLETE ===\n")
