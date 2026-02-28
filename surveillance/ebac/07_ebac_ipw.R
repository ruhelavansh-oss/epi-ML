#!/usr/bin/env Rscript
# =============================================================================
# 07_ebac_ipw.R — Phase 7c: Selection-Adjusted eBAC Analysis (IPW)
# =============================================================================
# Purpose:
#   Address selection into observed eBAC values using inverse-probability-of-
#   observation weighting (IPW) among eligible drinkers.
#
# Why:
#   In data dataset, eBAC is missing by design for many non-drinkers and still
#   partially missing among drinkers. Standard complete-case eBAC models estimate
#   associations in the observed domain. IPW targets the eligible-drinker domain
#   under MAR-type assumptions conditional on modeled covariates.
#
# Inputs:
#   PROJECT_ROOT/data/private/outputs/wrangled/data_wrangled.rds
#
# Outputs:
#   PROJECT_ROOT/data/private/outputs/ebac_ipw_observation_model_or.csv
#   PROJECT_ROOT/data/private/outputs/ebac_ipw_weight_diagnostics.csv
#   PROJECT_ROOT/data/private/outputs/ebac_ipw_covariate_balance.csv
#   PROJECT_ROOT/data/private/outputs/ebac_ipw_logistic_or.csv
#   PROJECT_ROOT/data/private/outputs/ebac_ipw_linear_coefficients.csv
#   PROJECT_ROOT/data/private/outputs/ebac_ipw_cannabis_comparison.csv
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


# ---- Paths ----
output_dir <- paths$output_private_dir
wrangled_path <- file.path(paths$wrangled_dir, "data_wrangled.rds")
if (!file.exists(wrangled_path)) {
  stop("Missing wrangled data: ", wrangled_path)
}
df <- readRDS(wrangled_path)

cat("=== PHASE 7c: eBAC SELECTION-ADJUSTED ANALYSIS (IPW) ===\n\n")

# ---- Required variables ----
required <- c(
  "weight", "alcohol_past12m", "ebac_tot", "ebac_legal",
  "cannabis_any_use", "age_group", "gender",
  "province_region", "mental_health", "physical_health"
)
missing <- setdiff(required, names(df))
if (length(missing) > 0) {
  stop("Missing required variables: ", paste(missing, collapse = ", "))
}

# ---- Helper functions ----
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

ess <- function(w) (sum(w)^2) / sum(w^2)

# =============================================================================
# 1) Define target and observation process
# =============================================================================
cat("--- 1) Define target (eligible drinkers) and observation process ---\n")

# Target domain for transport: respondents eligible for eBAC computation field
# in data instrument flow (alcohol_past12m == 1).
target <- df %>%
  filter(alcohol_past12m == 1) %>%
  mutate(
    R = as.integer(!is.na(ebac_tot))
  ) %>%
  select(
    R, weight, ebac_tot, ebac_legal, cannabis_any_use,
    age_group, gender, province_region, mental_health, physical_health
  ) %>%
  tidyr::drop_na(
    weight, cannabis_any_use, age_group, gender,
    province_region, mental_health, physical_health
  )

cat(sprintf("Eligible-drinker complete-covariate sample: %d\n", nrow(target)))
cat(sprintf("Observed eBAC in eligible sample (R=1): %d (%.1f%%)\n\n",
            sum(target$R == 1), 100 * mean(target$R == 1)))

# =============================================================================
# 2) Observation model and IPW construction
# =============================================================================
cat("--- 2) Observation model and IPW construction ---\n")

obs_formula <- R ~ cannabis_any_use + age_group + gender +
  province_region + mental_health + physical_health

obs_model <- glm(obs_formula, data = target, family = binomial())
obs_or <- extract_logistic_or(obs_model, "observation_model_R_equals_1")
print(obs_or, n = Inf, width = Inf)
write_csv(obs_or, file.path(output_dir, "ebac_ipw_observation_model_or.csv"))
cat("Saved: ebac_ipw_observation_model_or.csv\n")

target <- target %>%
  mutate(
    p_hat = predict(obs_model, type = "response"),
    p_hat_clip = pmin(pmax(p_hat, 0.01), 0.99),
    p_obs = mean(R),
    sw = ifelse(R == 1, p_obs / p_hat_clip, NA_real_)
  )

# Trim IPW at 1st/99th percentile among observed only (stability sensitivity)
q01 <- quantile(target$sw[target$R == 1], 0.01, na.rm = TRUE)
q99 <- quantile(target$sw[target$R == 1], 0.99, na.rm = TRUE)
target <- target %>%
  mutate(
    sw_trim = ifelse(R == 1, pmin(pmax(sw, q01), q99), NA_real_),
    w_combined = ifelse(R == 1, weight * sw, NA_real_),
    w_combined_trim = ifelse(R == 1, weight * sw_trim, NA_real_)
  )

obs_only <- target %>% filter(R == 1)
diag_tbl <- tibble(
  metric = c(
    "eligible_n",
    "observed_n",
    "observed_rate",
    "sw_min",
    "sw_q01",
    "sw_median",
    "sw_q99",
    "sw_max",
    "sw_trim_lower",
    "sw_trim_upper",
    "ess_survey_only",
    "ess_survey_x_ipw",
    "ess_survey_x_ipw_trim"
  ),
  value = c(
    nrow(target),
    nrow(obs_only),
    mean(target$R),
    min(obs_only$sw),
    quantile(obs_only$sw, 0.01),
    median(obs_only$sw),
    quantile(obs_only$sw, 0.99),
    max(obs_only$sw),
    q01,
    q99,
    ess(obs_only$weight),
    ess(obs_only$w_combined),
    ess(obs_only$w_combined_trim)
  )
)
print(diag_tbl, n = Inf)
write_csv(diag_tbl, file.path(output_dir, "ebac_ipw_weight_diagnostics.csv"))
cat("Saved: ebac_ipw_weight_diagnostics.csv\n\n")

# =============================================================================
# 3) Covariate balance check: observed vs target eligible domain
# =============================================================================
cat("--- 3) Covariate balance diagnostics (observed vs eligible target) ---\n")

balance_vars <- c(
  "cannabis_any_use", "age_group", "gender",
  "province_region", "mental_health", "physical_health"
)

target_base <- target %>%
  mutate(weight = weight, sample = "eligible_target")
obs_unadj <- target %>%
  filter(R == 1) %>%
  mutate(weight = weight, sample = "observed_unadjusted")
obs_ipw <- target %>%
  filter(R == 1) %>%
  mutate(weight = w_combined_trim, sample = "observed_ipw_adjusted")

stacked <- bind_rows(target_base, obs_unadj, obs_ipw)

balance_tbl <- map_dfr(balance_vars, function(v) {
  df <- stacked %>%
    mutate(level = as.character(.data[[v]])) %>%
    group_by(sample, level) %>%
    summarise(
      prop = sum(weight) / sum(stacked$weight[stacked$sample == first(sample)]),
      .groups = "drop"
    )

  tgt <- df %>% filter(sample == "eligible_target") %>% select(level, prop_target = prop)
  unadj <- df %>% filter(sample == "observed_unadjusted") %>% select(level, prop_obs_unadj = prop)
  adj <- df %>% filter(sample == "observed_ipw_adjusted") %>% select(level, prop_obs_ipw = prop)

  full_join(tgt, unadj, by = "level") %>%
    full_join(adj, by = "level") %>%
    mutate(
      variable = v,
      abs_diff_unadj = abs(prop_obs_unadj - prop_target),
      abs_diff_ipw = abs(prop_obs_ipw - prop_target)
    ) %>%
    select(variable, level, prop_target, prop_obs_unadj, prop_obs_ipw, abs_diff_unadj, abs_diff_ipw)
})

print(balance_tbl, n = 60)
write_csv(balance_tbl, file.path(output_dir, "ebac_ipw_covariate_balance.csv"))
cat("Saved: ebac_ipw_covariate_balance.csv\n\n")

# =============================================================================
# 4) Outcome models: observed-domain vs IPW-adjusted
# =============================================================================
cat("--- 4) Outcome model comparison ---\n")

des_obs <- svydesign(ids = ~1, weights = ~weight, data = obs_only)
des_ipw <- svydesign(ids = ~1, weights = ~w_combined_trim, data = obs_only)

f_bin <- ebac_legal ~ cannabis_any_use + age_group + gender +
  province_region + mental_health + physical_health
f_lin <- ebac_tot ~ cannabis_any_use + age_group + gender +
  province_region + mental_health + physical_health

fit_bin_obs <- svyglm(f_bin, design = des_obs, family = quasibinomial())
fit_bin_ipw <- svyglm(f_bin, design = des_ipw, family = quasibinomial())

fit_lin_obs <- svyglm(f_lin, design = des_obs)
fit_lin_ipw <- svyglm(f_lin, design = des_ipw)

or_compare <- bind_rows(
  extract_logistic_or(fit_bin_obs, "observed_domain_survey_weighted"),
  extract_logistic_or(fit_bin_ipw, "ipw_adjusted_eligible_domain")
)
lin_compare <- bind_rows(
  extract_linear(fit_lin_obs, "observed_domain_survey_weighted"),
  extract_linear(fit_lin_ipw, "ipw_adjusted_eligible_domain")
)

write_csv(or_compare, file.path(output_dir, "ebac_ipw_logistic_or.csv"))
write_csv(lin_compare, file.path(output_dir, "ebac_ipw_linear_coefficients.csv"))
cat("Saved: ebac_ipw_logistic_or.csv\n")
cat("Saved: ebac_ipw_linear_coefficients.csv\n\n")

# Focused comparison for cannabis term
can_bin <- or_compare %>%
  filter(term == "cannabis_any_use") %>%
  transmute(
    model,
    estimand = "ebac_legal OR",
    estimate = or,
    ci_lower95 = or_lower95,
    ci_upper95 = or_upper95,
    p_value
  )

can_lin <- lin_compare %>%
  filter(term == "cannabis_any_use") %>%
  transmute(
    model,
    estimand = "ebac_tot beta",
    estimate,
    ci_lower95,
    ci_upper95,
    p_value
  )

cannabis_comparison <- bind_rows(can_bin, can_lin)
print(cannabis_comparison, n = Inf, width = Inf)
write_csv(cannabis_comparison, file.path(output_dir, "ebac_ipw_cannabis_comparison.csv"))
cat("Saved: ebac_ipw_cannabis_comparison.csv\n\n")

# =============================================================================
# 5) Interpretation checkpoints
# =============================================================================
cat("--- 5) Interpretation checkpoints ---\n")

if (nrow(can_bin) == 2) {
  delta_or <- can_bin$estimate[can_bin$model == "ipw_adjusted_eligible_domain"] -
    can_bin$estimate[can_bin$model == "observed_domain_survey_weighted"]
  cat(sprintf("Cannabis OR change (IPW - observed): %.4f\n", delta_or))
}
if (nrow(can_lin) == 2) {
  delta_beta <- can_lin$estimate[can_lin$model == "ipw_adjusted_eligible_domain"] -
    can_lin$estimate[can_lin$model == "observed_domain_survey_weighted"]
  cat(sprintf("Cannabis beta change (IPW - observed): %.5f g/dL\n", delta_beta))
}

max_sw <- max(obs_only$sw, na.rm = TRUE)
if (max_sw > 10) {
  cat("WARNING: Large observation weights detected (max > 10). Initial Results may be unstable.\n")
} else {
  cat("Observation weights appear stable (max <= 10).\n")
}

cat("\n=== eBAC IPW ANALYSIS COMPLETE ===\n")
