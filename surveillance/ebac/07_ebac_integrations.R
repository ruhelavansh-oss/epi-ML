#!/usr/bin/env Rscript
# =============================================================================
# 07_ebac_integrations.R
# =============================================================================
# Final integrated eBAC workflow:
#   - Data audit and variable availability checks
#   - Survey-weighted descriptive and regression analysis
#   - Selection adjustment (IPW) for eBAC observation
#   - Causal contrasts (ATE / ATT / ATC / CATE) on eBAC_legal
#   - Double Machine Learning (Random Forest nuisance models; ATE + ATTE)
#   - SMOTE sensitivity (smotefamily)
#   - Crosswalk comparison with prior eBAC outputs
#
# NOTE:
#   - Primary inferential outputs remain survey-weighted, non-synthetic.
#   - SMOTE and DML are sensitivity/robustness layers.
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

output_dir <- paths$output_private_dir
wrangled_path <- file.path(paths$wrangled_dir, "cpads_pumf_wrangled.rds")

if (!file.exists(wrangled_path)) {
  stop("Missing wrangled data file: ", safe_label_path(wrangled_path, paths))
}
pumf <- readRDS(wrangled_path)

cat("=== PHASE 7e: FINAL INTEGRATED eBAC PIPELINE ===\n\n")

# -----------------------------------------------------------------------------
# 0) Variable audit and setup
# -----------------------------------------------------------------------------
required <- c(
  "wtpumf", "ebac_tot", "ebac_legal", "alcohol_past12m",
  "cannabis_any_use", "heavy_drinking_30d", "gender", "age_group",
  "province_region", "mental_health", "physical_health"
)
missing_req <- setdiff(required, names(pumf))
if (length(missing_req) > 0) {
  stop("Missing required variables: ", paste(missing_req, collapse = ", "))
}

# Optional demographic fields based on CPADS user guide definitions
has_dvdemq02 <- "dvdemq02" %in% names(pumf)  # sexual orientation
has_dvdemq6 <- "dvdemq6" %in% names(pumf)    # ethnicity
user_guide_ethnicity_var <- "dvdemq6"
has_user_guide_ethnicity <- user_guide_ethnicity_var %in% names(pumf)
race_named_vars <- names(pumf)[grepl("(^race$|race_)", names(pumf), ignore.case = TRUE)]

pumf <- pumf %>%
  mutate(
    sexual_orientation_dvdemq02 = if (has_dvdemq02) {
      factor(
        case_when(
          dvdemq02 == 1 ~ "Heterosexual",
          dvdemq02 == 2 ~ "Other",
          TRUE ~ NA_character_
        )
      )
    } else {
      factor(NA_character_)
    },
    ethnicity_dvdemq6 = if (has_dvdemq6) {
      factor(
        case_when(
          dvdemq6 == 1 ~ "Black",
          dvdemq6 == 2 ~ "East/Southeast Asian",
          dvdemq6 == 3 ~ "Indigenous (First Nations, Metis, Inuk/Inuit descent)",
          dvdemq6 == 4 ~ "Latino (Latin American, Hispanic descent)",
          dvdemq6 == 5 ~ "Middle Eastern",
          dvdemq6 == 6 ~ "South Asian",
          dvdemq6 == 7 ~ "White",
          dvdemq6 == 8 ~ "Other/Multiple",
          TRUE ~ NA_character_
        ),
        levels = c(
          "Black",
          "East/Southeast Asian",
          "Indigenous (First Nations, Metis, Inuk/Inuit descent)",
          "Latino (Latin American, Hispanic descent)",
          "Middle Eastern",
          "South Asian",
          "White",
          "Other/Multiple"
        )
      )
    } else {
      factor(NA_character_)
    }
  )

audit_tbl <- tibble(
  item = c(
    "n_total_wrangled",
    "ebac_tot_exists",
    "ebac_legal_exists",
    "demq3_exists",
    "demq4_exists",
    "sexual_orientation_dvdemq02_exists",
    "ethnicity_dvdemq6_exists",
    "user_guide_ethnicity_var",
    "named_race_vars_by_name_scan"
  ),
  value = c(
    nrow(pumf),
    "ebac_tot" %in% names(pumf),
    "ebac_legal" %in% names(pumf),
    "demq3" %in% names(pumf),
    "demq4" %in% names(pumf),
    has_dvdemq02,
    has_dvdemq6,
    user_guide_ethnicity_var,
    ifelse(length(race_named_vars) == 0, "NONE", paste(race_named_vars, collapse = ";"))
  )
)
print(audit_tbl, n = Inf)

user_guide_var_map <- tibble(
  variable_name = c("dvdemq6", "dvdemq02", "demq5"),
  user_guide_description = c(
    "What ethnicity are you?",
    "What term best describes your sexual orientation?",
    "Are you an international student?"
  ),
  exists_in_wrangled_data = c(
    "dvdemq6" %in% names(pumf),
    "dvdemq02" %in% names(pumf),
    "demq5" %in% names(pumf)
  ),
  coding_note = c(
    "1=Black; 2=East/Southeast Asian; 3=Indigenous; 4=Latino; 5=Middle Eastern; 6=South Asian; 7=White; 8=Other/Multiple; special codes recoded to NA",
    "1=Heterosexual; 2=Other; special codes recoded to NA",
    "1=Yes; 2=No; special codes recoded to NA"
  )
)
write_csv(user_guide_var_map, file.path(output_dir, "ebac_final_user_guide_variable_map.csv"))

# -----------------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------------
safe_confint <- function(model_obj) {
  ci <- if (inherits(model_obj, "glm") && !inherits(model_obj, "svyglm")) {
    tryCatch(confint.default(model_obj), error = function(e) NULL)
  } else {
    tryCatch(confint(model_obj), error = function(e) NULL)
  }
  if (is.null(ci)) {
    b <- coef(model_obj)
    se <- if (inherits(model_obj, "svyglm")) SE(model_obj) else summary(model_obj)$coefficients[, 2]
    ci <- cbind(b - 1.96 * se, b + 1.96 * se)
    rownames(ci) <- names(b)
  }
  ci
}

extract_or <- function(model_obj, model_name) {
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
  d <- data %>% filter(!is.na(.data[[outcome]]), !is.na(wtpumf))
  if (nrow(d) == 0) {
    return(tibble())
  }
  des <- svydesign(ids = ~1, weights = ~wtpumf, data = d)
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
    filter(!is.na(.data[[outcome]]), !is.na(.data[[group_var]]), !is.na(wtpumf))
  if (nrow(d) == 0) {
    return(tibble())
  }
  des <- svydesign(ids = ~1, weights = ~wtpumf, data = d)
  by_obj <- svyby(
    as.formula(paste0("~", outcome)),
    as.formula(paste0("~", group_var)),
    des,
    svymean,
    na.rm = TRUE,
    vartype = c("se", "ci")
  )
  by_df <- as.data.frame(by_obj)
  counts <- d %>%
    count(.data[[group_var]], name = "n_unweighted_nonmissing") %>%
    mutate(group_level = as.character(.data[[group_var]])) %>%
    select(group_level, n_unweighted_nonmissing)

  tibble(
    metric = metric_name,
    group_var = group_var,
    group_level = as.character(by_df[[group_var]]),
    estimate = as.numeric(by_df[[outcome]]),
    se = as.numeric(by_df$se),
    ci_lower95 = as.numeric(by_df$ci_l),
    ci_upper95 = as.numeric(by_df$ci_u)
  ) %>%
    left_join(counts, by = "group_level")
}

ess <- function(w) (sum(w)^2) / sum(w^2)

# -----------------------------------------------------------------------------
# 1a) eBAC formula audit (CPADS guide pp. 85-87)
# -----------------------------------------------------------------------------
cat("\n--- 1a) eBAC formula audit (Woman/Man coefficients) ---\n")

formula_required_vars <- c("gender", "alc13a", "alc13b_a", "alc13b_b", "demq3", "demq4", "ebac_tot", "ebac_legal")
formula_missing_vars <- setdiff(formula_required_vars, names(pumf))
formula_available_vars <- intersect(formula_required_vars, names(pumf))
formula_recompute_feasible <- length(formula_missing_vars) == 0

formula_input_audit <- tibble(
  item = c(
    "formula_recompute_feasible",
    "missing_formula_inputs",
    "available_formula_inputs",
    "n_woman_rows",
    "n_man_rows",
    "n_trans_nonbinary_rows"
  ),
  value = c(
    formula_recompute_feasible,
    ifelse(length(formula_missing_vars) == 0, "NONE", paste(formula_missing_vars, collapse = ";")),
    ifelse(length(formula_available_vars) == 0, "NONE", paste(formula_available_vars, collapse = ";")),
    sum(as.character(pumf$gender) == "Woman", na.rm = TRUE),
    sum(as.character(pumf$gender) == "Man", na.rm = TRUE),
    sum(as.character(pumf$gender) == "Transgender/Non-binary", na.rm = TRUE)
  )
)

if (formula_recompute_feasible) {
  ebac_formula_df <- pumf %>%
    mutate(
      gender_for_formula = case_when(
        as.character(gender) == "Woman" ~ "Woman",
        as.character(gender) == "Man" ~ "Man",
        TRUE ~ NA_character_
      ),
      grams_alcohol = 13.6 * as.numeric(alc13a),
      elapsed_hours = ((as.numeric(alc13b_a) * 60) + as.numeric(alc13b_b)) / 60,
      body_weight_kg = as.numeric(demq4),
      body_height_cm = as.numeric(demq3),
      r = case_when(
        gender_for_formula == "Woman" ~ 0.31223 - 0.006446 * body_weight_kg + 0.004466 * body_height_cm,
        gender_for_formula == "Man" ~ 0.31608 - 0.004821 * body_weight_kg + 0.004632 * body_height_cm,
        TRUE ~ NA_real_
      ),
      formula_inputs_complete = !is.na(gender_for_formula) &
        is.finite(grams_alcohol) &
        is.finite(elapsed_hours) &
        is.finite(body_weight_kg) &
        is.finite(body_height_cm) &
        is.finite(r) &
        grams_alcohol > 0 &
        body_weight_kg > 0 &
        body_height_cm > 0 &
        r > 0 &
        elapsed_hours >= 0,
      ebac_tot_formula = ifelse(
        formula_inputs_complete,
        ((grams_alcohol / (r * body_weight_kg)) - (0.017 * elapsed_hours)) / 10,
        NA_real_
      ),
      ebac_tot_formula = ifelse(ebac_tot_formula >= 0.5, NA_real_, ebac_tot_formula),
      ebac_legal_formula = ifelse(
        is.na(ebac_tot_formula),
        NA_integer_,
        as.integer(ebac_tot_formula > 0.08)
      )
    )

  compare_tot <- ebac_formula_df %>%
    filter(!is.na(ebac_tot_formula), !is.na(ebac_tot))
  compare_legal <- ebac_formula_df %>%
    filter(!is.na(ebac_legal_formula), !is.na(ebac_legal))
  compare_legal_off_boundary <- compare_legal %>% filter(ebac_tot != 0.08)

  formula_validation <- tibble(
    metric = c(
      "n_formula_inputs_complete",
      "n_formula_ebac_tot_computed",
      "n_compared_to_ebac_tot",
      "mae_formula_vs_ebac_tot",
      "rmse_formula_vs_ebac_tot",
      "corr_formula_vs_ebac_tot",
      "n_compared_to_ebac_legal",
      "agreement_formula_vs_ebac_legal",
      "n_compared_to_ebac_legal_off_boundary",
      "agreement_formula_vs_ebac_legal_off_boundary",
      "note"
    ),
    value = c(
      sum(ebac_formula_df$formula_inputs_complete, na.rm = TRUE),
      sum(!is.na(ebac_formula_df$ebac_tot_formula)),
      nrow(compare_tot),
      ifelse(nrow(compare_tot) > 0, mean(abs(compare_tot$ebac_tot_formula - compare_tot$ebac_tot)), NA_real_),
      ifelse(nrow(compare_tot) > 0, sqrt(mean((compare_tot$ebac_tot_formula - compare_tot$ebac_tot)^2)), NA_real_),
      ifelse(nrow(compare_tot) > 1, suppressWarnings(cor(compare_tot$ebac_tot_formula, compare_tot$ebac_tot)), NA_real_),
      nrow(compare_legal),
      ifelse(nrow(compare_legal) > 0, mean(compare_legal$ebac_legal_formula == compare_legal$ebac_legal), NA_real_),
      nrow(compare_legal_off_boundary),
      ifelse(nrow(compare_legal_off_boundary) > 0, mean(compare_legal_off_boundary$ebac_legal_formula == compare_legal_off_boundary$ebac_legal), NA_real_),
      "Formula applies only to Woman/Man rows; non-binary rows are excluded from direct formula recomputation."
    )
  )
} else {
  formula_validation <- tibble(
    metric = c("status", "missing_formula_inputs", "note"),
    value = c(
      "not_computable_missing_inputs",
      paste(formula_missing_vars, collapse = ";"),
      "Public wrangled PUMF includes ebac_tot/ebac_legal but lacks some direct formula inputs (typically demq3/demq4)."
    )
  )
}

print(formula_input_audit, n = Inf)
print(formula_validation, n = Inf)
write_csv(formula_input_audit, file.path(output_dir, "ebac_final_formula_input_audit.csv"))
write_csv(formula_validation, file.path(output_dir, "ebac_final_formula_validation.csv"))

# -----------------------------------------------------------------------------
# 1) Domain/sample accounting and QA checks
# -----------------------------------------------------------------------------
cat("\n--- 1) Domain/sample accounting and QA checks ---\n")

pumf <- pumf %>% mutate(ebac_observed = as.integer(!is.na(ebac_tot)))

sample_tbl <- tibble(
  sample_name = c(
    "Total wrangled cohort",
    "Past-12m alcohol users",
    "eBAC observed among alcohol users",
    "eBAC missing among alcohol users"
  ),
  n = c(
    nrow(pumf),
    sum(pumf$alcohol_past12m == 1, na.rm = TRUE),
    sum(pumf$alcohol_past12m == 1 & pumf$ebac_observed == 1, na.rm = TRUE),
    sum(pumf$alcohol_past12m == 1 & pumf$ebac_observed == 0, na.rm = TRUE)
  )
)
print(sample_tbl, n = Inf)

qa_tbl <- tibble(
  check_name = c(
    "ebac_tot_range_plausible",
    "ebac_legal_threshold_off_boundary",
    "ebac_missingness_rate_total",
    "ebac_missingness_given_alc12m0",
    "weights_positive"
  ),
  value = c(
    sprintf("[%.3f, %.3f]", min(pumf$ebac_tot, na.rm = TRUE), max(pumf$ebac_tot, na.rm = TRUE)),
    {
      tmp <- pumf %>% filter(!is.na(ebac_tot), !is.na(ebac_legal), ebac_tot != 0.08)
      sprintf("%.5f", mean(as.integer(tmp$ebac_tot > 0.08) == tmp$ebac_legal))
    },
    sprintf("%.4f", mean(is.na(pumf$ebac_tot))),
    sprintf("%.4f", mean(is.na(pumf$ebac_tot[pumf$alcohol_past12m == 0]))),
    as.character(all(pumf$wtpumf[!is.na(pumf$wtpumf)] > 0))
  ),
  pass = c(
    all(pumf$ebac_tot[!is.na(pumf$ebac_tot)] >= 0 & pumf$ebac_tot[!is.na(pumf$ebac_tot)] <= 0.8),
    {
      tmp <- pumf %>% filter(!is.na(ebac_tot), !is.na(ebac_legal), ebac_tot != 0.08)
      mean(as.integer(tmp$ebac_tot > 0.08) == tmp$ebac_legal) >= 0.999
    },
    mean(is.na(pumf$ebac_tot)) <= 0.40,
    mean(is.na(pumf$ebac_tot[pumf$alcohol_past12m == 0])) > 0.99,
    all(pumf$wtpumf[!is.na(pumf$wtpumf)] > 0)
  )
)
print(qa_tbl, n = Inf)

write_csv(audit_tbl, file.path(output_dir, "ebac_final_variable_audit.csv"))
write_csv(sample_tbl, file.path(output_dir, "ebac_final_domain_samples.csv"))
write_csv(qa_tbl, file.path(output_dir, "ebac_final_audit_checks.csv"))

# -----------------------------------------------------------------------------
# 2) Weighted descriptive summaries (observed eBAC domain)
# -----------------------------------------------------------------------------
cat("\n--- 2) Weighted descriptive summaries ---\n")

group_vars <- c(
  "cannabis_any_use", "gender", "age_group", "province_region",
  "mental_health", "physical_health"
)
if (has_dvdemq02) group_vars <- c(group_vars, "sexual_orientation_dvdemq02")
if (has_dvdemq6) group_vars <- c(group_vars, "ethnicity_dvdemq6")

desc_tbl <- bind_rows(
  weighted_mean_overall(pumf, "ebac_legal", "ebac_legal_prevalence"),
  weighted_mean_overall(pumf, "ebac_tot", "ebac_tot_mean"),
  map_dfr(group_vars, ~ weighted_mean_by_group(pumf, "ebac_legal", .x, "ebac_legal_prevalence")),
  map_dfr(group_vars, ~ weighted_mean_by_group(pumf, "ebac_tot", .x, "ebac_tot_mean"))
)
print(desc_tbl %>% arrange(metric, group_var, group_level), n = 80)
write_csv(desc_tbl, file.path(output_dir, "ebac_final_weighted_descriptives.csv"))

# -----------------------------------------------------------------------------
# 3) Weighted association models + interactions
# -----------------------------------------------------------------------------
cat("\n--- 3) Weighted association models ---\n")

model_vars <- c(
  "ebac_legal", "ebac_tot", "cannabis_any_use", "gender", "age_group",
  "province_region", "mental_health", "physical_health", "wtpumf"
)
df_obs <- pumf %>% select(all_of(model_vars)) %>% drop_na()
des_obs <- svydesign(ids = ~1, weights = ~wtpumf, data = df_obs)

fit_logit_main <- svyglm(
  ebac_legal ~ cannabis_any_use + gender + age_group + province_region +
    mental_health + physical_health,
  design = des_obs,
  family = quasibinomial()
)
fit_logit_int_gender <- svyglm(
  ebac_legal ~ cannabis_any_use * gender + age_group + province_region +
    mental_health + physical_health,
  design = des_obs,
  family = quasibinomial()
)
fit_logit_int_age <- svyglm(
  ebac_legal ~ cannabis_any_use * age_group + gender + province_region +
    mental_health + physical_health,
  design = des_obs,
  family = quasibinomial()
)
fit_lin_main <- svyglm(
  ebac_tot ~ cannabis_any_use + gender + age_group + province_region +
    mental_health + physical_health,
  design = des_obs
)

or_tbl <- bind_rows(
  extract_or(fit_logit_main, "weighted_main"),
  extract_or(fit_logit_int_gender, "weighted_interaction_gender"),
  extract_or(fit_logit_int_age, "weighted_interaction_age")
)
lin_tbl <- extract_linear(fit_lin_main, "weighted_linear_main")

int_tests <- bind_rows(
  tibble(
    test = "cannabis_any_use:gender",
    F_stat = as.numeric(regTermTest(fit_logit_int_gender, ~ cannabis_any_use:gender, method = "Wald")$Ftest),
    df_num = as.numeric(regTermTest(fit_logit_int_gender, ~ cannabis_any_use:gender, method = "Wald")$df),
    df_den = as.numeric(regTermTest(fit_logit_int_gender, ~ cannabis_any_use:gender, method = "Wald")$ddf),
    p_value = as.numeric(regTermTest(fit_logit_int_gender, ~ cannabis_any_use:gender, method = "Wald")$p)
  ),
  tibble(
    test = "cannabis_any_use:age_group",
    F_stat = as.numeric(regTermTest(fit_logit_int_age, ~ cannabis_any_use:age_group, method = "Wald")$Ftest),
    df_num = as.numeric(regTermTest(fit_logit_int_age, ~ cannabis_any_use:age_group, method = "Wald")$df),
    df_den = as.numeric(regTermTest(fit_logit_int_age, ~ cannabis_any_use:age_group, method = "Wald")$ddf),
    p_value = as.numeric(regTermTest(fit_logit_int_age, ~ cannabis_any_use:age_group, method = "Wald")$p)
  )
)

write_csv(or_tbl, file.path(output_dir, "ebac_final_weighted_or.csv"))
write_csv(lin_tbl, file.path(output_dir, "ebac_final_weighted_linear.csv"))
write_csv(int_tests, file.path(output_dir, "ebac_final_interaction_tests.csv"))

# -----------------------------------------------------------------------------
# 4) Selection adjustment (IPW) and comparison
# -----------------------------------------------------------------------------
cat("\n--- 4) Selection-adjusted models (IPW) ---\n")

target <- pumf %>%
  filter(alcohol_past12m == 1) %>%
  mutate(R = as.integer(!is.na(ebac_tot))) %>%
  select(
    R, wtpumf, ebac_legal, ebac_tot, cannabis_any_use,
    gender, age_group, province_region, mental_health, physical_health
  ) %>%
  drop_na(wtpumf, cannabis_any_use, gender, age_group, province_region, mental_health, physical_health)

obs_mod <- glm(
  R ~ cannabis_any_use + gender + age_group + province_region + mental_health + physical_health,
  data = target,
  family = binomial()
)
target <- target %>%
  mutate(
    p_hat = pmin(pmax(predict(obs_mod, type = "response"), 0.01), 0.99),
    p_obs = mean(R),
    sw = ifelse(R == 1, p_obs / p_hat, NA_real_)
  )
q01 <- quantile(target$sw[target$R == 1], 0.01, na.rm = TRUE)
q99 <- quantile(target$sw[target$R == 1], 0.99, na.rm = TRUE)
target <- target %>%
  mutate(
    sw_trim = ifelse(R == 1, pmin(pmax(sw, q01), q99), NA_real_),
    w_combined = ifelse(R == 1, wtpumf * sw_trim, NA_real_)
  )

ipw_diag <- tibble(
  metric = c("eligible_n", "observed_n", "observed_rate", "sw_min", "sw_q01", "sw_median", "sw_q99", "sw_max", "ess_weighted_obs", "ess_weighted_ipw"),
  value = c(
    nrow(target),
    sum(target$R == 1),
    mean(target$R),
    min(target$sw[target$R == 1], na.rm = TRUE),
    quantile(target$sw[target$R == 1], 0.01, na.rm = TRUE),
    median(target$sw[target$R == 1], na.rm = TRUE),
    quantile(target$sw[target$R == 1], 0.99, na.rm = TRUE),
    max(target$sw[target$R == 1], na.rm = TRUE),
    ess(target$wtpumf[target$R == 1]),
    ess(target$w_combined[target$R == 1])
  )
)
write_csv(ipw_diag, file.path(output_dir, "ebac_final_ipw_diagnostics.csv"))

obs_only <- target %>% filter(R == 1) %>% drop_na(ebac_legal, ebac_tot)
des_ipw <- svydesign(ids = ~1, weights = ~w_combined, data = obs_only)

fit_ipw_logit <- svyglm(
  ebac_legal ~ cannabis_any_use + gender + age_group + province_region + mental_health + physical_health,
  design = des_ipw,
  family = quasibinomial()
)
fit_ipw_lin <- svyglm(
  ebac_tot ~ cannabis_any_use + gender + age_group + province_region + mental_health + physical_health,
  design = des_ipw
)

ipw_or <- extract_or(fit_ipw_logit, "selection_adjusted_ipw")
ipw_lin <- extract_linear(fit_ipw_lin, "selection_adjusted_ipw")

compare_tbl <- bind_rows(
  or_tbl %>% filter(model == "weighted_main", term == "cannabis_any_use") %>%
    transmute(metric = "ebac_legal_or_cannabis", model = "weighted_main", estimate = or, ci_lower95 = or_lower95, ci_upper95 = or_upper95, p_value),
  ipw_or %>% filter(term == "cannabis_any_use") %>%
    transmute(metric = "ebac_legal_or_cannabis", model, estimate = or, ci_lower95 = or_lower95, ci_upper95 = or_upper95, p_value),
  lin_tbl %>% filter(term == "cannabis_any_use") %>%
    transmute(metric = "ebac_tot_beta_cannabis", model = "weighted_main", estimate, ci_lower95, ci_upper95, p_value),
  ipw_lin %>% filter(term == "cannabis_any_use") %>%
    transmute(metric = "ebac_tot_beta_cannabis", model, estimate, ci_lower95, ci_upper95, p_value)
)

write_csv(ipw_or, file.path(output_dir, "ebac_final_ipw_or.csv"))
write_csv(ipw_lin, file.path(output_dir, "ebac_final_ipw_linear.csv"))
write_csv(compare_tbl, file.path(output_dir, "ebac_final_ipw_comparison.csv"))

# -----------------------------------------------------------------------------
# 5) Causal contrasts (ATE/ATT/ATC/CATE) for eBAC_legal
# -----------------------------------------------------------------------------
cat("\n--- 5) Causal contrasts (ATE/ATT/ATC/CATE) ---\n")

causal_df <- obs_only %>%
  select(
    ebac_legal, cannabis_any_use, gender, age_group, province_region,
    mental_health, physical_health, w_combined
  ) %>%
  drop_na()

ps_mod <- glm(
  cannabis_any_use ~ gender + age_group + province_region + mental_health + physical_health,
  data = causal_df,
  family = quasibinomial(),
  weights = w_combined
)
causal_df <- causal_df %>%
  mutate(
    ps = pmin(pmax(predict(ps_mod, type = "response"), 0.01), 0.99),
    T = cannabis_any_use,
    Y = ebac_legal
  )

estimate_effects <- function(df) {
  w_base <- df$w_combined
  T <- df$T
  Y <- df$Y
  ps <- df$ps

  w_ate <- w_base * ifelse(T == 1, 1 / ps, 1 / (1 - ps))
  y1_ate <- weighted.mean(Y[T == 1], w_ate[T == 1])
  y0_ate <- weighted.mean(Y[T == 0], w_ate[T == 0])
  ate <- y1_ate - y0_ate

  w_att <- w_base * ifelse(T == 1, 1, ps / (1 - ps))
  y1_att <- weighted.mean(Y[T == 1], w_att[T == 1])
  y0_att <- weighted.mean(Y[T == 0], w_att[T == 0])
  att <- y1_att - y0_att

  w_atc <- w_base * ifelse(T == 1, (1 - ps) / ps, 1)
  y1_atc <- weighted.mean(Y[T == 1], w_atc[T == 1])
  y0_atc <- weighted.mean(Y[T == 0], w_atc[T == 0])
  atc <- y1_atc - y0_atc

  c(ATE = ate, ATT = att, ATC = atc)
}

point_eff <- estimate_effects(causal_df)
n_boot <- 200
boot_mat <- matrix(NA_real_, nrow = n_boot, ncol = 3)
colnames(boot_mat) <- c("ATE", "ATT", "ATC")

for (b in seq_len(n_boot)) {
  idx <- sample.int(nrow(causal_df), replace = TRUE)
  db <- causal_df[idx, ]
    ps_b <- tryCatch(
      glm(
        cannabis_any_use ~ gender + age_group + province_region + mental_health + physical_health,
        data = db,
        family = quasibinomial(),
        weights = w_combined
      ),
      error = function(e) NULL
    )
  if (is.null(ps_b)) next
  db$ps <- pmin(pmax(predict(ps_b, type = "response"), 0.01), 0.99)
  boot_mat[b, ] <- estimate_effects(db)
}

causal_tbl <- tibble(
  estimand = c("ATE", "ATT", "ATC"),
  estimate = as.numeric(point_eff[c("ATE", "ATT", "ATC")]),
  se = apply(boot_mat, 2, sd, na.rm = TRUE),
  ci_lower95 = estimate - 1.96 * se,
  ci_upper95 = estimate + 1.96 * se,
  n = nrow(causal_df),
  n_boot_valid = apply(!is.na(boot_mat), 2, sum)
)
print(causal_tbl)
write_csv(causal_tbl, file.path(output_dir, "ebac_final_causal_effects.csv"))

# CATE by gender and age_group (IPW-weighted subgroup ATE)
cate_tbl <- bind_rows(
  lapply(c("gender", "age_group"), function(v) {
    lvls <- unique(as.character(causal_df[[v]]))
    bind_rows(lapply(lvls, function(lv) {
      ds <- causal_df %>% filter(as.character(.data[[v]]) == lv)
      n_t <- sum(ds$T == 1)
      n_c <- sum(ds$T == 0)
      if (n_t < 50 || n_c < 50) {
        return(tibble(
          subgroup_var = v, subgroup_level = lv,
          n_treated = n_t, n_control = n_c,
          cate = NA_real_, se = NA_real_, ci_lower95 = NA_real_, ci_upper95 = NA_real_,
          note = "Insufficient overlap for stable CATE"
        ))
      }
      # point
      eff <- estimate_effects(ds)["ATE"]
      # bootstrap with fixed ps for speed in subgroup (sensitivity-style)
      bb <- replicate(120, {
        ii <- sample.int(nrow(ds), replace = TRUE)
        dsi <- ds[ii, ]
        estimate_effects(dsi)["ATE"]
      })
      se <- sd(bb, na.rm = TRUE)
      tibble(
        subgroup_var = v, subgroup_level = lv,
        n_treated = n_t, n_control = n_c,
        cate = as.numeric(eff), se = se,
        ci_lower95 = as.numeric(eff) - 1.96 * se,
        ci_upper95 = as.numeric(eff) + 1.96 * se,
        note = ""
      )
    }))
  })
)
print(cate_tbl, n = Inf)
write_csv(cate_tbl, file.path(output_dir, "ebac_final_cate.csv"))

# -----------------------------------------------------------------------------
# 6) DML Random Forest (ATE + ATTE) if available
# -----------------------------------------------------------------------------
cat("\n--- 6) DML random-forest estimation ---\n")
dml_status <- tibble(
  package_combo_available = all(c("DoubleML", "mlr3", "mlr3learners") %in% rownames(installed.packages())),
  run_completed = FALSE,
  note = NA_character_
)
dml_tbl <- tibble()

if (dml_status$package_combo_available) {
  suppressPackageStartupMessages({
    library(DoubleML)
    library(mlr3)
    library(mlr3learners)
  })

  dml_df <- causal_df %>%
    select(Y = Y, D = T, gender, age_group, province_region, mental_health, physical_health)
  X <- model.matrix(~ gender + age_group + province_region + mental_health + physical_health, data = dml_df)[, -1, drop = FALSE]
  dat <- data.frame(y = dml_df$Y, d = dml_df$D, X)
  dml_data <- DoubleMLData$new(dat, y_col = "y", d_cols = "d")
  ml_g <- lrn("regr.ranger", num.trees = 300, min.node.size = 5)
  ml_m <- lrn("classif.ranger", predict_type = "prob", num.trees = 300, min.node.size = 5)

  obj_ate <- DoubleMLIRM$new(dml_data, ml_g = ml_g, ml_m = ml_m, n_folds = 3, score = "ATE")
  obj_ate$fit()
  sum_ate <- as.data.frame(obj_ate$summary())

  obj_atte <- DoubleMLIRM$new(dml_data, ml_g = ml_g, ml_m = ml_m, n_folds = 3, score = "ATTE")
  obj_atte$fit()
  sum_atte <- as.data.frame(obj_atte$summary())

  parse_dml_summary <- function(df) {
    nm <- names(df)
    get_col <- function(pattern) {
      idx <- grep(pattern, nm, ignore.case = TRUE)
      if (length(idx) == 0) return(NA_real_)
      as.numeric(df[[idx[1]]][1])
    }
    tibble(
      estimate = get_col("^estimate\\.?$"),
      se = get_col("std\\.?\\s*error"),
      t_value = get_col("^t\\s*value$"),
      p_value = get_col("pr\\(>\\|t\\|\\)")
    )
  }

  ate_vals <- parse_dml_summary(sum_ate)
  atte_vals <- parse_dml_summary(sum_atte)

  dml_tbl <- bind_rows(
    tibble(
      estimand = "ATE_DML_RF",
      estimate = ate_vals$estimate,
      se = ate_vals$se,
      t_value = ate_vals$t_value,
      p_value = ate_vals$p_value
    ),
    tibble(
      estimand = "ATTE_DML_RF",
      estimate = atte_vals$estimate,
      se = atte_vals$se,
      t_value = atte_vals$t_value,
      p_value = atte_vals$p_value
    )
  )
  dml_status$run_completed <- TRUE
  dml_status$note <- "DML IRM with ranger learners completed (unweighted sensitivity)."
} else {
  dml_status$note <- "DoubleML/mlr3/learners not available; DML skipped."
}

write_csv(dml_status, file.path(output_dir, "ebac_final_dml_status.csv"))
write_csv(dml_tbl, file.path(output_dir, "ebac_final_dml_results.csv"))

# -----------------------------------------------------------------------------
# 7) SMOTE sensitivity (smotefamily)
# -----------------------------------------------------------------------------
cat("\n--- 7) SMOTE sensitivity (smotefamily) ---\n")

smote_status <- tibble(
  package_available = requireNamespace("smotefamily", quietly = TRUE),
  run_completed = FALSE,
  method = NA_character_,
  warning_count = 0L,
  class_ratio_before = NA_real_,
  class_ratio_after = NA_real_,
  note = NA_character_
)
smote_cmp <- tibble()
smote_or_all <- tibble()

if (smote_status$package_available) {
  x_mm <- model.matrix(
    ~ cannabis_any_use * gender + age_group + province_region + mental_health + physical_health,
    data = df_obs
  )[, -1, drop = FALSE]
  x_df <- as.data.frame(x_mm)
  names(x_df) <- make.names(names(x_df), unique = TRUE)
  y <- factor(ifelse(df_obs$ebac_legal == 1, "Yes", "No"), levels = c("No", "Yes"))

  tab0 <- table(y)
  smote_status$class_ratio_before <- as.numeric(max(tab0) / min(tab0))
  base_data <- x_df %>% mutate(Y = y)

  if (smote_status$class_ratio_before < 1.25) {
    smote_status$method <- "skipped_near_balanced"
    smote_status$note <- "Outcome is near-balanced (<1.25 ratio); SMOTE skipped by design."
    sm_data <- base_data
    smote_status$class_ratio_after <- smote_status$class_ratio_before
  } else {
    smote_status$method <- "smotefamily"
    smote_warnings <- character()
    sm <- withCallingHandlers(
      smotefamily::SMOTE(X = x_df, target = y, K = 5, dup_size = 0),
      warning = function(w) {
        smote_warnings <<- c(smote_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    smote_status$warning_count <- length(smote_warnings)

    if (length(smote_warnings) > 0) {
      smote_status$method <- "random_oversampling_fallback"
      smote_status$note <- paste0(
        "smotefamily emitted ", length(smote_warnings),
        " warning(s); fallback random oversampling used."
      )
      minority_label <- names(which.min(tab0))
      majority_n <- as.integer(max(tab0))
      minority_rows <- base_data %>% filter(Y == minority_label)
      n_to_add <- majority_n - nrow(minority_rows)
      if (n_to_add > 0) {
        add_rows <- minority_rows[sample(nrow(minority_rows), n_to_add, replace = TRUE), ]
        sm_data <- bind_rows(base_data, add_rows)
      } else {
        sm_data <- base_data
      }
    } else {
      sm_data <- as.data.frame(sm$data, check.names = FALSE)
      names(sm_data) <- c(names(x_df), "Y")
      sm_data$Y <- factor(sm_data$Y, levels = levels(y))
      smote_status$note <- "SMOTE completed with smotefamily on one-hot design matrix; cannabis terms extracted by pattern match."
    }
    tab1 <- table(sm_data$Y)
    smote_status$class_ratio_after <- as.numeric(max(tab1) / min(tab1))
  }

  fit_base <- glm(Y ~ ., data = base_data, family = binomial())
  fit_sm <- glm(Y ~ ., data = sm_data, family = binomial())
  or_base <- extract_or(fit_base, "unweighted_base_design_matrix")
  or_sm <- extract_or(fit_sm, paste0(smote_status$method, "_design_matrix"))
  smote_or_all <- bind_rows(or_base, or_sm)

  smote_cmp <- smote_or_all %>%
    filter(grepl("cannabis_any_use", term))

  smote_status$run_completed <- TRUE
} else {
  smote_status$method <- "skipped_package_unavailable"
  smote_status$note <- "smotefamily not available; SMOTE sensitivity skipped."
}

write_csv(smote_status, file.path(output_dir, "ebac_final_smote_status.csv"))
write_csv(smote_cmp, file.path(output_dir, "ebac_final_smote_compare.csv"))
write_csv(smote_or_all, file.path(output_dir, "ebac_final_smote_or.csv"))

# -----------------------------------------------------------------------------
# 8) Crosswalk with prior eBAC outputs
# -----------------------------------------------------------------------------
cat("\n--- 8) Crosswalk vs previous eBAC outputs ---\n")

prev_compare <- tibble()
path_prev_or <- file.path(output_dir, "ebac_logistic_or_primary.csv")
path_prev_ipw <- file.path(output_dir, "ebac_ipw_cannabis_comparison.csv")
if (file.exists(path_prev_or)) {
  prev_or <- read_csv(path_prev_or, show_col_types = FALSE) %>%
    filter(term == "cannabis_any_use") %>%
    transmute(source = "previous_ebac_logistic_or_primary", metric = "ebac_legal_or_cannabis", estimate = or)
  prev_compare <- bind_rows(prev_compare, prev_or)
}
if (file.exists(path_prev_ipw)) {
  prev_ip_raw <- read_csv(path_prev_ipw, show_col_types = FALSE)
  if ("metric" %in% names(prev_ip_raw)) {
    prev_ip <- prev_ip_raw %>%
      filter(metric == "ebac_legal_or_cannabis") %>%
      transmute(source = paste0("previous_", model), metric = metric, estimate = estimate)
  } else if ("estimand" %in% names(prev_ip_raw)) {
    prev_ip <- prev_ip_raw %>%
      filter(tolower(estimand) %in% c("ebac_legal or", "ebac_legal_or_cannabis")) %>%
      transmute(source = paste0("previous_", model), metric = "ebac_legal_or_cannabis", estimate = estimate)
  } else {
    prev_ip <- tibble()
  }
  prev_compare <- bind_rows(prev_compare, prev_ip)
}

new_compare <- bind_rows(
  compare_tbl %>% filter(metric == "ebac_legal_or_cannabis") %>% transmute(source = paste0("new_", model), metric, estimate)
)

crosswalk <- bind_rows(prev_compare, new_compare)
print(crosswalk, n = Inf)
write_csv(crosswalk, file.path(output_dir, "ebac_final_crosswalk_previous.csv"))

# -----------------------------------------------------------------------------
# 9) Key summary artifact
# -----------------------------------------------------------------------------
summary_tbl <- tibble(
  key = c(
    "weighted_ebac_legal_prevalence_observed_domain",
    "weighted_ebac_tot_mean_observed_domain",
    "weighted_main_or_cannabis_ebac_legal",
    "selection_adjusted_or_cannabis_ebac_legal",
    "causal_ate_ipw",
    "causal_att_ipw",
    "causal_atc_ipw",
    "dml_ate_rf",
    "dml_atte_rf",
    "smote_class_ratio_before",
    "smote_class_ratio_after",
    "ethnicity_variable_resolution",
    "race_named_vars_found"
  ),
  value = c(
    desc_tbl %>% filter(metric == "ebac_legal_prevalence", group_var == "Overall") %>% pull(estimate),
    desc_tbl %>% filter(metric == "ebac_tot_mean", group_var == "Overall") %>% pull(estimate),
    compare_tbl %>% filter(metric == "ebac_legal_or_cannabis", model == "weighted_main") %>% pull(estimate),
    compare_tbl %>% filter(metric == "ebac_legal_or_cannabis", model == "selection_adjusted_ipw") %>% pull(estimate),
    causal_tbl %>% filter(estimand == "ATE") %>% pull(estimate),
    causal_tbl %>% filter(estimand == "ATT") %>% pull(estimate),
    causal_tbl %>% filter(estimand == "ATC") %>% pull(estimate),
    ifelse(nrow(dml_tbl) > 0, dml_tbl %>% filter(estimand == "ATE_DML_RF") %>% pull(estimate), NA_real_),
    ifelse(nrow(dml_tbl) > 0, dml_tbl %>% filter(estimand == "ATTE_DML_RF") %>% pull(estimate), NA_real_),
    smote_status$class_ratio_before,
    smote_status$class_ratio_after,
    ifelse(
      has_user_guide_ethnicity,
      paste0("official_ethnicity_var=", user_guide_ethnicity_var),
      "official_ethnicity_var=NOT_FOUND"
    ),
    ifelse(
      has_user_guide_ethnicity,
      paste0("official_ethnicity_var=", user_guide_ethnicity_var, "; race_name_scan=", ifelse(length(race_named_vars) == 0, "NONE", paste(race_named_vars, collapse = ";"))),
      ifelse(length(race_named_vars) == 0, "NONE", paste(race_named_vars, collapse = ";"))
    )
  )
)
print(summary_tbl, n = Inf)
write_csv(summary_tbl, file.path(output_dir, "ebac_final_key_summary.csv"))

cat("\n=== FINAL eBAC PIPELINE COMPLETE ===\n")
