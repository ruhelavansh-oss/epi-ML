#!/usr/bin/env Rscript
# =============================================================================
# 07_ebac_gender_smote_sensitivity.R
# =============================================================================
# Purpose:
#   1) Properly model gender effect-modification for eBAC outcomes.
#   2) Run SMOTE as a predictive sensitivity analysis only (NOT primary inference).
#   3) Use smotefamily::SMOTE when available.
#
# Scientific stance:
#   - Primary inference remains survey-weighted, non-synthetic.
#   - SMOTE is optional and strictly sensitivity for class-imbalance robustness.
#   - If smotefamily is unavailable, script records a transparent "not run" status.
#
# Inputs:
#   PROJECT_ROOT/data/private/outputs/wrangled/cpads_pumf_wrangled.rds
#
# Outputs:
#   PROJECT_ROOT/data/private/outputs/ebac_gender_interaction_svy_or.csv
#   PROJECT_ROOT/data/private/outputs/ebac_gender_interaction_tests.csv
#   PROJECT_ROOT/data/private/outputs/ebac_gender_marginal_probs.csv
#   PROJECT_ROOT/data/private/outputs/ebac_smote_status.csv
#   PROJECT_ROOT/data/private/outputs/ebac_smote_or.csv         (if smotefamily available)
#   PROJECT_ROOT/data/private/outputs/ebac_smote_compare.csv    (if smotefamily available)
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
  stop("Missing wrangled file: ", wrangled_path)
}
pumf <- readRDS(wrangled_path)

required <- c(
  "wtpumf", "ebac_legal", "cannabis_any_use", "gender",
  "age_group", "province_region", "mental_health", "physical_health"
)
missing <- setdiff(required, names(pumf))
if (length(missing) > 0) {
  stop("Missing required fields: ", paste(missing, collapse = ", "))
}

cat("=== PHASE 7d: eBAC GENDER INTERACTION + SMOTE SENSITIVITY ===\n\n")

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

# =============================================================================
# 1) Primary survey-weighted interaction analysis (inferential)
# =============================================================================
cat("--- 1) Survey-weighted interaction analysis ---\n")

df_svy <- pumf %>%
  select(
    ebac_legal, cannabis_any_use, gender, age_group, province_region,
    mental_health, physical_health, wtpumf
  ) %>%
  drop_na()

cat(sprintf("Analysis sample (complete cases): %d\n", nrow(df_svy)))
cat("Outcome distribution (ebac_legal):\n")
print(prop.table(table(df_svy$ebac_legal)))
cat("\nBy gender:\n")
print(prop.table(table(df_svy$ebac_legal, df_svy$gender), 2))

des <- svydesign(ids = ~1, weights = ~wtpumf, data = df_svy)

fit_int <- svyglm(
  ebac_legal ~ cannabis_any_use * gender + age_group + province_region +
    mental_health + physical_health,
  design = des,
  family = quasibinomial()
)

or_int <- extract_or(fit_int, "svy_interaction_cannabis_by_gender")
write_csv(or_int, file.path(output_dir, "ebac_gender_interaction_svy_or.csv"))
cat("Saved: ebac_gender_interaction_svy_or.csv\n")

# Joint interaction test
int_test <- regTermTest(fit_int, ~ cannabis_any_use:gender, method = "Wald")
test_tbl <- tibble(
  test = "cannabis_any_use:gender (joint Wald)",
  F_stat = as.numeric(int_test$Ftest),
  df_num = as.numeric(int_test$df),
  df_den = as.numeric(int_test$ddf),
  p_value = as.numeric(int_test$p)
)
print(test_tbl)
write_csv(test_tbl, file.path(output_dir, "ebac_gender_interaction_tests.csv"))
cat("Saved: ebac_gender_interaction_tests.csv\n\n")

# Marginal predicted probabilities by gender and cannabis exposure
pred_grid <- tidyr::crossing(
  cannabis_any_use = c(0L, 1L),
  gender = levels(df_svy$gender),
  age_group = levels(df_svy$age_group)[1],
  province_region = levels(df_svy$province_region)[1],
  mental_health = levels(df_svy$mental_health)[1],
  physical_health = levels(df_svy$physical_health)[1]
) %>%
  mutate(
    age_group = factor(age_group, levels = levels(df_svy$age_group), ordered = is.ordered(df_svy$age_group)),
    province_region = factor(province_region, levels = levels(df_svy$province_region)),
    mental_health = factor(mental_health, levels = levels(df_svy$mental_health), ordered = is.ordered(df_svy$mental_health)),
    physical_health = factor(physical_health, levels = levels(df_svy$physical_health), ordered = is.ordered(df_svy$physical_health)),
    gender = factor(gender, levels = levels(df_svy$gender))
  )

pred <- predict(fit_int, newdata = pred_grid, type = "response")
pred_var <- attr(pred, "var")
pred_se <- if (!is.null(pred_var)) {
  if (is.matrix(pred_var)) {
    sqrt(diag(pred_var))
  } else if (length(pred_var) == length(pred)^2) {
    vmat <- matrix(pred_var, nrow = length(pred), ncol = length(pred))
    sqrt(diag(vmat))
  } else if (length(pred_var) == length(pred)) {
    sqrt(as.numeric(pred_var))
  } else {
    rep(NA_real_, length(pred))
  }
} else {
  rep(NA_real_, length(pred))
}
pred_tbl <- pred_grid %>%
  mutate(
    pred_prob = as.numeric(pred),
    se = as.numeric(pred_se),
    ci_lower95 = pmax(0, pred_prob - 1.96 * se),
    ci_upper95 = pmin(1, pred_prob + 1.96 * se)
  ) %>%
  select(gender, cannabis_any_use, pred_prob, se, ci_lower95, ci_upper95)
print(pred_tbl, n = Inf)
write_csv(pred_tbl, file.path(output_dir, "ebac_gender_marginal_probs.csv"))
cat("Saved: ebac_gender_marginal_probs.csv\n\n")

# =============================================================================
# 2) SMOTE sensitivity (predictive; non-survey)
# =============================================================================
cat("--- 2) SMOTE sensitivity (smotefamily::SMOTE, if available) ---\n")

smote_status <- tibble(
  smote_package = "smotefamily",
  package_available = requireNamespace("smotefamily", quietly = TRUE),
  run_completed = FALSE,
  method = NA_character_,
  warning_count = 0L,
  note = NA_character_,
  class_ratio_before = NA_real_,
  class_ratio_after = NA_real_
)

if (!smote_status$package_available) {
  smote_status$note <- paste(
    "smotefamily not installed in this environment.",
    "SMOTE sensitivity not run.",
    "Primary survey-weighted interaction analysis is valid and complete."
  )
  write_csv(smote_status, file.path(output_dir, "ebac_smote_status.csv"))
  cat("Saved: ebac_smote_status.csv\n")
  cat("smotefamily unavailable -> skipping SMOTE sensitivity.\n")
  cat("\n=== PHASE 7d COMPLETE (primary analysis done; SMOTE skipped) ===\n")
  quit(save = "no", status = 0)
}

# Build a design matrix that preserves categorical structure through one-hot
# encoding and includes cannabis-by-gender interactions explicitly.
df_smote <- df_svy %>%
  transmute(
    ebac_legal_f = factor(ifelse(ebac_legal == 1, "Yes", "No"), levels = c("No", "Yes")),
    cannabis_any_use = factor(cannabis_any_use, levels = c(0, 1)),
    gender = factor(gender),
    age_group = factor(age_group, levels = levels(df_svy$age_group), ordered = is.ordered(df_svy$age_group)),
    province_region = factor(province_region, levels = levels(df_svy$province_region)),
    mental_health = factor(mental_health, levels = levels(df_svy$mental_health), ordered = is.ordered(df_svy$mental_health)),
    physical_health = factor(physical_health, levels = levels(df_svy$physical_health), ordered = is.ordered(df_svy$physical_health))
  )

x_mm <- model.matrix(
  ~ cannabis_any_use * gender + age_group + province_region + mental_health + physical_health,
  data = df_smote
)[, -1, drop = FALSE]
x_df <- as.data.frame(x_mm)
names(x_df) <- make.names(names(x_df), unique = TRUE)
y <- df_smote$ebac_legal_f

class_tab <- table(y)
minority <- min(class_tab)
majority <- max(class_tab)
imbalance_ratio <- as.numeric(majority / minority)
smote_status$class_ratio_before <- imbalance_ratio
cat(sprintf("Class imbalance ratio (majority/minority): %.3f\n", imbalance_ratio))
base_data <- x_df %>%
  mutate(ebac_legal_f = y)

if (imbalance_ratio < 1.25) {
  smote_status$method <- "skipped_near_balanced"
  smote_status$note <- "Outcome is near-balanced (<1.25 ratio); SMOTE skipped by design."
  sm_data <- base_data
  smote_status$class_ratio_after <- imbalance_ratio
  cat("Outcome is near-balanced; skipping SMOTE and using baseline unweighted model.\n")
} else {
  smote_status$method <- "smotefamily"
  smote_warnings <- character()
  sm_res <- withCallingHandlers(
    smotefamily::SMOTE(
      X = x_df,
      target = y,
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
    smote_status$method <- "random_oversampling_fallback"
    smote_status$note <- paste0(
      "smotefamily emitted ", length(smote_warnings),
      " warning(s); fallback random oversampling used."
    )
    minority_label <- names(which.min(class_tab))
    majority_n <- as.integer(max(class_tab))
    minority_rows <- base_data %>% filter(ebac_legal_f == minority_label)
    n_to_add <- majority_n - nrow(minority_rows)
    if (n_to_add > 0) {
      add_rows <- minority_rows[sample(nrow(minority_rows), n_to_add, replace = TRUE), ]
      sm_data <- bind_rows(base_data, add_rows)
    } else {
      sm_data <- base_data
    }
  } else {
    sm_data <- as.data.frame(sm_res$data, check.names = FALSE)
    names(sm_data) <- c(names(x_df), "ebac_legal_f")
    sm_data$ebac_legal_f <- factor(sm_data$ebac_legal_f, levels = levels(y))
    smote_status$note <- paste(
      "SMOTE sensitivity run with smotefamily::SMOTE on one-hot design matrix.",
      "Interpret as predictive robustness only; not survey-weighted inference."
    )
  }
  post_tab <- table(sm_data$ebac_legal_f)
  smote_status$class_ratio_after <- as.numeric(max(post_tab) / min(post_tab))
  cat(sprintf(
    "Post-SMOTE class ratio (majority/minority): %.3f\n",
    smote_status$class_ratio_after
  ))
}

fit_base_unw <- glm(
  ebac_legal_f ~ .,
  data = base_data,
  family = binomial()
)
fit_smote <- glm(
  ebac_legal_f ~ .,
  data = sm_data,
  family = binomial()
)

or_base_unw <- extract_or(fit_base_unw, "unweighted_complete_case_design_matrix")
or_smote <- extract_or(fit_smote, paste0(smote_status$method, "_design_matrix"))
write_csv(or_smote, file.path(output_dir, "ebac_smote_or.csv"))

cmp <- bind_rows(or_base_unw, or_smote) %>%
  filter(grepl("^cannabis_any_use1($|\\.)", term))
print(cmp, n = Inf)
write_csv(cmp, file.path(output_dir, "ebac_smote_compare.csv"))

smote_status$run_completed <- TRUE
if (is.na(smote_status$note)) {
  smote_status$note <- "SMOTE sensitivity complete."
}
write_csv(smote_status, file.path(output_dir, "ebac_smote_status.csv"))

cat("Saved: ebac_smote_status.csv, ebac_smote_or.csv, ebac_smote_compare.csv\n")
cat("\n=== PHASE 7d COMPLETE ===\n")
