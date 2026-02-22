# #!/usr/bin/env Rscript
# # =============================================================================
# # 04_descriptive_stats.R — Phase 4: Descriptive Statistics & Probability
# # Epidemiological Study: Alcohol Use in Canada
# # =============================================================================
# # Computes statistics following Seeing Theory (Brown University) notation.
# # Chapters 1-3: Basic Probability, Compound Probability, Distributions.
# # =============================================================================

# suppressPackageStartupMessages({
#   library(tidyverse)
#   library(survey)
#   library(janitor)
# })

# options(scipen = 999)
# set.seed(42)


# wrangled_dir <- "PROJECT_ROOT/data/private/outputs/wrangled"
# output_dir  <- "PROJECT_ROOT/data/private/outputs"
# fig_dir     <- file.path(output_dir, "figures")

# cat("=== PHASE 4: DESCRIPTIVE STATISTICS & PROBABILITY ===\n\n")

# # --- Load wrangled data ---
# pumf <- readRDS(file.path(wrangled_dir, "cpads_pumf_wrangled.rds"))
# pumf_svy <- readRDS(file.path(wrangled_dir, "cpads_pumf_survey.rds"))
# cads_alc <- readRDS(file.path(wrangled_dir, "cads_alcohol.rds"))

# cat("CPADS_PUMF:", nrow(pumf), "observations\n")
# cat("CADS Alcohol:", nrow(cads_alc), "aggregate rows\n\n")

# # =============================================================================
# # 4.1 — Seeing Theory Ch.1: Basic Probability
# # =============================================================================
# cat(paste(rep("=", 62), collapse = ""), "\n")
# cat("SEEING THEORY CH.1: BASIC PROBABILITY\n")
# cat(paste(rep("=", 62), collapse = ""), "\n\n")

# # --- Sample Space S ---
# cat("--- 1.1 Sample Space Definition ---\n")
# cat("S = {all CPADS PUMF respondents}, |S| =", nrow(pumf), "\n")
# cat("For alcohol use: S_alc = {0 (never), 1 (ever used)}\n")
# cat("  n(alcohol_any_use = 1):", sum(pumf$alcohol_any_use == 1, na.rm = TRUE), "\n")
# cat("  n(alcohol_any_use = 0):", sum(pumf$alcohol_any_use == 0, na.rm = TRUE), "\n")
# cat("  n(NA):", sum(is.na(pumf$alcohol_any_use)), "\n\n")

# # --- P(A): Marginal Probabilities ---
# cat("--- 1.2 Marginal Probabilities P(A) ---\n")
# cat("Axioms: 0 <= P(A) <= 1, P(S) = 1, P(A^c) = 1 - P(A)\n\n")

# # Survey-weighted prevalence
# pumf_svy_wrangled <- svydesign(ids = ~1, weights = ~wtpumf, data = pumf)

# # P(Alcohol use)
# p_alc <- svymean(~alcohol_any_use, pumf_svy_wrangled, na.rm = TRUE)
# p_alc_ci <- confint(p_alc)
# cat(sprintf("P(Alcohol ever) = %.4f (95%% CI: %.4f, %.4f)\n",
#             coef(p_alc), p_alc_ci[1], p_alc_ci[2]))
# cat(sprintf("P(Alcohol never) = 1 - P(A) = %.4f\n\n", 1 - coef(p_alc)))

# # P(Binge drinking)
# p_binge <- svymean(~alcohol_binge, pumf_svy_wrangled, na.rm = TRUE)
# p_binge_ci <- confint(p_binge)
# cat(sprintf("P(Binge drinking) = %.4f (95%% CI: %.4f, %.4f)\n",
#             coef(p_binge), p_binge_ci[1], p_binge_ci[2]))

# # P(Cannabis use)
# p_can <- svymean(~cannabis_any_use, pumf_svy_wrangled, na.rm = TRUE)
# p_can_ci <- confint(p_can)
# cat(sprintf("P(Cannabis ever) = %.4f (95%% CI: %.4f, %.4f)\n\n",
#             coef(p_can), p_can_ci[1], p_can_ci[2]))

# # P by demographics
# cat("--- P(Alcohol) by Demographics ---\n")
# for (v in c("sex", "age_group", "province_region", "mental_health")) {
#   cat(sprintf("\nP(Alcohol | %s):\n", v))
#   fml <- as.formula(paste("~alcohol_any_use +", v))
#   tab <- svyby(~alcohol_any_use, as.formula(paste("~", v)),
#                pumf_svy_wrangled, svymean, na.rm = TRUE)
#   print(tab)
# }

# # --- E[X] and Var(X) ---
# cat("\n--- 1.3 Expectation and Variance ---\n")
# cat("E[X] = sum(x * P(X=x)),  Var(X) = E[(X - mu)^2]\n\n")

# # Survey weights
# wt_mean_age <- svymean(~age_groups, pumf_svy_wrangled)
# cat(sprintf("E[age_group_code] = %.3f\n", coef(wt_mean_age)))

# # For alcohol frequency (among drinkers)
# pumf_drinkers <- pumf %>% filter(alcohol_any_use == 1, !is.na(alc03))
# svy_drinkers <- svydesign(ids = ~1, weights = ~wtpumf, data = pumf_drinkers)
# freq_mean <- svymean(~alc03, svy_drinkers)
# freq_var <- svyvar(~alc03, svy_drinkers)
# cat(sprintf("E[alc_frequency_code | drinker] = %.3f\n", coef(freq_mean)))
# cat(sprintf("Var[alc_frequency_code | drinker] = %.3f\n", coef(freq_var)))
# cat(sprintf("SD[alc_frequency_code | drinker] = %.3f\n\n", sqrt(coef(freq_var))))

# # --- Markov's Inequality ---
# cat("--- 1.4 Markov's Inequality ---\n")
# cat("P(X >= a) <= E[X] / a  (for X >= 0)\n\n")

# # Using survey weight variable as an example of a non-negative continuous variable
# ex_wt <- mean(pumf$wtpumf)
# a_val <- 2.0
# markov_bound <- ex_wt / a_val
# empirical_prob <- mean(pumf$wtpumf >= a_val)
# cat(sprintf("Variable: wtpumf,  E[X] = %.3f,  a = %.1f\n", ex_wt, a_val))
# cat(sprintf("Markov bound: P(X >= %.1f) <= %.4f\n", a_val, markov_bound))
# cat(sprintf("Empirical:    P(X >= %.1f)  = %.4f\n", a_val, empirical_prob))
# cat(sprintf("Markov holds: %s (bound >= empirical)\n\n",
#             ifelse(markov_bound >= empirical_prob, "YES", "NO")))

# # --- Chebyshev's Inequality ---
# cat("--- 1.5 Chebyshev's Inequality ---\n")
# cat("P(|X - mu| >= k*sigma) <= 1/k^2\n\n")

# mu_wt <- mean(pumf$wtpumf)
# sd_wt <- sd(pumf$wtpumf)

# for (k in c(1, 2, 3)) {
#   cheby_bound <- 1 / k^2
#   empirical <- mean(abs(pumf$wtpumf - mu_wt) >= k * sd_wt)
#   cat(sprintf("k=%d: Chebyshev bound = %.4f, Empirical = %.4f, Holds: %s\n",
#               k, cheby_bound, empirical,
#               ifelse(cheby_bound >= empirical, "YES", "NO")))
# }

# # =============================================================================
# # 4.2 — Seeing Theory Ch.2: Compound Probability
# # =============================================================================
# cat("\n")
# cat(rep("=", 62), "\n")
# cat("SEEING THEORY CH.2: COMPOUND PROBABILITY\n")
# cat(rep("=", 62), "\n\n")

# # --- Joint Probabilities P(A ∩ B) ---
# cat("--- 2.1 Joint Probabilities ---\n")
# cat("P(A ∩ B) = P(Alcohol AND Cannabis)\n\n")

# # Create joint variable
# pumf$alc_and_can <- ifelse(
#   !is.na(pumf$alcohol_any_use) & !is.na(pumf$cannabis_any_use),
#   pumf$alcohol_any_use * pumf$cannabis_any_use,
#   NA
# )

# svy_joint <- svydesign(ids = ~1, weights = ~wtpumf, data = pumf)
# p_joint <- svymean(~alc_and_can, svy_joint, na.rm = TRUE)
# cat(sprintf("P(Alcohol ∩ Cannabis) = %.4f\n", coef(p_joint)))
# cat(sprintf("P(Alcohol) * P(Cannabis) = %.4f * %.4f = %.4f\n",
#             coef(p_alc), coef(p_can), coef(p_alc) * coef(p_can)))
# cat(sprintf("Difference: %.4f\n", coef(p_joint) - coef(p_alc) * coef(p_can)))
# cat("If P(A∩B) ≠ P(A)·P(B), then A and B are NOT independent.\n\n")

# # --- Conditional Probability P(A|B) ---
# cat("--- 2.2 Conditional Probability ---\n")
# cat("P(A|B) = P(A ∩ B) / P(B)\n\n")

# # P(Alcohol | Cannabis user)
# p_alc_given_can <- coef(p_joint) / coef(p_can)
# cat(sprintf("P(Alcohol | Cannabis) = P(A∩B)/P(B) = %.4f / %.4f = %.4f\n",
#             coef(p_joint), coef(p_can), p_alc_given_can))

# # Direct computation
# can_users <- pumf %>% filter(cannabis_any_use == 1)
# svy_can <- svydesign(ids = ~1, weights = ~wtpumf, data = can_users)
# p_alc_given_can_direct <- svymean(~alcohol_any_use, svy_can, na.rm = TRUE)
# cat(sprintf("Direct: P(Alcohol | Cannabis user) = %.4f\n\n",
#             coef(p_alc_given_can_direct)))

# # P(Cannabis | Alcohol user)
# p_can_given_alc <- coef(p_joint) / coef(p_alc)
# cat(sprintf("P(Cannabis | Alcohol) = P(A∩B)/P(A) = %.4f / %.4f = %.4f\n\n",
#             coef(p_joint), coef(p_alc), p_can_given_alc))

# # --- Independence Test ---
# cat("--- 2.3 Independence Testing ---\n")
# cat("H0: Alcohol use and Cannabis use are independent\n")
# cat("H1: They are dependent\n\n")

# # Chi-squared test (unweighted for simplicity, then survey-weighted)
# tab_2x2 <- table(pumf$alcohol_any_use, pumf$cannabis_any_use, useNA = "no")
# cat("Contingency table (unweighted):\n")
# print(tab_2x2)

# if (nrow(tab_2x2) >= 2 && ncol(tab_2x2) >= 2) {
#   chi_test <- chisq.test(tab_2x2)
#   cat(sprintf("\nChi-squared = %.2f, df = %d, p-value = %s\n",
#               chi_test$statistic, chi_test$parameter,
#               formatC(chi_test$p.value, format = "e", digits = 3)))

#   svy_chi <- svychisq(~alcohol_any_use + cannabis_any_use, svy_joint, statistic = "Chisq")
#   cat(sprintf("Survey-weighted Chi-squared = %.2f, p-value = %s\n\n",
#               svy_chi$statistic, formatC(svy_chi$p.value, format = "e", digits = 3)))
# } else {
#   cat("\nNote: alcohol_any_use has only 1 level (100% prevalence in CPADS PUMF).\n")
#   cat("All postsecondary respondents who answered reported alcohol use.\n")
#   cat("Using binge drinking × cannabis for independence test instead.\n\n")

#   tab_alt <- table(pumf$alcohol_binge, pumf$cannabis_any_use, useNA = "no",
#                    dnn = c("Binge", "Cannabis"))
#   print(tab_alt)
#   if (nrow(tab_alt) >= 2 && ncol(tab_alt) >= 2) {
#     chi_alt <- chisq.test(tab_alt)
#     cat(sprintf("\nChi-sq (Binge x Cannabis) = %.2f, df = %d, p = %s\n",
#                 chi_alt$statistic, chi_alt$parameter,
#                 formatC(chi_alt$p.value, format = "e", digits = 3)))

#     pumf_bc <- pumf %>% filter(!is.na(alcohol_binge), !is.na(cannabis_any_use))
#     svy_bc <- svydesign(ids = ~1, weights = ~wtpumf, data = pumf_bc)
#     svy_chi_alt <- svychisq(~alcohol_binge + cannabis_any_use, svy_bc, statistic = "Chisq")
#     cat(sprintf("Survey-weighted Chi-sq = %.2f, p = %s\n\n",
#                 svy_chi_alt$statistic, formatC(svy_chi_alt$p.value, format = "e", digits = 3)))
#   }
# }

# # --- Bayes' Rule ---
# cat("--- 2.4 Bayes' Rule ---\n")
# cat("P(B|A) = P(A|B) * P(B) / P(A)\n\n")

# # P(Poor mental health | Binge drinker) via Bayes
# pumf$poor_mh <- ifelse(pumf$mental_health %in% c("Fair", "Poor"), 1L,
#                 ifelse(!is.na(pumf$mental_health), 0L, NA_integer_))

# svy_mh <- svydesign(ids = ~1, weights = ~wtpumf, data = pumf)
# p_poor_mh <- svymean(~poor_mh, svy_mh, na.rm = TRUE)
# cat(sprintf("P(Poor/Fair MH) = %.4f\n", coef(p_poor_mh)))

# # P(Binge | Poor MH)
# binge_given_poor <- pumf %>% filter(poor_mh == 1)
# svy_bg <- svydesign(ids = ~1, weights = ~wtpumf, data = binge_given_poor)
# p_binge_given_poor <- svymean(~alcohol_binge, svy_bg, na.rm = TRUE)
# cat(sprintf("P(Binge | Poor MH) = %.4f\n", coef(p_binge_given_poor)))

# # Bayes: P(Poor MH | Binge) = P(Binge | Poor MH) * P(Poor MH) / P(Binge)
# p_poor_given_binge <- coef(p_binge_given_poor) * coef(p_poor_mh) / coef(p_binge)
# cat(sprintf("P(Poor MH | Binge) = P(Binge|Poor) * P(Poor) / P(Binge)\n"))
# cat(sprintf("                   = %.4f * %.4f / %.4f = %.4f\n\n",
#             coef(p_binge_given_poor), coef(p_poor_mh), coef(p_binge), p_poor_given_binge))

# # =============================================================================
# # 4.3 — Seeing Theory Ch.3: Distributions
# # =============================================================================
# cat(rep("=", 62), "\n")
# cat("SEEING THEORY CH.3: PROBABILITY DISTRIBUTIONS\n")
# cat(rep("=", 62), "\n\n")

# # --- Central Limit Theorem ---
# cat("--- 3.1 Central Limit Theorem Demonstration ---\n")
# cat("sqrt(n)(X_bar - mu)/sigma -> N(0,1) as n -> infinity\n\n")

# # Draw repeated samples of alcohol_binge, compute means
# binge_valid <- pumf$alcohol_binge[!is.na(pumf$alcohol_binge)]
# mu_binge <- mean(binge_valid)
# sigma_binge <- sd(binge_valid)

# n_samples <- 1000
# sample_sizes <- c(10, 30, 100, 500)

# cat("Population: P(binge) = ", round(mu_binge, 4), "\n")
# cat("Population SD =", round(sigma_binge, 4), "\n\n")

# clt_results <- tibble(
#   sample_size = integer(),
#   mean_of_means = numeric(),
#   sd_of_means = numeric(),
#   theoretical_se = numeric(),
#   shapiro_p = numeric()
# )
# for (n in sample_sizes) {
#   sample_means <- replicate(n_samples, mean(sample(binge_valid, n, replace = TRUE)))
#   z_scores <- sqrt(n) * (sample_means - mu_binge) / sigma_binge

#   clt_results <- clt_results %>% add_row(
#     sample_size = as.integer(n),
#     mean_of_means = round(mean(sample_means), 5),
#     sd_of_means = round(sd(sample_means), 5),
#     theoretical_se = round(sigma_binge / sqrt(n), 5),
#     shapiro_p = round(shapiro.test(z_scores[1:min(5000, length(z_scores))])$p.value, 4)
#   )
# }

# cat("CLT Convergence Table:\n")
# print(clt_results, n = Inf)

# # --- Distribution Fitting ---
# cat("\n--- 3.2 Distribution of Survey Weights ---\n")
# cat("Testing normality of wtpumf:\n")
# wt_sample <- sample(pumf$wtpumf, min(5000, nrow(pumf)))
# shapiro_wt <- shapiro.test(wt_sample)
# cat(sprintf("Shapiro-Wilk W = %.4f, p = %s\n",
#             shapiro_wt$statistic,
#             formatC(shapiro_wt$p.value, format = "e", digits = 3)))

# # Summary stats
# cat(sprintf("\nwtpumf summary:\n"))
# cat(sprintf("  Mean   = %.4f\n", mean(pumf$wtpumf)))
# cat(sprintf("  Median = %.4f\n", median(pumf$wtpumf)))
# cat(sprintf("  SD     = %.4f\n", sd(pumf$wtpumf)))
# cat(sprintf("  Skew   = %.4f\n", moments::skewness(pumf$wtpumf)))
# cat(sprintf("  Kurt   = %.4f\n", moments::kurtosis(pumf$wtpumf)))

# # --- Save results ---
# cat("\n--- Saving Phase 4 Initial Results ---\n")

# # Compile all probability estimates
# prob_table <- tibble(
#   quantity = c("P(Alcohol ever)", "P(Binge drinking)", "P(Cannabis ever)",
#                "P(Alcohol ∩ Cannabis)", "P(Alcohol|Cannabis)", "P(Cannabis|Alcohol)",
#                "P(Poor/Fair MH)", "P(Binge|Poor MH)", "P(Poor MH|Binge)"),
#   estimate = c(coef(p_alc), coef(p_binge), coef(p_can),
#                coef(p_joint), p_alc_given_can, p_can_given_alc,
#                coef(p_poor_mh), coef(p_binge_given_poor), p_poor_given_binge),
#   ci_lower = c(p_alc_ci[1], p_binge_ci[1], p_can_ci[1],
#                confint(p_joint)[1], NA, NA,
#                confint(p_poor_mh)[1], confint(p_binge_given_poor)[1], NA),
#   ci_upper = c(p_alc_ci[2], p_binge_ci[2], p_can_ci[2],
#                confint(p_joint)[2], NA, NA,
#                confint(p_poor_mh)[2], confint(p_binge_given_poor)[2], NA)
# )

# write_csv(prob_table, file.path(output_dir, "probability_estimates.csv"))
# write_csv(clt_results, file.path(output_dir, "clt_convergence.csv"))

# cat("Saved: probability_estimates.csv, clt_convergence.csv\n")
# cat("\n=== PHASE 4 COMPLETE ===\n")

#!/usr/bin/env Rscript
# =============================================================================
# 04_descriptive_stats.R — Phase 4: Descriptive Statistics & Probability
# CPADS 2021–2022 PUMF (Cleaned)
# =============================================================================
# Computes basic probability quantities (Seeing Theory style) using survey weights.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(survey)
  library(janitor)
})

options(scipen = 999)
set.seed(42)
source("surveillance/lib/config_paths.R")
paths <- get_paths()
init_paths(paths)

# ---- Paths: EVERYTHING under PROJECT_ROOT ----
data_dir <- paths$data_dir
output_dir <- paths$output_private_dir
wrangled_dir <- paths$wrangled_dir
fig_dir <- paths$figures_dir

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(wrangled_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== PHASE 4: DESCRIPTIVE STATISTICS & PROBABILITY ===\n\n")

# --- Load wrangled data ---
pumf <- readRDS(file.path(wrangled_dir, "cpads_pumf_wrangled.rds"))
pumf_svy <- readRDS(file.path(wrangled_dir, "cpads_pumf_survey.rds"))

# Optional: CADS alcohol aggregate (only if present)
cads_path <- file.path(wrangled_dir, "cads_alcohol.rds")
if (file.exists(cads_path)) {
  cads_alc <- readRDS(cads_path)
  cat("CADS Alcohol:", nrow(cads_alc), "aggregate rows\n")
} else {
  cads_alc <- NULL
  cat("CADS Alcohol: not found (skipping)\n")
}

cat("CPADS_PUMF:", nrow(pumf), "observations\n\n")

# Ensure expected variables exist (fail early with a clear message)
required_vars <- c(
  "wtpumf",
  "alcohol_ever",
  "alcohol_past12m",
  "heavy_drinking_30d",
  "cannabis_any_use",
  "gender",
  "age_group",
  "province_region",
  "mental_health",
  "alc06",
  "age_groups"
)
missing_vars <- setdiff(required_vars, names(pumf))
if (length(missing_vars) > 0) {
  stop(
    "Missing required variables in pumf: ",
    paste(missing_vars, collapse = ", ")
  )
}

# =============================================================================
# 4.1 — Seeing Theory Ch.1: Basic Probability
# =============================================================================
cat(paste(rep("=", 62), collapse = ""), "\n")
cat("SEEING THEORY CH.1: BASIC PROBABILITY\n")
cat(paste(rep("=", 62), collapse = ""), "\n\n")

# --- Sample Space S ---
cat("--- 1.1 Sample Space Definition ---\n")
cat("S = {all CPADS PUMF respondents}, |S| =", nrow(pumf), "\n\n")

cat("Binary indicators in wrangled data:\n")
cat("  alcohol_ever (lifetime)        : 0/1/NA\n")
cat("  alcohol_past12m (past 12 months): 0/1/NA\n")
cat("  heavy_drinking_30d (past 30d)  : 0/1/NA\n")
cat("  cannabis_any_use (past 12m)    : 0/1/NA\n\n")

cat("Unweighted counts:\n")
cat("  n(alcohol_ever = 1):", sum(pumf$alcohol_ever == 1, na.rm = TRUE), "\n")
cat("  n(alcohol_ever = 0):", sum(pumf$alcohol_ever == 0, na.rm = TRUE), "\n")
cat("  n(NA):", sum(is.na(pumf$alcohol_ever)), "\n\n")

# --- P(A): Marginal Probabilities ---
cat("--- 1.2 Marginal Probabilities P(A) (survey-weighted) ---\n")
cat("Axioms: 0 <= P(A) <= 1, P(S) = 1, P(A^c) = 1 - P(A)\n\n")

# P(Alcohol lifetime)
p_alc_ever <- svymean(~alcohol_ever, pumf_svy, na.rm = TRUE)
p_alc_ever_ci <- confint(p_alc_ever)
cat(sprintf(
  "P(Alcohol lifetime) = %.4f (95%% CI: %.4f, %.4f)\n",
  coef(p_alc_ever)[1],
  p_alc_ever_ci[1, 1],
  p_alc_ever_ci[1, 2]
))
cat(sprintf(
  "P(No lifetime alcohol) = 1 - P(A) = %.4f\n\n",
  1 - coef(p_alc_ever)[1]
))

# P(Alcohol past 12 months)
p_alc_12m <- svymean(~alcohol_past12m, pumf_svy, na.rm = TRUE)
p_alc_12m_ci <- confint(p_alc_12m)
cat(sprintf(
  "P(Alcohol past 12 months) = %.4f (95%% CI: %.4f, %.4f)\n",
  coef(p_alc_12m)[1],
  p_alc_12m_ci[1, 1],
  p_alc_12m_ci[1, 2]
))

# P(Heavy drinking past 30 days)
p_heavy_30d <- svymean(~heavy_drinking_30d, pumf_svy, na.rm = TRUE)
p_heavy_30d_ci <- confint(p_heavy_30d)
cat(sprintf(
  "P(Heavy drinking past 30 days) = %.4f (95%% CI: %.4f, %.4f)\n",
  coef(p_heavy_30d)[1],
  p_heavy_30d_ci[1, 1],
  p_heavy_30d_ci[1, 2]
))

# P(Cannabis past 12 months)
p_can_12m <- svymean(~cannabis_any_use, pumf_svy, na.rm = TRUE)
p_can_12m_ci <- confint(p_can_12m)
cat(sprintf(
  "P(Cannabis past 12 months) = %.4f (95%% CI: %.4f, %.4f)\n\n",
  coef(p_can_12m)[1],
  p_can_12m_ci[1, 1],
  p_can_12m_ci[1, 2]
))

# P by demographics (use wrangled variables)
cat("--- P(Alcohol past 12 months) by Demographics ---\n")
for (v in c("gender", "age_group", "province_region", "mental_health")) {
  cat(sprintf("\nP(Alcohol past 12 months | %s):\n", v))
  tab <- svyby(
    ~alcohol_past12m,
    as.formula(paste0("~", v)),
    pumf_svy,
    svymean,
    na.rm = TRUE,
    vartype = c("se", "ci")
  )
  print(tab)
}

# --- E[X] and Var(X) ---
cat("\n--- 1.3 Expectation and Variance ---\n")
cat("E[X] = sum(x * P(X=x)),  Var(X) = E[(X - mu)^2]\n\n")

# Example: expected age group code (weighted)
wt_mean_age_code <- svymean(~age_groups, pumf_svy, na.rm = TRUE)
cat(sprintf("E[age_groups (code)] = %.3f\n\n", coef(wt_mean_age_code)[1]))

# Alcohol frequency code among past-12-month drinkers: use alc06 (numeric codes)
pumf_drinkers_12m <- pumf %>% filter(alcohol_past12m == 1, !is.na(alc06))
svy_drinkers_12m <- svydesign(
  ids = ~1,
  weights = ~wtpumf,
  data = pumf_drinkers_12m
)
freq_mean <- svymean(~alc06, svy_drinkers_12m, na.rm = TRUE)
freq_var <- svyvar(~alc06, svy_drinkers_12m, na.rm = TRUE)
cat(sprintf(
  "E[alc06 frequency code | past-12m drinker] = %.3f\n",
  coef(freq_mean)[1]
))
cat(sprintf(
  "Var[alc06 frequency code | past-12m drinker] = %.3f\n",
  coef(freq_var)[1]
))
cat(sprintf(
  "SD[alc06 frequency code | past-12m drinker] = %.3f\n\n",
  sqrt(coef(freq_var)[1])
))

# --- Markov's Inequality ---
cat("--- 1.4 Markov's Inequality ---\n")
cat("P(X >= a) <= E[X] / a  (for X >= 0)\n\n")

ex_wt <- mean(pumf$wtpumf, na.rm = TRUE)
a_val <- 2.0
markov_bound <- ex_wt / a_val
empirical_prob <- mean(pumf$wtpumf >= a_val, na.rm = TRUE)
cat(sprintf("Variable: wtpumf,  E[X] = %.3f,  a = %.1f\n", ex_wt, a_val))
cat(sprintf("Markov bound: P(X >= %.1f) <= %.4f\n", a_val, markov_bound))
cat(sprintf("Empirical:    P(X >= %.1f)  = %.4f\n", a_val, empirical_prob))
cat(sprintf(
  "Markov holds: %s (bound >= empirical)\n\n",
  ifelse(markov_bound >= empirical_prob, "YES", "NO")
))

# --- Chebyshev's Inequality ---
cat("--- 1.5 Chebyshev's Inequality ---\n")
cat("P(|X - mu| >= k*sigma) <= 1/k^2\n\n")

mu_wt <- mean(pumf$wtpumf, na.rm = TRUE)
sd_wt <- sd(pumf$wtpumf, na.rm = TRUE)

for (k in c(1, 2, 3)) {
  cheby_bound <- 1 / k^2
  empirical <- mean(abs(pumf$wtpumf - mu_wt) >= k * sd_wt, na.rm = TRUE)
  cat(sprintf(
    "k=%d: Chebyshev bound = %.4f, Empirical = %.4f, Holds: %s\n",
    k,
    cheby_bound,
    empirical,
    ifelse(cheby_bound >= empirical, "YES", "NO")
  ))
}

# =============================================================================
# 4.2 — Seeing Theory Ch.2: Compound Probability
# =============================================================================
cat("\n")
cat(paste(rep("=", 62), collapse = ""), "\n")
cat("SEEING THEORY CH.2: COMPOUND PROBABILITY\n")
cat(paste(rep("=", 62), collapse = ""), "\n\n")

# --- Joint Probabilities P(A ∩ B) ---
cat("--- 2.1 Joint Probabilities ---\n")
cat("P(A ∩ B) = P(Alcohol past 12 months AND Cannabis past 12 months)\n\n")

# Create joint variable (1 if both 1, 0 if at least one 0; NA if either missing)
pumf <- pumf %>%
  mutate(
    alc12m_and_can12m = case_when(
      is.na(alcohol_past12m) | is.na(cannabis_any_use) ~ NA_integer_,
      alcohol_past12m == 1 & cannabis_any_use == 1 ~ 1L,
      TRUE ~ 0L
    )
  )

svy_joint <- svydesign(ids = ~1, weights = ~wtpumf, data = pumf)

p_joint <- svymean(~alc12m_and_can12m, svy_joint, na.rm = TRUE)
p_joint_ci <- confint(p_joint)

cat(sprintf(
  "P(Alcohol12m ∩ Cannabis12m) = %.4f (95%% CI: %.4f, %.4f)\n",
  coef(p_joint)[1],
  p_joint_ci[1, 1],
  p_joint_ci[1, 2]
))

cat(sprintf(
  "P(Alcohol12m) * P(Cannabis12m) = %.4f * %.4f = %.4f\n",
  coef(p_alc_12m)[1],
  coef(p_can_12m)[1],
  coef(p_alc_12m)[1] * coef(p_can_12m)[1]
))
cat(sprintf(
  "Difference: %.4f\n\n",
  coef(p_joint)[1] - coef(p_alc_12m)[1] * coef(p_can_12m)[1]
))

# --- Conditional Probability P(A|B) ---
cat("--- 2.2 Conditional Probability ---\n")
cat("P(A|B) = P(A ∩ B) / P(B)\n\n")

p_alc_given_can <- coef(p_joint)[1] / coef(p_can_12m)[1]
cat(sprintf(
  "P(Alcohol12m | Cannabis12m) = %.4f / %.4f = %.4f\n",
  coef(p_joint)[1],
  coef(p_can_12m)[1],
  p_alc_given_can
))

can_users <- pumf %>% filter(cannabis_any_use == 1)
svy_can <- svydesign(ids = ~1, weights = ~wtpumf, data = can_users)
p_alc_given_can_direct <- svymean(~alcohol_past12m, svy_can, na.rm = TRUE)
cat(sprintf(
  "Direct: P(Alcohol12m | Cannabis12m=1) = %.4f\n\n",
  coef(p_alc_given_can_direct)[1]
))

p_can_given_alc <- coef(p_joint)[1] / coef(p_alc_12m)[1]
cat(sprintf(
  "P(Cannabis12m | Alcohol12m) = %.4f / %.4f = %.4f\n\n",
  coef(p_joint)[1],
  coef(p_alc_12m)[1],
  p_can_given_alc
))

# --- Independence Test ---
cat("--- 2.3 Independence Testing ---\n")
cat("H0: Alcohol12m and Cannabis12m are independent\n")
cat("H1: They are dependent\n\n")

tab_2x2 <- table(pumf$alcohol_past12m, pumf$cannabis_any_use, useNA = "no")
cat("Contingency table (unweighted):\n")
print(tab_2x2)

if (nrow(tab_2x2) >= 2 && ncol(tab_2x2) >= 2) {
  chi_test <- chisq.test(tab_2x2)
  cat(sprintf(
    "\nChi-squared = %.2f, df = %d, p-value = %s\n",
    chi_test$statistic,
    chi_test$parameter,
    formatC(chi_test$p.value, format = "e", digits = 3)
  ))

  svy_chi <- svychisq(
    ~ alcohol_past12m + cannabis_any_use,
    svy_joint,
    statistic = "Chisq"
  )
  cat(sprintf(
    "Survey-weighted Chi-squared = %.2f, p-value = %s\n\n",
    svy_chi$statistic,
    formatC(svy_chi$p.value, format = "e", digits = 3)
  ))
} else {
  cat(
    "\nNote: alcohol_past12m has only 1 level; switching to heavy_drinking_30d × cannabis.\n\n"
  )

  tab_alt <- table(
    pumf$heavy_drinking_30d,
    pumf$cannabis_any_use,
    useNA = "no",
    dnn = c("Heavy30d", "Cannabis12m")
  )
  print(tab_alt)

  if (nrow(tab_alt) >= 2 && ncol(tab_alt) >= 2) {
    chi_alt <- chisq.test(tab_alt)
    cat(sprintf(
      "\nChi-sq (Heavy30d x Cannabis12m) = %.2f, df = %d, p = %s\n",
      chi_alt$statistic,
      chi_alt$parameter,
      formatC(chi_alt$p.value, format = "e", digits = 3)
    ))

    pumf_hc <- pumf %>%
      filter(!is.na(heavy_drinking_30d), !is.na(cannabis_any_use))
    svy_hc <- svydesign(ids = ~1, weights = ~wtpumf, data = pumf_hc)
    svy_chi_alt <- svychisq(
      ~ heavy_drinking_30d + cannabis_any_use,
      svy_hc,
      statistic = "Chisq"
    )
    cat(sprintf(
      "Survey-weighted Chi-sq = %.2f, p = %s\n\n",
      svy_chi_alt$statistic,
      formatC(svy_chi_alt$p.value, format = "e", digits = 3)
    ))
  }
}

# --- Bayes' Rule ---
cat("--- 2.4 Bayes' Rule ---\n")
cat("P(B|A) = P(A|B) * P(B) / P(A)\n\n")

# Define Poor/Fair mental health indicator from wrangled mental_health
pumf <- pumf %>%
  mutate(
    poor_mh = case_when(
      is.na(mental_health) ~ NA_integer_,
      mental_health %in% c("Fair", "Poor") ~ 1L,
      TRUE ~ 0L
    )
  )

svy_mh <- svydesign(ids = ~1, weights = ~wtpumf, data = pumf)
p_poor_mh <- svymean(~poor_mh, svy_mh, na.rm = TRUE)
p_poor_mh_ci <- confint(p_poor_mh)

cat(sprintf(
  "P(Poor/Fair MH) = %.4f (95%% CI: %.4f, %.4f)\n",
  coef(p_poor_mh)[1],
  p_poor_mh_ci[1, 1],
  p_poor_mh_ci[1, 2]
))

# Use heavy_drinking_30d as the “binge-like” event for Bayes example
heavy_given_poor <- pumf %>% filter(poor_mh == 1)
svy_hg <- svydesign(ids = ~1, weights = ~wtpumf, data = heavy_given_poor)
p_heavy_given_poor <- svymean(~heavy_drinking_30d, svy_hg, na.rm = TRUE)
p_heavy_given_poor_ci <- confint(p_heavy_given_poor)

cat(sprintf(
  "P(Heavy30d | Poor/Fair MH) = %.4f (95%% CI: %.4f, %.4f)\n",
  coef(p_heavy_given_poor)[1],
  p_heavy_given_poor_ci[1, 1],
  p_heavy_given_poor_ci[1, 2]
))

# Bayes: P(Poor MH | Heavy) = P(Heavy | Poor) * P(Poor) / P(Heavy)
p_poor_given_heavy <- coef(p_heavy_given_poor)[1] *
  coef(p_poor_mh)[1] /
  coef(p_heavy_30d)[1]
cat("Bayes:\n")
cat(sprintf(
  "P(Poor MH | Heavy30d) = %.4f * %.4f / %.4f = %.4f\n\n",
  coef(p_heavy_given_poor)[1],
  coef(p_poor_mh)[1],
  coef(p_heavy_30d)[1],
  p_poor_given_heavy
))

# =============================================================================
# 4.3 — Seeing Theory Ch.3: Distributions
# =============================================================================
cat(paste(rep("=", 62), collapse = ""), "\n")
cat("SEEING THEORY CH.3: PROBABILITY DISTRIBUTIONS\n")
cat(paste(rep("=", 62), collapse = ""), "\n\n")

# --- Central Limit Theorem ---
cat("--- 3.1 Central Limit Theorem Demonstration ---\n")
cat("sqrt(n)(X_bar - mu)/sigma -> N(0,1) as n -> infinity\n\n")

# Use heavy_drinking_30d as Bernoulli example
heavy_valid <- pumf$heavy_drinking_30d[!is.na(pumf$heavy_drinking_30d)]
mu_heavy <- mean(heavy_valid)
sigma_heavy <- sd(heavy_valid)

n_samples <- 1000
sample_sizes <- c(10, 30, 100, 500)

cat("Population: P(heavy30d) = ", round(mu_heavy, 4), "\n")
cat("Population SD =", round(sigma_heavy, 4), "\n\n")

clt_results <- tibble(
  sample_size = integer(),
  mean_of_means = numeric(),
  sd_of_means = numeric(),
  theoretical_se = numeric(),
  shapiro_p = numeric()
)

for (n in sample_sizes) {
  sample_means <- replicate(
    n_samples,
    mean(sample(heavy_valid, n, replace = TRUE))
  )
  z_scores <- sqrt(n) * (sample_means - mu_heavy) / sigma_heavy

  clt_results <- clt_results %>%
    add_row(
      sample_size = as.integer(n),
      mean_of_means = round(mean(sample_means), 5),
      sd_of_means = round(sd(sample_means), 5),
      theoretical_se = round(sigma_heavy / sqrt(n), 5),
      shapiro_p = round(
        shapiro.test(z_scores[1:min(5000, length(z_scores))])$p.value,
        4
      )
    )
}

cat("CLT Convergence Table:\n")
print(clt_results, n = Inf)

# --- Distribution of Survey Weights ---
cat("\n--- 3.2 Distribution of Survey Weights ---\n")
cat("Testing normality of wtpumf:\n")
wt_sample <- sample(pumf$wtpumf, min(5000, nrow(pumf)))
shapiro_wt <- shapiro.test(wt_sample)
cat(sprintf(
  "Shapiro-Wilk W = %.4f, p = %s\n",
  shapiro_wt$statistic,
  formatC(shapiro_wt$p.value, format = "e", digits = 3)
))

# Summary stats (no extra packages)
wt_mean <- mean(pumf$wtpumf, na.rm = TRUE)
wt_median <- median(pumf$wtpumf, na.rm = TRUE)
wt_sd <- sd(pumf$wtpumf, na.rm = TRUE)

# Sample skewness/kurtosis (moment estimators)
wt_centered <- pumf$wtpumf[!is.na(pumf$wtpumf)] - wt_mean
m2 <- mean(wt_centered^2)
m3 <- mean(wt_centered^3)
m4 <- mean(wt_centered^4)
wt_skew <- m3 / (m2^(3 / 2))
wt_kurt <- m4 / (m2^2)

cat("\nwtpumf summary:\n")
cat(sprintf("  Mean   = %.4f\n", wt_mean))
cat(sprintf("  Median = %.4f\n", wt_median))
cat(sprintf("  SD     = %.4f\n", wt_sd))
cat(sprintf("  Skew   = %.4f\n", wt_skew))
cat(sprintf("  Kurt   = %.4f\n", wt_kurt))

# --- Save results ---
cat("\n--- Saving Phase 4 Initial Results ---\n")

prob_table <- tibble(
  quantity = c(
    "P(Alcohol lifetime)",
    "P(Alcohol past 12 months)",
    "P(Heavy drinking past 30 days)",
    "P(Cannabis past 12 months)",
    "P(Alcohol12m ∩ Cannabis12m)",
    "P(Alcohol12m | Cannabis12m)",
    "P(Cannabis12m | Alcohol12m)",
    "P(Poor/Fair MH)",
    "P(Heavy30d | Poor/Fair MH)",
    "P(Poor/Fair MH | Heavy30d)"
  ),
  estimate = c(
    coef(p_alc_ever)[1],
    coef(p_alc_12m)[1],
    coef(p_heavy_30d)[1],
    coef(p_can_12m)[1],
    coef(p_joint)[1],
    p_alc_given_can,
    p_can_given_alc,
    coef(p_poor_mh)[1],
    coef(p_heavy_given_poor)[1],
    p_poor_given_heavy
  ),
  ci_lower = c(
    p_alc_ever_ci[1, 1],
    p_alc_12m_ci[1, 1],
    p_heavy_30d_ci[1, 1],
    p_can_12m_ci[1, 1],
    p_joint_ci[1, 1],
    NA_real_,
    NA_real_,
    p_poor_mh_ci[1, 1],
    p_heavy_given_poor_ci[1, 1],
    NA_real_
  ),
  ci_upper = c(
    p_alc_ever_ci[1, 2],
    p_alc_12m_ci[1, 2],
    p_heavy_30d_ci[1, 2],
    p_can_12m_ci[1, 2],
    p_joint_ci[1, 2],
    NA_real_,
    NA_real_,
    p_poor_mh_ci[1, 2],
    p_heavy_given_poor_ci[1, 2],
    NA_real_
  )
)

write_csv(prob_table, file.path(output_dir, "probability_estimates.csv"))
write_csv(clt_results, file.path(output_dir, "clt_convergence.csv"))

cat("Saved: probability_estimates.csv, clt_convergence.csv\n")
cat("\n=== PHASE 4 COMPLETE ===\n")
