#!/usr/bin/env Rscript
# =============================================================================
# 05_power_design.R — Phase 5: Power Design (Corrected for CPADS PUMF workflow)
# =============================================================================
# Uses wrangled CPADS PUMF outputs under PROJECT_ROOT/data/private/outputs.
# Outcome: heavy_drinking_30d (binary 0/1; derived in Phase 3 wrangling).
#
# Notes:
#   - pwr::* assumes simple random sampling (SRS).
#   - For survey data, we report both:
#       (a) naive SRS power using n_unweighted
#       (b) DEFF-adjusted power using n_eff = n_unweighted / DEFF
#   - We avoid hard-coded null prevalence (e.g., 0.90). Instead:
#       * one-proportion: power vs a user-relevant null grid (e.g., 0.05–0.30)
#       * two-proportion: uses observed gender prevalences (weighted)
#       * chi-square: power for small/medium effects + observed w from contingency table
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(survey)
  library(pwr)
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

cat("=== PHASE 5: POWER ANALYSIS (CPADS PUMF) ===\n\n")

# --- Load wrangled data + survey design ---
pumf_path <- file.path(wrangled_dir, "cpads_pumf_wrangled.rds")
svy_path  <- file.path(wrangled_dir, "cpads_pumf_survey.rds")

if (!file.exists(pumf_path)) stop("Missing file: ", pumf_path)
pumf <- readRDS(pumf_path)

if (file.exists(svy_path)) {
  pumf_svy <- readRDS(svy_path)
} else {
  if (!("wtpumf" %in% names(pumf))) stop("Cannot build survey design: missing wtpumf")
  pumf_svy <- survey::svydesign(ids = ~1, weights = ~wtpumf, data = pumf)
}

# ---- Safety checks for expected variables ----
required <- c("wtpumf", "heavy_drinking_30d", "gender", "age_group", "province_region", "mental_health")
missing <- setdiff(required, names(pumf))
if (length(missing) > 0) stop("Missing required variables in pumf: ", paste(missing, collapse = ", "))

# =============================================================================
# Helpers
# =============================================================================
fmt_p <- function(p) formatC(p, format = "e", digits = 4)

cohens_h <- function(p1, p2) {
  2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))
}

safe_deff <- function(svy_mean_obj) {
  # Try to extract design effect (DEFF). If unavailable, return NA.
  d <- tryCatch(survey::deff(svy_mean_obj), error = function(e) NA)
  if (is.null(d)) return(NA_real_)
  if (is.matrix(d)) return(as.numeric(d[1, 1]))
  if (is.numeric(d)) return(as.numeric(d[1]))
  NA_real_
}

eff_n <- function(n, deff_val) {
  if (is.na(deff_val) || deff_val <= 0) return(NA_real_)
  n / deff_val
}

# =============================================================================
# 0) Observed prevalence (unweighted + survey-weighted) and DEFF
# =============================================================================
cat("--- 0. Observed prevalence for heavy_drinking_30d ---\n")

n_total <- sum(!is.na(pumf$heavy_drinking_30d))
y_total <- sum(pumf$heavy_drinking_30d == 1, na.rm = TRUE)
p_unw   <- y_total / n_total

svy_est <- survey::svymean(~heavy_drinking_30d, pumf_svy, na.rm = TRUE, deff = "replace")
svy_ci  <- confint(svy_est)

p_wt <- as.numeric(coef(svy_est)[1])
se_wt <- as.numeric(SE(svy_est)[1])
ci_wt_l <- as.numeric(svy_ci[1, 1])
ci_wt_u <- as.numeric(svy_ci[1, 2])

deff_val <- safe_deff(svy_est)
n_eff_total <- eff_n(n_total, deff_val)

cat(sprintf("  Unweighted: n=%d, y=%d, p=%.4f\n", n_total, y_total, p_unw))
cat(sprintf("  Survey-weighted: p_wt=%.4f (SE=%.4f, 95%% CI: %.4f, %.4f)\n", p_wt, se_wt, ci_wt_l, ci_wt_u))
cat(sprintf("  DEFF (if available): %s\n",
            ifelse(is.na(deff_val), "NA (survey::deff unavailable for this object/version)", sprintf("%.4f", deff_val))))
cat(sprintf("  Effective n (n/DEFF): %s\n\n",
            ifelse(is.na(n_eff_total), "NA", sprintf("%.1f", n_eff_total))))

# =============================================================================
# 1) One-proportion power: detect difference from a null p0 (grid)
# =============================================================================
cat("--- 1. One-Proportion Test Power (grid of null prevalences p0) ---\n")
cat("Power to detect difference between observed p (survey-weighted) and p0.\n")
cat("This avoids hard-coded, wrong nulls (e.g., 0.90).\n\n")

# Choose a reasonable null grid around plausible prevalence values.
# You can edit this grid to your specific hypothesis (e.g., literature benchmark).
p0_grid <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30)

oneprop_tbl <- tibble(
  p0 = p0_grid
) %>%
  dplyr::mutate(
    p_obs = p_wt,
    h = cohens_h(p_obs, p0),
    n = n_total,
    n_eff = n_eff_total,
    power_srs = map_dbl(h, ~ pwr.p.test(h = .x, n = n_total, sig.level = 0.05, alternative = "two.sided")$power),
    power_deff = ifelse(
      is.na(n_eff_total),
      NA_real_,
      map_dbl(h, ~ pwr.p.test(h = .x, n = n_eff_total, sig.level = 0.05, alternative = "two.sided")$power)
    )
  )

print(oneprop_tbl %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round(.x, 6))), n = Inf)
cat("\n")

# =============================================================================
# 2) Two-proportion power: Gender (uses observed weighted prevalences)
# =============================================================================
cat("--- 2. Two-Proportion Test Power (Gender; observed weighted prevalences) ---\n")

# Get weighted prevalence by gender
prev_by_gender <- survey::svyby(~heavy_drinking_30d, ~gender, pumf_svy, survey::svymean, na.rm = TRUE)
df_g <- as.data.frame(prev_by_gender)

if (!("gender" %in% names(df_g))) stop("Unexpected svyby output: missing gender column.")
# Identify prevalence column robustly (usually "heavy_drinking_30d")
prev_col <- if ("heavy_drinking_30d" %in% names(df_g)) "heavy_drinking_30d" else {
  # fallback: unique numeric column other than SE-like and gender
  cand <- setdiff(names(df_g), "gender")
  cand <- cand[!grepl("^se\\.|^SE\\.|\\.se$|^var\\.|\\.var$", cand)]
  num_cand <- cand[vapply(df_g[cand], is.numeric, logical(1))]
  if (length(num_cand) != 1) stop("Cannot uniquely identify prevalence column in gender svyby output: ", paste(names(df_g), collapse = ", "))
  num_cand[1]
}

gender_levels <- as.character(df_g$gender)
p_gender <- setNames(as.numeric(df_g[[prev_col]]), gender_levels)

# Unweighted n per gender (nonmissing outcome and gender)
ok_g <- !is.na(pumf$heavy_drinking_30d) & !is.na(pumf$gender)
n_by_gender <- table(pumf$gender[ok_g])
n_by_gender <- n_by_gender[names(p_gender)]

two_prop_rows <- list()
gender_pairs <- combn(names(p_gender), 2, simplify = FALSE)

for (pair in gender_pairs) {
  g1 <- pair[1]; g2 <- pair[2]
  p1 <- as.numeric(p_gender[g1])
  p2 <- as.numeric(p_gender[g2])

  # If any prevalence is NA, skip
  if (is.na(p1) || is.na(p2)) next

  h <- cohens_h(p1, p2)

  # SRS approximation: use group-specific unweighted n (not n_total/2)
  n1 <- as.numeric(n_by_gender[g1])
  n2 <- as.numeric(n_by_gender[g2])
  if (is.na(n1) || is.na(n2) || n1 < 2 || n2 < 2) next

  # pwr.2p.test expects equal n per group; use the smaller group size for a conservative bound
  n_eq <- min(n1, n2)

  pow_srs <- pwr.2p.test(h = h, n = n_eq, sig.level = 0.05, alternative = "two.sided")$power

  # DEFF-adjusted: use overall DEFF as a conservative proxy if available
  n_eq_eff <- if (!is.na(deff_val)) n_eq / deff_val else NA_real_
  pow_deff <- if (!is.na(n_eq_eff)) pwr.2p.test(h = h, n = n_eq_eff, sig.level = 0.05, alternative = "two.sided")$power else NA_real_

  two_prop_rows[[length(two_prop_rows) + 1]] <- tibble(
    group1 = g1, group2 = g2,
    p1 = p1, p2 = p2,
    h = h,
    n1 = n1, n2 = n2, n_eq = n_eq,
    power_srs = pow_srs,
    n_eq_eff = n_eq_eff,
    power_deff = pow_deff
  )
}

two_prop_tbl <- bind_rows(two_prop_rows)

if (nrow(two_prop_tbl) == 0) {
  cat("  No valid gender pairs found for two-proportion power.\n\n")
} else {
  print(two_prop_tbl %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round(.x, 6))), n = Inf)
  cat("\n")
}

# =============================================================================
# 3) Chi-squared association power: small/medium + observed Cohen's w
# =============================================================================
cat("--- 3. Chi-Squared Test Power (association; unweighted) ---\n")
cat("pwr.chisq.test is for Pearson chi-square under SRS.\n")
cat("We report power for small/medium w + observed w from the contingency table.\n\n")

cohens_w_from_tab <- function(tab) {
  if (any(tab < 0)) stop("Invalid table: negative counts.")
  if (sum(tab) == 0) return(NA_real_)
  if (nrow(tab) < 2 || ncol(tab) < 2) return(NA_real_)
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
  sqrt(as.numeric(chi$statistic) / sum(tab))
}

# Example: Heavy drinking x Gender
tab_hxg <- table(pumf$heavy_drinking_30d, pumf$gender, useNA = "no")
w_obs_gender <- cohens_w_from_tab(tab_hxg)
df_gender <- (nrow(tab_hxg) - 1) * (ncol(tab_hxg) - 1)

w_small <- 0.10
w_med   <- 0.30

pwr_chi_small <- pwr.chisq.test(w = w_small, N = n_total, df = df_gender, sig.level = 0.05)$power
pwr_chi_med   <- pwr.chisq.test(w = w_med,   N = n_total, df = df_gender, sig.level = 0.05)$power

cat(sprintf("  Heavy x Gender df=%d, N=%d\n", df_gender, n_total))
cat(sprintf("  Power (w=0.10 small): %.4f\n", pwr_chi_small))
cat(sprintf("  Power (w=0.30 medium): %.4f\n", pwr_chi_med))

if (!is.na(w_obs_gender)) {
  pwr_chi_obs <- pwr.chisq.test(w = w_obs_gender, N = n_total, df = df_gender, sig.level = 0.05)$power
  cat(sprintf("  Observed Cohen's w (unweighted): %.4f -> implied power: %.4f\n\n", w_obs_gender, pwr_chi_obs))
} else {
  cat("  Observed w: NA (table not suitable)\n\n")
}

# =============================================================================
# 4) Minimum Detectable Effects (MDE) at 80% power
# =============================================================================
cat("--- 4. Minimum Detectable Effect (MDE) at 80% power ---\n")

mde_1p_srs <- pwr.p.test(n = n_total, sig.level = 0.05, power = 0.80, alternative = "two.sided")$h
mde_1p_eff <- if (!is.na(n_eff_total)) pwr.p.test(n = n_eff_total, sig.level = 0.05, power = 0.80, alternative = "two.sided")$h else NA_real_

cat(sprintf("  One-proportion MDE (SRS): h = %.6f (n=%d)\n", mde_1p_srs, n_total))
cat(sprintf("  One-proportion MDE (DEFF-adjusted): h = %s (n_eff=%s)\n",
            ifelse(is.na(mde_1p_eff), "NA", sprintf("%.6f", mde_1p_eff)),
            ifelse(is.na(n_eff_total), "NA", sprintf("%.1f", n_eff_total))))

# For two-proportion, choose a reference pair if available: largest two groups by n
if (length(n_by_gender) >= 2) {
  n_by_gender_sorted <- sort(n_by_gender, decreasing = TRUE)
  gA <- names(n_by_gender_sorted)[1]
  gB <- names(n_by_gender_sorted)[2]
  n_eq_ref <- min(as.numeric(n_by_gender_sorted[1]), as.numeric(n_by_gender_sorted[2]))

  if (is.finite(n_eq_ref) && n_eq_ref > 1) {
    mde_2p_srs <- pwr.2p.test(n = n_eq_ref, sig.level = 0.05, power = 0.80, alternative = "two.sided")$h
    n_eq_ref_eff <- if (!is.na(deff_val)) n_eq_ref / deff_val else NA_real_
    mde_2p_eff <- if (!is.na(n_eq_ref_eff)) pwr.2p.test(n = n_eq_ref_eff, sig.level = 0.05, power = 0.80, alternative = "two.sided")$h else NA_real_

    cat(sprintf("  Two-proportion MDE (SRS): h = %.6f (n_per_group=%d; %s vs %s)\n",
                mde_2p_srs, n_eq_ref, gA, gB))
    cat(sprintf("  Two-proportion MDE (DEFF-adjusted): h = %s (n_eff_per_group=%s)\n",
                ifelse(is.na(mde_2p_eff), "NA", sprintf("%.6f", mde_2p_eff)),
                ifelse(is.na(n_eq_ref_eff), "NA", sprintf("%.1f", n_eq_ref_eff))))
  } else {
    cat("  Two-proportion MDE: skipped (insufficient n per group)\n")
  }
} else {
  cat("  Two-proportion MDE: skipped (gender groups insufficient)\n")
}

mde_chi_srs <- pwr.chisq.test(N = n_total, df = df_gender, sig.level = 0.05, power = 0.80)$w
cat(sprintf("  Chi-square MDE (SRS): w = %.6f (df=%d, N=%d)\n\n", mde_chi_srs, df_gender, n_total))

# =============================================================================
# 5) Power curves (SRS + DEFF-adjusted)
# =============================================================================
cat("--- 5. Generating Power Curves ---\n")

effect_grid <- seq(0.01, 0.30, by = 0.005)

power_curves <- tibble(
  effect_size = effect_grid,
  power_1p_srs = map_dbl(effect_grid, ~ pwr.p.test(h = .x, n = n_total, sig.level = 0.05)$power),
  power_2p_srs = map_dbl(effect_grid, ~ pwr.2p.test(h = .x, n = floor(n_total/2), sig.level = 0.05)$power),
  power_chi_srs = map_dbl(effect_grid, ~ pwr.chisq.test(w = .x, N = n_total, df = df_gender, sig.level = 0.05)$power)
)

if (!is.na(n_eff_total)) {
  power_curves <- power_curves %>%
    dplyr::mutate(
      power_1p_deff = map_dbl(effect_grid, ~ pwr.p.test(h = .x, n = n_eff_total, sig.level = 0.05)$power),
      power_2p_deff = map_dbl(effect_grid, ~ pwr.2p.test(h = .x, n = floor(n_eff_total/2), sig.level = 0.05)$power),
      power_chi_deff = map_dbl(effect_grid, ~ pwr.chisq.test(w = .x, N = n_eff_total, df = df_gender, sig.level = 0.05)$power)
    )
}

# Plot
pdf(file.path(fig_dir, "power_curves.pdf"), width = 10, height = 6)

plot_df <- power_curves %>%
  tidyr::pivot_longer(-effect_size, names_to = "curve", values_to = "power") %>%
  dplyr::mutate(
    curve = dplyr::recode(curve,
      power_1p_srs = "One-proportion (SRS)",
      power_2p_srs = "Two-proportion (SRS)",
      power_chi_srs = paste0("Chi-square (SRS; df=", df_gender, ")"),
      power_1p_deff = "One-proportion (DEFF-adjusted)",
      power_2p_deff = "Two-proportion (DEFF-adjusted)",
      power_chi_deff = paste0("Chi-square (DEFF-adjusted; df=", df_gender, ")")
    )
  )

ggplot(plot_df, aes(x = effect_size, y = power, color = curve)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0.80, linetype = "dashed", alpha = 0.5) +
  labs(
    title = sprintf("Power Curves: CPADS PUMF heavy_drinking_30d (n_nonmissing=%d)", n_total),
    subtitle = ifelse(is.na(deff_val),
                      "SRS power shown (DEFF unavailable in this setup)",
                      sprintf("SRS and DEFF-adjusted power shown (DEFF=%.3f; n_eff=%.1f)", deff_val, n_eff_total)),
    x = "Effect size (Cohen's h or w)",
    y = "Power",
    color = "Curve"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1))

dev.off()
cat("Saved: figures/power_curves.pdf\n\n")

# =============================================================================
# 6) Save outputs
# =============================================================================
cat("--- 6. Saving Initial Results ---\n")

write_csv(oneprop_tbl, file.path(output_dir, "power_one_proportion_grid.csv"))
cat("Saved: power_one_proportion_grid.csv\n")

if (exists("two_prop_tbl") && nrow(two_prop_tbl) > 0) {
  write_csv(two_prop_tbl, file.path(output_dir, "power_two_proportion_gender.csv"))
  cat("Saved: power_two_proportion_gender.csv\n")
}

power_summary <- tibble(
  metric = c(
    "Observed prevalence (unweighted)",
    "Observed prevalence (survey-weighted)",
    "DEFF (svymean; if available)",
    "n_nonmissing",
    "n_eff (if available)",
    "MDE one-proportion h (SRS, 80%)",
    "MDE one-proportion h (DEFF, 80%)",
    "MDE chi-square w (SRS, 80%)"
  ),
  value = c(
    p_unw,
    p_wt,
    deff_val,
    n_total,
    n_eff_total,
    mde_1p_srs,
    mde_1p_eff,
    mde_chi_srs
  )
)

write_csv(power_summary, file.path(output_dir, "power_analysis_summary.csv"))
cat("Saved: power_analysis_summary.csv\n")

cat("\n=== POWER ANALYSIS COMPLETE ===\n")
