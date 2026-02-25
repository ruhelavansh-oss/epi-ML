# #!/usr/bin/env Rscript
# # =============================================================================
# # 04_distributions.R — Phase 4: Distribution Fitting & Testing
# # =============================================================================
# # Fits probability distributions, tests assumptions, generates QQ plots.
# # =============================================================================

# suppressPackageStartupMessages({
#   library(tidyverse)
#   library(MASS)
# })

# options(scipen = 999)
# set.seed(42)


# wrangled_dir <- "PROJECT_ROOT/data/private/outputs/wrangled"
# output_dir  <- "PROJECT_ROOT/data/private/outputs"
# fig_dir     <- file.path(output_dir, "figures")

# cat("=== DISTRIBUTION FITTING & TESTING ===\n\n")

# df <- readRDS(file.path(wrangled_dir, "data_wrangled.rds"))

# # --- Variables to test ---
# test_vars <- list(
#   weight = df$weight,
#   alc03  = df$alc03[!is.na(df$alc03) & df$alc03 <= 7],
#   alc06  = df$alc06[!is.na(df$alc06) & df$alc06 <= 30]
# )

# dist_results <- tibble(
#   variable = character(),
#   distribution = character(),
#   test = character(),
#   statistic = numeric(),
#   p_value = numeric(),
#   conclusion = character()
# )

# for (vname in names(test_vars)) {
#   vals <- test_vars[[vname]]
#   vals <- vals[!is.na(vals)]
#   cat(sprintf("\n--- %s (n=%d) ---\n", vname, length(vals)))
#   cat(sprintf("  Mean=%.3f, SD=%.3f, Min=%.3f, Max=%.3f\n",
#               mean(vals), sd(vals), min(vals), max(vals)))

#   # Shapiro-Wilk (sample if > 5000)
#   test_sample <- if (length(vals) > 5000) sample(vals, 5000) else vals
#   sw <- shapiro.test(test_sample)
#   cat(sprintf("  Shapiro-Wilk: W=%.4f, p=%s\n",
#               sw$statistic, formatC(sw$p.value, format = "e", digits = 3)))

#   dist_results <- dist_results %>% add_row(
#     variable = vname, distribution = "Normal",
#     test = "Shapiro-Wilk", statistic = sw$statistic,
#     p_value = sw$p.value,
#     conclusion = ifelse(sw$p.value < 0.05, "Reject normality", "Fail to reject")
#   )

#   # KS test against normal
#   ks <- ks.test(test_sample, "pnorm", mean(test_sample), sd(test_sample))
#   cat(sprintf("  KS (normal): D=%.4f, p=%s\n",
#               ks$statistic, formatC(ks$p.value, format = "e", digits = 3)))

#   dist_results <- dist_results %>% add_row(
#     variable = vname, distribution = "Normal",
#     test = "KS", statistic = ks$statistic,
#     p_value = ks$p.value,
#     conclusion = ifelse(ks$p.value < 0.05, "Reject normality", "Fail to reject")
#   )

#   # For positive continuous data, try log-normal
#   if (all(vals > 0)) {
#     log_vals <- log(test_sample)
#     sw_log <- shapiro.test(log_vals)
#     cat(sprintf("  Shapiro-Wilk (log): W=%.4f, p=%s\n",
#                 sw_log$statistic, formatC(sw_log$p.value, format = "e", digits = 3)))

#     dist_results <- dist_results %>% add_row(
#       variable = vname, distribution = "Log-Normal",
#       test = "Shapiro-Wilk on log(X)", statistic = sw_log$statistic,
#       p_value = sw_log$p.value,
#       conclusion = ifelse(sw_log$p.value < 0.05, "Reject log-normality", "Fail to reject")
#     )
#   }
# }

# # --- Binary variables: test Binomial ---
# cat("\n--- Binary Variables: Binomial Distribution ---\n")
# for (bv in c("alcohol_any_use", "alcohol_binge", "cannabis_any_use")) {
#   vals <- df[[bv]][!is.na(df[[bv]])]
#   n_total <- length(vals)
#   n_success <- sum(vals)
#   p_hat <- n_success / n_total
#   cat(sprintf("  %s: n=%d, successes=%d, p_hat=%.4f\n", bv, n_total, n_success, p_hat))
#   cat(sprintf("    E[X] = n*p = %.1f, Var[X] = n*p*(1-p) = %.1f\n",
#               n_total * p_hat, n_total * p_hat * (1 - p_hat)))
#   # 95% CI for p (Wald)
#   se_p <- sqrt(p_hat * (1 - p_hat) / n_total)
#   cat(sprintf("    95%% CI (Wald): (%.4f, %.4f)\n", p_hat - 1.96*se_p, p_hat + 1.96*se_p))
# }

# # --- Save ---
# cat("\n--- Saving Distribution Initial Results ---\n")
# write_csv(dist_results, file.path(output_dir, "distribution_tests.csv"))
# cat("Saved: distribution_tests.csv\n")

# # --- QQ plots ---
# cat("Generating QQ plots...\n")

# pdf(file.path(fig_dir, "qq_plots.pdf"), width = 10, height = 8)
# par(mfrow = c(2, 2))

# qqnorm(sample(df$weight, 5000), main = "QQ: weight vs Normal")
# qqline(sample(df$weight, 5000), col = "red")

# alc03_filtered <- df$alc03[!is.na(df$alc03) & df$alc03 <= 7]
# qqnorm(sample(alc03_filtered, min(5000, length(alc03_filtered))),
#        main = "QQ: alc03 (frequency) vs Normal")
# qqline(sample(alc03_filtered, min(5000, length(alc03_filtered))), col = "red")

# alc06_filtered <- df$alc06[!is.na(df$alc06) & df$alc06 <= 30]
# if (length(alc06_filtered) > 10) {
#   qqnorm(sample(alc06_filtered, min(5000, length(alc06_filtered))),
#          main = "QQ: alc06 (drinks) vs Normal")
#   qqline(sample(alc06_filtered, min(5000, length(alc06_filtered))), col = "red")
# }

# # Log-normal QQ for weights
# qqnorm(log(sample(df$weight, 5000)), main = "QQ: log(weight) vs Normal")
# qqline(log(sample(df$weight, 5000)), col = "red")

# dev.off()
# cat("Saved: figures/qq_plots.pdf\n")

# cat("\n=== DISTRIBUTIONS ANALYSIS COMPLETE ===\n")

#!/usr/bin/env Rscript
# =============================================================================
# 04_distributions.R — Phase 4: Distribution Fitting & Testing
# =============================================================================
# Fits simple distribution diagnostics (normal/log-normal) and generates QQ plots.
# Notes:
# - Shapiro-Wilk is limited to n<=5000 for stability.
# - KS test is approximate when parameters are estimated from the data.
# - data frequency codes (e.g., alc03/alc06) are ordinal category codes, not continuous.
# =============================================================================
#!/usr/bin/env Rscript
# =============================================================================
# 04_distributions.R — Phase 4: Distribution Fitting & Testing
# =============================================================================
# Fits simple distribution diagnostics (normal/log-normal) and generates QQ plots.
# Notes:
# - Shapiro-Wilk is limited to n<=5000 for stability.
# - KS test is approximate when parameters are estimated from the data.
# - data frequency codes (e.g., alc03/alc06) are ordinal category codes, not continuous.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(MASS)
  library(survey) # <-- REQUIRED for svydesign/svymean/SE/confint
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

cat("=== DISTRIBUTION FITTING & TESTING ===\n\n")

df <- readRDS(file.path(wrangled_dir, "data_wrangled.rds"))

# ---- Safety checks for expected variables ----
required <- c(
  "weight",
  "alc03",
  "alc06",
  "alcohol_ever",
  "alcohol_past12m",
  "heavy_drinking_30d",
  "cannabis_any_use"
)
missing <- setdiff(required, names(df))
if (length(missing) > 0) {
  stop("Missing required variables in df: ", paste(missing, collapse = ", "))
}

# --- Variables to test (treat as numeric diagnostics only) ---
test_vars <- list(
  weight = df$weight,
  alc03 = df$alc03[!is.na(df$alc03) & df$alc03 >= 1 & df$alc03 <= 7],
  alc06 = df$alc06[!is.na(df$alc06) & df$alc06 >= 1 & df$alc06 <= 6]
)

dist_results <- tibble(
  variable = character(),
  distribution = character(),
  test = character(),
  statistic = numeric(),
  p_value = numeric(),
  conclusion = character(),
  n = integer()
)

run_sw <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 3) {
    return(NULL)
  }
  xs <- if (length(x) > 5000) sample(x, 5000) else x
  shapiro.test(xs)
}

run_ks_norm <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 3) {
    return(NULL)
  }
  xs <- if (length(x) > 5000) sample(x, 5000) else x
  suppressWarnings(ks.test(xs, "pnorm", mean(xs), sd(xs)))
}

for (vname in names(test_vars)) {
  vals <- test_vars[[vname]]
  vals <- vals[!is.na(vals)]

  cat(sprintf("\n--- %s (n=%d) ---\n", vname, length(vals)))
  cat(sprintf(
    "  Mean=%.3f, SD=%.3f, Min=%.3f, Max=%.3f\n",
    mean(vals),
    sd(vals),
    min(vals),
    max(vals)
  ))

  # Shapiro-Wilk
  sw <- run_sw(vals)
  if (!is.null(sw)) {
    cat(sprintf(
      "  Shapiro-Wilk: W=%.4f, p=%s\n",
      sw$statistic,
      formatC(sw$p.value, format = "e", digits = 3)
    ))

    dist_results <- dist_results %>%
      add_row(
        variable = vname,
        distribution = "Normal",
        test = "Shapiro-Wilk",
        statistic = as.numeric(sw$statistic),
        p_value = sw$p.value,
        conclusion = ifelse(
          sw$p.value < 0.05,
          "Reject normality",
          "Fail to reject"
        ),
        n = length(vals)
      )
  }

  # KS test vs normal (approximate due to estimated parameters)
  ks <- run_ks_norm(vals)
  if (!is.null(ks)) {
    cat(sprintf(
      "  KS (normal, params estimated): D=%.4f, p=%s\n",
      ks$statistic,
      formatC(ks$p.value, format = "e", digits = 3)
    ))

    dist_results <- dist_results %>%
      add_row(
        variable = vname,
        distribution = "Normal",
        test = "KS (params estimated)",
        statistic = as.numeric(ks$statistic),
        p_value = ks$p.value,
        conclusion = ifelse(
          ks$p.value < 0.05,
          "Reject normality",
          "Fail to reject"
        ),
        n = length(vals)
      )
  }

  # Log-normal check for positive data only
  if (length(vals) >= 3 && all(vals > 0)) {
    xs <- if (length(vals) > 5000) sample(vals, 5000) else vals
    sw_log <- shapiro.test(log(xs))
    cat(sprintf(
      "  Shapiro-Wilk (log): W=%.4f, p=%s\n",
      sw_log$statistic,
      formatC(sw_log$p.value, format = "e", digits = 3)
    ))

    dist_results <- dist_results %>%
      add_row(
        variable = vname,
        distribution = "Log-Normal",
        test = "Shapiro-Wilk on log(X)",
        statistic = as.numeric(sw_log$statistic),
        p_value = sw_log$p.value,
        conclusion = ifelse(
          sw_log$p.value < 0.05,
          "Reject log-normality",
          "Fail to reject"
        ),
        n = length(vals)
      )
  }
}

# --- Binary variables: Binomial summaries (unweighted) ---
cat(
  "\n--- Binary Variables: Binomial Distribution (unweighted summaries) ---\n"
)

binary_vars <- c(
  "alcohol_ever",
  "alcohol_past12m",
  "heavy_drinking_30d",
  "cannabis_any_use"
)

binom_summary <- tibble(
  variable = character(),
  n = integer(),
  successes = integer(),
  p_hat = numeric(),
  ci_low_wald = numeric(),
  ci_high_wald = numeric()
)

for (bv in binary_vars) {
  vals <- df[[bv]][!is.na(df[[bv]])]
  n_total <- length(vals)
  n_success <- sum(vals == 1)
  p_hat <- if (n_total > 0) n_success / n_total else NA_real_

  if (is.na(p_hat)) {
    next
  }

  se_p <- sqrt(p_hat * (1 - p_hat) / n_total)
  ci_low <- max(0, p_hat - 1.96 * se_p)
  ci_high <- min(1, p_hat + 1.96 * se_p)

  cat(sprintf(
    "  %s: n=%d, successes=%d, p_hat=%.4f\n",
    bv,
    n_total,
    n_success,
    p_hat
  ))
  cat(sprintf(
    "    E[X] = n*p = %.1f, Var[X] = n*p*(1-p) = %.1f\n",
    n_total * p_hat,
    n_total * p_hat * (1 - p_hat)
  ))
  cat(sprintf("    95%% CI (Wald): (%.4f, %.4f)\n", ci_low, ci_high))

  binom_summary <- binom_summary %>%
    add_row(
      variable = bv,
      n = n_total,
      successes = n_success,
      p_hat = p_hat,
      ci_low_wald = ci_low,
      ci_high_wald = ci_high
    )
}

# --- Save distribution + unweighted binomial outputs ---
cat("\n--- Saving Distribution Initial Results ---\n")
write_csv(dist_results, file.path(output_dir, "distribution_tests.csv"))
write_csv(binom_summary, file.path(output_dir, "binomial_summaries.csv"))
cat("Saved: distribution_tests.csv, binomial_summaries.csv\n")

# --- QQ plots ---
cat("Generating QQ plots...\n")

pdf(file.path(fig_dir, "qq_plots.pdf"), width = 10, height = 8)
par(mfrow = c(2, 2))

# QQ: weights vs Normal
wt_vec <- df$weight[!is.na(df$weight)]
if (length(wt_vec) >= 10) {
  wt_samp <- sample(wt_vec, min(5000, length(wt_vec)))
  qqnorm(wt_samp, main = "QQ: weight vs Normal")
  qqline(wt_samp, col = "red")
} else {
  plot.new()
  title("QQ: weight (insufficient data)")
}

# QQ: alc03 code vs Normal (ordinal code)
alc03_filtered <- df$alc03[
  !is.na(df$alc03) & df$alc03 >= 1 & df$alc03 <= 7
]
if (length(alc03_filtered) >= 10) {
  alc03_samp <- sample(alc03_filtered, min(5000, length(alc03_filtered)))
  qqnorm(alc03_samp, main = "QQ: alc03 (ordinal code) vs Normal")
  qqline(alc03_samp, col = "red")
} else {
  plot.new()
  title("QQ: alc03 (insufficient data)")
}

# QQ: alc06 code vs Normal (ordinal code)
alc06_filtered <- df$alc06[
  !is.na(df$alc06) & df$alc06 >= 1 & df$alc06 <= 6
]
if (length(alc06_filtered) >= 10) {
  alc06_samp <- sample(alc06_filtered, min(5000, length(alc06_filtered)))
  qqnorm(alc06_samp, main = "QQ: alc06 (ordinal code) vs Normal")
  qqline(alc06_samp, col = "red")
} else {
  plot.new()
  title("QQ: alc06 (insufficient data)")
}

# QQ: log(weights) vs Normal (log-normal diagnostic)
wt_pos <- df$weight[!is.na(df$weight) & df$weight > 0]
if (length(wt_pos) >= 10) {
  wt_pos_samp <- sample(wt_pos, min(5000, length(wt_pos)))
  qqnorm(log(wt_pos_samp), main = "QQ: log(weight) vs Normal")
  qqline(log(wt_pos_samp), col = "red")
} else {
  plot.new()
  title("QQ: log(weight) (insufficient data)")
}

dev.off()
cat("Saved: figures/qq_plots.pdf\n")

# --- Binary variables: Binomial summaries (survey-weighted) ---
cat(
  "\n--- Binary Variables: Binomial Distribution (survey-weighted prevalence) ---\n"
)

svy_path <- file.path(wrangled_dir, "data_survey.rds")
if (file.exists(svy_path)) {
  df_svy <- readRDS(svy_path)
} else {
  df_svy <- svydesign(ids = ~1, weights = ~weight, data = df)
}

svy_binom_summary <- tibble(
  variable = character(),
  p_hat = numeric(),
  se = numeric(),
  ci_low = numeric(),
  ci_high = numeric(),
  n_unweighted_nonmissing = integer()
)

for (bv in binary_vars) {
  n_nm <- sum(!is.na(df[[bv]]))

  est <- svymean(as.formula(paste0("~", bv)), df_svy, na.rm = TRUE)
  ci <- confint(est)

  p_hat <- as.numeric(coef(est)[1])
  se <- as.numeric(SE(est)[1])
  ci_l <- as.numeric(ci[1, 1])
  ci_u <- as.numeric(ci[1, 2])

  cat(sprintf(
    "  %s: p̂_wt=%.4f (SE=%.4f, 95%% CI: %.4f, %.4f), n_nonmissing=%d\n",
    bv,
    p_hat,
    se,
    ci_l,
    ci_u,
    n_nm
  ))

  svy_binom_summary <- svy_binom_summary %>%
    add_row(
      variable = bv,
      p_hat = p_hat,
      se = se,
      ci_low = ci_l,
      ci_high = ci_u,
      n_unweighted_nonmissing = n_nm
    )
}

write_csv(
  svy_binom_summary,
  file.path(output_dir, "binomial_summaries_survey_weighted.csv")
)
cat("Saved: binomial_summaries_survey_weighted.csv\n")

cat("\n=== DISTRIBUTIONS ANALYSIS COMPLETE ===\n")

# suppressPackageStartupMessages({
#   library(tidyverse)
#   library(MASS)
# })

# options(scipen = 999)
# set.seed(42)

# # ---- Paths: EVERYTHING under PROJECT_ROOT ----
# data_dir    <- "PROJECT_ROOT"
# output_dir  <- file.path(data_dir, "outputs")
# wrangled_dir <- file.path(output_dir, "wrangled")
# fig_dir     <- file.path(output_dir, "figures")

# dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
# dir.create(wrangled_dir, recursive = TRUE, showWarnings = FALSE)
# dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# cat("=== DISTRIBUTION FITTING & TESTING ===\n\n")

# df <- readRDS(file.path(wrangled_dir, "data_wrangled.rds"))

# # ---- Safety checks for expected variables ----
# required <- c(
#   "weight",
#   "alc03", "alc06",
#   "alcohol_ever", "alcohol_past12m", "heavy_drinking_30d", "cannabis_any_use"
# )
# missing <- setdiff(required, names(df))
# if (length(missing) > 0) {
#   stop("Missing required variables in df: ", paste(missing, collapse = ", "))
# }

# # --- Variables to test (treat as numeric diagnostics only) ---
# # weight: positive continuous weights
# # alc03/alc06: ordinal codes; distribution tests are for demonstration, not strict inference
# test_vars <- list(
#   weight = df$weight,
#   alc03  = df$alc03[!is.na(df$alc03) & df$alc03 >= 1 & df$alc03 <= 7],
#   alc06  = df$alc06[!is.na(df$alc06) & df$alc06 >= 1 & df$alc06 <= 6]
# )

# dist_results <- tibble(
#   variable = character(),
#   distribution = character(),
#   test = character(),
#   statistic = numeric(),
#   p_value = numeric(),
#   conclusion = character(),
#   n = integer()
# )

# run_sw <- function(x) {
#   x <- x[!is.na(x)]
#   if (length(x) < 3) return(NULL)
#   xs <- if (length(x) > 5000) sample(x, 5000) else x
#   shapiro.test(xs)
# }

# run_ks_norm <- function(x) {
#   x <- x[!is.na(x)]
#   if (length(x) < 3) return(NULL)
#   xs <- if (length(x) > 5000) sample(x, 5000) else x
#   suppressWarnings(ks.test(xs, "pnorm", mean(xs), sd(xs)))
# }

# for (vname in names(test_vars)) {
#   vals <- test_vars[[vname]]
#   vals <- vals[!is.na(vals)]

#   cat(sprintf("\n--- %s (n=%d) ---\n", vname, length(vals)))
#   cat(sprintf("  Mean=%.3f, SD=%.3f, Min=%.3f, Max=%.3f\n",
#               mean(vals), sd(vals), min(vals), max(vals)))

#   # Shapiro-Wilk
#   sw <- run_sw(vals)
#   if (!is.null(sw)) {
#     cat(sprintf("  Shapiro-Wilk: W=%.4f, p=%s\n",
#                 sw$statistic, formatC(sw$p.value, format = "e", digits = 3)))

#     dist_results <- dist_results %>% add_row(
#       variable = vname, distribution = "Normal",
#       test = "Shapiro-Wilk", statistic = as.numeric(sw$statistic),
#       p_value = sw$p.value,
#       conclusion = ifelse(sw$p.value < 0.05, "Reject normality", "Fail to reject"),
#       n = length(vals)
#     )
#   }

#   # KS test vs normal (approximate due to estimated parameters)
#   ks <- run_ks_norm(vals)
#   if (!is.null(ks)) {
#     cat(sprintf("  KS (normal, params estimated): D=%.4f, p=%s\n",
#                 ks$statistic, formatC(ks$p.value, format = "e", digits = 3)))

#     dist_results <- dist_results %>% add_row(
#       variable = vname, distribution = "Normal",
#       test = "KS (params estimated)", statistic = as.numeric(ks$statistic),
#       p_value = ks$p.value,
#       conclusion = ifelse(ks$p.value < 0.05, "Reject normality", "Fail to reject"),
#       n = length(vals)
#     )
#   }

#   # Log-normal check for positive data only
#   if (length(vals) >= 3 && all(vals > 0)) {
#     xs <- if (length(vals) > 5000) sample(vals, 5000) else vals
#     sw_log <- shapiro.test(log(xs))
#     cat(sprintf("  Shapiro-Wilk (log): W=%.4f, p=%s\n",
#                 sw_log$statistic, formatC(sw_log$p.value, format = "e", digits = 3)))

#     dist_results <- dist_results %>% add_row(
#       variable = vname, distribution = "Log-Normal",
#       test = "Shapiro-Wilk on log(X)", statistic = as.numeric(sw_log$statistic),
#       p_value = sw_log$p.value,
#       conclusion = ifelse(sw_log$p.value < 0.05, "Reject log-normality", "Fail to reject"),
#       n = length(vals)
#     )
#   }
# }

# # --- Binary variables: Binomial summaries ---
# cat("\n--- Binary Variables: Binomial Distribution (unweighted summaries) ---\n")

# binary_vars <- c("alcohol_ever", "alcohol_past12m", "heavy_drinking_30d", "cannabis_any_use")

# binom_summary <- tibble(
#   variable = character(),
#   n = integer(),
#   successes = integer(),
#   p_hat = numeric(),
#   ci_low_wald = numeric(),
#   ci_high_wald = numeric()
# )

# for (bv in binary_vars) {
#   vals <- df[[bv]][!is.na(df[[bv]])]
#   n_total <- length(vals)
#   n_success <- sum(vals == 1)
#   p_hat <- if (n_total > 0) n_success / n_total else NA_real_

#   if (is.na(p_hat)) next

#   se_p <- sqrt(p_hat * (1 - p_hat) / n_total)
#   ci_low <- max(0, p_hat - 1.96 * se_p)
#   ci_high <- min(1, p_hat + 1.96 * se_p)

#   cat(sprintf("  %s: n=%d, successes=%d, p_hat=%.4f\n", bv, n_total, n_success, p_hat))
#   cat(sprintf("    E[X] = n*p = %.1f, Var[X] = n*p*(1-p) = %.1f\n",
#               n_total * p_hat, n_total * p_hat * (1 - p_hat)))
#   cat(sprintf("    95%% CI (Wald): (%.4f, %.4f)\n", ci_low, ci_high))

#   binom_summary <- binom_summary %>% add_row(
#     variable = bv,
#     n = n_total,
#     successes = n_success,
#     p_hat = p_hat,
#     ci_low_wald = ci_low,
#     ci_high_wald = ci_high
#   )
# }

# # --- Save ---
# cat("\n--- Saving Distribution Initial Results ---\n")
# write_csv(dist_results, file.path(output_dir, "distribution_tests.csv"))
# write_csv(binom_summary, file.path(output_dir, "binomial_summaries.csv"))
# cat("Saved: distribution_tests.csv, binomial_summaries.csv\n")

# # --- QQ plots ---
# cat("Generating QQ plots...\n")

# pdf(file.path(fig_dir, "qq_plots.pdf"), width = 10, height = 8)
# par(mfrow = c(2, 2))

# # QQ: weights vs Normal
# wt_samp <- sample(df$weight[!is.na(df$weight)], min(5000, sum(!is.na(df$weight))))
# qqnorm(wt_samp, main = "QQ: weight vs Normal")
# qqline(wt_samp, col = "red")

# # QQ: alc03 code vs Normal (ordinal code)
# alc03_filtered <- df$alc03[!is.na(df$alc03) & df$alc03 >= 1 & df$alc03 <= 7]
# alc03_samp <- sample(alc03_filtered, min(5000, length(alc03_filtered)))
# qqnorm(alc03_samp, main = "QQ: alc03 (ordinal code) vs Normal")
# qqline(alc03_samp, col = "red")

# # QQ: alc06 code vs Normal (ordinal code)
# alc06_filtered <- df$alc06[!is.na(df$alc06) & df$alc06 >= 1 & df$alc06 <= 6]
# if (length(alc06_filtered) > 10) {
#   alc06_samp <- sample(alc06_filtered, min(5000, length(alc06_filtered)))
#   qqnorm(alc06_samp, main = "QQ: alc06 (ordinal code) vs Normal")
#   qqline(alc06_samp, col = "red")
# } else {
#   plot.new()
#   title("QQ: alc06 (insufficient data after filtering)")
# }

# # QQ: log(weights) vs Normal (log-normal diagnostic)
# wt_pos <- df$weight[!is.na(df$weight) & df$weight > 0]
# wt_pos_samp <- sample(wt_pos, min(5000, length(wt_pos)))
# qqnorm(log(wt_pos_samp), main = "QQ: log(weight) vs Normal")
# qqline(log(wt_pos_samp), col = "red")

# dev.off()
# cat("Saved: figures/qq_plots.pdf\n")

# # --- Binary variables: Binomial summaries (survey-weighted) ---
# cat("\n--- Binary Variables: Binomial Distribution (survey-weighted prevalence) ---\n")

# # Prefer the saved survey design object (consistent with Phase 3)
# svy_path <- file.path(wrangled_dir, "data_survey.rds")
# if (file.exists(svy_path)) {
#   df_svy <- readRDS(svy_path)
# } else {
#   df_svy <- svydesign(ids = ~1, weights = ~weight, data = df)
# }

# svy_binom_summary <- tibble(
#   variable = character(),
#   p_hat = numeric(),
#   ci_low = numeric(),
#   ci_high = numeric(),
#   se = numeric(),
#   n_unweighted_nonmissing = integer()
# )

# for (bv in binary_vars) {
#   # n (unweighted non-missing) for transparency
#   n_nm <- sum(!is.na(df[[bv]]))

#   est <- svymean(as.formula(paste0("~", bv)), df_svy, na.rm = TRUE)
#   ci  <- confint(est)

#   p_hat <- as.numeric(coef(est)[1])
#   se    <- as.numeric(SE(est)[1])
#   ci_l  <- as.numeric(ci[1, 1])
#   ci_u  <- as.numeric(ci[1, 2])

#   cat(sprintf("  %s: p̂_wt=%.4f (SE=%.4f, 95%% CI: %.4f, %.4f), n_nonmissing=%d\n",
#               bv, p_hat, se, ci_l, ci_u, n_nm))

#   svy_binom_summary <- svy_binom_summary %>% add_row(
#     variable = bv,
#     p_hat = p_hat,
#     ci_low = ci_l,
#     ci_high = ci_u,
#     se = se,
#     n_unweighted_nonmissing = n_nm
#   )
# }

# write_csv(svy_binom_summary, file.path(output_dir, "binomial_summaries_survey_weighted.csv"))
# cat("Saved: binomial_summaries_survey_weighted.csv\n")

# cat("\n=== DISTRIBUTIONS ANALYSIS COMPLETE ===\n")
