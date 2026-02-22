#!/usr/bin/env Rscript
# =============================================================================
# 05_bayesian.R — Phase 5b: Bayesian Inference (Seeing Theory Ch.5 style)
# CPADS 2021–2022 PUMF (Cleaned)
# =============================================================================
# Beta-Binomial conjugacy for a binary prevalence indicator.
# NOTE:
#   - Conjugate Beta-Binomial update is demonstrated on UNWEIGHTED counts.
#   - Survey-weighted prevalence + CI are reported for comparison (svymean).
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse) # dplyr, ggplot2, readr, tibble, tidyr
  library(BayesFactor) # contingencyTableBF
  library(survey) # svymean, svydesign
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

cat("=== PHASE 5b: BAYESIAN INFERENCE (Seeing Theory Ch.5) ===\n\n")

# --- Load wrangled data ---
pumf_path <- file.path(wrangled_dir, "cpads_pumf_wrangled.rds")
svy_path <- file.path(wrangled_dir, "cpads_pumf_survey.rds")

if (!file.exists(pumf_path)) {
  stop("Missing file: ", pumf_path)
}
pumf <- readRDS(pumf_path)

# Prefer saved survey design object; otherwise build
if (file.exists(svy_path)) {
  pumf_svy <- readRDS(svy_path)
} else {
  if (!("wtpumf" %in% names(pumf))) {
    stop("Cannot build survey design: missing wtpumf")
  }
  pumf_svy <- survey::svydesign(ids = ~1, weights = ~wtpumf, data = pumf)
}

# ---- Safety checks ----
required <- c(
  "heavy_drinking_30d",
  "gender",
  "age_group",
  "province_region",
  "mental_health",
  "wtpumf"
)
missing <- setdiff(required, names(pumf))
if (length(missing) > 0) {
  stop("Missing required variables in pumf: ", paste(missing, collapse = ", "))
}

# =============================================================================
# Data target for Bayesian update
# Using heavy_drinking_30d as the binary prevalence target
# =============================================================================
binge_data <- dplyr::filter(pumf, !is.na(heavy_drinking_30d))
n_total <- nrow(binge_data)
if (n_total == 0) {
  stop("No non-missing heavy_drinking_30d observations; cannot proceed.")
}

n_binge <- sum(binge_data$heavy_drinking_30d == 1)
n_no <- sum(binge_data$heavy_drinking_30d == 0)
if ((n_binge + n_no) != n_total) {
  # This would indicate values not in {0,1} slipped in
  bad_vals <- binge_data$heavy_drinking_30d[
    !(binge_data$heavy_drinking_30d %in% c(0, 1))
  ]
  stop(
    "heavy_drinking_30d contains non-binary values after NA filtering. Examples: ",
    paste(head(unique(bad_vals), 10), collapse = ", ")
  )
}

p_obs <- n_binge / n_total

cat("Heavy drinking (past 30 days) summary (unweighted):\n")
cat(sprintf(
  "  n = %d, successes = %d, failures = %d\n",
  n_total,
  n_binge,
  n_no
))
cat(sprintf("  Observed proportion = %.4f\n\n", p_obs))

# Survey-weighted prevalence comparison
svy_est <- survey::svymean(~heavy_drinking_30d, pumf_svy, na.rm = TRUE)
svy_ci <- confint(svy_est)

cat("Survey-weighted prevalence comparison:\n")
cat(sprintf(
  "  p̂_wt = %.4f (SE=%.4f, 95%% CI: %.4f, %.4f)\n\n",
  as.numeric(coef(svy_est)[1]),
  as.numeric(SE(svy_est)[1]),
  as.numeric(svy_ci[1, 1]),
  as.numeric(svy_ci[1, 2])
))

# =============================================================================
# 5b.1 — Prior Specification
# =============================================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 1: PRIOR SPECIFICATION\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# Priors for prevalence θ in [0,1]
# Choose priors that make sense for heavy_drinking_30d; keep one informative but not extreme.
priors <- tibble::tibble(
  prior_name = c("Uninformative", "Weakly Informative", "Informative"),
  alpha_prior = c(1, 2, 10),
  beta_prior = c(1, 2, 30)
) %>%
  dplyr::mutate(
    prior_mean = alpha_prior / (alpha_prior + beta_prior),
    prior_var = (alpha_prior * beta_prior) /
      ((alpha_prior + beta_prior)^2 * (alpha_prior + beta_prior + 1)),
    prior_sd = sqrt(prior_var),
    eff_n = alpha_prior + beta_prior
  )

cat("Prior distributions for prevalence θ:\n\n")
for (i in seq_len(nrow(priors))) {
  cat(sprintf(
    "  %s: Beta(%.0f, %.0f)\n",
    priors$prior_name[i],
    priors$alpha_prior[i],
    priors$beta_prior[i]
  ))
  cat(sprintf(
    "    E[θ] = %.4f, SD[θ] = %.4f, Eff. n = %.0f\n",
    priors$prior_mean[i],
    priors$prior_sd[i],
    priors$eff_n[i]
  ))
}
cat("\n")

# =============================================================================
# 5b.2 — Posterior Computation (Conjugate Beta-Binomial)
# =============================================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 2: POSTERIOR COMPUTATION\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("Conjugate update: Beta(alpha0, beta0) + Binomial(n, y) -> Beta(alpha0+y, beta0+n-y)\n\n")

posteriors <- priors %>%
  dplyr::mutate(
    alpha_post = alpha_prior + n_binge,
    beta_post = beta_prior + n_no,
    post_mean = alpha_post / (alpha_post + beta_post),
    post_var = (alpha_post * beta_post) /
      ((alpha_post + beta_post)^2 * (alpha_post + beta_post + 1)),
    post_sd = sqrt(post_var),
    ci_lower = qbeta(0.025, alpha_post, beta_post),
    ci_upper = qbeta(0.975, alpha_post, beta_post),
    hpd_lower = ci_lower,
    hpd_upper = ci_upper
  )

cat("Posterior distributions:\n\n")
for (i in seq_len(nrow(posteriors))) {
  cat(sprintf(
    "  %s prior -> Posterior: Beta(%.0f, %.0f)\n",
    posteriors$prior_name[i],
    posteriors$alpha_post[i],
    posteriors$beta_post[i]
  ))
  cat(sprintf(
    "    E[theta|data] = %.6f, SD = %.6f\n",
    posteriors$post_mean[i],
    posteriors$post_sd[i]
  ))
  cat(sprintf(
    "    95%% CrI: [%.6f, %.6f]\n",
    posteriors$ci_lower[i],
    posteriors$ci_upper[i]
  ))
}
cat("\n")

# Frequentist (unweighted) comparison (Wilson CI from prop.test)
freq_ci <- prop.test(n_binge, n_total, conf.level = 0.95)
cat("Frequentist comparison (Wilson CI; unweighted counts):\n")
cat(sprintf(
  "  p̂ = %.6f, 95%% CI: [%.6f, %.6f]\n\n",
  p_obs,
  freq_ci$conf.int[1],
  freq_ci$conf.int[2]
))

# =============================================================================
# 5b.3 — Prior vs Posterior Plots
# =============================================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 3: PRIOR VS POSTERIOR PLOTS\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

grid_low <- max(0, p_obs - 0.25)
grid_high <- min(1, p_obs + 0.25)
theta_seq <- seq(grid_low, grid_high, length.out = 1200)

plot_data <- vector("list", nrow(posteriors))
for (i in seq_len(nrow(posteriors))) {
  prior_dens <- dbeta(
    theta_seq,
    posteriors$alpha_prior[i],
    posteriors$beta_prior[i]
  )
  post_dens <- dbeta(
    theta_seq,
    posteriors$alpha_post[i],
    posteriors$beta_post[i]
  )
  prior_scaled <- if (max(prior_dens) > 0) {
    prior_dens * (max(post_dens) / max(prior_dens)) * 0.3
  } else {
    prior_dens
  }

  plot_data[[i]] <- tibble::tibble(
    theta = rep(theta_seq, 2),
    density = c(prior_scaled, post_dens),
    type = rep(c("Prior", "Posterior"), each = length(theta_seq)),
    prior_name = posteriors$prior_name[i]
  )
}

plot_df <- dplyr::bind_rows(plot_data) %>%
  dplyr::mutate(prior_name = factor(prior_name, levels = priors$prior_name))

tryCatch(
  {
    p_bay <- ggplot2::ggplot(
      plot_df,
      ggplot2::aes(x = theta, y = density, color = type, linetype = type)
    ) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_vline(
        xintercept = p_obs,
        linetype = "dashed",
        alpha = 0.5
      ) +
      ggplot2::facet_wrap(~prior_name, scales = "free_y", ncol = 1) +
      ggplot2::scale_color_manual(
        values = c("Prior" = "grey60", "Posterior" = "steelblue")
      ) +
      ggplot2::scale_linetype_manual(
        values = c("Prior" = "dashed", "Posterior" = "solid")
      ) +
      ggplot2::labs(
        title = "Bayesian Updating: Heavy Drinking (past 30 days) Prevalence",
        subtitle = sprintf(
          "Data: %d successes / %d trials (unweighted)",
          n_binge,
          n_total
        ),
        x = expression(theta),
        y = "Density",
        color = "",
        linetype = ""
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom")

    ggplot2::ggsave(
      file.path(fig_dir, "bayesian_prior_posterior.pdf"),
      p_bay,
      width = 8,
      height = 10
    )
    ggplot2::ggsave(
      file.path(fig_dir, "bayesian_prior_posterior.png"),
      p_bay,
      width = 8,
      height = 10,
      dpi = 300
    )
    cat("Saved: bayesian_prior_posterior.pdf/.png\n")
  },
  error = function(e) cat("Plot error:", e$message, "\n")
)

# =============================================================================
# 5b.4 — Bayesian Credible vs Frequentist vs Survey-weighted Intervals
# =============================================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 4: CREDIBLE vs CONFIDENCE INTERVALS\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

ci_comparison <- posteriors %>%
  dplyr::select(prior_name, post_mean, ci_lower, ci_upper) %>%
  dplyr::mutate(
    ci_width = ci_upper - ci_lower,
    type = "Bayesian CrI"
  ) %>%
  dplyr::bind_rows(
    tibble::tibble(
      prior_name = "Frequentist (Wilson; unweighted)",
      post_mean = p_obs,
      ci_lower = as.numeric(freq_ci$conf.int[1]),
      ci_upper = as.numeric(freq_ci$conf.int[2]),
      ci_width = as.numeric(freq_ci$conf.int[2] - freq_ci$conf.int[1]),
      type = "Frequentist CI"
    ),
    tibble::tibble(
      prior_name = "Survey-weighted (svymean)",
      post_mean = as.numeric(coef(svy_est)[1]),
      ci_lower = as.numeric(svy_ci[1, 1]),
      ci_upper = as.numeric(svy_ci[1, 2]),
      ci_width = as.numeric(svy_ci[1, 2] - svy_ci[1, 1]),
      type = "Survey-weighted CI"
    )
  )

cat("Comparison table:\n\n")
print(as.data.frame(ci_comparison), row.names = FALSE)
cat("\n")

tryCatch(
  {
    ci_plot_df <- ci_comparison %>%
      dplyr::mutate(label = paste0(prior_name, " (", type, ")"))

    p_ci <- ggplot2::ggplot(
      ci_plot_df,
      ggplot2::aes(
        y = reorder(label, post_mean),
        x = post_mean,
        xmin = ci_lower,
        xmax = ci_upper,
        color = type
      )
    ) +
      ggplot2::geom_pointrange(linewidth = 0.8) +
      ggplot2::geom_vline(
        xintercept = p_obs,
        linetype = "dashed",
        alpha = 0.5
      ) +
      ggplot2::labs(
        title = "Credible Intervals vs Confidence Intervals",
        subtitle = "Heavy drinking (past 30 days) prevalence",
        x = expression(theta),
        y = "",
        color = "Interval Type"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom")

    ggplot2::ggsave(
      file.path(fig_dir, "bayesian_vs_frequentist_ci.pdf"),
      p_ci,
      width = 9,
      height = 5
    )
    ggplot2::ggsave(
      file.path(fig_dir, "bayesian_vs_frequentist_ci.png"),
      p_ci,
      width = 9,
      height = 5,
      dpi = 300
    )
    cat("Saved: bayesian_vs_frequentist_ci.pdf/.png\n\n")
  },
  error = function(e) cat("CI plot error:", e$message, "\n")
)

# =============================================================================
# 5b.5 — Bayes Factors for Key Hypotheses (contingency tables)
# =============================================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 5: BAYES FACTORS\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

bf_results <- tibble::tibble(
  test_name = character(),
  comparison = character(),
  bf10 = numeric(),
  interpretation = character()
)

interpret_bf <- function(bf) {
  if (bf > 100) {
    return("Extreme evidence for H1")
  }
  if (bf > 30) {
    return("Very strong evidence for H1")
  }
  if (bf > 10) {
    return("Strong evidence for H1")
  }
  if (bf > 3) {
    return("Moderate evidence for H1")
  }
  if (bf > 1) {
    return("Anecdotal evidence for H1")
  }
  if (bf > 1 / 3) {
    return("Anecdotal evidence for H0")
  }
  if (bf > 1 / 10) {
    return("Moderate evidence for H0")
  }
  if (bf > 1 / 30) {
    return("Strong evidence for H0")
  }
  if (bf > 1 / 100) {
    return("Very strong evidence for H0")
  }
  "Extreme evidence for H0"
}

run_bf_ct <- function(x, y, test_name, comparison_label) {
  ok <- !is.na(x) & !is.na(y)
  tab <- table(x[ok], y[ok])
  cat("Contingency table:\n")
  print(tab)

  if (nrow(tab) < 2 || ncol(tab) < 2) {
    cat("Skipping BF: table does not have >=2 levels in both dimensions.\n\n")
    return(invisible(NULL))
  }

  tryCatch(
    {
      bf_obj <- BayesFactor::contingencyTableBF(tab, sampleType = "jointMulti")
      bf_val <- BayesFactor::extractBF(bf_obj)$bf
      cat(sprintf("\nBF10 = %.4f — %s\n\n", bf_val, interpret_bf(bf_val)))

      bf_results <<- dplyr::bind_rows(
        bf_results,
        tibble::tibble(
          test_name = test_name,
          comparison = comparison_label,
          bf10 = bf_val,
          interpretation = interpret_bf(bf_val)
        )
      )
    },
    error = function(e) cat("BF error:", e$message, "\n\n")
  )
}

cat("--- 5.1 Heavy drinking by Gender (BayesFactor) ---\n")
run_bf_ct(
  pumf$heavy_drinking_30d,
  pumf$gender,
  "heavy_x_gender",
  "Heavy30d ~ Gender"
)

cat("--- 5.2 Heavy drinking by Age Group (BayesFactor) ---\n")
run_bf_ct(
  pumf$heavy_drinking_30d,
  pumf$age_group,
  "heavy_x_age",
  "Heavy30d ~ Age Group"
)

cat("--- 5.3 Heavy drinking by Region (BayesFactor) ---\n")
run_bf_ct(
  pumf$heavy_drinking_30d,
  pumf$province_region,
  "heavy_x_region",
  "Heavy30d ~ Region"
)

cat("--- 5.4 Heavy drinking by Mental Health (BayesFactor) ---\n")
run_bf_ct(
  pumf$heavy_drinking_30d,
  pumf$mental_health,
  "heavy_x_mh",
  "Heavy30d ~ Mental Health"
)

# =============================================================================
# 5b.6 — Posterior Predictive Check
# =============================================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 6: POSTERIOR PREDICTIVE CHECK\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

alpha_post_unif <- 1 + n_binge
beta_post_unif <- 1 + n_no

n_draws <- 10000
theta_draws <- rbeta(n_draws, alpha_post_unif, beta_post_unif)

n_rep <- 1000
y_rep <- rbinom(n_draws, size = n_rep, prob = theta_draws)
p_rep <- y_rep / n_rep

cat(sprintf("Posterior predictive for n_rep=%d:\n", n_rep))
cat(sprintf("  Mean predicted prevalence: %.4f\n", mean(p_rep)))
cat(sprintf("  SD: %.4f\n", sd(p_rep)))
cat(sprintf(
  "  95%% Prediction interval: [%.4f, %.4f]\n",
  as.numeric(stats::quantile(p_rep, 0.025)),
  as.numeric(stats::quantile(p_rep, 0.975))
))
cat(sprintf("  Observed prevalence (unweighted): %.4f\n\n", p_obs))

ppp <- mean(p_rep >= p_obs)
cat(sprintf("Posterior predictive p-value: %.4f\n", ppp))
cat("(Values near 0.50 indicate good model fit)\n\n")

# =============================================================================
# 5b.7 — Save Initial Results
# =============================================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 7: SAVING RESULTS\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

readr::write_csv(
  posteriors %>%
    dplyr::select(
      prior_name,
      alpha_prior,
      beta_prior,
      prior_mean,
      prior_sd,
      alpha_post,
      beta_post,
      post_mean,
      post_sd,
      ci_lower,
      ci_upper
    ),
  file.path(output_dir, "bayesian_posterior_summaries.csv")
)
cat("Saved: bayesian_posterior_summaries.csv\n")

readr::write_csv(
  bf_results,
  file.path(output_dir, "bayesian_bayes_factors.csv")
)
cat("Saved: bayesian_bayes_factors.csv\n")

readr::write_csv(
  ci_comparison,
  file.path(output_dir, "bayesian_vs_frequentist_ci.csv")
)
cat("Saved: bayesian_vs_frequentist_ci.csv\n")

cat("\n=== PHASE 5b: BAYESIAN INFERENCE COMPLETE ===\n")
