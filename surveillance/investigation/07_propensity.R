#!/usr/bin/env Rscript
# =============================================================================
# 07_propensity.R — Phase 7: Propensity Scores and IPW
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(cobalt)
  library(MatchIt)
})

options(scipen = 999)
set.seed(42)
source("surveillance/lib/config_paths.R")
paths <- get_paths()
init_paths(paths)


wrangled_dir <- paths$wrangled_dir
output_dir <- paths$output_private_dir
fig_dir <- paths$figures_dir
cat("=== PHASE 7: PROPENSITY SCORES & IPW ===\n\n")

pumf <- readRDS(file.path(wrangled_dir, "cpads_pumf_wrangled.rds"))

# Complete cases for causal analysis
covars <- c("age_group", "gender", "province_region", "mental_health", "physical_health")
analysis_vars <- c("cannabis_any_use", "heavy_drinking_30d", covars, "wtpumf")
df <- pumf %>%
  dplyr::select(dplyr::all_of(analysis_vars)) %>%
  tidyr::drop_na()
cat("Analysis sample (complete cases):", nrow(df), "observations\n\n")

# --- 1. Propensity Score Estimation ---
cat("--- 1. Propensity Score Model ---\n")
ps_formula <- as.formula(paste("cannabis_any_use ~",
                                paste(covars, collapse = " + ")))
ps_model <- glm(ps_formula, data = df, family = binomial)
cat("PS model summary:\n")
print(summary(ps_model))

df$ps <- predict(ps_model, type = "response")
cat(sprintf("\nPS distribution: mean=%.4f, sd=%.4f, range=[%.4f, %.4f]\n",
            mean(df$ps), sd(df$ps), min(df$ps), max(df$ps)))

# --- 2. IPW Weights ---
cat("\n--- 2. IPW Weights ---\n")
df$ipw <- ifelse(df$cannabis_any_use == 1,
                 1 / df$ps,
                 1 / (1 - df$ps))

# Trim extreme weights at 1st and 99th percentile
q01 <- quantile(df$ipw, 0.01)
q99 <- quantile(df$ipw, 0.99)
df$ipw_trimmed <- pmin(pmax(df$ipw, q01), q99)
cat(sprintf("IPW: mean=%.3f, sd=%.3f, range=[%.3f, %.3f]\n",
            mean(df$ipw), sd(df$ipw), min(df$ipw), max(df$ipw)))
cat(sprintf("IPW trimmed (1-99%%): range=[%.3f, %.3f]\n", q01, q99))

# --- 3. Balance Diagnostics ---
cat("\n--- 3. Balance Diagnostics ---\n")
bal <- bal.tab(ps_formula, data = df, weights = df$ipw_trimmed,
               method = "weighting", estimand = "ATE",
               un = TRUE, disp.v.ratio = TRUE)
print(bal)

# Love plot
tryCatch({
  pdf(file.path(fig_dir, "balance_plot.pdf"), width = 8, height = 6)
  love.plot(ps_formula, data = df, weights = df$ipw_trimmed,
            method = "weighting", estimand = "ATE",
            binary = "std", thresholds = c(m = 0.1),
            title = "Covariate Balance: Cannabis Use (IPW)")
  dev.off()
  cat("Saved: figures/balance_plot.pdf\n")
}, error = function(e) {
  cat("Balance plot error:", e$message, "\n")
  tryCatch(dev.off(), error = function(x) NULL)
})

# --- 4. IPW-weighted ATE ---
cat("\n--- 4. IPW-Weighted ATE ---\n")
# ATE = weighted mean(Y|T=1) - weighted mean(Y|T=0)
treated <- df %>% dplyr::filter(cannabis_any_use == 1)
control <- df %>% dplyr::filter(cannabis_any_use == 0)

y1_ipw <- weighted.mean(treated$heavy_drinking_30d, treated$ipw_trimmed)
y0_ipw <- weighted.mean(control$heavy_drinking_30d, control$ipw_trimmed)
ate_ipw <- y1_ipw - y0_ipw

cat(sprintf("E[Y(1)] (IPW) = %.4f\n", y1_ipw))
cat(sprintf("E[Y(0)] (IPW) = %.4f\n", y0_ipw))
cat(sprintf("ATE (IPW)     = %.4f\n", ate_ipw))

# Bootstrap SE
n_boot <- 500
ate_boot <- numeric(n_boot)
for (b in 1:n_boot) {
  idx <- sample(nrow(df), replace = TRUE)
  df_b <- df[idx, ]
  ps_b <- predict(glm(ps_formula, data = df_b, family = binomial), type = "response")
  ipw_b <- ifelse(df_b$cannabis_any_use == 1, 1/ps_b, 1/(1-ps_b))
  ipw_b <- pmin(pmax(ipw_b, q01), q99)
  t_b <- df_b %>% dplyr::filter(cannabis_any_use == 1)
  c_b <- df_b %>% dplyr::filter(cannabis_any_use == 0)
  ate_boot[b] <- weighted.mean(t_b$heavy_drinking_30d, ipw_b[df_b$cannabis_any_use == 1]) -
                 weighted.mean(c_b$heavy_drinking_30d, ipw_b[df_b$cannabis_any_use == 0])
}
se_ipw <- sd(ate_boot)
ci_lo <- ate_ipw - 1.96 * se_ipw
ci_hi <- ate_ipw + 1.96 * se_ipw

cat(sprintf("SE (bootstrap) = %.4f\n", se_ipw))
cat(sprintf("95%% CI: [%.4f, %.4f]\n", ci_lo, ci_hi))

# Save
ipw_results <- tibble(
  estimand = "ATE",
  method = "IPW (trimmed)",
  estimate = ate_ipw,
  se = se_ipw,
  ci_lower = ci_lo,
  ci_upper = ci_hi,
  n = nrow(df)
)
write_csv(ipw_results, file.path(output_dir, "ipw_results.csv"))

cat("\n=== PROPENSITY SCORE ANALYSIS COMPLETE ===\n")
