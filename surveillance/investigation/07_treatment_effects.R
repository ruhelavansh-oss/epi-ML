#!/usr/bin/env Rscript
# =============================================================================
# 07_treatment_effects.R - Phase 7: Treatment Effect Estimation
# Epidemiological Study: Alcohol Use in Canada (CPADS PUMF)
# =============================================================================
# Estimates:
#   - ATE  (Average Treatment Effect)
#   - ATT  (Average Treatment Effect on the Treated)
#   - ATC  (Average Treatment Effect on the Controls)
#   - CATE (Conditional ATE by subgroups)
#   - Sensitivity analysis (Rosenbaum bounds)
# Treatment: cannabis_any_use -> Outcome: heavy_drinking_30d
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(MatchIt)
  library(cobalt)
})

options(scipen = 999)
set.seed(42)
source("surveillance/lib/config_paths.R")
paths <- get_paths()
init_paths(paths)


wrangled_dir <- paths$wrangled_dir
output_dir <- paths$output_private_dir
fig_dir <- paths$figures_dir
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

cat("=== PHASE 7: TREATMENT EFFECT ESTIMATION ===\n\n")

# --- Load data ---
pumf <- readRDS(file.path(wrangled_dir, "cpads_pumf_wrangled.rds"))

covars <- c("age_group", "gender", "province_region", "mental_health", "physical_health")
analysis_vars <- c("cannabis_any_use", "heavy_drinking_30d", covars, "wtpumf")
df <- pumf %>%
  dplyr::select(dplyr::all_of(analysis_vars)) %>%
  tidyr::drop_na()
cat("Analysis sample (complete cases):", nrow(df), "observations\n")
cat("Treatment: cannabis_any_use (1=yes, 0=no)\n")
cat("Outcome:   heavy_drinking_30d (1=yes, 0=no)\n")
cat(sprintf("Treated: %d (%.1f%%)\n", sum(df$cannabis_any_use == 1),
            100 * mean(df$cannabis_any_use == 1)))
cat(sprintf("Control: %d (%.1f%%)\n\n", sum(df$cannabis_any_use == 0),
            100 * mean(df$cannabis_any_use == 0)))

# =============================================================================
# 1. ATE via IPW (Horvitz-Thompson)
# =============================================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 1: ATE - INVERSE PROBABILITY WEIGHTING\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

ps_fml <- as.formula(paste("cannabis_any_use ~", paste(covars, collapse = " + ")))
ps_mod <- glm(ps_fml, data = df, family = binomial)
df$ps <- predict(ps_mod, type = "response")

# Trim propensity scores to [0.01, 0.99]
df$ps_trim <- pmin(pmax(df$ps, 0.01), 0.99)

# ATE weights
df$w_ate <- ifelse(df$cannabis_any_use == 1,
                   1 / df$ps_trim,
                   1 / (1 - df$ps_trim))

# ATT weights
df$w_att <- ifelse(df$cannabis_any_use == 1,
                   1,
                   df$ps_trim / (1 - df$ps_trim))

# ATC weights
df$w_atc <- ifelse(df$cannabis_any_use == 1,
                   (1 - df$ps_trim) / df$ps_trim,
                   1)

# --- Compute ATE ---
y1_ate <- weighted.mean(df$heavy_drinking_30d[df$cannabis_any_use == 1],
                         df$w_ate[df$cannabis_any_use == 1])
y0_ate <- weighted.mean(df$heavy_drinking_30d[df$cannabis_any_use == 0],
                         df$w_ate[df$cannabis_any_use == 0])
ate_ipw <- y1_ate - y0_ate
cat(sprintf("ATE (IPW): E[Y(1)] = %.4f, E[Y(0)] = %.4f, ATE = %.4f\n", y1_ate, y0_ate, ate_ipw))

# --- Compute ATT ---
y1_att <- mean(df$heavy_drinking_30d[df$cannabis_any_use == 1])
y0_att <- weighted.mean(df$heavy_drinking_30d[df$cannabis_any_use == 0],
                         df$w_att[df$cannabis_any_use == 0])
att_ipw <- y1_att - y0_att
cat(sprintf("ATT (IPW): E[Y(1)|T=1] = %.4f, E[Y(0)|T=1] = %.4f, ATT = %.4f\n",
            y1_att, y0_att, att_ipw))

# --- Compute ATC ---
y1_atc <- weighted.mean(df$heavy_drinking_30d[df$cannabis_any_use == 1],
                         df$w_atc[df$cannabis_any_use == 1])
y0_atc <- mean(df$heavy_drinking_30d[df$cannabis_any_use == 0])
atc_ipw <- y1_atc - y0_atc
cat(sprintf("ATC (IPW): E[Y(1)|T=0] = %.4f, E[Y(0)|T=0] = %.4f, ATC = %.4f\n\n",
            y1_atc, y0_atc, atc_ipw))

# --- Bootstrap SEs ---
cat("Computing bootstrap standard errors (500 replicates)...\n")
n_boot <- 500
boot_ate <- boot_att <- boot_atc <- numeric(n_boot)

for (b in 1:n_boot) {
  idx <- sample(nrow(df), replace = TRUE)
  db <- df[idx, ]
  ps_b <- predict(glm(ps_fml, data = db, family = binomial), type = "response")
  ps_b <- pmin(pmax(ps_b, 0.01), 0.99)

  # ATE
  w_ate_b <- ifelse(db$cannabis_any_use == 1, 1/ps_b, 1/(1-ps_b))
  boot_ate[b] <- weighted.mean(db$heavy_drinking_30d[db$cannabis_any_use == 1],
                                w_ate_b[db$cannabis_any_use == 1]) -
                 weighted.mean(db$heavy_drinking_30d[db$cannabis_any_use == 0],
                                w_ate_b[db$cannabis_any_use == 0])

  # ATT
  w_att_b <- ifelse(db$cannabis_any_use == 1, 1, ps_b/(1-ps_b))
  boot_att[b] <- mean(db$heavy_drinking_30d[db$cannabis_any_use == 1]) -
                 weighted.mean(db$heavy_drinking_30d[db$cannabis_any_use == 0],
                                w_att_b[db$cannabis_any_use == 0])

  # ATC
  w_atc_b <- ifelse(db$cannabis_any_use == 1, (1-ps_b)/ps_b, 1)
  boot_atc[b] <- weighted.mean(db$heavy_drinking_30d[db$cannabis_any_use == 1],
                                w_atc_b[db$cannabis_any_use == 1]) -
                 mean(db$heavy_drinking_30d[db$cannabis_any_use == 0])
}

se_ate <- sd(boot_ate)
se_att <- sd(boot_att)
se_atc <- sd(boot_atc)

cat(sprintf("\nATE = %.4f (SE=%.4f), 95%% CI: [%.4f, %.4f]\n",
            ate_ipw, se_ate, ate_ipw - 1.96*se_ate, ate_ipw + 1.96*se_ate))
cat(sprintf("ATT = %.4f (SE=%.4f), 95%% CI: [%.4f, %.4f]\n",
            att_ipw, se_att, att_ipw - 1.96*se_att, att_ipw + 1.96*se_att))
cat(sprintf("ATC = %.4f (SE=%.4f), 95%% CI: [%.4f, %.4f]\n\n",
            atc_ipw, se_atc, atc_ipw - 1.96*se_atc, atc_ipw + 1.96*se_atc))

# Initialize SE variables for matching methods (computed below)
se_att_match <- NA
se_ate_sub <- NA

# =============================================================================
# 2. ATE/ATT via Matching (MatchIt)
# =============================================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 2: ATE/ATT VIA MATCHING\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# Nearest-neighbor matching (1:1) for ATT
cat("--- 2.1 Nearest-Neighbor Matching (ATT) ---\n")
m_att <- matchit(ps_fml, data = df, method = "nearest", estimand = "ATT",
                  distance = "glm", ratio = 1)
cat("Match summary:\n")
print(summary(m_att, un = FALSE))

matched_att <- match.data(m_att)
att_match <- mean(matched_att$heavy_drinking_30d[matched_att$cannabis_any_use == 1]) -
             weighted.mean(matched_att$heavy_drinking_30d[matched_att$cannabis_any_use == 0],
                           matched_att$weights[matched_att$cannabis_any_use == 0])

# Regression-based SE on matched data (Ho et al., 2007; MatchIt recommended)
mod_att_m <- lm(heavy_drinking_30d ~ cannabis_any_use, data = matched_att, weights = weights)
se_att_match <- summary(mod_att_m)$coefficients["cannabis_any_use", "Std. Error"]
cat(sprintf("\nATT (matching) = %.4f (SE=%.4f), 95%% CI: [%.4f, %.4f]\n\n",
            att_match, se_att_match,
            att_match - 1.96*se_att_match, att_match + 1.96*se_att_match))

# Subclassification matching for ATE (full matching exceeds memory at n=31K)
cat("--- 2.2 Subclassification Matching (ATE) ---\n")
tryCatch({
  m_ate <- matchit(ps_fml, data = df, method = "subclass", estimand = "ATE",
                    distance = "glm", subclass = 10)
  cat("Match summary:\n")
  print(summary(m_ate, un = FALSE))

  matched_ate <- match.data(m_ate)
  ate_match <- weighted.mean(matched_ate$heavy_drinking_30d[matched_ate$cannabis_any_use == 1],
                              matched_ate$weights[matched_ate$cannabis_any_use == 1]) -
               weighted.mean(matched_ate$heavy_drinking_30d[matched_ate$cannabis_any_use == 0],
                              matched_ate$weights[matched_ate$cannabis_any_use == 0])

  # Regression-based SE on subclassified data (Ho et al., 2007)
  mod_ate_m <- lm(heavy_drinking_30d ~ cannabis_any_use, data = matched_ate, weights = weights)
  se_ate_sub <<- summary(mod_ate_m)$coefficients["cannabis_any_use", "Std. Error"]
  cat(sprintf("\nATE (subclassification) = %.4f (SE=%.4f), 95%% CI: [%.4f, %.4f]\n\n",
              ate_match, se_ate_sub,
              ate_match - 1.96*se_ate_sub, ate_match + 1.96*se_ate_sub))
}, error = function(e) {
  cat("Matching error:", e$message, "\n")
  ate_match <<- ate_ipw  # Fallback to IPW
  cat("Using IPW estimate as fallback for ATE matching.\n\n")
})

# =============================================================================
# 3. CATE - Subgroup Analysis
# =============================================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 3: CONDITIONAL AVERAGE TREATMENT EFFECTS (CATE)\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cate_results <- tibble(
  subgroup_var   = character(),
  subgroup_level = character(),
  n_treated      = integer(),
  n_control      = integer(),
  cate           = numeric(),
  se             = numeric(),
  ci_lower       = numeric(),
  ci_upper       = numeric()
)

compute_cate <- function(data, var_name) {
  levels <- unique(data[[var_name]])
  for (lev in levels) {
    sub <- data %>% dplyr::filter(.data[[var_name]] == lev)
    n_t <- sum(sub$cannabis_any_use == 1)
    n_c <- sum(sub$cannabis_any_use == 0)
    if (n_t < 20 | n_c < 20) next

    p1 <- mean(sub$heavy_drinking_30d[sub$cannabis_any_use == 1])
    p0 <- mean(sub$heavy_drinking_30d[sub$cannabis_any_use == 0])
    cate <- p1 - p0
    se <- sqrt(p1*(1-p1)/n_t + p0*(1-p0)/n_c)

    cate_results <<- cate_results %>% add_row(
      subgroup_var = var_name, subgroup_level = as.character(lev),
      n_treated = n_t, n_control = n_c,
      cate = cate, se = se,
      ci_lower = cate - 1.96*se, ci_upper = cate + 1.96*se)
  }
}

for (v in covars) compute_cate(df, v)

cat("CATE estimates by subgroup (unadjusted):\n\n")
print(as.data.frame(cate_results), row.names = FALSE)
cat("\n")

# CATE forest plot
tryCatch({
  cate_results$label <- paste0(cate_results$subgroup_var, ": ", cate_results$subgroup_level)

  p_cate <- ggplot(cate_results, aes(x = cate, y = reorder(label, cate),
                                      xmin = ci_lower, xmax = ci_upper)) +
    geom_pointrange(color = "steelblue") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = ate_ipw, linetype = "dotted", color = "red", alpha = 0.5) +
    labs(title = "Conditional Average Treatment Effects (CATE)",
         subtitle = "Cannabis use -> Heavy drinking, by subgroup",
         x = "CATE (Risk Difference)", y = "") +
    theme_minimal()

  ggsave(file.path(fig_dir, "cate_forest_plot.pdf"), p_cate, width = 10, height = 8)
  ggsave(file.path(fig_dir, "cate_forest_plot.png"), p_cate, width = 10, height = 8, dpi = 300)
  cat("Saved: cate_forest_plot.pdf/.png\n\n")
}, error = function(e) cat("CATE plot error:", e$message, "\n\n"))

# =============================================================================
# 4. Sensitivity Analysis - Rosenbaum Bounds
# =============================================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 4: SENSITIVITY ANALYSIS (ROSENBAUM BOUNDS)\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("Rosenbaum bounds assess how sensitive the causal conclusion is\n")
cat("to unmeasured confounding (violation of ignorability).\n\n")
cat("Gamma (G) = odds ratio of differential treatment assignment\n")
cat("due to unmeasured confounder.\n\n")

# Manual Rosenbaum bounds calculation
# For a binary outcome, use McNemar-style bounds on matched pairs
# Approximate using Wilcoxon signed-rank approach

# Use matched data from NN matching
tryCatch({
  matched_pairs <- matched_att %>%
    dplyr::filter(cannabis_any_use == 1) %>%
    dplyr::select(subclass, y_treated = heavy_drinking_30d) %>%
    dplyr::left_join(
      matched_att %>%
        dplyr::filter(cannabis_any_use == 0) %>%
        dplyr::select(subclass, y_control = heavy_drinking_30d),
      by = "subclass"
    ) %>%
    tidyr::drop_na()

  matched_pairs$diff <- matched_pairs$y_treated - matched_pairs$y_control

  # Count discordant pairs
  n_plus  <- sum(matched_pairs$diff > 0)  # treated=1, control=0
  n_minus <- sum(matched_pairs$diff < 0)  # treated=0, control=1
  n_tied  <- sum(matched_pairs$diff == 0)
  n_disc  <- n_plus + n_minus

  cat(sprintf("Matched pairs: %d total, %d discordant\n", nrow(matched_pairs), n_disc))
  cat(sprintf("  Concordant (tied): %d\n", n_tied))
  cat(sprintf("  Treated > Control: %d\n", n_plus))
  cat(sprintf("  Control > Treated: %d\n\n", n_minus))

  if (n_disc > 0) {
    # Rosenbaum bounds: for each Gamma, compute range of p-values
    gammas <- c(1.0, 1.1, 1.2, 1.5, 2.0, 3.0, 5.0)
    sens_results <- tibble(gamma = numeric(), p_lower = numeric(), p_upper = numeric())

    for (g in gammas) {
      # Under Gamma, probability that treated has higher outcome is between
      # 1/(1+Gamma) and Gamma/(1+Gamma)
      p_lo <- 1 / (1 + g)
      p_hi <- g / (1 + g)

      # McNemar test statistic bounds
      # E[T+] under p_lo and p_hi
      e_lo <- n_disc * p_lo
      e_hi <- n_disc * p_hi
      v_lo <- n_disc * p_lo * (1 - p_lo)
      v_hi <- n_disc * p_hi * (1 - p_hi)

      # Standardized test statistics
      z_lo <- (n_plus - e_hi) / sqrt(v_hi)  # most conservative
      z_hi <- (n_plus - e_lo) / sqrt(v_lo)  # most favorable

      p_lower <- pnorm(z_lo, lower.tail = FALSE)
      p_upper <- pnorm(z_hi, lower.tail = FALSE)

      sens_results <- sens_results %>% add_row(
        gamma = g, p_lower = p_lower, p_upper = p_upper)
    }

    cat("Rosenbaum Sensitivity Analysis:\n")
    cat("Gamma = 1 means no unmeasured confounding.\n")
    cat("Larger Gamma = more confounding allowed.\n\n")
    print(as.data.frame(sens_results %>%
      dplyr::mutate(p_lower = formatC(p_lower, format = "e", digits = 3),
             p_upper = formatC(p_upper, format = "e", digits = 3))),
      row.names = FALSE)

    # Find critical Gamma
    critical_gamma <- sens_results %>%
      dplyr::filter(as.numeric(p_upper) > 0.05) %>%
      dplyr::slice(1)
    if (nrow(critical_gamma) > 0) {
      cat(sprintf("\nConclusion becomes non-significant (p > 0.05) at Gamma = %.1f\n",
                  critical_gamma$gamma))
    } else {
      cat("\nConclusion remains significant even at Gamma = 5.0\n")
    }
  }
}, error = function(e) cat("Sensitivity analysis error:", e$message, "\n"))

# =============================================================================
# 5. Summary Table
# =============================================================================
cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 5: SUMMARY OF ALL TREATMENT EFFECTS\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

est_vec <- c(ate_ipw, ate_match, att_ipw, att_match, atc_ipw)
se_vec  <- c(se_ate, se_ate_sub, se_att, se_att_match, se_atc)

summary_table <- tibble(
  estimand = c("ATE", "ATE", "ATT", "ATT", "ATC"),
  method   = c("IPW", "Subclassification", "IPW", "NN Matching", "IPW"),
  estimate = est_vec,
  se       = se_vec,
  ci_lower = est_vec - 1.96 * se_vec,
  ci_upper = est_vec + 1.96 * se_vec
)

cat("Treatment Effect Estimates (Cannabis -> Heavy Drinking):\n\n")
print(as.data.frame(summary_table), row.names = FALSE)

# Save results
write_csv(summary_table, file.path(output_dir, "treatment_effects_summary.csv"))
cat("\nSaved: treatment_effects_summary.csv\n")

write_csv(cate_results %>% dplyr::select(-label),
          file.path(output_dir, "cate_subgroup_estimates.csv"))
cat("Saved: cate_subgroup_estimates.csv\n")

cat("\n=== TREATMENT EFFECT ESTIMATION COMPLETE ===\n")
