#!/usr/bin/env Rscript
# =============================================================================
# 07_causal_estimators.R — Phase 7: Compare Causal Estimators
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

options(scipen = 999)
set.seed(42)
source("surveillance/lib/config_paths.R")
paths <- get_paths()
init_paths(paths)


wrangled_dir <- paths$wrangled_dir
output_dir <- paths$output_private_dir
cat("=== PHASE 7: CAUSAL ESTIMATOR COMPARISON ===\n\n")

pumf <- readRDS(file.path(wrangled_dir, "cpads_pumf_wrangled.rds"))

covars <- c("age_group", "gender", "province_region", "mental_health", "physical_health")
df <- pumf %>%
  dplyr::select(cannabis_any_use, heavy_drinking_30d, dplyr::all_of(covars), wtpumf) %>%
  tidyr::drop_na()
cat("Analysis sample:", nrow(df), "\n\n")

results <- tibble(method = character(), ate = numeric(), se = numeric(),
                  ci_lower = numeric(), ci_upper = numeric())

# --- 1. Naive (unadjusted) ---
cat("--- 1. Naive Estimator ---\n")
p1_naive <- mean(df$heavy_drinking_30d[df$cannabis_any_use == 1])
p0_naive <- mean(df$heavy_drinking_30d[df$cannabis_any_use == 0])
ate_naive <- p1_naive - p0_naive
n1 <- sum(df$cannabis_any_use == 1)
n0 <- sum(df$cannabis_any_use == 0)
se_naive <- sqrt(p1_naive*(1-p1_naive)/n1 + p0_naive*(1-p0_naive)/n0)
cat(sprintf("  ATE = %.4f (SE=%.4f)\n", ate_naive, se_naive))
results <- results %>% add_row(method = "Naive", ate = ate_naive, se = se_naive,
  ci_lower = ate_naive - 1.96*se_naive, ci_upper = ate_naive + 1.96*se_naive)

# --- 2. Regression-adjusted ---
cat("\n--- 2. Regression-Adjusted ---\n")
fml <- as.formula(paste("heavy_drinking_30d ~ cannabis_any_use +",
                         paste(covars, collapse = " + ")))
mod <- glm(fml, data = df, family = binomial)
ate_reg <- coef(mod)["cannabis_any_use"]
se_reg <- summary(mod)$coefficients["cannabis_any_use", "Std. Error"]
# Marginal effect approximation
avg_pred_1 <- mean(predict(mod, newdata = df %>% dplyr::mutate(cannabis_any_use = 1), type = "response"))
avg_pred_0 <- mean(predict(mod, newdata = df %>% dplyr::mutate(cannabis_any_use = 0), type = "response"))
ate_gcomp <- avg_pred_1 - avg_pred_0
cat(sprintf("  Log-OR = %.4f (SE=%.4f)\n", ate_reg, se_reg))
cat(sprintf("  G-computation ATE = %.4f\n", ate_gcomp))

# Bootstrap SE for g-comp
n_boot <- 500
gcomp_boot <- numeric(n_boot)
for (b in 1:n_boot) {
  idx <- sample(nrow(df), replace = TRUE)
  df_b <- df[idx, ]
  mod_b <- glm(fml, data = df_b, family = binomial)
  p1_b <- mean(predict(mod_b, newdata = df_b %>% dplyr::mutate(cannabis_any_use = 1), type = "response"))
  p0_b <- mean(predict(mod_b, newdata = df_b %>% dplyr::mutate(cannabis_any_use = 0), type = "response"))
  gcomp_boot[b] <- p1_b - p0_b
}
se_gcomp <- sd(gcomp_boot)
cat(sprintf("  G-comp SE (bootstrap) = %.4f\n", se_gcomp))

results <- results %>% add_row(method = "G-computation", ate = ate_gcomp, se = se_gcomp,
  ci_lower = ate_gcomp - 1.96*se_gcomp, ci_upper = ate_gcomp + 1.96*se_gcomp)

# --- 3. IPW ---
cat("\n--- 3. IPW ---\n")
ps_fml <- as.formula(paste("cannabis_any_use ~", paste(covars, collapse = " + ")))
ps_mod <- glm(ps_fml, data = df, family = binomial)
df$ps <- predict(ps_mod, type = "response")
df$ipw <- ifelse(df$cannabis_any_use == 1, 1/df$ps, 1/(1-df$ps))
q01 <- quantile(df$ipw, 0.01); q99 <- quantile(df$ipw, 0.99)
df$ipw_t <- pmin(pmax(df$ipw, q01), q99)

y1_ipw <- weighted.mean(df$heavy_drinking_30d[df$cannabis_any_use == 1],
                         df$ipw_t[df$cannabis_any_use == 1])
y0_ipw <- weighted.mean(df$heavy_drinking_30d[df$cannabis_any_use == 0],
                         df$ipw_t[df$cannabis_any_use == 0])
ate_ipw <- y1_ipw - y0_ipw

ipw_boot <- numeric(n_boot)
for (b in 1:n_boot) {
  idx <- sample(nrow(df), replace = TRUE)
  db <- df[idx, ]
  ps_b <- predict(glm(ps_fml, data = db, family = binomial), type = "response")
  w_b <- ifelse(db$cannabis_any_use == 1, 1/ps_b, 1/(1-ps_b))
  w_b <- pmin(pmax(w_b, q01), q99)
  ipw_boot[b] <- weighted.mean(db$heavy_drinking_30d[db$cannabis_any_use == 1],
                                w_b[db$cannabis_any_use == 1]) -
                 weighted.mean(db$heavy_drinking_30d[db$cannabis_any_use == 0],
                                w_b[db$cannabis_any_use == 0])
}
se_ipw <- sd(ipw_boot)
cat(sprintf("  ATE = %.4f (SE=%.4f)\n", ate_ipw, se_ipw))

results <- results %>% add_row(method = "IPW (trimmed)", ate = ate_ipw, se = se_ipw,
  ci_lower = ate_ipw - 1.96*se_ipw, ci_upper = ate_ipw + 1.96*se_ipw)

# --- 4. AIPW ---
cat("\n--- 4. AIPW ---\n")

# Manual AIPW is deterministic and does not rely on external learners.
ps_aipw <- pmin(pmax(df$ps, 0.01), 0.99)
mu1 <- predict(mod, newdata = df %>% dplyr::mutate(cannabis_any_use = 1), type = "response")
mu0 <- predict(mod, newdata = df %>% dplyr::mutate(cannabis_any_use = 0), type = "response")
aipw_scores <- df$cannabis_any_use / ps_aipw * (df$heavy_drinking_30d - mu1) + mu1 -
               (1 - df$cannabis_any_use) / (1 - ps_aipw) * (df$heavy_drinking_30d - mu0) - mu0
ate_aipw_manual <- mean(aipw_scores)
se_aipw_manual <- sd(aipw_scores) / sqrt(nrow(df))
cat(sprintf("  Manual AIPW ATE (primary) = %.4f (SE=%.4f)\n", ate_aipw_manual, se_aipw_manual))

results <- results %>% add_row(
  method = "AIPW (manual)",
  ate = ate_aipw_manual,
  se = se_aipw_manual,
  ci_lower = ate_aipw_manual - 1.96 * se_aipw_manual,
  ci_upper = ate_aipw_manual + 1.96 * se_aipw_manual
)

# Optional package cross-check (only when dependencies are available).
has_aipw_pkg <- requireNamespace("AIPW", quietly = TRUE)
has_sl_pkg <- requireNamespace("SuperLearner", quietly = TRUE) || requireNamespace("sl3", quietly = TRUE)
run_pkg_crosscheck <- tolower(Sys.getenv("CPADS_ENABLE_AIPW_PACKAGE_CROSSCHECK", "false")) %in% c("1", "true", "yes")

if (run_pkg_crosscheck && has_aipw_pkg && has_sl_pkg) {
  cat("  Attempting AIPW package cross-check...\n")
  tryCatch({
    if (requireNamespace("SuperLearner", quietly = TRUE)) {
      suppressPackageStartupMessages(library(SuperLearner))
    }
    if (requireNamespace("sl3", quietly = TRUE)) {
      suppressPackageStartupMessages(library(sl3))
    }

    aipw_obj <- AIPW::AIPW$new(
      Y = df$heavy_drinking_30d,
      A = df$cannabis_any_use,
      W = model.matrix(~ age_group + gender + province_region + mental_health + physical_health, data = df)[, -1],
      Q.SL.library = "SL.glm",
      g.SL.library = "SL.glm",
      k_split = 5,
      verbose = FALSE
    )
    aipw_obj$fit()
    aipw_res <- tryCatch(aipw_obj$result, error = function(e) NULL)

    if (!is.null(aipw_res) && is.data.frame(aipw_res)) {
      names_lower <- tolower(names(aipw_res))
      idx_estimand <- grep("estimand", names_lower)
      idx_estimate <- grep("^estimate$", names_lower)
      idx_se <- grep("^se$", names_lower)
      if (length(idx_estimand) > 0 && length(idx_estimate) > 0 && length(idx_se) > 0) {
        is_ate <- tolower(as.character(aipw_res[[idx_estimand[1]]])) == "ate"
        if (any(is_ate)) {
          ate_pkg <- as.numeric(aipw_res[[idx_estimate[1]]][which(is_ate)[1]])
          se_pkg <- as.numeric(aipw_res[[idx_se[1]]][which(is_ate)[1]])
          cat(sprintf("  Package AIPW ATE (cross-check) = %.4f (SE=%.4f)\n", ate_pkg, se_pkg))
          results <<- results %>% add_row(
            method = "AIPW (package cross-check)",
            ate = ate_pkg,
            se = se_pkg,
            ci_lower = ate_pkg - 1.96 * se_pkg,
            ci_upper = ate_pkg + 1.96 * se_pkg
          )
        } else {
          cat("  Package AIPW cross-check skipped: ATE row not found in result object.\n")
        }
      } else {
        cat("  Package AIPW cross-check skipped: unrecognized result schema.\n")
      }
    } else {
      cat("  Package AIPW cross-check skipped: result object unavailable.\n")
    }
  }, error = function(e) {
    cat("  Package AIPW cross-check failed:", e$message, "\n")
  })
} else {
  if (!run_pkg_crosscheck) {
    cat("  Package AIPW cross-check skipped (default). Set CPADS_ENABLE_AIPW_PACKAGE_CROSSCHECK=true to enable.\n")
  } else {
    cat("  Package AIPW cross-check skipped: requires AIPW + (SuperLearner or sl3).\n")
  }
}

# --- Summary ---
cat("\n=== CAUSAL ESTIMATOR COMPARISON ===\n")
print(results, n = Inf, width = Inf)

write_csv(results, file.path(output_dir, "causal_estimator_comparison.csv"))
cat("\nSaved: causal_estimator_comparison.csv\n")

cat("\n=== CAUSAL ESTIMATOR COMPARISON COMPLETE ===\n")
