# #!/usr/bin/env Rscript
# # =============================================================================
# # 05_frequentist.R — Phase 5a: Frequentist Inference
# # Epidemiological Study: Alcohol Use in Canada (data dataset)
# # =============================================================================
# # Following Seeing Theory Chapter 4 (Frequentist Inference):
# #   - Survey-weighted confidence intervals for binge drinking prevalence
# #   - Hypothesis tests for demographic differences
# #   - Effect sizes (Cohen's h for proportions)
# #   - Multiple comparison corrections (Bonferroni, FDR/BH)
# # =============================================================================

# suppressPackageStartupMessages({
#   library(tidyverse)
#   library(survey)
#   library(effectsize)
# })

# options(scipen = 999)
# set.seed(42)


# wrangled_dir <- "PROJECT_ROOT/data/private/outputs/wrangled"
# output_dir  <- "PROJECT_ROOT/data/private/outputs"
# fig_dir     <- file.path(output_dir, "figures")

# dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# cat("=== PHASE 5a: FREQUENTIST INFERENCE (Seeing Theory Ch.4) ===\n\n")

# # --- Load wrangled data ---
# df     <- readRDS(file.path(wrangled_dir, "data_wrangled.rds"))
# df_svy <- readRDS(file.path(wrangled_dir, "data_survey.rds"))
# cads_alc <- readRDS(file.path(wrangled_dir, "cads_alcohol.rds"))

# cat("data dataset:", nrow(df), "observations\n")
# cat("CADS Alcohol:", nrow(cads_alc), "aggregate rows\n")
# cat("Primary outcome: alcohol_binge (binary: 0/1)\n")
# cat("Note: alcohol_any_use is 100% = 1 among all respondents,\n")
# cat("      so binge drinking is used as the primary binary.\n\n")

# # =============================================================================
# # 5a.1 — Point Estimates and Confidence Intervals
# # =============================================================================
# cat(paste(rep("=", 70), collapse = ""), "\n")
# cat("SECTION 1: SURVEY-WEIGHTED CONFIDENCE INTERVALS\n")
# cat(paste(rep("=", 70), collapse = ""), "\n\n")

# # --- Overall binge prevalence ---
# cat("--- 1.1 Overall Binge Drinking Prevalence ---\n")
# p_binge_overall <- svymean(~alcohol_binge, df_svy, na.rm = TRUE)
# ci_binge_overall <- confint(p_binge_overall)
# n_binge_valid <- sum(!is.na(df$alcohol_binge))

# cat(sprintf("  Weighted prevalence : %.4f (%.2f%%)\n",
#             coef(p_binge_overall), coef(p_binge_overall) * 100))
# cat(sprintf("  Standard error      : %.4f\n", SE(p_binge_overall)))
# cat(sprintf("  95%% CI             : [%.4f, %.4f]\n",
#             ci_binge_overall[1], ci_binge_overall[2]))
# cat(sprintf("  Valid n             : %d\n\n", n_binge_valid))

# # --- Binge prevalence by sex ---
# cat("--- 1.2 Binge Prevalence by Sex ---\n")
# binge_by_sex <- svyby(~alcohol_binge, ~sex, df_svy, svymean, na.rm = TRUE)
# ci_by_sex <- confint(binge_by_sex)
# binge_sex_df <- data.frame(
#   sex       = binge_by_sex$sex,
#   prev      = binge_by_sex$alcohol_binge,
#   se        = SE(binge_by_sex),
#   ci_lower  = ci_by_sex[, 1],
#   ci_upper  = ci_by_sex[, 2]
# )
# print(binge_sex_df, row.names = FALSE)
# cat("\n")

# # --- Binge prevalence by age_group ---
# cat("--- 1.3 Binge Prevalence by Age Group ---\n")
# binge_by_age <- svyby(~alcohol_binge, ~age_group, df_svy, svymean, na.rm = TRUE)
# ci_by_age <- confint(binge_by_age)
# binge_age_df <- data.frame(
#   age_group = binge_by_age$age_group,
#   prev      = binge_by_age$alcohol_binge,
#   se        = SE(binge_by_age),
#   ci_lower  = ci_by_age[, 1],
#   ci_upper  = ci_by_age[, 2]
# )
# print(binge_age_df, row.names = FALSE)
# cat("\n")

# # --- Binge prevalence by province_region ---
# cat("--- 1.4 Binge Prevalence by Province/Region ---\n")
# binge_by_region <- svyby(~alcohol_binge, ~province_region, df_svy,
#                           svymean, na.rm = TRUE)
# ci_by_region <- confint(binge_by_region)
# binge_region_df <- data.frame(
#   region    = binge_by_region$province_region,
#   prev      = binge_by_region$alcohol_binge,
#   se        = SE(binge_by_region),
#   ci_lower  = ci_by_region[, 1],
#   ci_upper  = ci_by_region[, 2]
# )
# print(binge_region_df, row.names = FALSE)
# cat("\n")

# # --- Binge prevalence by mental health ---
# cat("--- 1.5 Binge Prevalence by Mental Health ---\n")
# binge_by_mh <- svyby(~alcohol_binge, ~mental_health, df_svy,
#                       svymean, na.rm = TRUE)
# ci_by_mh <- confint(binge_by_mh)
# binge_mh_df <- data.frame(
#   mental_health = binge_by_mh$mental_health,
#   prev          = binge_by_mh$alcohol_binge,
#   se            = SE(binge_by_mh),
#   ci_lower      = ci_by_mh[, 1],
#   ci_upper      = ci_by_mh[, 2]
# )
# print(binge_mh_df, row.names = FALSE)
# cat("\n")

# # =============================================================================
# # 5a.2 — Hypothesis Tests
# # =============================================================================
# cat(paste(rep("=", 70), collapse = ""), "\n")
# cat("SECTION 2: HYPOTHESIS TESTS\n")
# cat(paste(rep("=", 70), collapse = ""), "\n\n")

# # Collector for all p-values (for multiple comparison correction)
# all_tests <- tibble(
#   test_name   = character(),
#   comparison  = character(),
#   statistic   = numeric(),
#   df          = numeric(),
#   p_value     = numeric(),
#   method      = character()
# )

# # --- 2.1 Sex differences: svyglm ---
# cat("--- 2.1 Binge Drinking by Sex (svyglm logistic) ---\n")
# cat("H0: No difference in binge prevalence between sexes\n")
# cat("H1: Binge prevalence differs by sex\n\n")

# mod_sex <- svyglm(alcohol_binge ~ sex, design = df_svy,
#                    family = quasibinomial())
# sex_summary <- summary(mod_sex)
# cat("Logistic regression coefficients:\n")
# print(coef(sex_summary))
# cat("\n")

# # Wald test for overall sex effect
# sex_anova <- regTermTest(mod_sex, ~sex, method = "Wald")
# cat(sprintf("Wald test for sex effect: F = %.4f, df = (%.0f, %.0f), p = %s\n\n",
#             sex_anova$Ftest, sex_anova$df, sex_anova$ddf,
#             formatC(sex_anova$p, format = "e", digits = 4)))

# all_tests <- all_tests %>% add_row(
#   test_name  = "sex_overall",
#   comparison = "Female vs Male vs Other",
#   statistic  = sex_anova$Ftest,
#   df         = sex_anova$df,
#   p_value    = sex_anova$p,
#   method     = "Wald F-test (svyglm)"
# )

# # Pairwise: Male vs Female (svyttest)
# cat("--- 2.1b Pairwise: Male vs Female (svyttest) ---\n")
# df_mf <- df %>% filter(sex %in% c("Male", "Female"), !is.na(alcohol_binge))
# df_mf$sex <- droplevels(df_mf$sex)
# svy_mf <- svydesign(ids = ~1, weights = ~weight, data = df_mf)

# ttest_sex <- svyttest(alcohol_binge ~ sex, design = svy_mf)
# cat(sprintf("svyttest Male vs Female:\n"))
# cat(sprintf("  t = %.4f, df = %.1f, p = %s\n",
#             ttest_sex$statistic, ttest_sex$parameter,
#             formatC(ttest_sex$p.value, format = "e", digits = 4)))
# cat(sprintf("  Mean difference (F - M) = %.4f\n",
#             ttest_sex$estimate[1] - ttest_sex$estimate[2]))
# cat(sprintf("  95%% CI for difference: [%.4f, %.4f]\n\n",
#             ttest_sex$conf.int[1], ttest_sex$conf.int[2]))

# all_tests <- all_tests %>% add_row(
#   test_name  = "sex_male_vs_female",
#   comparison = "Male vs Female",
#   statistic  = ttest_sex$statistic,
#   df         = ttest_sex$parameter,
#   p_value    = ttest_sex$p.value,
#   method     = "svyttest"
# )

# # --- 2.2 Age group differences: svyglm ---
# cat("--- 2.2 Binge Drinking by Age Group (svyglm logistic) ---\n")
# cat("H0: No difference in binge prevalence across age groups\n")
# cat("H1: Binge prevalence differs by age group\n\n")

# mod_age <- svyglm(alcohol_binge ~ age_group, design = df_svy,
#                    family = quasibinomial())
# age_summary <- summary(mod_age)
# cat("Logistic regression coefficients:\n")
# print(coef(age_summary))
# cat("\n")

# age_anova <- regTermTest(mod_age, ~age_group, method = "Wald")
# cat(sprintf("Wald test for age_group effect: F = %.4f, df = (%.0f, %.0f), p = %s\n\n",
#             age_anova$Ftest, age_anova$df, age_anova$ddf,
#             formatC(age_anova$p, format = "e", digits = 4)))

# all_tests <- all_tests %>% add_row(
#   test_name  = "age_group_overall",
#   comparison = "17-19 vs 20-24 vs 25-29 vs 30+",
#   statistic  = age_anova$Ftest,
#   df         = age_anova$df,
#   p_value    = age_anova$p,
#   method     = "Wald F-test (svyglm)"
# )

# # Pairwise age comparisons (each pair via svyttest)
# cat("--- 2.2b Pairwise Age Group Comparisons (svyttest) ---\n")
# age_levels <- levels(df$age_group)
# age_pairs <- combn(age_levels, 2, simplify = FALSE)

# for (pair in age_pairs) {
#   df_pair <- df %>%
#     filter(age_group %in% pair, !is.na(alcohol_binge)) %>%
#     mutate(age_group = factor(as.character(age_group), levels = pair))
#   svy_pair <- svydesign(ids = ~1, weights = ~weight, data = df_pair)

#   tt <- svyttest(alcohol_binge ~ age_group, design = svy_pair)
#   label <- paste(pair, collapse = " vs ")
#   cat(sprintf("  %s: t = %.3f, p = %s\n",
#               label, tt$statistic, formatC(tt$p.value, format = "e", digits = 3)))

#   all_tests <- all_tests %>% add_row(
#     test_name  = paste0("age_", paste(pair, collapse = "_vs_")),
#     comparison = label,
#     statistic  = tt$statistic,
#     df         = tt$parameter,
#     p_value    = tt$p.value,
#     method     = "svyttest"
#   )
# }
# cat("\n")

# # --- 2.3 Region differences: svyglm ---
# cat("--- 2.3 Binge Drinking by Province/Region (svyglm logistic) ---\n")
# cat("H0: No difference in binge prevalence across regions\n")
# cat("H1: Binge prevalence differs by region\n\n")

# mod_region <- svyglm(alcohol_binge ~ province_region, design = df_svy,
#                       family = quasibinomial())
# region_summary <- summary(mod_region)
# cat("Logistic regression coefficients:\n")
# print(coef(region_summary))
# cat("\n")

# region_anova <- regTermTest(mod_region, ~province_region, method = "Wald")
# cat(sprintf("Wald test for region effect: F = %.4f, df = (%.0f, %.0f), p = %s\n\n",
#             region_anova$Ftest, region_anova$df, region_anova$ddf,
#             formatC(region_anova$p, format = "e", digits = 4)))

# all_tests <- all_tests %>% add_row(
#   test_name  = "region_overall",
#   comparison = "Atlantic vs Ontario vs Quebec vs West",
#   statistic  = region_anova$Ftest,
#   df         = region_anova$df,
#   p_value    = region_anova$p,
#   method     = "Wald F-test (svyglm)"
# )

# # Pairwise region comparisons
# cat("--- 2.3b Pairwise Region Comparisons (svyttest) ---\n")
# region_levels <- levels(df$province_region)
# region_pairs <- combn(region_levels, 2, simplify = FALSE)

# for (pair in region_pairs) {
#   df_pair <- df %>%
#     filter(province_region %in% pair, !is.na(alcohol_binge)) %>%
#     mutate(province_region = factor(as.character(province_region), levels = pair))
#   svy_pair <- svydesign(ids = ~1, weights = ~weight, data = df_pair)

#   tt <- svyttest(alcohol_binge ~ province_region, design = svy_pair)
#   label <- paste(pair, collapse = " vs ")
#   cat(sprintf("  %s: t = %.3f, p = %s\n",
#               label, tt$statistic, formatC(tt$p.value, format = "e", digits = 3)))

#   all_tests <- all_tests %>% add_row(
#     test_name  = paste0("region_", paste(pair, collapse = "_vs_")),
#     comparison = label,
#     statistic  = tt$statistic,
#     df         = tt$parameter,
#     p_value    = tt$p.value,
#     method     = "svyttest"
#   )
# }
# cat("\n")

# # --- 2.4 Mental health differences: svyglm ---
# cat("--- 2.4 Binge Drinking by Mental Health (svyglm logistic) ---\n")
# cat("H0: No difference in binge prevalence across mental health levels\n")
# cat("H1: Binge prevalence differs by mental health status\n\n")

# mod_mh <- svyglm(alcohol_binge ~ mental_health, design = df_svy,
#                    family = quasibinomial())
# mh_summary <- summary(mod_mh)
# cat("Logistic regression coefficients:\n")
# print(coef(mh_summary))
# cat("\n")

# mh_anova <- regTermTest(mod_mh, ~mental_health, method = "Wald")
# cat(sprintf("Wald test for mental_health effect: F = %.4f, df = (%.0f, %.0f), p = %s\n\n",
#             mh_anova$Ftest, mh_anova$df, mh_anova$ddf,
#             formatC(mh_anova$p, format = "e", digits = 4)))

# all_tests <- all_tests %>% add_row(
#   test_name  = "mental_health_overall",
#   comparison = "Excellent vs Very Good vs Good vs Fair vs Poor",
#   statistic  = mh_anova$Ftest,
#   df         = mh_anova$df,
#   p_value    = mh_anova$p,
#   method     = "Wald F-test (svyglm)"
# )

# # =============================================================================
# # 5a.3 — Effect Sizes
# # =============================================================================
# cat(paste(rep("=", 70), collapse = ""), "\n")
# cat("SECTION 3: EFFECT SIZES (Cohen's h for proportions)\n")
# cat(paste(rep("=", 70), collapse = ""), "\n\n")

# cat("Cohen's h = 2 * arcsin(sqrt(p1)) - 2 * arcsin(sqrt(p2))\n")
# cat("Interpretation: |h| < 0.20 = small, 0.20-0.50 = medium, > 0.80 = large\n\n")

# # Helper: compute Cohen's h from two proportions
# cohens_h <- function(p1, p2) {
#   2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))
# }

# effect_sizes <- tibble(
#   comparison = character(),
#   p1         = numeric(),
#   p2         = numeric(),
#   cohens_h   = numeric(),
#   abs_h      = numeric(),
#   magnitude  = character()
# )

# classify_h <- function(h) {
#   ah <- abs(h)
#   if (ah < 0.20) return("Small")
#   if (ah < 0.50) return("Medium")
#   return("Large")
# }

# # --- Effect sizes by sex (Male vs Female) ---
# cat("--- 3.1 Effect Sizes by Sex ---\n")
# p_male   <- binge_sex_df$prev[binge_sex_df$sex == "Male"]
# p_female <- binge_sex_df$prev[binge_sex_df$sex == "Female"]
# h_sex    <- cohens_h(p_male, p_female)
# cat(sprintf("  Male (%.4f) vs Female (%.4f): h = %.4f (%s)\n",
#             p_male, p_female, h_sex, classify_h(h_sex)))

# effect_sizes <- effect_sizes %>% add_row(
#   comparison = "Male vs Female",
#   p1 = p_male, p2 = p_female,
#   cohens_h = h_sex, abs_h = abs(h_sex),
#   magnitude = classify_h(h_sex)
# )

# # If Other category present
# if ("Other" %in% binge_sex_df$sex) {
#   p_other <- binge_sex_df$prev[binge_sex_df$sex == "Other"]
#   h_mo <- cohens_h(p_male, p_other)
#   h_fo <- cohens_h(p_female, p_other)
#   cat(sprintf("  Male (%.4f) vs Other (%.4f): h = %.4f (%s)\n",
#               p_male, p_other, h_mo, classify_h(h_mo)))
#   cat(sprintf("  Female (%.4f) vs Other (%.4f): h = %.4f (%s)\n",
#               p_female, p_other, h_fo, classify_h(h_fo)))

#   effect_sizes <- effect_sizes %>% add_row(
#     comparison = "Male vs Other",
#     p1 = p_male, p2 = p_other,
#     cohens_h = h_mo, abs_h = abs(h_mo),
#     magnitude = classify_h(h_mo)
#   )
#   effect_sizes <- effect_sizes %>% add_row(
#     comparison = "Female vs Other",
#     p1 = p_female, p2 = p_other,
#     cohens_h = h_fo, abs_h = abs(h_fo),
#     magnitude = classify_h(h_fo)
#   )
# }
# cat("\n")

# # --- Effect sizes by age group (pairwise) ---
# cat("--- 3.2 Effect Sizes by Age Group ---\n")
# for (i in 1:(nrow(binge_age_df) - 1)) {
#   for (j in (i + 1):nrow(binge_age_df)) {
#     p_i <- binge_age_df$prev[i]
#     p_j <- binge_age_df$prev[j]
#     h_ij <- cohens_h(p_i, p_j)
#     label <- paste(binge_age_df$age_group[i], "vs", binge_age_df$age_group[j])
#     cat(sprintf("  %s: h = %.4f (%s)\n", label, h_ij, classify_h(h_ij)))

#     effect_sizes <- effect_sizes %>% add_row(
#       comparison = label,
#       p1 = p_i, p2 = p_j,
#       cohens_h = h_ij, abs_h = abs(h_ij),
#       magnitude = classify_h(h_ij)
#     )
#   }
# }
# cat("\n")

# # --- Effect sizes by region (pairwise) ---
# cat("--- 3.3 Effect Sizes by Region ---\n")
# for (i in 1:(nrow(binge_region_df) - 1)) {
#   for (j in (i + 1):nrow(binge_region_df)) {
#     p_i <- binge_region_df$prev[i]
#     p_j <- binge_region_df$prev[j]
#     h_ij <- cohens_h(p_i, p_j)
#     label <- paste(binge_region_df$region[i], "vs", binge_region_df$region[j])
#     cat(sprintf("  %s: h = %.4f (%s)\n", label, h_ij, classify_h(h_ij)))

#     effect_sizes <- effect_sizes %>% add_row(
#       comparison = label,
#       p1 = p_i, p2 = p_j,
#       cohens_h = h_ij, abs_h = abs(h_ij),
#       magnitude = classify_h(h_ij)
#     )
#   }
# }
# cat("\n")

# # --- Effect sizes by mental health (Excellent vs Poor as bookends) ---
# cat("--- 3.4 Effect Sizes by Mental Health (selected pairs) ---\n")
# mh_levels <- as.character(binge_mh_df$mental_health)
# mh_prevs  <- setNames(binge_mh_df$prev, mh_levels)

# mh_pairs_of_interest <- list(
#   c("Excellent", "Very Good"),
#   c("Excellent", "Good"),
#   c("Excellent", "Fair"),
#   c("Excellent", "Poor"),
#   c("Very Good", "Poor"),
#   c("Good", "Poor"),
#   c("Fair", "Poor")
# )

# for (pair in mh_pairs_of_interest) {
#   if (all(pair %in% mh_levels)) {
#     p_i <- mh_prevs[pair[1]]
#     p_j <- mh_prevs[pair[2]]
#     h_ij <- cohens_h(p_i, p_j)
#     label <- paste(pair, collapse = " vs ")
#     cat(sprintf("  %s: h = %.4f (%s)\n", label, h_ij, classify_h(h_ij)))

#     effect_sizes <- effect_sizes %>% add_row(
#       comparison = label,
#       p1 = p_i, p2 = p_j,
#       cohens_h = h_ij, abs_h = abs(h_ij),
#       magnitude = classify_h(h_ij)
#     )
#   }
# }
# cat("\n")

# # --- Cohen's w for overall association (effectsize package) ---
# cat("--- 3.5 Cohen's w for Categorical Associations ---\n")
# cat("Cohen's w: < 0.10 = small, 0.10-0.30 = medium, > 0.50 = large\n\n")

# # Binge x Sex
# tab_binge_sex <- table(
#   df$alcohol_binge[!is.na(df$alcohol_binge) & !is.na(df$sex)],
#   df$sex[!is.na(df$alcohol_binge) & !is.na(df$sex)]
# )
# chi_sex <- chisq.test(tab_binge_sex)
# w_sex <- sqrt(chi_sex$statistic / sum(tab_binge_sex))
# cat(sprintf("  Binge x Sex:    Chi-sq = %.2f, w = %.4f\n",
#             chi_sex$statistic, w_sex))

# # Binge x Age Group
# tab_binge_age <- table(
#   df$alcohol_binge[!is.na(df$alcohol_binge) & !is.na(df$age_group)],
#   df$age_group[!is.na(df$alcohol_binge) & !is.na(df$age_group)]
# )
# chi_age <- chisq.test(tab_binge_age)
# w_age <- sqrt(chi_age$statistic / sum(tab_binge_age))
# cat(sprintf("  Binge x Age:    Chi-sq = %.2f, w = %.4f\n",
#             chi_age$statistic, w_age))

# # Binge x Region
# tab_binge_reg <- table(
#   df$alcohol_binge[!is.na(df$alcohol_binge) & !is.na(df$province_region)],
#   df$province_region[!is.na(df$alcohol_binge) & !is.na(df$province_region)]
# )
# chi_reg <- chisq.test(tab_binge_reg)
# w_reg <- sqrt(chi_reg$statistic / sum(tab_binge_reg))
# cat(sprintf("  Binge x Region: Chi-sq = %.2f, w = %.4f\n",
#             chi_reg$statistic, w_reg))

# # Binge x Mental Health
# tab_binge_mh <- table(
#   df$alcohol_binge[!is.na(df$alcohol_binge) & !is.na(df$mental_health)],
#   df$mental_health[!is.na(df$alcohol_binge) & !is.na(df$mental_health)]
# )
# chi_mh <- chisq.test(tab_binge_mh)
# w_mh <- sqrt(chi_mh$statistic / sum(tab_binge_mh))
# cat(sprintf("  Binge x MH:     Chi-sq = %.2f, w = %.4f\n\n",
#             chi_mh$statistic, w_mh))

# # =============================================================================
# # 5a.4 — Multiple Comparison Corrections
# # =============================================================================
# cat(paste(rep("=", 70), collapse = ""), "\n")
# cat("SECTION 4: MULTIPLE COMPARISON CORRECTIONS\n")
# cat(paste(rep("=", 70), collapse = ""), "\n\n")

# cat("Problem: testing", nrow(all_tests), "hypotheses inflates family-wise error rate.\n")
# cat("Applying Bonferroni and FDR (Benjamini-Hochberg) corrections.\n\n")

# # Apply corrections
# all_tests <- all_tests %>%
#   mutate(
#     p_bonferroni = pmin(p_value * n(), 1),
#     p_fdr_bh     = p.adjust(p_value, method = "BH"),
#     sig_nominal  = ifelse(p_value < 0.05, "*", ""),
#     sig_bonf     = ifelse(p_bonferroni < 0.05, "*", ""),
#     sig_fdr      = ifelse(p_fdr_bh < 0.05, "*", "")
#   )

# cat("--- 4.1 Bonferroni Correction ---\n")
# cat(sprintf("  Adjusted alpha = 0.05 / %d = %.5f\n", nrow(all_tests),
#             0.05 / nrow(all_tests)))
# cat(sprintf("  Tests significant at nominal alpha:     %d / %d\n",
#             sum(all_tests$p_value < 0.05), nrow(all_tests)))
# cat(sprintf("  Tests significant after Bonferroni:     %d / %d\n\n",
#             sum(all_tests$p_bonferroni < 0.05), nrow(all_tests)))

# cat("--- 4.2 FDR (Benjamini-Hochberg) Correction ---\n")
# cat(sprintf("  Tests significant after FDR correction: %d / %d\n\n",
#             sum(all_tests$p_fdr_bh < 0.05), nrow(all_tests)))

# # Print full results table
# cat("--- 4.3 Full Hypothesis Test Initial Results ---\n")
# results_print <- all_tests %>%
#   mutate(
#     p_value      = formatC(p_value, format = "e", digits = 3),
#     p_bonferroni = formatC(as.numeric(p_bonferroni), format = "e", digits = 3),
#     p_fdr_bh     = formatC(as.numeric(p_fdr_bh), format = "e", digits = 3),
#     statistic    = round(statistic, 4)
#   ) %>%
#   select(test_name, comparison, method, statistic, p_value,
#          p_bonferroni, p_fdr_bh, sig_nominal, sig_bonf, sig_fdr)

# print(as.data.frame(results_print), right = FALSE, row.names = FALSE)
# cat("\nLegend: * = significant at alpha = 0.05\n\n")

# # --- Interpretation summary ---
# cat("--- 4.4 Interpretation Summary ---\n")
# cat("Tests surviving Bonferroni correction (most conservative):\n")
# bonf_sig <- all_tests %>% filter(p_bonferroni < 0.05)
# if (nrow(bonf_sig) > 0) {
#   for (i in 1:nrow(bonf_sig)) {
#     cat(sprintf("  - %s (%s): p_bonf = %s\n",
#                 bonf_sig$test_name[i], bonf_sig$comparison[i],
#                 formatC(bonf_sig$p_bonferroni[i], format = "e", digits = 3)))
#   }
# } else {
#   cat("  (None)\n")
# }

# cat("\nTests surviving FDR correction (less conservative):\n")
# fdr_sig <- all_tests %>% filter(p_fdr_bh < 0.05)
# if (nrow(fdr_sig) > 0) {
#   for (i in 1:nrow(fdr_sig)) {
#     cat(sprintf("  - %s (%s): p_fdr = %s\n",
#                 fdr_sig$test_name[i], fdr_sig$comparison[i],
#                 formatC(fdr_sig$p_fdr_bh[i], format = "e", digits = 3)))
#   }
# } else {
#   cat("  (None)\n")
# }
# cat("\n")

# # =============================================================================
# # 5a.5 — Save Initial Results
# # =============================================================================
# cat(paste(rep("=", 70), collapse = ""), "\n")
# cat("SECTION 5: SAVING RESULTS\n")
# cat(paste(rep("=", 70), collapse = ""), "\n\n")

# # Combine prevalence CI tables
# ci_all <- bind_rows(
#   tibble(variable = "Overall", level = "All",
#          prev = coef(p_binge_overall), se = SE(p_binge_overall)[1],
#          ci_lower = ci_binge_overall[1], ci_upper = ci_binge_overall[2]),
#   binge_sex_df %>% rename(variable = sex, level = sex) %>%
#     mutate(variable = "Sex", level = as.character(level)),
#   binge_age_df %>%
#     mutate(variable = "Age Group", level = as.character(age_group)) %>%
#     select(variable, level, prev, se, ci_lower, ci_upper),
#   binge_region_df %>%
#     mutate(variable = "Province/Region", level = as.character(region)) %>%
#     select(variable, level, prev, se, ci_lower, ci_upper),
#   binge_mh_df %>%
#     mutate(variable = "Mental Health", level = as.character(mental_health)) %>%
#     select(variable, level, prev, se, ci_lower, ci_upper)
# )

# write_csv(ci_all, file.path(output_dir, "frequentist_binge_prevalence_ci.csv"))
# cat("Saved: frequentist_binge_prevalence_ci.csv\n")

# # Save hypothesis tests (flatten matrix columns)
# all_tests_flat <- all_tests %>%
#   mutate(across(where(is.matrix), ~as.numeric(.)),
#          across(where(~is.list(.) || !is.atomic(.)), ~as.numeric(.)))
# write_csv(all_tests_flat, file.path(output_dir, "frequentist_hypothesis_tests.csv"))
# cat("Saved: frequentist_hypothesis_tests.csv\n")

# # Save effect sizes
# write_csv(effect_sizes, file.path(output_dir, "frequentist_effect_sizes.csv"))
# cat("Saved: frequentist_effect_sizes.csv\n")

# cat("\n=== PHASE 5a: FREQUENTIST INFERENCE COMPLETE ===\n")

#!/usr/bin/env Rscript
# =============================================================================
# 05_frequentist.R — Phase 5a: Frequentist Inference
# data 2021–2022 dataset (Cleaned) — Seeing Theory Ch.4 style
# =============================================================================
# Survey-weighted (where appropriate):
#   1) Prevalence estimates + 95% CIs for heavy_drinking_30d overall and by:
#        - gender, age_group, province_region, mental_health
#   2) Hypothesis tests for group differences:
#        - svyglm (quasibinomial) + regTermTest (Wald)
#        - Pairwise svyttest for each pair of factor levels
#   3) Effect sizes:
#        - Cohen's h computed from survey-weighted prevalences
#        - Cohen's w from unweighted chi-square (descriptive)
#   4) Multiplicity corrections over *all* collected p-values (Bonferroni + BH/FDR)
#   5) Save outputs under PROJECT_ROOT/data/private/outputs
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

# ---- Paths: EVERYTHING under PROJECT_ROOT ----
data_dir <- paths$data_dir
output_dir <- paths$output_private_dir
wrangled_dir <- paths$wrangled_dir
fig_dir <- paths$figures_dir

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(wrangled_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== PHASE 5a: FREQUENTIST INFERENCE (Seeing Theory Ch.4) ===\n\n")

# --- Load wrangled data ---
df_path <- file.path(wrangled_dir, "data_wrangled.rds")
svy_path <- file.path(wrangled_dir, "data_survey.rds")

if (!file.exists(df_path)) {
  stop("Missing file: ", df_path)
}
df <- readRDS(df_path)

# Prefer persisted survey object to preserve consistency with Phase 3
if (file.exists(svy_path)) {
  df_svy <- readRDS(svy_path)
} else {
  if (!("weight" %in% names(df))) {
    stop("Cannot build survey design: missing weight")
  }
  df_svy <- svydesign(ids = ~1, weights = ~weight, data = df)
}

# ---- Safety checks ----
required <- c(
  "heavy_drinking_30d",
  "gender",
  "age_group",
  "province_region",
  "mental_health",
  "weight"
)
missing <- setdiff(required, names(df))
if (length(missing) > 0) {
  stop("Missing required variables in df: ", paste(missing, collapse = ", "))
}

# Ensure outcome is numeric 0/1 with NA allowed (svymean expects numeric for mean-as-prevalence)
# (This is not "coercion to dodge errors": it enforces the model's mathematical domain.)
if (!is.numeric(df$heavy_drinking_30d)) {
  stop(
    "heavy_drinking_30d must be numeric 0/1/NA. Found type: ",
    typeof(df$heavy_drinking_30d)
  )
}
bad_vals <- setdiff(
  unique(df$heavy_drinking_30d[!is.na(df$heavy_drinking_30d)]),
  c(0, 1)
)
if (length(bad_vals) > 0) {
  stop(
    "heavy_drinking_30d contains non {0,1} values: ",
    paste(bad_vals, collapse = ", ")
  )
}

cat("data dataset:", nrow(df), "observations\n")
cat("Primary outcome: heavy_drinking_30d (binary: 0/1)\n\n")

# =============================================================================
# Helpers (explicit namespaces to avoid masking issues)
# =============================================================================
fmt_p <- function(p) formatC(p, format = "e", digits = 4)

cohens_h <- function(p1, p2) 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))

classify_h <- function(h) {
  ah <- abs(h)
  if (is.na(ah)) {
    return(NA_character_)
  }
  if (ah < 0.20) {
    return("Small")
  }
  if (ah < 0.50) {
    return("Medium")
  }
  return("Large")
}

# Build a tidy prevalence table from svyby() using coef() + vcov() + confint()
# This avoids assuming *any* particular internal shape of SE()/confint() returns.
# svyby_prev <- function(outcome, group, design) {
#   f_out <- stats::as.formula(paste0("~", outcome))
#   f_grp <- stats::as.formula(paste0("~", group))

#   est <- survey::svyby(f_out, f_grp, design, survey::svymean, na.rm = TRUE)
#   # coef(est) returns a matrix with columns for each mean; here we want the outcome column
#   b <- stats::coef(est)
#   if (!(outcome %in% colnames(b))) {
#     stop("svyby_prev(): outcome column not found in coef(est). Have: ",
#          paste(colnames(b), collapse = ", "))
#   }

#   prev <- as.numeric(b[, outcome])

#   # For svyby objects, vcov(est) is block-diagonal over groups; for a single mean per group
#   # we can take the diagonal elements corresponding to the outcome.
#   V <- stats::vcov(est)
#   # When there's only one parameter per group, length(diag(V)) == nrow(est).
#   # When there are multiple parameters, we need the indices for the outcome.
#   # survey names the coef vector; we use that to pick the right diagonal entries.
#   b_vec <- as.numeric(stats::coef(est))
#   names(b_vec) <- rownames(stats::coef(est)) # usually includes group labels; keeps alignment stable

#   # More reliable: use SE(est) but treat it as a vector aligned to rows/params.
#   # For svyby with one mean, SE(est) should align to rows; if it returns matrix, take the right column by name.
#   se_raw <- survey::SE(est)
#   se <- if (is.matrix(se_raw)) {
#     # pick the column for the outcome if present, else error (don’t guess)
#     if (outcome %in% colnames(se_raw)) as.numeric(se_raw[, outcome]) else
#       stop("svyby_prev(): SE(est) is matrix but lacks outcome column. Have: ",
#            paste(colnames(se_raw), collapse = ", "))
#   } else {
#     as.numeric(se_raw)
#   }

#   ci <- suppressWarnings(confint(est))
#   ci_lower <- ci_upper <- rep(NA_real_, length(prev))
#   if (is.matrix(ci)) {
#     # confint for svyby returns rows aligned with groups and columns (2) for lower/upper per parameter;
#     # if it returns more columns for multiple params, select by outcome name if available
#     if (ncol(ci) == 2 && nrow(ci) == length(prev)) {
#       ci_lower <- as.numeric(ci[, 1])
#       ci_upper <- as.numeric(ci[, 2])
#     } else if (!is.null(colnames(ci))) {
#       # Try to find columns like "2.5 % heavy_drinking_30d" etc. (survey versions differ)
#       # If ambiguous, fail loudly rather than silently mis-map.
#       stop("svyby_prev(): confint(est) returned unexpected shape (",
#            nrow(ci), "x", ncol(ci), "). Please print str(confint(est)).")
#     } else {
#       stop("svyby_prev(): confint(est) returned unexpected shape without colnames.")
#     }
#   } else {
#     stop("svyby_prev(): confint(est) did not return a matrix; got: ", class(ci)[1])
#   }

#   tibble::tibble(
#     level = as.character(est[[group]]),
#     prev = prev,
#     se = se,
#     ci_lower = ci_lower,
#     ci_upper = ci_upper,
#     n_unweighted_nonmissing = as.integer(tapply(!is.na(design$variables[[outcome]]),
#                                                design$variables[[group]], sum)[level])
#   )
# }

svyby_prev <- function(outcome, group, design) {
  f_out <- stats::as.formula(paste0("~", outcome))
  f_grp <- stats::as.formula(paste0("~", group))

  est <- survey::svyby(f_out, f_grp, design, survey::svymean, na.rm = TRUE)

  # svyby results are data.frame-like; use that as the canonical container
  df <- as.data.frame(est)

  if (!(group %in% names(df))) {
    stop(
      "svyby_prev(): group column not found in svyby output. Have: ",
      paste(names(df), collapse = ", ")
    )
  }

  # Identify the estimate column robustly.
  # Priority:
  #  1) exact match to outcome
  #  2) columns containing outcome as a substring
  #  3) single remaining numeric column (excluding group and obvious SE columns)
  cand <- setdiff(names(df), group)

  # drop obvious SE/variance columns from candidates
  cand_no_se <- cand[
    !grepl("^se\\.|^SE\\.|\\.se$|^var\\.|^VAR\\.|\\.var$", cand)
  ]

  est_col <- NULL
  if (outcome %in% cand_no_se) {
    est_col <- outcome
  } else {
    hit <- cand_no_se[grepl(outcome, cand_no_se, fixed = TRUE)]
    if (length(hit) == 1) {
      est_col <- hit
    } else {
      # fall back to "the" numeric estimate column if unique
      num_cand <- cand_no_se[vapply(df[cand_no_se], is.numeric, logical(1))]
      if (length(num_cand) == 1) {
        est_col <- num_cand
      } else {
        stop(
          "svyby_prev(): could not uniquely identify estimate column.\n",
          "  outcome = ",
          outcome,
          "\n",
          "  candidates = ",
          paste(cand, collapse = ", "),
          "\n",
          "  numeric candidates (excluding SE-like cols) = ",
          paste(num_cand, collapse = ", ")
        )
      }
    }
  }

  prev <- as.numeric(df[[est_col]])

  # Extract SE robustly
  se_raw <- survey::SE(est)
  se <- NULL
  if (is.matrix(se_raw)) {
    if (!is.null(colnames(se_raw)) && est_col %in% colnames(se_raw)) {
      se <- as.numeric(se_raw[, est_col])
    } else if (ncol(se_raw) == 1 && nrow(se_raw) == nrow(df)) {
      se <- as.numeric(se_raw[, 1])
    } else {
      stop(
        "svyby_prev(): SE(est) is matrix with unexpected shape.\n",
        "  dim(SE) = ",
        paste(dim(se_raw), collapse = "x"),
        "\n",
        "  colnames(SE) = ",
        paste(colnames(se_raw), collapse = ", ")
      )
    }
  } else {
    # vector case: must align to rows
    if (length(se_raw) != nrow(df)) {
      stop(
        "svyby_prev(): SE(est) is a vector but length != nrow(est).\n",
        "  length(SE) = ",
        length(se_raw),
        ", nrow(est) = ",
        nrow(df)
      )
    }
    se <- as.numeric(se_raw)
  }

  # Confidence intervals: use confint if it returns row-aligned 2-col matrix;
  # otherwise compute standard Wald CI (estimate ± 1.96*SE) explicitly.
  ci <- suppressWarnings(try(confint(est), silent = TRUE))
  if (
    !inherits(ci, "try-error") &&
      is.matrix(ci) &&
      nrow(ci) == nrow(df) &&
      ncol(ci) == 2
  ) {
    ci_lower <- as.numeric(ci[, 1])
    ci_upper <- as.numeric(ci[, 2])
  } else {
    # Deterministic, explicit fallback (not coercion): if confint is not row-aligned,
    # report standard normal-approx CI for a mean/prevalence.
    ci_lower <- pmax(0, prev - 1.96 * se)
    ci_upper <- pmin(1, prev + 1.96 * se)
  }

  # Unweighted non-missing counts by group (from design variables)
  x <- design$variables[[outcome]]
  g <- design$variables[[group]]
  ok <- !is.na(x) & !is.na(g)
  n_map <- tapply(ok, g, sum)
  n_nm <- as.integer(n_map[as.character(df[[group]])])

  tibble::tibble(
    level = as.character(df[[group]]),
    prev = prev,
    se = se,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    n_unweighted_nonmissing = n_nm
  )
}

# Hypothesis testing wrappers
all_tests <- tibble::tibble(
  test_name = character(),
  comparison = character(),
  statistic = numeric(),
  df = numeric(),
  p_value = numeric(),
  method = character()
)

run_overall_wald <- function(group_var, label) {
  cat(sprintf("--- %s (svyglm quasibinomial + Wald) ---\n", label))
  f <- stats::as.formula(paste0("heavy_drinking_30d ~ ", group_var))
  mod <- survey::svyglm(f, design = df_svy, family = quasibinomial())
  an <- survey::regTermTest(
    mod,
    stats::as.formula(paste0("~", group_var)),
    method = "Wald"
  )

  cat("Model coefficients:\n")
  print(coef(summary(mod)))
  cat("\n")
  cat(sprintf(
    "Wald test: F = %.4f, df = (%.0f, %.0f), p = %s\n\n",
    as.numeric(an$Ftest),
    as.numeric(an$df),
    as.numeric(an$ddf),
    fmt_p(an$p)
  ))

  all_tests <<- dplyr::bind_rows(
    all_tests,
    tibble::tibble(
      test_name = paste0(group_var, "_overall"),
      comparison = label,
      statistic = as.numeric(an$Ftest),
      df = as.numeric(an$df),
      p_value = as.numeric(an$p),
      method = "Wald F-test (svyglm)"
    )
  )
}

run_pairwise_ttests <- function(group_var, label_prefix) {
  cat(sprintf("--- Pairwise comparisons: %s (svyttest) ---\n", label_prefix))
  lvls <- levels(df[[group_var]])
  if (is.null(lvls) || length(lvls) < 2) {
    cat("Skipping pairwise: <2 levels.\n\n")
    return(invisible(NULL))
  }

  pairs <- combn(lvls, 2, simplify = FALSE)
  for (pair in pairs) {
    df_pair <- df %>%
      dplyr::filter(!is.na(heavy_drinking_30d), !is.na(.data[[group_var]])) %>%
      dplyr::filter(.data[[group_var]] %in% pair) %>%
      dplyr::mutate(
        !!group_var := factor(as.character(.data[[group_var]]), levels = pair)
      )

    # Guard against degenerate designs
    if (nrow(df_pair) < 20) {
      next
    }
    if (length(unique(df_pair$heavy_drinking_30d)) < 2) {
      next
    }

    svy_pair <- survey::svydesign(ids = ~1, weights = ~weight, data = df_pair)
    tt <- survey::svyttest(
      stats::as.formula(paste0("heavy_drinking_30d ~ ", group_var)),
      design = svy_pair
    )

    comp <- paste(pair, collapse = " vs ")
    cat(sprintf(
      "  %s: t = %.3f, df = %.1f, p = %s\n",
      comp,
      as.numeric(tt$statistic),
      as.numeric(tt$parameter),
      fmt_p(tt$p.value)
    ))

    all_tests <<- dplyr::bind_rows(
      all_tests,
      tibble::tibble(
        test_name = paste0(group_var, "_", paste(pair, collapse = "_vs_")),
        comparison = paste(label_prefix, ":", comp),
        statistic = as.numeric(tt$statistic),
        df = as.numeric(tt$parameter),
        p_value = as.numeric(tt$p.value),
        method = "svyttest"
      )
    )
  }
  cat("\n")
}

# =============================================================================
# 5a.1 — Survey-weighted Prevalence + 95% CIs
# =============================================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 1: SURVEY-WEIGHTED CONFIDENCE INTERVALS\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("--- 1.1 Overall Heavy Drinking (past 30 days) ---\n")
p_overall <- survey::svymean(~heavy_drinking_30d, df_svy, na.rm = TRUE)
ci_overall <- confint(p_overall)
cat(sprintf(
  "  Weighted prevalence : %.4f (%.2f%%)\n",
  as.numeric(coef(p_overall)[1]),
  100 * as.numeric(coef(p_overall)[1])
))
cat(sprintf("  Standard error      : %.4f\n", as.numeric(SE(p_overall)[1])))
cat(sprintf(
  "  95%% CI              : [%.4f, %.4f]\n",
  as.numeric(ci_overall[1, 1]),
  as.numeric(ci_overall[1, 2])
))
cat(sprintf(
  "  Valid n (unweighted): %d\n\n",
  sum(!is.na(df$heavy_drinking_30d))
))

cat("--- 1.2 By Gender ---\n")
prev_gender <- svyby_prev("heavy_drinking_30d", "gender", df_svy)
print(prev_gender, n = Inf)
cat("\n")

cat("--- 1.3 By Age Group ---\n")
prev_age <- svyby_prev("heavy_drinking_30d", "age_group", df_svy)
print(prev_age, n = Inf)
cat("\n")

cat("--- 1.4 By Province/Region ---\n")
prev_region <- svyby_prev("heavy_drinking_30d", "province_region", df_svy)
print(prev_region, n = Inf)
cat("\n")

cat("--- 1.5 By Mental Health ---\n")
prev_mh <- svyby_prev("heavy_drinking_30d", "mental_health", df_svy)
print(prev_mh, n = Inf)
cat("\n")

# =============================================================================
# 5a.2 — Hypothesis Tests
# =============================================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 2: HYPOTHESIS TESTS\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

run_overall_wald("gender", "Heavy drinking prevalence differs by gender")
run_pairwise_ttests("gender", "Gender")

run_overall_wald("age_group", "Heavy drinking prevalence differs by age group")
run_pairwise_ttests("age_group", "Age group")

run_overall_wald(
  "province_region",
  "Heavy drinking prevalence differs by province/region"
)
run_pairwise_ttests("province_region", "Province/Region")

run_overall_wald(
  "mental_health",
  "Heavy drinking prevalence differs by mental health"
)
run_pairwise_ttests("mental_health", "Mental health")

# =============================================================================
# 5a.3 — Effect Sizes
# =============================================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 3: EFFECT SIZES (Cohen's h; weighted prevalences)\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

effect_sizes <- tibble::tibble(
  variable = character(),
  comparison = character(),
  p1 = numeric(),
  p2 = numeric(),
  cohens_h = numeric(),
  abs_h = numeric(),
  magnitude = character()
)

add_h_pairs <- function(var_name, prev_tbl) {
  if (nrow(prev_tbl) < 2) {
    return(invisible(NULL))
  }
  pairs <- combn(prev_tbl$level, 2, simplify = FALSE)
  prev_map <- setNames(prev_tbl$prev, prev_tbl$level)

  for (pair in pairs) {
    p1 <- as.numeric(prev_map[pair[1]])
    p2 <- as.numeric(prev_map[pair[2]])
    h <- cohens_h(p1, p2)

    effect_sizes <<- dplyr::bind_rows(
      effect_sizes,
      tibble::tibble(
        variable = var_name,
        comparison = paste(pair, collapse = " vs "),
        p1 = p1,
        p2 = p2,
        cohens_h = h,
        abs_h = abs(h),
        magnitude = classify_h(h)
      )
    )
  }
}

add_h_pairs("gender", prev_gender)
add_h_pairs("age_group", prev_age)
add_h_pairs("province_region", prev_region)
add_h_pairs("mental_health", prev_mh)

cat(
  "Computed Cohen's h for all pairwise level contrasts (using weighted prevalences).\n\n"
)

# Descriptive Cohen's w (unweighted) for association bookkeeping
cat("--- Cohen's w (unweighted chi-square; descriptive) ---\n")
cohens_w_unweighted <- function(x, g) {
  ok <- !is.na(x) & !is.na(g)
  tab <- table(x[ok], g[ok])
  if (nrow(tab) < 2 || ncol(tab) < 2) {
    return(tibble::tibble(chi_sq = NA_real_, w = NA_real_))
  }
  chi <- suppressWarnings(stats::chisq.test(tab))
  tibble::tibble(
    chi_sq = as.numeric(chi$statistic),
    w = sqrt(as.numeric(chi$statistic) / sum(tab))
  )
}

w_gender <- cohens_w_unweighted(df$heavy_drinking_30d, df$gender)
w_age <- cohens_w_unweighted(df$heavy_drinking_30d, df$age_group)
w_region <- cohens_w_unweighted(df$heavy_drinking_30d, df$province_region)
w_mh <- cohens_w_unweighted(df$heavy_drinking_30d, df$mental_health)

cat(sprintf(
  "  Heavy x Gender:        Chi-sq = %.2f, w = %.4f\n",
  w_gender$chi_sq,
  w_gender$w
))
cat(sprintf(
  "  Heavy x Age group:     Chi-sq = %.2f, w = %.4f\n",
  w_age$chi_sq,
  w_age$w
))
cat(sprintf(
  "  Heavy x Region:        Chi-sq = %.2f, w = %.4f\n",
  w_region$chi_sq,
  w_region$w
))
cat(sprintf(
  "  Heavy x Mental health: Chi-sq = %.2f, w = %.4f\n\n",
  w_mh$chi_sq,
  w_mh$w
))

# =============================================================================
# 5a.4 — Multiple Comparison Corrections
# =============================================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 4: MULTIPLE COMPARISON CORRECTIONS\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

if (nrow(all_tests) == 0) {
  cat("No hypothesis tests collected; skipping multiplicity correction.\n\n")
} else {
  all_tests <- all_tests %>%
    dplyr::mutate(
      p_bonferroni = pmin(p_value * nrow(all_tests), 1),
      p_fdr_bh = stats::p.adjust(p_value, method = "BH"),
      sig_nominal = ifelse(p_value < 0.05, "*", ""),
      sig_bonf = ifelse(p_bonferroni < 0.05, "*", ""),
      sig_fdr = ifelse(p_fdr_bh < 0.05, "*", "")
    )

  cat("--- Summary ---\n")
  cat(sprintf(
    "  Tests significant at nominal alpha: %d / %d\n",
    sum(all_tests$p_value < 0.05),
    nrow(all_tests)
  ))
  cat(sprintf(
    "  Tests significant after Bonferroni: %d / %d\n",
    sum(all_tests$p_bonferroni < 0.05),
    nrow(all_tests)
  ))
  cat(sprintf(
    "  Tests significant after FDR (BH):   %d / %d\n\n",
    sum(all_tests$p_fdr_bh < 0.05),
    nrow(all_tests)
  ))

  cat("--- Full results table ---\n")
  results_print <- all_tests %>%
    dplyr::mutate(
      p_value = formatC(p_value, format = "e", digits = 3),
      p_bonferroni = formatC(p_bonferroni, format = "e", digits = 3),
      p_fdr_bh = formatC(p_fdr_bh, format = "e", digits = 3),
      statistic = round(statistic, 4),
      df = round(df, 3)
    ) %>%
    dplyr::select(
      test_name,
      comparison,
      method,
      statistic,
      df,
      p_value,
      p_bonferroni,
      p_fdr_bh,
      sig_nominal,
      sig_bonf,
      sig_fdr
    )

  print(as.data.frame(results_print), right = FALSE, row.names = FALSE)
  cat("\nLegend: * = significant at alpha = 0.05\n\n")
}

# =============================================================================
# 5a.5 — Save Initial Results
# =============================================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 5: SAVING RESULTS\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

ci_all <- dplyr::bind_rows(
  tibble::tibble(
    variable = "Overall",
    level = "All",
    prev = as.numeric(coef(p_overall)[1]),
    se = as.numeric(SE(p_overall)[1]),
    ci_lower = as.numeric(ci_overall[1, 1]),
    ci_upper = as.numeric(ci_overall[1, 2]),
    n_unweighted_nonmissing = sum(!is.na(df$heavy_drinking_30d))
  ),
  prev_gender %>%
    dplyr::mutate(variable = "Gender") %>%
    dplyr::select(
      variable,
      level,
      prev,
      se,
      ci_lower,
      ci_upper,
      n_unweighted_nonmissing
    ),
  prev_age %>%
    dplyr::mutate(variable = "Age Group") %>%
    dplyr::select(
      variable,
      level,
      prev,
      se,
      ci_lower,
      ci_upper,
      n_unweighted_nonmissing
    ),
  prev_region %>%
    dplyr::mutate(variable = "Province/Region") %>%
    dplyr::select(
      variable,
      level,
      prev,
      se,
      ci_lower,
      ci_upper,
      n_unweighted_nonmissing
    ),
  prev_mh %>%
    dplyr::mutate(variable = "Mental Health") %>%
    dplyr::select(
      variable,
      level,
      prev,
      se,
      ci_lower,
      ci_upper,
      n_unweighted_nonmissing
    )
)

readr::write_csv(
  ci_all,
  file.path(output_dir, "frequentist_heavy_drinking_prevalence_ci.csv")
)
cat("Saved: frequentist_heavy_drinking_prevalence_ci.csv\n")

readr::write_csv(
  all_tests,
  file.path(output_dir, "frequentist_hypothesis_tests.csv")
)
cat("Saved: frequentist_hypothesis_tests.csv\n")

readr::write_csv(
  effect_sizes,
  file.path(output_dir, "frequentist_effect_sizes.csv")
)
cat("Saved: frequentist_effect_sizes.csv\n")

cat("\n=== PHASE 5a: FREQUENTIST INFERENCE COMPLETE ===\n")
