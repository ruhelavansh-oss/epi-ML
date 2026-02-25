#!/usr/bin/env Rscript
# =============================================================================
# 05_power_design.R — Phase 5: Power Design (Official-doc aligned)
# =============================================================================
# Primary objective:
#   A priori interaction-term power planning for gender-difference analyses.
# Secondary objective:
#   Preserve legacy post hoc outputs for backward compatibility.
#
# This module keeps CPADS analyses observational and generates prospective
# trial-planning artifacts separately (stratified block randomization).
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

output_dir <- paths$output_private_dir
wrangled_dir <- paths$wrangled_dir
fig_dir <- paths$figures_dir
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(wrangled_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== PHASE 5: POWER DESIGN (OFFICIAL-DOC ALIGNED) ===\n\n")

pumf_path <- file.path(wrangled_dir, "cpads_pumf_wrangled.rds")
svy_path <- file.path(wrangled_dir, "cpads_pumf_survey.rds")
if (!file.exists(pumf_path)) stop("Missing file: ", pumf_path)
pumf <- readRDS(pumf_path)

if (file.exists(svy_path)) {
  pumf_svy <- readRDS(svy_path)
} else {
  if (!("wtpumf" %in% names(pumf))) stop("Cannot build survey design: missing wtpumf")
  pumf_svy <- survey::svydesign(ids = ~1, weights = ~wtpumf, data = pumf)
}

required <- c("wtpumf", "heavy_drinking_30d", "gender", "cannabis_any_use")
missing <- setdiff(required, names(pumf))
if (length(missing) > 0) stop("Missing required variables: ", paste(missing, collapse = ", "))

fmt_or_na <- function(x, digits = 4) ifelse(is.na(x), "NA", formatC(x, format = "f", digits = digits))

cohens_h <- function(p1, p2) {
  2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))
}

safe_deff <- function(svy_mean_obj) {
  d <- tryCatch(survey::deff(svy_mean_obj), error = function(e) NA)
  if (is.null(d)) return(NA_real_)
  if (is.matrix(d)) return(as.numeric(d[1, 1]))
  if (is.numeric(d)) return(as.numeric(d[1]))
  NA_real_
}

# ----------------------------------------------------------------------------
# Legacy descriptive/post hoc outputs (backward compatibility)
# ----------------------------------------------------------------------------
cat("--- Legacy descriptive/post hoc power outputs (compatibility) ---\n")

n_total <- sum(!is.na(pumf$heavy_drinking_30d))
y_total <- sum(pumf$heavy_drinking_30d == 1, na.rm = TRUE)
p_unw <- y_total / n_total

svy_est <- survey::svymean(~heavy_drinking_30d, pumf_svy, na.rm = TRUE, deff = "replace")
svy_ci <- confint(svy_est)
p_wt <- as.numeric(coef(svy_est)[1])
deff_val <- safe_deff(svy_est)
n_eff_total <- if (!is.na(deff_val) && deff_val > 0) n_total / deff_val else NA_real_

p0_grid <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30)
oneprop_tbl <- tibble(p0 = p0_grid) %>%
  mutate(
    p_obs = p_wt,
    h = cohens_h(p_obs, p0),
    n = n_total,
    n_eff = n_eff_total,
    power_srs = map_dbl(h, ~ pwr.p.test(h = .x, n = n_total, sig.level = 0.05, alternative = "two.sided")$power),
    power_deff = ifelse(
      is.na(n_eff_total),
      NA_real_,
      map_dbl(h, ~ pwr.p.test(h = .x, n = n_eff_total, sig.level = 0.05, alternative = "two.sided")$power)
    ),
    analysis_mode = "observational",
    power_scope = "descriptive_post_hoc"
  )

prev_by_gender <- survey::svyby(~heavy_drinking_30d, ~gender, pumf_svy, survey::svymean, na.rm = TRUE)
df_g <- as.data.frame(prev_by_gender)
prev_col <- if ("heavy_drinking_30d" %in% names(df_g)) "heavy_drinking_30d" else setdiff(names(df_g), "gender")[1]
gender_levels <- as.character(df_g$gender)
p_gender <- setNames(as.numeric(df_g[[prev_col]]), gender_levels)
ok_g <- !is.na(pumf$heavy_drinking_30d) & !is.na(pumf$gender)
n_by_gender <- table(pumf$gender[ok_g])
n_by_gender <- n_by_gender[names(p_gender)]

two_prop_rows <- list()
for (pair in combn(names(p_gender), 2, simplify = FALSE)) {
  g1 <- pair[1]
  g2 <- pair[2]
  p1 <- as.numeric(p_gender[g1])
  p2 <- as.numeric(p_gender[g2])
  if (is.na(p1) || is.na(p2)) next
  n1 <- as.numeric(n_by_gender[g1])
  n2 <- as.numeric(n_by_gender[g2])
  if (is.na(n1) || is.na(n2) || n1 < 2 || n2 < 2) next
  h <- cohens_h(p1, p2)
  n_eq <- min(n1, n2)
  pow_srs <- pwr.2p.test(h = h, n = n_eq, sig.level = 0.05, alternative = "two.sided")$power
  n_eq_eff <- if (!is.na(deff_val)) n_eq / deff_val else NA_real_
  pow_deff <- if (!is.na(n_eq_eff)) pwr.2p.test(h = h, n = n_eq_eff, sig.level = 0.05, alternative = "two.sided")$power else NA_real_
  two_prop_rows[[length(two_prop_rows) + 1]] <- tibble(
    group1 = g1,
    group2 = g2,
    p1 = p1,
    p2 = p2,
    h = h,
    n1 = n1,
    n2 = n2,
    n_eq = n_eq,
    power_srs = pow_srs,
    n_eq_eff = n_eq_eff,
    power_deff = pow_deff,
    analysis_mode = "observational",
    power_scope = "descriptive_post_hoc"
  )
}
two_prop_tbl <- bind_rows(two_prop_rows)

power_summary <- tibble(
  metric = c(
    "Observed prevalence (unweighted)",
    "Observed prevalence (survey-weighted)",
    "DEFF (svymean; if available)",
    "n_nonmissing",
    "n_eff (if available)",
    "Power design mode"
  ),
  value = c(p_unw, p_wt, deff_val, n_total, n_eff_total, NA_real_),
  text_value = c(NA, NA, NA, NA, NA, "legacy_descriptive_post_hoc"),
  analysis_mode = "observational",
  power_scope = "descriptive_post_hoc"
)

write_csv(oneprop_tbl, file.path(output_dir, "power_one_proportion_grid.csv"))
if (nrow(two_prop_tbl) > 0) {
  write_csv(two_prop_tbl, file.path(output_dir, "power_two_proportion_gender.csv"))
}
write_csv(power_summary, file.path(output_dir, "power_analysis_summary.csv"))

# ----------------------------------------------------------------------------
# Interaction-first a priori power planning
# ----------------------------------------------------------------------------
cat("--- A priori interaction power planning ---\n")

power_targets <- c(0.80, 0.85, 0.90, 0.95, 0.99, 0.999)
alpha <- 0.05

endpoint_defs <- tribble(
  ~endpoint, ~outcome_var,
  "heavy_drinking_30d", "heavy_drinking_30d",
  "ebac_legal", "ebac_legal"
)

if (!("ebac_legal" %in% names(pumf))) {
  endpoint_defs <- endpoint_defs %>% filter(outcome_var != "ebac_legal")
}

build_cell_assumptions <- function(df, outcome_var) {
  d <- df %>%
    select(all_of(c(outcome_var, "cannabis_any_use", "gender", "wtpumf"))) %>%
    drop_na()

  d$gender <- droplevels(factor(d$gender))
  svy <- svydesign(ids = ~1, weights = ~wtpumf, data = d)

  cell <- svyby(
    as.formula(paste0("~", outcome_var)),
    ~gender + cannabis_any_use,
    svy,
    svymean,
    na.rm = TRUE,
    vartype = NULL,
    keep.var = FALSE
  ) %>%
    as_tibble()

  prob_col <- names(cell)[vapply(cell, is.numeric, logical(1))]
  prob_col <- setdiff(prob_col, c("cannabis_any_use"))
  if (length(prob_col) == 0) stop("Unable to identify weighted prevalence column for ", outcome_var)
  prob_col <- prob_col[1]

  cell <- cell %>%
    rename(prob = !!sym(prob_col)) %>%
    mutate(prob = pmin(pmax(prob, 0.01), 0.99))

  gender_props <- d %>% count(gender, name = "n") %>% mutate(prop = n / sum(n))

  cell_wide <- cell %>%
    mutate(cannabis_any_use = as.integer(as.character(cannabis_any_use))) %>%
    select(gender, cannabis_any_use, prob) %>%
    pivot_wider(names_from = cannabis_any_use, values_from = prob, names_prefix = "cannabis_") %>%
    mutate(
      p0 = pmin(pmax(cannabis_0, 0.01), 0.99),
      p1 = pmin(pmax(cannabis_1, 0.01), 0.99)
    ) %>%
    select(gender, p0, p1)

  odds <- function(p) p / (1 - p)
  lor <- log(odds(cell_wide$p1) / odds(cell_wide$p0))
  lor_center <- mean(lor, na.rm = TRUE)
  dir <- ifelse(lor >= lor_center, 1, -1)

  scenario_tbl <- bind_rows(
    cell_wide %>% mutate(scenario = "pilot_observed"),
    cell_wide %>%
      mutate(
        scenario = "conservative_benchmark",
        lor_new = lor_center + 0.30 * dir,
        p1 = plogis(qlogis(p0) + lor_new)
      ) %>%
      select(gender, p0, p1, scenario)
  ) %>%
    left_join(gender_props %>% select(gender, observed_prop = prop), by = "gender")

  list(data = d, assumptions = scenario_tbl, gender_props = gender_props)
}

allocate_by_strategy <- function(total_n, levels_gender, observed_props, strategy) {
  k <- length(levels_gender)
  if (strategy == "equal_strata") {
    base <- floor(total_n / k)
    rem <- total_n - base * k
    n_vec <- rep(base, k)
    if (rem > 0) n_vec[seq_len(rem)] <- n_vec[seq_len(rem)] + 1
    names(n_vec) <- levels_gender
    return(n_vec)
  }

  props <- observed_props[levels_gender]
  props[is.na(props)] <- 0
  if (sum(props) <= 0) props <- rep(1 / k, k)
  props <- props / sum(props)
  raw <- total_n * props
  n_vec <- floor(raw)
  rem <- total_n - sum(n_vec)
  if (rem > 0) {
    add_idx <- order(raw - n_vec, decreasing = TRUE)[seq_len(rem)]
    n_vec[add_idx] <- n_vec[add_idx] + 1
  }
  names(n_vec) <- levels_gender
  n_vec
}

fast_interaction_plan <- function(assump_df, target_power, strategy, alpha = 0.05) {
  # Fast closed-form approximation: O(k) per target/strategy/scenario
  # where k = number of gender strata (vs Monte Carlo simulation loops).
  genders <- as.character(assump_df$gender)
  k <- length(genders)
  if (k < 2) {
    return(list(required_n = NA_integer_, estimated_power = NA_real_, status = "not_reached"))
  }

  ref_gender <- if ("Woman" %in% genders) "Woman" else genders[1]
  non_ref <- setdiff(genders, ref_gender)
  m_tests <- length(non_ref)
  alpha_adj <- alpha / max(1, m_tests) # Bonferroni control across interaction terms
  z_alpha <- qnorm(1 - alpha_adj / 2)
  z_beta <- qnorm(target_power)

  alloc_props <- if (strategy == "equal_strata") {
    setNames(rep(1 / k, k), genders)
  } else {
    p <- setNames(assump_df$observed_prop, assump_df$gender)[genders]
    p[is.na(p)] <- 0
    if (sum(p) <= 0) p <- rep(1 / k, k)
    p <- p / sum(p)
    p
  }

  ref_row <- assump_df %>% filter(gender == ref_gender) %>% slice(1)
  a_ref <- ref_row$p1 * (1 - ref_row$p1) + ref_row$p0 * (1 - ref_row$p0)
  rd_ref <- ref_row$p1 - ref_row$p0
  pi_ref <- alloc_props[[ref_gender]]

  pair_tbl <- map_dfr(non_ref, function(g) {
    gr <- assump_df %>% filter(gender == g) %>% slice(1)
    a_g <- gr$p1 * (1 - gr$p1) + gr$p0 * (1 - gr$p0)
    rd_g <- gr$p1 - gr$p0
    delta <- rd_g - rd_ref
    pi_g <- alloc_props[[g]]

    se2_const <- 2 * (a_g / pi_g + a_ref / pi_ref)
    n_req <- if (abs(delta) < 1e-9) {
      NA_real_
    } else {
      ceiling(se2_const * (z_alpha + z_beta)^2 / (delta^2))
    }
    tibble(
      reference_gender = ref_gender,
      comparison_gender = g,
      delta_rd = delta,
      se2_const = se2_const,
      pair_required_n = n_req
    )
  })

  if (all(is.na(pair_tbl$pair_required_n))) {
    return(list(required_n = NA_integer_, estimated_power = NA_real_, status = "not_reached", details = pair_tbl))
  }

  required_n <- as.integer(max(pair_tbl$pair_required_n, na.rm = TRUE))
  required_n <- max(required_n, 4L * k)

  # Conservative achieved power summary at required N: minimum across interaction terms.
  achieved_tbl <- pair_tbl %>%
    mutate(
      z_eff = abs(delta_rd) / sqrt(se2_const / required_n),
      pair_power = pnorm(z_eff - z_alpha)
    )
  estimated_power <- min(achieved_tbl$pair_power, na.rm = TRUE)
  status <- ifelse(is.finite(estimated_power) && estimated_power >= target_power, "reached", "not_reached")

  list(
    required_n = required_n,
    estimated_power = estimated_power,
    status = status,
    details = achieved_tbl
  )
}

power_target_rows <- list()
assumption_rows <- list()
detail_rows <- list()

for (e in seq_len(nrow(endpoint_defs))) {
  endpoint <- endpoint_defs$endpoint[e]
  outcome_var <- endpoint_defs$outcome_var[e]
  cat(sprintf("Planning endpoint: %s\n", endpoint))

  built <- build_cell_assumptions(pumf, outcome_var)
  assumptions <- built$assumptions

  assumption_rows[[length(assumption_rows) + 1]] <- assumptions %>%
    mutate(endpoint = endpoint, outcome = outcome_var)

  for (scenario_name in unique(assumptions$scenario)) {
    assump_sc <- assumptions %>% filter(scenario == scenario_name)

    for (strategy in c("equal_strata", "observed_strata")) {
      prev_n <- 0
      for (pt in power_targets) {
        fit <- fast_interaction_plan(assump_sc, target_power = pt, strategy = strategy, alpha = alpha)
        req_n <- fit$required_n
        if (!is.na(req_n)) {
          req_n <- max(req_n, prev_n)
          prev_n <- req_n
        }

        power_target_rows[[length(power_target_rows) + 1]] <- tibble(
          endpoint = endpoint,
          outcome = outcome_var,
          scenario = scenario_name,
          allocation_strategy = strategy,
          target_power = pt,
          alpha = alpha,
          required_n = req_n,
          estimated_power = fit$estimated_power,
          status = fit$status,
          compute_method = "analytic_interaction_rd_fast",
          analysis_mode = "observational",
          power_scope = "a_priori_interaction"
        )

        if (!is.null(fit$details) && nrow(fit$details) > 0) {
          detail_rows[[length(detail_rows) + 1]] <- fit$details %>%
            mutate(
              endpoint = endpoint,
              scenario = scenario_name,
              allocation_strategy = strategy,
              target_power = pt,
              alpha = alpha,
              compute_method = "analytic_interaction_rd_fast",
              analysis_mode = "observational",
              power_scope = "a_priori_interaction"
            )
        }
      }
    }
  }
}

interaction_targets <- bind_rows(power_target_rows)
interaction_assumptions <- bind_rows(assumption_rows) %>%
  mutate(
    assumption_type = "cell_probability",
    analysis_mode = "observational",
    power_scope = "a_priori_interaction"
  )

interaction_details <- bind_rows(detail_rows)

imbalance_penalty <- interaction_targets %>%
  select(endpoint, scenario, target_power, allocation_strategy, required_n, status) %>%
  pivot_wider(names_from = allocation_strategy, values_from = c(required_n, status)) %>%
  mutate(
    imbalance_penalty_n = required_n_observed_strata - required_n_equal_strata,
    penalty_status = case_when(
      is.na(required_n_observed_strata) | is.na(required_n_equal_strata) ~ "not_estimable",
      imbalance_penalty_n >= 0 ~ "non_negative",
      TRUE ~ "unexpected_negative"
    ),
    analysis_mode = "observational",
    power_scope = "a_priori_interaction"
  )

write_csv(interaction_targets, file.path(output_dir, "power_interaction_sample_size_targets.csv"))
write_csv(interaction_assumptions, file.path(output_dir, "power_interaction_assumptions.csv"))
if (nrow(interaction_details) > 0) {
  write_csv(interaction_details, file.path(output_dir, "power_interaction_pairwise_details.csv"))
}
write_csv(imbalance_penalty, file.path(output_dir, "power_interaction_imbalance_penalty.csv"))

# ----------------------------------------------------------------------------
# Prospective stratified block randomization planner
# ----------------------------------------------------------------------------
cat("--- Prospective randomization block planning ---\n")

make_block_schedule <- function(strata_counts, block_sizes = c(4L, 6L, 8L), seed = 42L) {
  set.seed(seed)
  out <- list()

  for (g in names(strata_counts)) {
    n_g <- as.integer(strata_counts[[g]])
    assigned <- 0L
    rows <- list()
    block_id <- 0L

    while (assigned < n_g) {
      remaining <- n_g - assigned
      possible <- block_sizes[block_sizes <= remaining]
      if (length(possible) == 0) {
        b <- remaining
      } else {
        b <- sample(possible, 1)
      }

      block_id <- block_id + 1L
      n_t <- floor(b / 2)
      n_c <- b - n_t
      alloc <- sample(c(rep("Control", n_c), rep("Treatment", n_t)))

      rows[[length(rows) + 1]] <- tibble(
        gender = g,
        block_id = block_id,
        block_size = b,
        unit_in_block = seq_len(b),
        assignment = alloc
      )

      assigned <- assigned + b
    }

    out[[length(out) + 1]] <- bind_rows(rows)
  }

  bind_rows(out)
}

blueprint_rows <- list()
for (endpoint in unique(interaction_targets$endpoint)) {
  rows <- interaction_targets %>%
    filter(endpoint == !!endpoint, scenario == "pilot_observed", allocation_strategy == "equal_strata") %>%
    arrange(target_power)

  for (i in seq_len(nrow(rows))) {
    r <- rows[i, ]
    if (is.na(r$required_n)) next

    a <- interaction_assumptions %>%
      filter(endpoint == !!endpoint, scenario == "pilot_observed") %>%
      distinct(gender, observed_prop)
    n_g <- allocate_by_strategy(r$required_n, a$gender, setNames(a$observed_prop, a$gender), "equal_strata")

    blueprint_rows[[length(blueprint_rows) + 1]] <- tibble(
      endpoint = endpoint,
      scenario = "pilot_observed",
      target_power = r$target_power,
      required_n = r$required_n,
      gender = names(n_g),
      stratum_n = as.integer(n_g),
      block_sizes_allowed = "4,6,8",
      analysis_mode = "observational",
      design_mode = "prospective_trial_planning"
    )
  }
}
randomization_blueprint <- bind_rows(blueprint_rows)
write_csv(randomization_blueprint, file.path(output_dir, "randomization_block_blueprints.csv"))

for (endpoint in unique(interaction_targets$endpoint)) {
  pick <- interaction_targets %>%
    filter(
      endpoint == !!endpoint,
      scenario == "pilot_observed",
      allocation_strategy == "equal_strata",
      target_power == 0.80,
      status == "reached"
    ) %>%
    slice_head(n = 1)

  if (nrow(pick) == 0 || is.na(pick$required_n[1])) next

  a <- interaction_assumptions %>%
    filter(endpoint == !!endpoint, scenario == "pilot_observed") %>%
    distinct(gender, observed_prop)
  n_g <- allocate_by_strategy(pick$required_n[1], a$gender, setNames(a$observed_prop, a$gender), "equal_strata")

  sched <- make_block_schedule(n_g, block_sizes = c(4L, 6L, 8L), seed = 42L) %>%
    mutate(
      endpoint = endpoint,
      target_power = 0.80,
      analysis_mode = "observational",
      design_mode = "prospective_trial_planning"
    ) %>%
    select(endpoint, target_power, gender, block_id, block_size, unit_in_block, assignment, analysis_mode, design_mode)

  out_name <- paste0("randomization_schedule_example_", endpoint, ".csv")
  write_csv(sched, file.path(output_dir, out_name))
}

# ----------------------------------------------------------------------------
# Official doc alignment checklist artifact
# ----------------------------------------------------------------------------
alignment_check <- tribble(
  ~requirement_id, ~source_doc, ~requirement_text, ~implementation_evidence_path, ~status,
  "OFFICIAL-01", "Sex-differences commentary", "Use hypothesis-driven interaction-focused power planning", "surveillance/investigation/05_power_design.R", "satisfied",
  "OFFICIAL-02", "Sex-differences commentary", "Ensure adequate power for sex/gender interaction analyses", "data/public/outputs/power_interaction_sample_size_targets.csv", "satisfied",
  "OFFICIAL-03", "Sex-differences commentary", "Promote near-equal subgroup sample planning", "data/public/outputs/power_interaction_imbalance_penalty.csv", "satisfied",
  "OFFICIAL-04", "Sex-differences commentary", "Include transgender/gender-diverse groups when available", "surveillance/investigation/05_power_design.R", "satisfied",
  "OFFICIAL-05", "CPADS PUMF guidance", "Keep observational CPADS analyses clearly non-randomized", "surveillance/investigation/09_report.R", "satisfied",
  "OFFICIAL-06", "CPADS PUMF guidance", "Respect available variable definitions in analytic data", "surveillance/investigation/03_data_wrangling.R", "partial_data_constraint",
  "OFFICIAL-07", "Sex-differences commentary", "Collect two-step sex-at-birth + current gender to reduce misclassification", "data/public/outputs/official_doc_alignment_checklist.csv", "partial_data_constraint",
  "OFFICIAL-08", "Study design upgrade", "Provide prospective stratified block randomization planning artifacts", "data/public/outputs/randomization_block_blueprints.csv", "not_applicable_observational"
) %>%
  mutate(
    analysis_mode = "observational",
    design_mode = ifelse(requirement_id == "OFFICIAL-08", "prospective_trial_planning", "observational")
  )

write_csv(alignment_check, file.path(output_dir, "official_doc_alignment_checklist.csv"))

# ----------------------------------------------------------------------------
# Keep existing power curves figure for continuity
# ----------------------------------------------------------------------------
effect_grid <- seq(0.01, 0.30, by = 0.005)
df_gender <- max(1, (length(unique(na.omit(pumf$gender))) - 1))
power_curves <- tibble(
  effect_size = effect_grid,
  power_1p_srs = map_dbl(effect_grid, ~ pwr.p.test(h = .x, n = n_total, sig.level = 0.05)$power),
  power_2p_srs = map_dbl(effect_grid, ~ pwr.2p.test(h = .x, n = floor(n_total / 2), sig.level = 0.05)$power),
  power_chi_srs = map_dbl(effect_grid, ~ pwr.chisq.test(w = .x, N = n_total, df = df_gender, sig.level = 0.05)$power)
)

pdf(file.path(fig_dir, "power_curves.pdf"), width = 10, height = 6)
plot_df <- power_curves %>%
  pivot_longer(-effect_size, names_to = "curve", values_to = "power")
ggplot(plot_df, aes(x = effect_size, y = power, color = curve)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0.80, linetype = "dashed", alpha = 0.5) +
  labs(
    title = sprintf("Power Curves: CPADS heavy_drinking_30d (n_nonmissing=%d)", n_total),
    subtitle = "Legacy descriptive curves retained for compatibility",
    x = "Effect size (Cohen's h or w)",
    y = "Power",
    color = "Curve"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1))
dev.off()

cat("Saved legacy + interaction power outputs.\n")
cat("=== POWER DESIGN COMPLETE ===\n")
