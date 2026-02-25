#!/usr/bin/env Rscript
# =============================================================================
# 05_power_design.R — Phase 5: Power Design (Official-doc aligned)
# =============================================================================
# Primary objective:
#   A priori interaction-term power planning for gender-difference analyses.
# Secondary objective:
#   Preserve legacy post hoc outputs for backward compatibility.
#
# This module keeps data analyses observational and generates prospective
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

df_path <- file.path(wrangled_dir, "data_wrangled.rds")
svy_path <- file.path(wrangled_dir, "data_survey.rds")
if (!file.exists(df_path)) stop("Missing file: ", df_path)
df <- readRDS(df_path)

if (file.exists(svy_path)) {
  df_svy <- readRDS(svy_path)
} else {
  if (!("weight" %in% names(df))) stop("Cannot build survey design: missing weight")
  df_svy <- survey::svydesign(ids = ~1, weights = ~weight, data = df)
}

required <- c("weight", "heavy_drinking_30d", "gender", "cannabis_any_use")
missing <- setdiff(required, names(df))
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

weighted_var <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  x <- x[ok]
  w <- w[ok]
  sw <- sum(w)
  if (!is.finite(sw) || sw <= 0) return(NA_real_)
  mu <- sum(w * x) / sw
  sum(w * (x - mu)^2) / sw
}

# ----------------------------------------------------------------------------
# Legacy descriptive/post hoc outputs (backward compatibility)
# ----------------------------------------------------------------------------
cat("--- Legacy descriptive/post hoc power outputs (compatibility) ---\n")

n_total <- sum(!is.na(df$heavy_drinking_30d))
y_total <- sum(df$heavy_drinking_30d == 1, na.rm = TRUE)
p_unw <- y_total / n_total

svy_est <- survey::svymean(~heavy_drinking_30d, df_svy, na.rm = TRUE, deff = "replace")
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

prev_by_gender <- survey::svyby(~heavy_drinking_30d, ~gender, df_svy, survey::svymean, na.rm = TRUE)
df_g <- as.data.frame(prev_by_gender)
prev_col <- if ("heavy_drinking_30d" %in% names(df_g)) "heavy_drinking_30d" else setdiff(names(df_g), "gender")[1]
gender_levels <- as.character(df_g$gender)
p_gender <- setNames(as.numeric(df_g[[prev_col]]), gender_levels)
ok_g <- !is.na(df$heavy_drinking_30d) & !is.na(df$gender)
n_by_gender <- table(df$gender[ok_g])
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
  value = c(as.character(p_unw), as.character(p_wt), as.character(deff_val), as.character(n_total), as.character(n_eff_total), "legacy_descriptive_post_hoc"),
  text_value = c("n/a", "n/a", "n/a", "n/a", "n/a", "legacy_descriptive_post_hoc"),
  analysis_mode = "observational",
  power_scope = "descriptive_post_hoc"
)

write_csv(oneprop_tbl, file.path(output_dir, "power_one_proportion_grid.csv"))
if (nrow(two_prop_tbl) > 0) {
  write_csv(two_prop_tbl, file.path(output_dir, "power_two_proportion_gender.csv"))
}
write_csv(power_summary, file.path(output_dir, "power_summary.csv"))

# ----------------------------------------------------------------------------
# Interaction-first a priori power planning
# ----------------------------------------------------------------------------
cat("--- A priori interaction power planning ---\n")

power_targets <- c(0.80, 0.85, 0.90, 0.95, 0.99, 0.999)
alpha <- 0.05

endpoint_defs <- tribble(
  ~endpoint, ~outcome_var, ~outcome_type,
  "heavy_drinking_30d", "heavy_drinking_30d", "binary",
  "ebac_legal", "ebac_legal", "binary",
  "ebac_tot", "ebac_tot", "continuous"
)

endpoint_defs <- endpoint_defs %>% filter(outcome_var %in% names(df))

ebac_formula_inputs <- c("alc13a", "alc13b_a", "alc13b_b", "demq3", "demq4", "gender")
ebac_formula_missing <- setdiff(ebac_formula_inputs, names(df))
ebac_endpoint_anchors <- tibble(
  endpoint = c("ebac_tot", "ebac_legal"),
  endpoint_definition = c(
    "data derived eBAC (g/dL) on heaviest drinking day (Seidl/Widmark-based)",
    "Derived legal-threshold indicator: ebac_tot > 0.08"
  ),
  source_doc = "data 2021-22 dataset User Guide pp.85-87",
  formula_inputs_required_for_recompute = paste(ebac_formula_inputs, collapse = ";"),
  formula_recompute_feasible_in_public_df = length(ebac_formula_missing) == 0,
  missing_formula_inputs = ifelse(length(ebac_formula_missing) == 0, "NONE", paste(ebac_formula_missing, collapse = ";")),
  power_endpoint_usage = "Power planning uses data-derived measured endpoints (ebac_tot, ebac_legal).",
  analysis_mode = "observational",
  power_scope = "a_priori_interaction"
)
write_csv(ebac_endpoint_anchors, file.path(output_dir, "power_ebac_endpoint_anchors.csv"))

build_cell_assumptions <- function(df, outcome_var, outcome_type) {
  d <- df %>%
    select(all_of(c(outcome_var, "cannabis_any_use", "gender", "weight"))) %>%
    mutate(cannabis_any_use = suppressWarnings(as.integer(as.character(cannabis_any_use)))) %>%
    filter(cannabis_any_use %in% c(0L, 1L)) %>%
    drop_na()

  if (nrow(d) == 0) {
    return(list(data = d, assumptions = tibble(), gender_props = tibble()))
  }

  d$gender <- droplevels(factor(d$gender))
  gender_props <- d %>% count(gender, name = "n") %>% mutate(prop = n / sum(n))

  cell_stats <- d %>%
    group_by(gender, cannabis_any_use) %>%
    summarise(
      mean_y = weighted.mean(.data[[outcome_var]], weight, na.rm = TRUE),
      var_y = weighted_var(.data[[outcome_var]], weight),
      n_cell = n(),
      .groups = "drop"
    )

  valid_gender <- cell_stats %>%
    count(gender, name = "n_cannabis_cells") %>%
    filter(n_cannabis_cells == 2) %>%
    pull(gender)

  cell_wide <- cell_stats %>%
    filter(gender %in% valid_gender) %>%
    select(gender, cannabis_any_use, mean_y, var_y) %>%
    pivot_wider(names_from = cannabis_any_use, values_from = c(mean_y, var_y), names_glue = "{.value}{cannabis_any_use}") %>%
    rename(
      mean0 = mean_y0,
      mean1 = mean_y1,
      var0 = var_y0,
      var1 = var_y1
    ) %>%
    mutate(
      var0 = pmax(var0, 1e-8),
      var1 = pmax(var1, 1e-8)
    )

  if (nrow(cell_wide) == 0) {
    return(list(data = d, assumptions = tibble(), gender_props = gender_props))
  }

  if (identical(outcome_type, "binary")) {
    cell_wide <- cell_wide %>%
      mutate(
        mean0 = pmin(pmax(mean0, 0.01), 0.99),
        mean1 = pmin(pmax(mean1, 0.01), 0.99),
        var0 = mean0 * (1 - mean0),
        var1 = mean1 * (1 - mean1)
      )

    odds <- function(p) p / (1 - p)
    lor <- log(odds(cell_wide$mean1) / odds(cell_wide$mean0))
    lor_center <- mean(lor, na.rm = TRUE)
    dir <- ifelse(lor >= lor_center, 1, -1)

    scenario_tbl <- bind_rows(
      cell_wide %>% mutate(scenario = "pilot_observed"),
      cell_wide %>%
        mutate(
          scenario = "conservative_benchmark",
          lor_new = lor_center + 0.30 * dir,
          mean1 = plogis(qlogis(mean0) + lor_new),
          var1 = mean1 * (1 - mean1)
        ) %>%
        select(gender, mean0, mean1, var0, var1, scenario)
    )
  } else {
    # For continuous outcomes (eBAC total), keep pilot-observed assumptions.
    # This avoids introducing arbitrary benchmark shifts in a scale-dependent metric.
    scenario_tbl <- cell_wide %>% mutate(scenario = "pilot_observed")
  }

  scenario_tbl <- scenario_tbl %>%
    mutate(
      outcome_type = outcome_type
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
  fractional_target <- total_n * props
  n_vec <- floor(fractional_target)
  rem <- total_n - sum(n_vec)
  if (rem > 0) {
    add_idx <- order(fractional_target - n_vec, decreasing = TRUE)[seq_len(rem)]
    n_vec[add_idx] <- n_vec[add_idx] + 1
  }
  names(n_vec) <- levels_gender
  n_vec
}

stop_if_false <- function(cond, msg) {
  if (!isTRUE(cond)) stop(msg, call. = FALSE)
}

# Deterministic decomposition for balanced block randomization.
# This module currently standardizes on block sizes 4/6/8.
build_balanced_blocks <- function(target_n, block_sizes = c(4L, 6L, 8L)) {
  block_sizes <- sort(unique(as.integer(block_sizes)))
  if (!identical(block_sizes, c(4L, 6L, 8L))) {
    stop("This planner currently requires block_sizes = c(4, 6, 8).", call. = FALSE)
  }

  target_n <- as.integer(target_n)
  if (length(target_n) != 1 || is.na(target_n) || target_n < 0L) {
    stop("Invalid stratum target size for block planning.", call. = FALSE)
  }

  # Only even Ns can be fully allocated into balanced two-arm blocks.
  top_up_n <- target_n %% 2L
  scheduled_n <- target_n - top_up_n

  if (scheduled_n > 0L && scheduled_n < min(block_sizes)) {
    top_up_n <- target_n
    scheduled_n <- 0L
  }

  blocks <- integer()
  if (scheduled_n > 0L) {
    n8 <- scheduled_n %/% 8L
    rem <- scheduled_n %% 8L
    blocks <- rep(8L, n8)

    if (rem == 4L) {
      blocks <- c(blocks, 4L)
    } else if (rem == 6L) {
      blocks <- c(blocks, 6L)
    } else if (rem == 2L) {
      stop_if_false(length(blocks) > 0L, "Unable to decompose stratum into allowed block sizes.")
      blocks <- c(blocks[-length(blocks)], 6L, 4L)
    } else if (rem != 0L) {
      stop("Unexpected remainder in block decomposition.", call. = FALSE)
    }
  }

  stop_if_false(
    sum(blocks) == scheduled_n,
    paste0("Block decomposition mismatch: scheduled_n=", scheduled_n, ", sum(blocks)=", sum(blocks))
  )

  list(
    blocks = as.integer(blocks),
    scheduled_n = as.integer(scheduled_n),
    top_up_n = as.integer(top_up_n)
  )
}

fast_interaction_plan <- function(assump_df, target_power, strategy, alpha = 0.05) {
  # Fast closed-form approximation: O(k) per target/strategy/scenario
  # where k = number of gender strata (vs Monte Carlo simulation loops).
  genders <- as.character(assump_df$gender)
  k <- length(genders)
  if (k < 2) {
    return(list(required_n = NA_integer_, estimated_power = NA_real_, status = "not_reached"))
  }

  ref_gender <- if ("Female" %in% genders) "Female" else genders[1]
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
  a_ref <- pmax(ref_row$var1 + ref_row$var0, 1e-8)
  rd_ref <- ref_row$mean1 - ref_row$mean0
  pi_ref <- alloc_props[[ref_gender]]

  pair_tbl <- map_dfr(non_ref, function(g) {
    gr <- assump_df %>% filter(gender == g) %>% slice(1)
    a_g <- pmax(gr$var1 + gr$var0, 1e-8)
    rd_g <- gr$mean1 - gr$mean0
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
feasibility_rows <- list()
primary_levels <- c("Female", "Male")

for (e in seq_len(nrow(endpoint_defs))) {
  endpoint <- endpoint_defs$endpoint[e]
  outcome_var <- endpoint_defs$outcome_var[e]
  outcome_type <- endpoint_defs$outcome_type[e]
  cat(sprintf("Planning endpoint: %s\n", endpoint))

  built <- build_cell_assumptions(df, outcome_var, outcome_type)
  assumptions <- built$assumptions
  if (nrow(assumptions) == 0) next

  assumption_rows[[length(assumption_rows) + 1]] <- assumptions %>%
    mutate(endpoint = endpoint, outcome = outcome_var, outcome_type = outcome_type)

  # Primary interaction target: Female vs Male.
  # Other gender categories are retained as explicit feasibility flags.
  has_primary <- all(primary_levels %in% assumptions$gender)
  if (!has_primary) {
    warning("Primary levels Female/Male not available for endpoint: ", endpoint)
    next
  }

  for (scenario_name in unique(assumptions$scenario)) {
    assump_sc <- assumptions %>%
      filter(scenario == scenario_name, gender %in% primary_levels)

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
          outcome_type = outcome_type,
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

    non_primary <- assumptions %>%
      filter(scenario == scenario_name, !(gender %in% primary_levels)) %>%
      select(gender, observed_prop) %>%
      distinct()
    if (nrow(non_primary) > 0) {
      feasibility_rows[[length(feasibility_rows) + 1]] <- non_primary %>%
        mutate(
          endpoint = endpoint,
          scenario = scenario_name,
          status = "underpowered_flagged_non_primary",
          note = "Primary interaction-power design targets Female vs Male; non-primary groups are retained as feasibility flags.",
          analysis_mode = "observational",
          power_scope = "a_priori_interaction"
        )
    }
  }
}

interaction_targets <- bind_rows(power_target_rows)
interaction_assumptions <- bind_rows(assumption_rows) %>%
  mutate(
    assumption_type = case_when(
      outcome_type == "binary" ~ "cell_probability",
      outcome_type == "continuous" ~ "cell_mean_variance",
      TRUE ~ "cell_parameter"
    ),
    analysis_mode = "observational",
    power_scope = "a_priori_interaction"
  )

interaction_details <- bind_rows(detail_rows)
interaction_feasibility <- bind_rows(feasibility_rows)

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
if (nrow(interaction_feasibility) > 0) {
  write_csv(interaction_feasibility, file.path(output_dir, "power_interaction_feasibility_flags.csv"))
}
write_csv(imbalance_penalty, file.path(output_dir, "power_interaction_imbalance_penalty.csv"))

# Explicit integer allocation table (no fractional participants):
# Female/Male n1/n2 for each endpoint x scenario x strategy x target power.
interaction_group_alloc <- map_dfr(seq_len(nrow(interaction_targets)), function(i) {
  r <- interaction_targets[i, ]
  if (is.na(r$required_n)) return(tibble())

  a <- interaction_assumptions %>%
    filter(endpoint == r$endpoint, scenario == r$scenario, gender %in% primary_levels) %>%
    distinct(gender, observed_prop)
  if (nrow(a) < 2) return(tibble())

  n_g <- allocate_by_strategy(
    total_n = as.integer(r$required_n),
    levels_gender = as.character(a$gender),
    observed_props = setNames(a$observed_prop, a$gender),
    strategy = as.character(r$allocation_strategy)
  )

  n_female <- if ("Female" %in% names(n_g)) as.integer(n_g[["Female"]]) else NA_integer_
  n_male <- if ("Male" %in% names(n_g)) as.integer(n_g[["Male"]]) else NA_integer_

  tibble(
    endpoint = r$endpoint,
    outcome = r$outcome,
    outcome_type = r$outcome_type,
    scenario = r$scenario,
    allocation_strategy = r$allocation_strategy,
    target_power = r$target_power,
    alpha = r$alpha,
    total_n = as.integer(r$required_n),
    group1 = "Female",
    n1 = n_female,
    group2 = "Male",
    n2 = n_male,
    n_sum_check = as.integer(n_female + n_male),
    integer_n_check = ifelse(
      is.na(n_female) | is.na(n_male),
      FALSE,
      (n_female %% 1L == 0L) & (n_male %% 1L == 0L) & (as.integer(r$required_n) %% 1L == 0L)
    ),
    status = r$status,
    compute_method = r$compute_method,
    analysis_mode = r$analysis_mode,
    power_scope = r$power_scope
  )
})

write_csv(interaction_group_alloc, file.path(output_dir, "power_interaction_group_allocations.csv"))

# ----------------------------------------------------------------------------
# G*Power-style two-group references (standardized effect-size planning)
# ----------------------------------------------------------------------------
cat("--- G*Power-style two-group references ---\n")

gpower_effects <- tribble(
  ~test_family, ~effect_metric, ~effect_size,
  "two_sample_t", "d", 0.20,
  "two_sample_t", "d", 0.30,
  "two_sample_t", "d", 0.40,
  "two_sample_t", "d", 0.42, # ~180 total at 80% reference
  "two_sample_t", "d", 0.50,
  "two_sample_t", "d", 0.80,
  "two_proportion", "h", 0.20,
  "two_proportion", "h", 0.30,
  "two_proportion", "h", 0.40, # ~200 total at 80% reference
  "two_proportion", "h", 0.50,
  "two_proportion", "h", 0.80
)

gpower_rows <- list()
for (i in seq_len(nrow(gpower_effects))) {
  row <- gpower_effects[i, ]
  for (pt in power_targets) {
    n_per_group <- if (row$test_family == "two_sample_t") {
      ceiling(pwr.t.test(
        n = NULL, d = row$effect_size, sig.level = alpha, power = pt,
        type = "two.sample", alternative = "two.sided"
      )$n)
    } else {
      ceiling(pwr.2p.test(
        n = NULL, h = row$effect_size, sig.level = alpha, power = pt,
        alternative = "two.sided"
      )$n)
    }

    gpower_rows[[length(gpower_rows) + 1]] <- tibble(
      test_family = row$test_family,
      effect_metric = row$effect_metric,
      effect_size = row$effect_size,
      target_power = pt,
      alpha = alpha,
      n_per_group = as.integer(n_per_group),
      total_n = as.integer(2L * n_per_group),
      group_design = "equal_two_group",
      compute_method = "pwr_gpower_style",
      analysis_mode = "observational",
      design_mode = "prospective_trial_planning",
      power_scope = "gpower_reference"
    )
  }
}
gpower_tbl <- bind_rows(gpower_rows)
write_csv(gpower_tbl, file.path(output_dir, "power_gpower_reference_two_group.csv"))

# ----------------------------------------------------------------------------
# Prospective stratified block randomization planner
# ----------------------------------------------------------------------------
cat("--- Prospective randomization block planning ---\n")

make_block_schedule <- function(strata_counts, block_sizes = c(4L, 6L, 8L), seed = 42L) {
  set.seed(seed)
  out_rows <- list()
  summary_rows <- list()

  for (g in names(strata_counts)) {
    n_g <- as.integer(strata_counts[[g]])
    plan <- build_balanced_blocks(n_g, block_sizes = block_sizes)
    blocks <- plan$blocks
    block_id <- 0L
    rows <- list()

    if (length(blocks) > 0) {
      # Randomize block order while preserving only allowed block sizes.
      blocks <- sample(blocks, size = length(blocks), replace = FALSE)
      for (b in blocks) {
        block_id <- block_id + 1L
        n_half <- as.integer(b / 2L)
        alloc <- sample(c(rep("Control", n_half), rep("Treatment", n_half)))

        rows[[length(rows) + 1]] <- tibble(
          gender = g,
          block_id = block_id,
          block_size = as.integer(b),
          unit_in_block = seq_len(as.integer(b)),
          assignment = alloc
        )
      }
      out_rows[[length(out_rows) + 1]] <- bind_rows(rows)
    }

    summary_rows[[length(summary_rows) + 1]] <- tibble(
      gender = g,
      stratum_target_n = n_g,
      scheduled_n = plan$scheduled_n,
      top_up_n = plan$top_up_n,
      top_up_required = plan$top_up_n > 0L,
      n_blocks = length(blocks),
      block_sizes_allowed = paste(block_sizes, collapse = ",")
    )
  }

  schedule_df <- bind_rows(out_rows)
  summary_df <- bind_rows(summary_rows)

  if (nrow(schedule_df) > 0) {
    qc <- schedule_df %>%
      count(gender, block_id, block_size, assignment, name = "n") %>%
      pivot_wider(names_from = assignment, values_from = n, values_fill = list(n = 0L))
    if (!("Control" %in% names(qc))) qc$Control <- 0L
    if (!("Treatment" %in% names(qc))) qc$Treatment <- 0L
    qc <- qc %>%
      mutate(
        size_ok = (Control + Treatment) == block_size,
        balanced_block = Control == Treatment
      )

    stop_if_false(all(qc$size_ok), "Randomization schedule contains malformed block sizes.")
    stop_if_false(all(qc$balanced_block), "Randomization schedule contains unbalanced assignments within blocks.")
    stop_if_false(
      all(qc$block_size %in% block_sizes),
      "Randomization schedule contains block sizes outside allowed values."
    )
  }

  list(schedule = schedule_df, summary = summary_df)
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
      filter(endpoint == !!endpoint, scenario == "pilot_observed", gender %in% primary_levels) %>%
      distinct(gender, observed_prop)
    if (nrow(a) == 0) next
    n_g <- allocate_by_strategy(r$required_n, a$gender, setNames(a$observed_prop, a$gender), "equal_strata")
    block_meta <- map_dfr(names(n_g), function(g) {
      plan <- build_balanced_blocks(as.integer(n_g[[g]]), block_sizes = c(4L, 6L, 8L))
      tibble(
        gender = g,
        stratum_n = as.integer(n_g[[g]]),
        scheduled_stratum_n = plan$scheduled_n,
        top_up_n = plan$top_up_n
      )
    })

    blueprint_rows[[length(blueprint_rows) + 1]] <- tibble(
      endpoint = endpoint,
      scenario = "pilot_observed",
      target_power = r$target_power,
      required_n = r$required_n,
      gender = block_meta$gender,
      stratum_n = block_meta$stratum_n,
      scheduled_stratum_n = block_meta$scheduled_stratum_n,
      top_up_n = block_meta$top_up_n,
      block_sizes_allowed = "4,6,8",
      analysis_mode = "observational",
      design_mode = "prospective_trial_planning"
    )
  }
}
randomization_blueprint <- bind_rows(blueprint_rows)
write_csv(randomization_blueprint, file.path(output_dir, "randomization_block_blueprints.csv"))

schedule_files_written <- character()
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
    filter(endpoint == !!endpoint, scenario == "pilot_observed", gender %in% primary_levels) %>%
    distinct(gender, observed_prop)
  if (nrow(a) == 0) next
  n_g <- allocate_by_strategy(pick$required_n[1], a$gender, setNames(a$observed_prop, a$gender), "equal_strata")

  plan <- make_block_schedule(n_g, block_sizes = c(4L, 6L, 8L), seed = 42L)
  sched <- plan$schedule %>%
    left_join(
      plan$summary %>%
        select(gender, stratum_target_n, scheduled_n, top_up_n, top_up_required),
      by = "gender"
    ) %>%
    mutate(
      endpoint = endpoint,
      target_power = 0.80,
      analysis_mode = "observational",
      design_mode = "prospective_trial_planning"
    ) %>%
    select(
      endpoint, target_power, gender, block_id, block_size, unit_in_block, assignment,
      stratum_target_n, scheduled_n, top_up_n, top_up_required,
      analysis_mode, design_mode
    )

  out_name <- paste0("randomization_schedule_example_", endpoint, ".csv")
  out_path <- file.path(output_dir, out_name)
  write_csv(sched, out_path)
  schedule_files_written <- c(schedule_files_written, out_path)
}

# ----------------------------------------------------------------------------
# Quality gates (required by guardrail.md)
# ----------------------------------------------------------------------------
cat("--- Quality gate checks ---\n")

monotonic_check <- interaction_targets %>%
  arrange(endpoint, scenario, allocation_strategy, target_power) %>%
  group_by(endpoint, scenario, allocation_strategy) %>%
  summarise(
    monotonic_required_n = {
      x <- required_n[!is.na(required_n)]
      length(x) <= 1 || all(diff(x) >= 0)
    },
    .groups = "drop"
  )
if (any(!monotonic_check$monotonic_required_n)) {
  bad <- monotonic_check %>%
    filter(!monotonic_required_n) %>%
    transmute(label = paste(endpoint, scenario, allocation_strategy, sep = "|")) %>%
    pull(label)
  stop("Power monotonicity check failed for: ", paste(bad, collapse = ", "), call. = FALSE)
}

equal_alloc_check <- interaction_group_alloc %>%
  filter(allocation_strategy == "equal_strata") %>%
  mutate(
    integrity_ok = integer_n_check & (n_sum_check == total_n) &
      !is.na(n1) & !is.na(n2) & abs(n1 - n2) <= 1L
  )
stop_if_false(nrow(equal_alloc_check) > 0, "Equal-allocation integrity check could not run.")
stop_if_false(
  all(equal_alloc_check$integrity_ok),
  "Equal-allocation integrity check failed in power_interaction_group_allocations.csv."
)

penalty_estimable <- imbalance_penalty %>% filter(!is.na(imbalance_penalty_n))
stop_if_false(nrow(penalty_estimable) > 0, "Imbalance penalty check could not run.")
stop_if_false(
  all(penalty_estimable$imbalance_penalty_n >= 0),
  "Observed-imbalance penalty is negative for at least one estimable row."
)

stop_if_false(length(schedule_files_written) > 0, "Randomization schedule generation failed.")
schedule_qc <- map_dfr(schedule_files_written, ~ read_csv(.x, show_col_types = FALSE))
stop_if_false(nrow(schedule_qc) > 0, "Randomization schedule QC could not run (empty schedules).")
stop_if_false(
  all(schedule_qc$block_size %in% c(4L, 6L, 8L)),
  "Randomization schedule contains disallowed block sizes (allowed: 4, 6, 8)."
)
block_qc <- schedule_qc %>%
  count(endpoint, target_power, gender, block_id, block_size, assignment, name = "n") %>%
  pivot_wider(names_from = assignment, values_from = n, values_fill = list(n = 0L))
if (!("Control" %in% names(block_qc))) block_qc$Control <- 0L
if (!("Treatment" %in% names(block_qc))) block_qc$Treatment <- 0L
block_qc <- block_qc %>%
  mutate(
    size_ok = (Control + Treatment) == block_size,
    balanced_block = Control == Treatment
  )
stop_if_false(all(block_qc$size_ok), "Randomization block size accounting check failed.")
stop_if_false(all(block_qc$balanced_block), "Randomization block balance check failed.")

cat("Quality gate checks passed.\n")

# ----------------------------------------------------------------------------
# Official doc alignment checklist artifact
# ----------------------------------------------------------------------------
alignment_check <- tribble(
  ~requirement_id, ~source_doc, ~requirement_text, ~implementation_evidence_path, ~status,
  "OFFICIAL-01", "Sex-differences commentary", "Use hypothesis-driven interaction-focused power planning", "surveillance/investigation/05_power_design.R", "satisfied",
  "OFFICIAL-02", "Sex-differences commentary", "Ensure adequate power for predefined interaction analyses", "data/public/outputs/power_interaction_sample_size_targets.csv", "satisfied",
  "OFFICIAL-03", "Sex-differences commentary", "Promote near-equal subgroup sample planning", "data/public/outputs/power_interaction_group_allocations.csv", "satisfied",
  "OFFICIAL-04", "Sex-differences commentary", "Power should be robust to unequal subgroup sizes", "data/public/outputs/power_interaction_imbalance_penalty.csv", "satisfied",
  "OFFICIAL-05", "Sex-differences commentary", "Include non-binary groups when available", "data/public/outputs/power_interaction_feasibility_flags.csv", "satisfied",
  "OFFICIAL-06", "Sex-differences commentary", "Operationalize groups using available sex categories (Female/Male/Non-binary)", "surveillance/investigation/03_data_wrangling.R", "satisfied",
  "OFFICIAL-07", "Sex-differences commentary", "Set primary interaction power on Female vs Male and retain non-primary groups as explicit feasibility outputs", "data/public/outputs/power_interaction_feasibility_flags.csv", "satisfied",
  "OFFICIAL-08", "Sex-differences commentary", "Capture within-group physiological variability (e.g., hormone-cycle/lifespan indicators) when available", "surveillance/ebac/07_ebac_integrations.R", "partial_data_constraint",
  "OFFICIAL-09", "Sex-differences commentary", "Report subgroup baseline summaries by gender (self-reported)", "data/public/outputs/frequentist_heavy_drinking_prevalence_ci.csv", "satisfied",
  "OFFICIAL-10", "data dataset guide pages 85-87", "Use documented eBAC derivation context and legal-threshold mapping", "surveillance/investigation/09_report.R", "satisfied",
  "OFFICIAL-11", "data dataset guide pages 85-87", "Handle missing anthropometrics in public dataset by using measured eBAC outputs", "surveillance/ebac/07_ebac.R", "satisfied",
  "OFFICIAL-12", "data dataset guidance", "Keep observational data analyses clearly non-randomized", "surveillance/investigation/09_report.R", "satisfied",
  "OFFICIAL-13", "Study design upgrade", "Provide prospective stratified block randomization planning artifacts", "data/public/outputs/randomization_block_blueprints.csv", "not_applicable_observational",
  "OFFICIAL-14", "data dataset guide pages 85-87", "Anchor eBAC power endpoints to documented Seidl/Widmark formula definitions", "data/public/outputs/power_ebac_endpoint_anchors.csv", "satisfied"
) %>%
  mutate(
    analysis_mode = "observational",
    design_mode = ifelse(requirement_id == "OFFICIAL-13", "prospective_trial_planning", "observational")
  )

write_csv(alignment_check, file.path(output_dir, "official_doc_alignment_checklist.csv"))

cat("Saved interaction power and planning outputs.\n")
cat("=== POWER DESIGN COMPLETE ===\n")
