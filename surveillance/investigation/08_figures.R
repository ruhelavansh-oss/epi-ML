#!/usr/bin/env Rscript
# =============================================================================
# 08_figures.R - Phase 8: Publication Figures
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(tibble)
  library(stringr)
  library(forcats)
  library(purrr)
  library(survey)
})

options(scipen = 999)
set.seed(42)
source("surveillance/lib/config_paths.R")
paths <- get_paths()
init_paths(paths)


wrangled_dir <- paths$wrangled_dir
output_dir <- paths$output_private_dir
fig_dir <- paths$figures_dir
cat("=== PHASE 8: GENERATING FIGURES ===\n\n")

# --- 1. CSADS Alcohol Temporal Trends ---
cat("--- 1. CSADS Alcohol Trends ---\n")
csads_path <- file.path(wrangled_dir, "csads_alcohol.rds")
if (!file.exists(csads_path)) {
  cat("CSADS trend skipped: optional csads_alcohol.rds not available.\n")
} else tryCatch({
  csads_alc <- readRDS(csads_path)

  trend_data <- csads_alc %>%
    dplyr::filter(question == "Drinking alcohol",
           group == "Overall" | subgroup == "Overall",
           period_of_time == "Past 12 months") %>%
    dplyr::filter(!is.na(prevalence_pct), !is.na(school_year_num)) %>%
    dplyr::distinct(school_year_num, .keep_all = TRUE) %>%
    dplyr::arrange(school_year_num)

  if (nrow(trend_data) > 0) {
    p1 <- ggplot(trend_data, aes(x = school_year_num, y = prevalence_pct)) +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_point(color = "steelblue", size = 3) +
      {if (all(!is.na(trend_data$ci_lower) & !is.na(trend_data$ci_upper)))
        geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "steelblue")} +
      labs(title = "Alcohol Use Among Canadian Students (Grades 7-12)",
           subtitle = "Past 12-month prevalence, CSADS 2008-2023",
           x = "School Year", y = "Prevalence (%)") +
      theme_minimal() +
      scale_x_continuous(breaks = unique(trend_data$school_year_num))

    ggsave(file.path(fig_dir, "csads_alcohol_trends.pdf"), p1, width = 10, height = 6)
    ggsave(file.path(fig_dir, "csads_alcohol_trends.png"), p1, width = 10, height = 6, dpi = 300)
    cat("Saved: csads_alcohol_trends.pdf/.png\n")
  } else {
    cat("No trend data available after filtering\n")
  }
}, error = function(e) cat("CSADS trend error:", e$message, "\n"))

# --- 2. CADS Substance Comparison ---
cat("\n--- 2. CADS Substance Comparison ---\n")
cads_summary_path <- file.path(output_dir, "cads_prevalence_summary.csv")
if (!file.exists(cads_summary_path)) {
  cat("CADS comparison skipped: optional cads_prevalence_summary.csv not available.\n")
} else tryCatch({
  cads_summary <- read_csv(cads_summary_path,
                           show_col_types = FALSE)

  p2 <- ggplot(cads_summary, aes(x = reorder(substance, mean_prevalence),
                                  y = mean_prevalence)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    coord_flip() +
    labs(title = "Mean Prevalence by Substance Type",
         subtitle = "CADS 2023 - All disaggregation levels",
         x = "", y = "Mean Prevalence (%)") +
    theme_minimal()

  ggsave(file.path(fig_dir, "cads_substance_comparison.pdf"), p2, width = 10, height = 6)
  ggsave(file.path(fig_dir, "cads_substance_comparison.png"), p2, width = 10, height = 6, dpi = 300)
  cat("Saved: cads_substance_comparison.pdf/.png\n")
}, error = function(e) cat("CADS comparison error:", e$message, "\n"))

# --- 3. Heavy Drinking by Demographics ---
cat("\n--- 3. Heavy Drinking by Demographics ---\n")
tryCatch({
  df <- readRDS(file.path(wrangled_dir, "data_wrangled.rds"))

  # Survey-weighted proportions
  demo_data <- df %>%
    dplyr::filter(!is.na(heavy_drinking_30d), !is.na(age_group), !is.na(gender)) %>%
    dplyr::group_by(age_group, gender) %>%
    dplyr::summarise(
      n = n(),
      heavy_pct = 100 * weighted.mean(heavy_drinking_30d, weight, na.rm = TRUE),
      .groups = "drop"
    )

  p3 <- ggplot(demo_data, aes(x = age_group, y = heavy_pct, fill = gender)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    labs(title = "Heavy Drinking Prevalence by Age and Gender",
         subtitle = "data dataset - Survey-weighted estimates",
         x = "Age Group", y = "Heavy Drinking (%)", fill = "Gender") +
    theme_minimal()

  ggsave(file.path(fig_dir, "binge_by_demographics.pdf"), p3, width = 10, height = 6)
  ggsave(file.path(fig_dir, "binge_by_demographics.png"), p3, width = 10, height = 6, dpi = 300)
  cat("Saved: binge_by_demographics.pdf/.png\n")
}, error = function(e) cat("Demographics figure error:", e$message, "\n"))

# --- 4. PHAC Opioid Death Trends ---
cat("\n--- 4. PHAC Opioid Death Trends ---\n")
phac_path <- file.path(wrangled_dir, "phac_national_annual.rds")
if (!file.exists(phac_path)) {
  cat("PHAC trends skipped: optional phac_national_annual.rds not available.\n")
} else tryCatch({
  phac_annual <- readRDS(phac_path)

  opioid_deaths <- phac_annual %>%
    dplyr::filter(substance == "Opioids", source == "Deaths", !is.na(year)) %>%
    dplyr::group_by(year) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(year)

  if (nrow(opioid_deaths) > 0) {
    p4 <- ggplot(opioid_deaths, aes(x = year, y = value)) +
      geom_line(color = "darkred", linewidth = 1) +
      geom_point(color = "darkred", size = 3) +
      labs(title = "Opioid-Related Deaths in Canada",
           subtitle = "Crude rate per 100,000, PHAC national data",
           x = "Year", y = "Crude Death Rate (per 100,000)") +
      theme_minimal()

    ggsave(file.path(fig_dir, "phac_opioid_deaths_trend.pdf"), p4, width = 10, height = 6)
    ggsave(file.path(fig_dir, "phac_opioid_deaths_trend.png"), p4, width = 10, height = 6, dpi = 300)
    cat("Saved: phac_opioid_deaths_trend.pdf/.png\n")
  }
}, error = function(e) cat("PHAC trends error:", e$message, "\n"))

# --- 5. Mental Health x Heavy Drinking ---
cat("\n--- 5. Mental Health x Heavy Drinking ---\n")
tryCatch({
  df <- readRDS(file.path(wrangled_dir, "data_wrangled.rds"))

  mh_data <- df %>%
    dplyr::filter(!is.na(heavy_drinking_30d), !is.na(mental_health)) %>%
    dplyr::group_by(mental_health) %>%
    dplyr::summarise(
      heavy_pct = 100 * weighted.mean(heavy_drinking_30d, weight, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )

  p5 <- ggplot(mh_data, aes(x = mental_health, y = heavy_pct)) +
    geom_col(fill = "steelblue", alpha = 0.8, width = 0.6) +
    labs(title = "Heavy Drinking by Self-Rated Mental Health",
         subtitle = "data dataset - Survey-weighted",
         x = "Mental Health Rating", y = "Heavy Drinking (%)") +
    theme_minimal()

  ggsave(file.path(fig_dir, "binge_by_mental_health.pdf"), p5, width = 8, height = 6)
  ggsave(file.path(fig_dir, "binge_by_mental_health.png"), p5, width = 8, height = 6, dpi = 300)
  cat("Saved: binge_by_mental_health.pdf/.png\n")
}, error = function(e) cat("MH figure error:", e$message, "\n"))

cat("\n=== ALL FIGURES GENERATED ===\n")
