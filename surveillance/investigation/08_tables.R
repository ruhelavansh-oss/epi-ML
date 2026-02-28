#!/usr/bin/env Rscript
# =============================================================================
# 08_tables.R - Phase 8: Publication Tables
# Epidemiological Study: Alcohol Use in Canada
# =============================================================================
# Generates formatted tables for the final report:
#   Table 1: data dataset sample characteristics stratified by heavy drinking
#   Table 2: CADS national alcohol prevalence by question (Year=2023)
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
  library(gtsummary)
  library(gt)
  library(survey)
})

options(scipen = 999)
set.seed(42)
source("surveillance/lib/config_paths.R")
paths <- get_paths()
init_paths(paths)


wrangled_dir <- paths$wrangled_dir
output_dir <- paths$output_private_dir
report_dir <- paths$output_public_dir
if (!dir.exists(report_dir)) dir.create(report_dir, recursive = TRUE)

cat("=== PHASE 8: PUBLICATION TABLES ===\n\n")

# =============================================================================
# TABLE 1: Sample Characteristics by Heavy Drinking Status (data dataset)
# =============================================================================
cat(paste(rep("=", 62), collapse = ""), "\n")
cat("TABLE 1: data dataset SAMPLE CHARACTERISTICS\n")
cat(paste(rep("=", 62), collapse = ""), "\n\n")

# --- Load data dataset wrangled data ---
df <- readRDS(file.path(wrangled_dir, "data_wrangled.rds"))
cat("Loaded data dataset:", nrow(df), "observations,", ncol(df), "variables\n\n")

# --- Prepare analysis subset ---
# Convert integer 0/1 variables to labelled factors for gtsummary
tbl_data <- df %>%
  dplyr::select(age_group, gender, province_region, mental_health, physical_health,
         heavy_drinking_30d, cannabis_any_use) %>%
  dplyr::mutate(
    heavy_drinking_30d   = factor(heavy_drinking_30d,   levels = c(0, 1), labels = c("No", "Yes")),
    cannabis_any_use = factor(cannabis_any_use, levels = c(0, 1), labels = c("No", "Yes"))
  )

cat("Variables for Table 1:\n")
cat("  age_group       :", nlevels(tbl_data$age_group), "levels\n")
cat("  gender          :", nlevels(tbl_data$gender), "levels\n")
cat("  province_region :", nlevels(tbl_data$province_region), "levels\n")
cat("  mental_health   :", nlevels(tbl_data$mental_health), "levels\n")
cat("  physical_health :", nlevels(tbl_data$physical_health), "levels\n")
cat("  heavy_drinking_30d:", nlevels(tbl_data$heavy_drinking_30d), "levels (stratifier)\n")
cat("  cannabis_any_use:", nlevels(tbl_data$cannabis_any_use), "levels\n\n")

# --- Build Table 1 with gtsummary ---
cat("Building Table 1 with gtsummary::tbl_summary()...\n")

tbl1 <- tbl_data %>%
  tbl_summary(
    by = heavy_drinking_30d,
    label = list(
      age_group       ~ "Age Group",
      gender          ~ "Gender",
      province_region ~ "Province/Region",
      mental_health   ~ "Self-Rated Mental Health",
      physical_health ~ "Self-Rated Physical Health",
      cannabis_any_use ~ "Cannabis Use (Any)"
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "ifany",
    missing_text = "Missing"
  ) %>%
  add_p(
    test = list(
      all_categorical() ~ "chisq.test"
    )
  ) %>%
  add_overall() %>%
  modify_header(label ~ "**Characteristic**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Heavy Drinking Status**") %>%
  modify_caption("**Table 1. Sample Characteristics of data dataset Respondents (N = 40,931)**") %>%
  bold_labels()

# --- Print Table 1 summary to stdout ---
cat("\n--- Table 1: Sample Characteristics by Heavy Drinking Status ---\n\n")
print(tbl1)

# --- Save as HTML ---
tbl1_html_path <- file.path(report_dir, "table1.html")
tbl1 %>%
  as_gt() %>%
  gt::gtsave(filename = tbl1_html_path)

cat("\nTable 1 saved to:", safe_label_path(tbl1_html_path, paths), "\n\n")

# =============================================================================
# TABLE 2: CADS Alcohol Prevalence by Question (National, Year = 2023)
# =============================================================================
cat(paste(rep("=", 62), collapse = ""), "\n")
cat("TABLE 2: CADS NATIONAL ALCOHOL PREVALENCE (2023)\n")
cat(paste(rep("=", 62), collapse = ""), "\n\n")

cads_path <- file.path(wrangled_dir, "cads_alcohol.rds")
tbl2_csv_path <- file.path(output_dir, "table2_cads_alcohol_prevalence.csv")
tbl2_html_path <- file.path(report_dir, "table2.html")

if (!file.exists(cads_path)) {
  cat("Skipping Table 2: missing file ", safe_label_path(cads_path, paths), "\n\n", sep = "")
} else {
  # --- Load CADS alcohol aggregate data ---
  cads_alc <- readRDS(cads_path)
  cat("Loaded CADS Alcohol:", nrow(cads_alc), "aggregate rows\n\n")

  # --- Filter for overall Year estimates (one row per question) ---
  # Some questions have multiple Year rows (e.g., categorical breakdowns).
  # Keep only the first row per question to get the overall prevalence.
  tbl2_data <- cads_alc %>%
    dplyr::filter(disagg_var1 == "Year") %>%
    dplyr::group_by(question) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(order) %>%
    dplyr::select(
      Question    = question,
      Prevalence  = colpercent,
      CI_Lower    = collowercl,
      CI_Upper    = coluppercl,
      Numerator   = numerator,
      Denominator = denominator
    ) %>%
    dplyr::mutate(
      `95% CI` = paste0("(", CI_Lower, ", ", CI_Upper, ")")
    )

  # --- Print Table 2 summary to stdout ---
  cat("--- Table 2: Alcohol Prevalence by Question (Year = 2023) ---\n\n")
  tbl2_print <- tbl2_data %>%
    dplyr::select(Question, `Prevalence (%)` = Prevalence, `95% CI`, Numerator, Denominator)

  print(as.data.frame(tbl2_print), right = FALSE, row.names = FALSE)

  # --- Save as CSV ---
  write.csv(tbl2_data %>% dplyr::select(-`95% CI`), tbl2_csv_path, row.names = FALSE)
  cat("\nTable 2 saved to:", safe_label_path(tbl2_csv_path, paths), "\n\n")

  # --- Save formatted version with gt ---
  tbl2_gt <- tbl2_data %>%
    dplyr::select(Question, `Prevalence (%)` = Prevalence, `95% CI`, Numerator, Denominator) %>%
    gt() %>%
    tab_header(
      title = "Table 2. National Alcohol Use Prevalence Among Canadians Aged 15+",
      subtitle = "Canadian Alcohol and Drugs Survey (CADS), 2023"
    ) %>%
    cols_align(align = "left", columns = Question) %>%
    cols_align(align = "right", columns = c(`Prevalence (%)`, `95% CI`, Numerator, Denominator))

  gt::gtsave(tbl2_gt, filename = tbl2_html_path)
  cat("Table 2 HTML saved to:", safe_label_path(tbl2_html_path, paths), "\n\n")
}

# =============================================================================
# Summary
# =============================================================================
cat(paste(rep("=", 62), collapse = ""), "\n")
cat("PHASE 8 TABLES COMPLETE\n")
cat(paste(rep("=", 62), collapse = ""), "\n")
cat("Outputs generated:\n")
cat("  1.", safe_label_path(tbl1_html_path, paths), "\n")
cat("  2.", safe_label_path(tbl2_csv_path, paths), "(if CADS file available)\n")
cat("  3.", safe_label_path(tbl2_html_path, paths), "(if CADS file available)\n")
cat("\nDone.\n")
