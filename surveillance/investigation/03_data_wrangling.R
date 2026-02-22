# ============================================================
# Preliminary Descriptive Information for Research Study
# ============================================================
#----------------Data Wrangling Operations----------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(survey)
  library(janitor)
})
options(scipen = 999)
set.seed(42)
source("surveillance/lib/config_paths.R")
paths <- get_paths()
init_paths(paths)

# ---- Paths ----
data_dir <- paths$data_dir
output_dir <- paths$output_private_dir
wrangled_dir <- paths$wrangled_dir
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(wrangled_dir, recursive = TRUE, showWarnings = FALSE)
cat("=== WRANGLING CPADS_PUMF ===\n\n")
# --- loading the observational datafile ---
assert_required_files(paths, "raw_pumf_file")
pumf_raw <- read_csv(
  paths$raw_pumf_file,
  show_col_types = FALSE
)
cat("Raw dimensions:", nrow(pumf_raw), "x", ncol(pumf_raw), "\n")
wrangling_log <- tibble(
  step = character(),
  description = character(),
  rows_before = integer(),
  rows_after = integer(),
  cols_affected = character()
)
log_step <- function(step, desc, rows_b, rows_a, cols) {
  wrangling_log <<- wrangling_log %>%
    add_row(
      step = step,
      description = desc,
      rows_before = rows_b,
      rows_after = rows_a,
      cols_affected = cols
    )
}
# Data Wrangling
pumf <- pumf_raw %>% clean_names()
cat("Step 1: Column names wrangled (janitor::clean_names)\n")
log_step("1", "clean_names()", nrow(pumf), nrow(pumf), "all")
# CPADS PUMF convention: 97/98/99 and 997/998/999 are special missing codes
preserve_cols <- c("seqid", "wtpumf")
recode_cols <- setdiff(names(pumf), preserve_cols)
special_codes <- c(97, 98, 99, 997, 998, 999)
pre_special <- pumf %>%
  dplyr::select(dplyr::all_of(recode_cols)) %>%
  dplyr::summarise(dplyr::across(
    dplyr::everything(),
    ~ sum(.x %in% special_codes)
  )) %>%
  tidyr::pivot_longer(
    dplyr::everything(),
    names_to = "variable",
    values_to = "n_special"
  ) %>%
  dplyr::filter(n_special > 0) %>%
  dplyr::arrange(dplyr::desc(n_special))

cat("\nStep 2: Recoding special values to NA\n")
cat("Variables with special codes:", nrow(pre_special), "\n")
cat(
  "Total special-code cells:",
  sum(pre_special$n_special),
  "of",
  nrow(pumf) * length(recode_cols),
  "(",
  round(
    100 * sum(pre_special$n_special) / (nrow(pumf) * length(recode_cols)),
    1
  ),
  "%)\n"
)
pumf_wrangled <- pumf %>%
  dplyr::mutate(dplyr::across(
    dplyr::all_of(recode_cols),
    ~ dplyr::if_else(.x %in% special_codes, NA_real_, as.numeric(.x))
  ))
post_special <- pumf_wrangled %>%
  dplyr::select(dplyr::all_of(recode_cols)) %>%
  dplyr::summarise(dplyr::across(
    dplyr::everything(),
    ~ sum(.x %in% special_codes, na.rm = TRUE)
  )) %>%
  tidyr::pivot_longer(
    dplyr::everything(),
    names_to = "variable",
    values_to = "n_special"
  ) %>%
  dplyr::filter(n_special > 0)

cat("Special codes remaining after recode:", sum(post_special$n_special), "\n")
log_step(
  "2",
  "Recode 97/98/99/997/998/999 to NA",
  nrow(pumf_wrangled),
  nrow(pumf_wrangled),
  paste(nrow(pre_special), "columns")
)
cat("\nStep 3: Mapping derived variables\n")
pumf_wrangled <- pumf_wrangled %>%
  mutate(
    # Gender (dvdemq01): (1) Woman (2) Man (3) T/NB, (98) prefers to not say, (99) doesn't know
    gender = factor(
      case_when(
        dvdemq01 == 1 ~ "Woman",
        dvdemq01 == 2 ~ "Man",
        dvdemq01 == 3 ~ "Transgender/Non-binary",
        TRUE ~ NA_character_
      ),
      levels = c("Woman", "Man", "Transgender/Non-binary")
    ),
    # Age groups
    age_group = factor(
      case_when(
        age_groups == 1 ~ "16-19",
        age_groups == 2 ~ "20-22",
        age_groups == 3 ~ "23-25",
        age_groups == 4 ~ "26+",
        TRUE ~ NA_character_
      ),
      levels = c("16-19", "20-22", "23-25", "26+"),
      ordered = TRUE
    ),
    # Provincial Territory (region)
    province_region = factor(
      case_when(
        region == 1 ~ "Atlantic",
        region == 2 ~ "Quebec",
        region == 3 ~ "Ontario",
        region == 4 ~ "Western",
        TRUE ~ NA_character_
      ),
      levels = c("Atlantic", "Quebec", "Ontario", "Western")
    ),
    # Alcohol: lifetime + past 12 months
    alcohol_ever = case_when(
      alc03 == 1 ~ 1L,
      alc03 == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    alcohol_past12m = case_when(
      alc05 == 1 ~ 1L,
      alc05 == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    # Alcohol frequency (past 30 days): alc06
    alcohol_frequency_30d = factor(
      case_when(
        alc06 == 1 ~ "daily_or_almost",
        alc06 == 2 ~ "2_to_5_times_week",
        alc06 == 3 ~ "once_week",
        alc06 == 4 ~ "2_to_3_times_past30",
        alc06 == 5 ~ "once_past30",
        alc06 == 6 ~ "not_past30",
        TRUE ~ NA_character_
      ),
      levels = c(
        "daily_or_almost",
        "2_to_5_times_week",
        "once_week",
        "2_to_3_times_past30",
        "once_past30",
        "not_past30"
      ),
      ordered = TRUE
    ),
    # Heavy drinking (past 30 days): prefer alc12_30d_prev_total then fall back to alc12_30d_prev
    heavy_drinking_30d = case_when(
      alc12_30d_prev_total == 1 ~ 1L,
      alc12_30d_prev_total == 0 ~ 0L,
      alc12_30d_prev == 1 ~ 1L,
      alc12_30d_prev == 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    # Cannabis use in past 12 months (can05)
    cannabis_any_use = case_when(
      can05 == 1 ~ 1L,
      can05 == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    # Health (hwbq01, hwbq02)
    physical_health = factor(
      case_when(
        hwbq01 == 1 ~ "Excellent",
        hwbq01 == 2 ~ "Very Good",
        hwbq01 == 3 ~ "Good",
        hwbq01 == 4 ~ "Fair",
        hwbq01 == 5 ~ "Poor",
        TRUE ~ NA_character_
      ),
      levels = c("Excellent", "Very Good", "Good", "Fair", "Poor"),
      ordered = TRUE
    ),
    mental_health = factor(
      case_when(
        hwbq02 == 1 ~ "Excellent",
        hwbq02 == 2 ~ "Very Good",
        hwbq02 == 3 ~ "Good",
        hwbq02 == 4 ~ "Fair",
        hwbq02 == 5 ~ "Poor",
        TRUE ~ NA_character_
      ),
      levels = c("Excellent", "Very Good", "Good", "Fair", "Poor"),
      ordered = TRUE
    )
  )
derived_vars <- c(
  "gender",
  "age_group",
  "province_region",
  "alcohol_ever",
  "alcohol_past12m",
  "alcohol_frequency_30d",
  "heavy_drinking_30d",
  "cannabis_any_use",
  "physical_health",
  "mental_health"
)
cat("  Created:", paste(derived_vars, collapse = ", "), "\n")
log_step(
  "3",
  "Create derived variables",
  nrow(pumf_wrangled),
  nrow(pumf_wrangled),
  paste(derived_vars, collapse = ", ")
)
cat("\nStep 4: Derived variable distributions\n")
for (v in derived_vars) {
  cat(sprintf("\n  %s:\n", v))
  x <- pumf_wrangled[[v]]
  if (is.factor(x) || is.character(x)) {
    tab <- table(x, useNA = "ifany")
    pct <- round(100 * prop.table(tab), 1)
    print(data.frame(
      level = names(tab),
      n = as.integer(tab),
      pct = as.numeric(pct)
    ))
  } else {
    cat(sprintf(
      "0: %d (%.1f%%), 1: %d (%.1f%%), NA: %d (%.1f%%)\n",
      sum(x == 0, na.rm = TRUE),
      100 * mean(x == 0, na.rm = TRUE),
      sum(x == 1, na.rm = TRUE),
      100 * mean(x == 1, na.rm = TRUE),
      sum(is.na(x)),
      100 * mean(is.na(x))
    ))
  }
}
cat("\n\nStep 5: Post-wrangling NA summary\n")
na_after <- pumf_wrangled %>%
  summarise(across(everything(), ~ round(100 * mean(is.na(.x)), 1))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") %>%
  filter(pct_na > 0) %>%
  arrange(desc(pct_na))
cat("Variables with NA after wrangling (top 30):\n")
print(na_after, n = 30)
cat("\nStep 6: Creating survey design object\n")
pumf_svy <- svydesign(ids = ~1, weights = ~wtpumf, data = pumf_wrangled)
cat("Survey design created with", nrow(pumf_wrangled), "observations\n")
cat("Sum of weights:", round(sum(pumf_wrangled$wtpumf, na.rm = TRUE), 1), "\n")
# --- Save outputs ---
cat("\nSaving wrangled data...\n")
saveRDS(pumf_wrangled, file.path(wrangled_dir, "cpads_pumf_wrangled.rds"))
saveRDS(pumf_svy, file.path(wrangled_dir, "cpads_pumf_survey.rds"))
write_csv(wrangling_log, file.path(output_dir, "cpads_pumf_wrangling_log.csv"))
write_csv(na_after, file.path(output_dir, "cpads_pumf_na_summary.csv"))
cat("cpads_pumf_wrangled.rds:", nrow(pumf_wrangled), "x", ncol(pumf_wrangled), "\n")
cat("cpads_pumf_survey.rds (svydesign object)\n")
cat("cpads_pumf_wrangling_log.csv\n")
cat("cpads_pumf_na_summary.csv\n")
cat("\n=== CPADS_PUMF WRANGLING COMPLETE ===\n")
