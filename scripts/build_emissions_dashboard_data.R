#!/usr/bin/env Rscript
source("surveillance/lib/config_paths.R")
paths <- get_paths()
init_paths(paths)
setwd(paths$project_root)

logs_path <- file.path(paths$logs_dir, "emissions", "module_emissions_log.csv")
out_dir <- file.path(paths$output_private_dir, "emissions")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

events_out <- file.path(out_dir, "emissions_module_events.csv")
runs_out <- file.path(out_dir, "emissions_run_summary.csv")
modules_out <- file.path(out_dir, "emissions_module_summary.csv")
reco_out <- file.path(out_dir, "emissions_recommendations.csv")

write_csv_if_changed <- function(df, path) {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  write.csv(df, tmp, row.names = FALSE)
  if (file.exists(path)) {
    same <- identical(unname(tools::md5sum(path)), unname(tools::md5sum(tmp)))
    if (same) return(invisible(FALSE))
  }
  file.copy(tmp, path, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
  invisible(TRUE)
}

module_ref <- data.frame(
  module = c(
    "investigation_03_data_wrangling",
    "investigation_04_descriptive_stats",
    "investigation_04_distributions",
    "investigation_05_power_design",
    "investigation_05_frequentist",
    "investigation_05_bayesian",
    "investigation_06_logistic",
    "investigation_06_model_comparison",
    "investigation_06_regression",
    "investigation_07_propensity",
    "investigation_07_causal_estimators",
    "investigation_07_treatment_effects",
    "investigation_07_dag",
    "investigation_07_meta_synthesis",
    "investigation_08_figures",
    "investigation_08_tables",
    "investigation_09_report",
    "ebac_07_ebac",
    "ebac_07_ebac_ipw",
    "ebac_07_ebac_gender_smote_sensitivity",
    "ebac_07_ebac_integrations"
  ),
  module_label = c(
    "Wrangling",
    "Descriptives",
    "Distributions",
    "Power",
    "Frequentist",
    "Bayesian",
    "Logistic",
    "Comparisons",
    "Regressions",
    "Propensity",
    "Causal",
    "Treatment",
    "DAG",
    "Meta",
    "Figures",
    "Tables",
    "Report",
    "Core",
    "IPW",
    "SMOTE",
    "Integrations"
  ),
  page_href = c(
    "code/data-wrangling.html",
    "code/descriptive-statistics.html",
    "code/distribution-tests.html",
    "code/power-design.html",
    "code/frequentist-inference.html",
    "code/bayesian-inference.html",
    "code/logistic-models.html",
    "code/model-comparison.html",
    "code/regression-models.html",
    "code/propensity-scores.html",
    "code/causal-estimators.html",
    "code/treatment-effects.html",
    "code/dag-specification.html",
    "code/meta-synthesis.html",
    "code/figures.html",
    "code/tables.html",
    "code/final-report.html",
    "code/ebac-core.html",
    "code/ebac-selection-adjustment-ipw.html",
    "code/ebac-gender-smote-sensitivity.html",
    "code/ebac-integrations.html"
  ),
  stringsAsFactors = FALSE
)

module_fallback_label <- function(x) {
  out <- gsub("^investigation_[0-9]+_", "", x)
  out <- gsub("^ebac_[0-9]+_", "ebac ", out)
  out <- gsub("_", " ", out)
  tools::toTitleCase(out)
}

normalize_run_source <- function(run_source, cloud_provider) {
  source <- tolower(trimws(as.character(run_source)))
  provider <- trimws(as.character(cloud_provider))
  provider[is.na(cloud_provider) | provider %in% c("NA", "N/A", "NULL", "null", "None")] <- ""
  source[!(source %in% c("local", "cloud"))] <- NA_character_
  inferred <- ifelse(nzchar(provider), "cloud", "local")
  source[is.na(source)] <- inferred[is.na(source)]
  source
}

attach_module_reference <- function(df) {
  idx <- match(df$module, module_ref$module)
  df$module_label <- module_ref$module_label[idx]
  df$page_href <- module_ref$page_href[idx]
  df$module_label[is.na(df$module_label)] <- module_fallback_label(df$module[is.na(df$module_label)])
  df$page_href[is.na(df$page_href)] <- "code/index.html"
  df
}

parse_run_id_time <- function(run_id) {
  core <- sub("-.*$", "", run_id)
  parsed <- as.POSIXct(core, format = "%Y%m%dT%H%M%SZ", tz = "UTC")
  parsed
}

parse_event_time <- function(x, run_id) {
  x <- as.character(x)
  x_norm <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", x)
  parsed <- as.POSIXct(x_norm, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  miss <- is.na(parsed)
  if (any(miss)) {
    parsed[miss] <- as.POSIXct(x[miss], tz = "UTC")
  }
  miss <- is.na(parsed)
  if (any(miss)) {
    parsed[miss] <- parse_run_id_time(run_id[miss])
  }
  parsed
}

build_recommendations <- function(module_summary, run_summary) {
  out <- data.frame(
    priority = integer(),
    recommendation = character(),
    rationale = character(),
    stringsAsFactors = FALSE
  )

  total_emissions <- sum(run_summary$total_emissions_kg, na.rm = TRUE)
  if (nrow(module_summary) > 0 && is.finite(total_emissions) && total_emissions > 0) {
    top_idx <- which.max(module_summary$total_emissions_kg)
    top_module <- module_summary[top_idx, ]
    top_module_name <- if ("module_label" %in% names(top_module)) top_module$module_label else top_module$module
    out <- rbind(
      out,
      data.frame(
        priority = 1L,
        recommendation = paste0("Prioritize optimization and caching for ", top_module_name, "."),
        rationale = sprintf(
          "This module contributes %.1f%% of tracked run emissions.",
          top_module$emissions_share_pct
        ),
        stringsAsFactors = FALSE
      )
    )
  }

  out <- rbind(
    out,
    data.frame(
      priority = 2L,
      recommendation = "Run only modules with changed inputs during development.",
      rationale = "Selective reruns reduce avoidable compute time and electricity use.",
      stringsAsFactors = FALSE
    )
  )

  longest <- if (nrow(run_summary) > 0) run_summary[which.max(run_summary$total_duration_seconds), ] else NULL
  if (!is.null(longest) && is.finite(longest$total_duration_seconds) && longest$total_duration_seconds > 1800) {
    out <- rbind(
      out,
      data.frame(
        priority = 3L,
        recommendation = "Schedule long runs during lower-carbon grid periods when possible.",
        rationale = sprintf(
          "Longest run duration was %.1f minutes; moving heavy jobs can lower emissions intensity.",
          longest$total_duration_seconds / 60
        ),
        stringsAsFactors = FALSE
      )
    )
  } else {
    out <- rbind(
      out,
      data.frame(
        priority = 3L,
        recommendation = "Use energy-efficient hardware for repeated experiment sweeps.",
        rationale = "Hardware efficiency directly lowers kWh and emissions for equivalent workloads.",
        stringsAsFactors = FALSE
      )
    )
  }

  out[order(out$priority), , drop = FALSE]
}

if (!file.exists(logs_path)) {
  cat(
    "No emissions log found at ",
    safe_label_path(logs_path, paths),
    ". Skipping emissions dashboard data refresh.\n",
    sep = ""
  )
  quit(status = 0)
}

events <- tryCatch(
  read.csv(logs_path, stringsAsFactors = FALSE),
  error = function(e) data.frame()
)

if (nrow(events) == 0) {
  cat("Emissions log is empty. Skipping emissions dashboard data refresh.\n")
  quit(status = 0)
}

needed_cols <- c(
  "timestamp_utc", "run_id", "module", "status",
  "duration_seconds", "emissions_kg", "energy_kwh",
  "country_iso_code", "region", "run_source", "cloud_provider"
)
for (nm in needed_cols) {
  if (!nm %in% names(events)) events[[nm]] <- NA
}

events <- events[, needed_cols, drop = FALSE]
events$run_id <- as.character(events$run_id)
events$module <- as.character(events$module)
events$status <- as.character(events$status)
events$country_iso_code <- as.character(events$country_iso_code)
events$region <- as.character(events$region)
events$cloud_provider <- as.character(events$cloud_provider)
events$run_source <- normalize_run_source(events$run_source, events$cloud_provider)
events$timestamp_utc <- parse_event_time(events$timestamp_utc, events$run_id)

num_cols <- c("duration_seconds", "emissions_kg", "energy_kwh")
for (nm in num_cols) {
  events[[nm]] <- suppressWarnings(as.numeric(events[[nm]]))
  events[[nm]][is.na(events[[nm]])] <- 0
}

events <- events[order(events$run_id, events$timestamp_utc, events$module), , drop = FALSE]
events$event_end_utc <- events$timestamp_utc + events$duration_seconds

run_start_map <- tapply(events$timestamp_utc, events$run_id, min, na.rm = TRUE)
run_end_map <- tapply(events$event_end_utc, events$run_id, max, na.rm = TRUE)
events$run_start_utc <- run_start_map[events$run_id]
events$run_end_utc <- run_end_map[events$run_id]
events$run_start_utc <- as.POSIXct(events$run_start_utc, origin = "1970-01-01", tz = "UTC")
events$run_end_utc <- as.POSIXct(events$run_end_utc, origin = "1970-01-01", tz = "UTC")
events$elapsed_minutes_run <- as.numeric(difftime(events$timestamp_utc, events$run_start_utc, units = "mins"))
events$cumulative_emissions_kg_run <- ave(events$emissions_kg, events$run_id, FUN = cumsum)
events <- attach_module_reference(events)

events$timestamp_utc_chr <- format(events$timestamp_utc, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
events_public <- data.frame(
  timestamp_utc = events$timestamp_utc_chr,
  run_id = events$run_id,
  run_source = events$run_source,
  module = events$module,
  module_label = events$module_label,
  page_href = events$page_href,
  status = events$status,
  duration_seconds = round(events$duration_seconds, 6),
  elapsed_minutes_run = round(events$elapsed_minutes_run, 6),
  cumulative_emissions_kg_run = round(events$cumulative_emissions_kg_run, 12),
  emissions_kg = round(events$emissions_kg, 12),
  energy_kwh = round(events$energy_kwh, 12),
  country_iso_code = events$country_iso_code,
  region = events$region,
  stringsAsFactors = FALSE
)

run_totals <- aggregate(
  events[, c("duration_seconds", "emissions_kg", "energy_kwh"), drop = FALSE],
  by = list(run_id = events$run_id),
  FUN = sum
)
run_modules <- aggregate(
  events$module,
  by = list(run_id = events$run_id),
  FUN = function(x) length(unique(x[nzchar(x)]))
)
names(run_modules)[2] <- "module_count"
run_start <- aggregate(events$run_start_utc, by = list(run_id = events$run_id), FUN = min)
names(run_start)[2] <- "start_time_utc"
run_end <- aggregate(events$run_end_utc, by = list(run_id = events$run_id), FUN = max)
names(run_end)[2] <- "end_time_utc"
run_source <- aggregate(
  events$run_source,
  by = list(run_id = events$run_id),
  FUN = function(x) if (any(x == "cloud", na.rm = TRUE)) "cloud" else "local"
)
names(run_source)[2] <- "run_source"
run_start$start_time_utc <- as.POSIXct(run_start$start_time_utc, origin = "1970-01-01", tz = "UTC")
run_end$end_time_utc <- as.POSIXct(run_end$end_time_utc, origin = "1970-01-01", tz = "UTC")

run_summary <- Reduce(
  function(x, y) merge(x, y, by = "run_id", all = TRUE),
  list(run_totals, run_modules, run_start, run_end, run_source)
)
names(run_summary)[names(run_summary) == "duration_seconds"] <- "total_duration_seconds"
names(run_summary)[names(run_summary) == "emissions_kg"] <- "total_emissions_kg"
names(run_summary)[names(run_summary) == "energy_kwh"] <- "total_energy_kwh"
run_summary$avg_emissions_rate_kg_per_hour <- ifelse(
  run_summary$total_duration_seconds > 0,
  run_summary$total_emissions_kg / (run_summary$total_duration_seconds / 3600),
  0
)
run_summary <- run_summary[order(run_summary$start_time_utc), , drop = FALSE]
run_summary$start_time_utc <- format(run_summary$start_time_utc, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
run_summary$end_time_utc <- format(run_summary$end_time_utc, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

module_totals <- aggregate(
  events[, c("duration_seconds", "emissions_kg", "energy_kwh"), drop = FALSE],
  by = list(module = events$module),
  FUN = sum
)
module_exec <- aggregate(events$run_id, by = list(module = events$module), FUN = length)
names(module_exec)[2] <- "executions"
module_runs <- aggregate(events$run_id, by = list(module = events$module), FUN = function(x) length(unique(x)))
names(module_runs)[2] <- "run_count"
module_failed <- aggregate(events$status != "ok", by = list(module = events$module), FUN = sum)
names(module_failed)[2] <- "failed_executions"
module_mean <- aggregate(events$emissions_kg, by = list(module = events$module), FUN = mean)
names(module_mean)[2] <- "mean_emissions_kg"
module_median <- aggregate(events$emissions_kg, by = list(module = events$module), FUN = median)
names(module_median)[2] <- "median_emissions_kg"

module_summary <- Reduce(
  function(x, y) merge(x, y, by = "module", all = TRUE),
  list(module_totals, module_exec, module_runs, module_failed, module_mean, module_median)
)
names(module_summary)[names(module_summary) == "duration_seconds"] <- "total_duration_seconds"
names(module_summary)[names(module_summary) == "emissions_kg"] <- "total_emissions_kg"
names(module_summary)[names(module_summary) == "energy_kwh"] <- "total_energy_kwh"

module_summary$total_emissions_kg[is.na(module_summary$total_emissions_kg)] <- 0
total_emissions <- sum(module_summary$total_emissions_kg, na.rm = TRUE)
module_summary$emissions_share_pct <- if (total_emissions > 0) {
  100 * module_summary$total_emissions_kg / total_emissions
} else {
  0
}
module_summary$emissions_rate_kg_per_hour <- ifelse(
  module_summary$total_duration_seconds > 0,
  module_summary$total_emissions_kg / (module_summary$total_duration_seconds / 3600),
  0
)
module_summary$avg_duration_seconds <- ifelse(
  module_summary$executions > 0,
  module_summary$total_duration_seconds / module_summary$executions,
  0
)
module_summary$avg_energy_kwh <- ifelse(
  module_summary$executions > 0,
  module_summary$total_energy_kwh / module_summary$executions,
  0
)
module_summary <- attach_module_reference(module_summary)
module_summary <- module_summary[order(module_summary$total_emissions_kg, decreasing = TRUE), , drop = FALSE]

recommendations <- build_recommendations(module_summary, run_summary)

write_csv_if_changed(events_public, events_out)
write_csv_if_changed(run_summary, runs_out)
write_csv_if_changed(module_summary, modules_out)
write_csv_if_changed(recommendations, reco_out)

cat(
  "Built emissions dashboard datasets in ",
  safe_label_path(out_dir, paths),
  " from ",
  safe_label_path(logs_path, paths),
  ".\n",
  sep = ""
)
