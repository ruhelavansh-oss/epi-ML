#!/usr/bin/env Rscript
source("surveillance/lib/config_paths.R")
paths <- get_paths()
setwd(paths$project_root)

check <- function(name, pass, detail = "") {
  status <- if (isTRUE(pass)) "PASS" else "FAIL"
  cat(sprintf("[%s] %s", status, name))
  if (nzchar(detail)) cat(" - ", detail, sep = "")
  cat("\n")
  isTRUE(pass)
}

all_ok <- TRUE

quarto_yml <- file.path(paths$project_root, "_quarto.yml")
manifest <- file.path(paths$project_root, "manifest.json")

all_ok <- check("Quarto config exists", file.exists(quarto_yml), safe_label_path(quarto_yml, paths)) && all_ok
all_ok <- check("Manifest exists", file.exists(manifest), safe_label_path(manifest, paths)) && all_ok

if (file.exists(manifest)) {
  manifest_txt <- paste(readLines(manifest, warn = FALSE), collapse = "\n")
  leaks <- grepl("\"data/private/|\"data/public/|CPADS_PUMF\\.csv", manifest_txt)
  all_ok <- check(
    "Manifest excludes private data paths",
    !leaks,
    "manifest.json must not reference private data files"
  ) && all_ok
}

scan_code <- system2(file.path(R.home("bin"), "Rscript"), "scripts/security_scan.R")
all_ok <- check("Security scan", identical(scan_code, 0L), "Rscript scripts/security_scan.R") && all_ok

parity_code <- system2(file.path(R.home("bin"), "Rscript"), "scripts/check_output_parity.R")
all_ok <- check("Output parity", identical(parity_code, 0L), "Rscript scripts/check_output_parity.R") && all_ok

public_outputs_dir <- file.path(paths$public_data_dir, "outputs")
required_outputs <- c(
  "power_one_proportion_grid.csv",
  "power_two_proportion_gender.csv",
  "power_analysis_summary.csv",
  "power_interaction_sample_size_targets.csv",
  "power_interaction_assumptions.csv",
  "power_interaction_group_allocations.csv",
  "power_interaction_imbalance_penalty.csv",
  "power_interaction_pairwise_details.csv",
  "power_interaction_feasibility_flags.csv",
  "power_gpower_reference_two_group.csv",
  "power_ebac_endpoint_anchors.csv",
  "randomization_block_blueprints.csv",
  "randomization_schedule_example_heavy_drinking_30d.csv",
  "randomization_schedule_example_ebac_legal.csv",
  "randomization_schedule_example_ebac_tot.csv",
  "official_doc_alignment_checklist.csv"
)
missing_required <- required_outputs[!file.exists(file.path(public_outputs_dir, required_outputs))]
all_ok <- check(
  "Required published outputs exist",
  length(missing_required) == 0L,
  if (length(missing_required) == 0L) {
    paste0(length(required_outputs), " required outputs found")
  } else {
    paste("missing:", paste(utils::head(missing_required, 5), collapse = ", "))
  }
) && all_ok

read_csv_safe <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
}

interaction_targets <- read_csv_safe(file.path(public_outputs_dir, "power_interaction_sample_size_targets.csv"))
interaction_mode_ok <- !is.null(interaction_targets) &&
  ("analysis_mode" %in% names(interaction_targets)) &&
  all(unique(interaction_targets$analysis_mode) == "observational")
all_ok <- check(
  "Interaction power outputs use observational analysis mode",
  interaction_mode_ok,
  "power_interaction_sample_size_targets.csv"
) && all_ok

gpower_ref <- read_csv_safe(file.path(public_outputs_dir, "power_gpower_reference_two_group.csv"))
gpower_mode_ok <- !is.null(gpower_ref) &&
  all(c("analysis_mode", "design_mode") %in% names(gpower_ref)) &&
  all(unique(gpower_ref$analysis_mode) == "observational") &&
  all(unique(gpower_ref$design_mode) == "prospective_trial_planning")
all_ok <- check(
  "G*Power reference uses required mode flags",
  gpower_mode_ok,
  "power_gpower_reference_two_group.csv"
) && all_ok

rand_blueprint <- read_csv_safe(file.path(public_outputs_dir, "randomization_block_blueprints.csv"))
rand_mode_ok <- !is.null(rand_blueprint) &&
  all(c("analysis_mode", "design_mode", "block_sizes_allowed") %in% names(rand_blueprint)) &&
  all(unique(rand_blueprint$analysis_mode) == "observational") &&
  all(unique(rand_blueprint$design_mode) == "prospective_trial_planning") &&
  all(rand_blueprint$block_sizes_allowed == "4,6,8")
all_ok <- check(
  "Randomization blueprint mode flags and block policy",
  rand_mode_ok,
  "randomization_block_blueprints.csv"
) && all_ok

schedule_files <- list.files(
  public_outputs_dir,
  pattern = "^randomization_schedule_example_.*[.]csv$",
  full.names = TRUE
)
schedule_ok <- FALSE
if (length(schedule_files) > 0) {
  sched <- do.call(rbind, lapply(schedule_files, function(path) {
    df <- read_csv_safe(path)
    if (is.null(df)) return(NULL)
    df$source_file <- basename(path)
    df
  }))
  if (!is.null(sched) && nrow(sched) > 0 && all(c("block_size", "assignment", "block_id") %in% names(sched))) {
    block_counts <- aggregate(
      unit_in_block ~ source_file + block_id + block_size + assignment + gender + endpoint + target_power,
      data = sched,
      FUN = length
    )
    block_wide <- reshape(
      block_counts,
      idvar = c("source_file", "block_id", "block_size", "gender", "endpoint", "target_power"),
      timevar = "assignment",
      direction = "wide"
    )
    if (!("unit_in_block.Control" %in% names(block_wide))) block_wide$unit_in_block.Control <- 0L
    if (!("unit_in_block.Treatment" %in% names(block_wide))) block_wide$unit_in_block.Treatment <- 0L
    size_ok <- (block_wide$unit_in_block.Control + block_wide$unit_in_block.Treatment) == block_wide$block_size
    balance_ok <- block_wide$unit_in_block.Control == block_wide$unit_in_block.Treatment
    allowed_ok <- block_wide$block_size %in% c(4, 6, 8)
    schedule_ok <- all(size_ok) && all(balance_ok) && all(allowed_ok)
  }
}
all_ok <- check(
  "Randomization schedules have balanced allowed blocks",
  schedule_ok,
  "block sizes 4/6/8 and 1:1 assignment per block"
) && all_ok

tracked <- if (nzchar(Sys.which("git"))) {
  tryCatch(system2("git", "ls-files", stdout = TRUE, stderr = FALSE), error = function(e) character())
} else character()

private_tracked_files <- grep("^data/private/", tracked, value = TRUE)
allowed_private_tracked <- "data/private/.gitkeep"
private_disallowed <- setdiff(private_tracked_files, allowed_private_tracked)
private_tracked <- length(private_disallowed) == 0
private_detail <- if (private_tracked) {
  "data/private/** should be ignored (except data/private/.gitkeep)"
} else {
  paste0(
    "tracked private path(s): ",
    paste(utils::head(private_disallowed, 5), collapse = ", ")
  )
}
all_ok <- check("No private data tracked in git", private_tracked, private_detail) && all_ok

site_tracked <- any(grepl("^_site/", tracked))
all_ok <- check("Built site not tracked", !site_tracked, "_site/ should be ignored") && all_ok

if (!all_ok) {
  stop("Connect Cloud publish readiness check failed.")
}

cat("\nAll readiness checks passed.\n")
