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

list_publishable <- function(root, exts = c("csv", "pdf", "png", "html", "txt", "md")) {
  if (!dir.exists(root)) return(character())
  pattern <- paste0("\\.(", paste(exts, collapse = "|"), ")$")
  files <- list.files(root, recursive = TRUE, full.names = FALSE, include.dirs = FALSE)
  files[grepl(pattern, tolower(files))]
}

private_root <- paths$output_private_dir
public_root <- file.path(paths$public_data_dir, "outputs")
manifest_path <- file.path(paths$public_data_dir, "outputs_manifest.csv")

all_ok <- TRUE

private_files <- sort(list_publishable(private_root))
public_files <- sort(list_publishable(public_root))

all_ok <- check(
  "Private output directory exists",
  dir.exists(private_root),
  safe_label_path(private_root, paths)
) && all_ok
all_ok <- check(
  "Public output directory exists",
  dir.exists(public_root),
  safe_label_path(public_root, paths)
) && all_ok

missing_in_public <- setdiff(private_files, public_files)
extra_in_public <- setdiff(public_files, private_files)

all_ok <- check(
  "Private->public artifact parity",
  length(missing_in_public) == 0L,
  if (length(missing_in_public) == 0L) {
    paste0(length(private_files), " private publishable artifact(s) mirrored to public")
  } else {
    paste("missing in public:", paste(utils::head(missing_in_public, 5), collapse = ", "))
  }
) && all_ok

all_ok <- check(
  "No orphan public artifacts",
  length(extra_in_public) == 0L,
  if (length(extra_in_public) == 0L) {
    paste0(length(public_files), " public publishable artifact(s) aligned to private")
  } else {
    paste("extra in public:", paste(utils::head(extra_in_public, 5), collapse = ", "))
  }
) && all_ok

private_csv <- private_files[grepl("\\.csv$", tolower(private_files))]
public_csv <- public_files[grepl("\\.csv$", tolower(public_files))]
missing_csv <- setdiff(private_csv, public_csv)

all_ok <- check(
  "CSV parity (private->public)",
  length(missing_csv) == 0L,
  if (length(missing_csv) == 0L) {
    paste0(length(private_csv), " CSV file(s) mirrored")
  } else {
    paste("missing CSV:", paste(utils::head(missing_csv, 5), collapse = ", "))
  }
) && all_ok

if (file.exists(manifest_path)) {
  manifest <- tryCatch(read.csv(manifest_path, stringsAsFactors = FALSE), error = function(e) NULL)
  has_output_col <- !is.null(manifest) && ("output" %in% names(manifest))
  manifest_outputs <- if (has_output_col) sort(manifest$output) else character()
  missing_manifest <- setdiff(private_files, manifest_outputs)
  stale_manifest <- setdiff(manifest_outputs, private_files)

  all_ok <- check(
    "Outputs manifest format",
    has_output_col,
    safe_label_path(manifest_path, paths)
  ) && all_ok

  if (has_output_col) {
    all_ok <- check(
      "Manifest includes all private publishable artifacts",
      length(missing_manifest) == 0L,
      if (length(missing_manifest) == 0L) {
        paste0("rows=", nrow(manifest))
      } else {
        paste("missing rows:", paste(utils::head(missing_manifest, 5), collapse = ", "))
      }
    ) && all_ok

    all_ok <- check(
      "Manifest has no stale rows",
      length(stale_manifest) == 0L,
      if (length(stale_manifest) == 0L) {
        "no stale rows"
      } else {
        paste("stale rows:", paste(utils::head(stale_manifest, 5), collapse = ", "))
      }
    ) && all_ok
  }
} else {
  all_ok <- check(
    "Outputs manifest exists",
    FALSE,
    safe_label_path(manifest_path, paths)
  ) && all_ok
}

if (!all_ok) {
  stop("Output parity check failed.", call. = FALSE)
}

cat("\nOutput parity check passed.\n")
