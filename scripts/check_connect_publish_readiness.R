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

tracked <- if (nzchar(Sys.which("git"))) {
  tryCatch(system2("git", "ls-files", stdout = TRUE, stderr = FALSE), error = function(e) character())
} else character()

private_tracked <- length(grep("^data/private/", tracked)) == 0
all_ok <- check("No private data tracked in git", private_tracked, "data/private/** should be ignored") && all_ok

site_tracked <- any(grepl("^_site/", tracked))
all_ok <- check("Built site not tracked", !site_tracked, "_site/ should be ignored") && all_ok

if (!all_ok) {
  stop("Connect Cloud publish readiness check failed.")
}

cat("\nAll readiness checks passed.\n")
