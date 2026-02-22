#!/usr/bin/env Rscript
source("surveillance/lib/config_paths.R")
paths <- get_paths()
setwd(paths$project_root)

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop(
    "Package 'rsconnect' is required to generate manifest.json. ",
    "Install with: install.packages('rsconnect')"
  )
}

manifest_path <- file.path(paths$project_root, "manifest.json")

collect_manifest_files <- function(project_root) {
  tracked <- character()
  if (nzchar(Sys.which("git")) && dir.exists(file.path(project_root, ".git"))) {
    tracked <- tryCatch(system2("git", c("ls-files"), stdout = TRUE, stderr = FALSE), error = function(e) character())
  }

  if (length(tracked) > 0) {
    return(tracked)
  }

  list.files(project_root, recursive = TRUE, full.names = FALSE, include.dirs = FALSE)
}

is_excluded <- function(rel_path) {
  grepl(
    "^(data/private/|data/public/|_site/|logs/|Rlibs/|renv/library/|renv/staging/|reports/public/|reports/source/|\\.quarto/|\\.git/)",
    rel_path
  )
}

app_files <- collect_manifest_files(paths$project_root)
app_files <- app_files[!is_excluded(app_files)]
app_files <- unique(c("_quarto.yml", app_files))
app_files <- app_files[file.exists(app_files)]

rsconnect::writeManifest(
  appDir = ".",
  appFiles = app_files,
  appPrimaryDoc = "_quarto.yml",
  contentCategory = "site",
  forceGenerate = TRUE
)

if (!file.exists(manifest_path)) {
  stop("manifest.json was not created.")
}

cat(
  "Generated:",
  safe_label_path(manifest_path, paths),
  "with",
  length(app_files),
  "app files.\n"
)
