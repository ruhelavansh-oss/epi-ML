#!/usr/bin/env Rscript
source("surveillance/lib/config_paths.R")
paths <- get_paths()
setwd(paths$project_root)

args <- commandArgs(trailingOnly = TRUE)
rscript_bin <- file.path(R.home("bin"), "Rscript")

render_script <- file.path(paths$project_root, "scripts", "render_site.R")
render_code <- system2(rscript_bin, render_script)
if (!identical(render_code, 0L)) {
  stop("Pre-preview render failed with exit code ", render_code)
}

preview_args <- c("preview", ".", "--render", "all", args)
code <- system2("quarto", preview_args)
if (!identical(code, 0L)) {
  stop("Quarto preview failed with exit code ", code)
}
