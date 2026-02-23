#!/usr/bin/env Rscript
args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- grep('^--file=', args_all, value = TRUE)
self_path <- if (length(file_arg) > 0) sub('^--file=', '', file_arg[[1]]) else file.path('scripts', 'entrypoints', '06_regression.R')
project_root <- normalizePath(dirname(self_path), winslash = '/', mustWork = FALSE)
for (i in seq_len(8)) {
  if (file.exists(file.path(project_root, 'surveillance', 'lib', 'config_paths.R'))) break
  parent <- dirname(project_root)
  if (identical(parent, project_root)) break
  project_root <- parent
}
setwd(project_root)
source(file.path(project_root, 'surveillance/investigation/06_regression.R'), local = FALSE, chdir = FALSE)
