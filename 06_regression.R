#!/usr/bin/env Rscript
args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- grep('^--file=', args_all, value = TRUE)
self_path <- if (length(file_arg) > 0) sub('^--file=', '', file_arg[[1]]) else '06_regression.R'
project_root <- dirname(normalizePath(self_path, winslash = '/', mustWork = FALSE))
setwd(project_root)
source(file.path(project_root, 'surveillance/investigation/06_regression.R'), local = FALSE, chdir = FALSE)
