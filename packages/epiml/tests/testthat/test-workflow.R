library(epiml)

test_that("run_pipeline supports custom workflow map", {
  root <- file.path(tempdir(), paste0("epiml-workflow-", as.integer(runif(1, 1, 1e8))))
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(root, "_quarto.yml"))
  dir.create(file.path(root, "data", "public"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "scripts"), recursive = TRUE, showWarnings = FALSE)

  writeLines(c("#!/usr/bin/env Rscript", "quit(status = 0)"), file.path(root, "scripts", "alpha.R"))
  writeLines(c("#!/usr/bin/env Rscript", "quit(status = 0)"), file.path(root, "scripts", "beta.R"))

  smap <- c(prepare = "scripts/alpha.R", run = "scripts/beta.R")

  res <- run_pipeline(project_root = root, script_map = smap, verbose = FALSE)

  expect_equal(as.character(res$step), c("prepare", "run"))
  expect_true(all(res$status == 0))
})
