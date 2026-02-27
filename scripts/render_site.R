#!/usr/bin/env Rscript
source("surveillance/lib/config_paths.R")
paths <- get_paths()
setwd(paths$project_root)

scan_code <- system2(file.path(R.home("bin"), "Rscript"), "scripts/security_scan.R")
if (!identical(scan_code, 0L)) {
  stop("Security scan failed. Refusing to render site.")
}

publish_script <- file.path(paths$project_root, "scripts", "publish_public_artifacts.R")
if (file.exists(publish_script)) {
  pub_code <- system2(file.path(R.home("bin"), "Rscript"), publish_script)
  if (!identical(pub_code, 0L)) {
    warning("Public artifact publish step failed (exit code ", pub_code, "). Continuing render.")
  }
}

code <- system2("quarto", c("render", "--to", "html", "--cache-refresh"))
if (!identical(code, 0L)) {
  stop("Quarto render failed with exit code ", code)
}

post_scan_code <- system2(
  file.path(R.home("bin"), "Rscript"),
  c("scripts/security_scan.R", "--include-rendered")
)
if (!identical(post_scan_code, 0L)) {
  stop("Post-render security scan failed. Review generated artifacts.")
}

cat("Site render and post-render security checks completed successfully.\n")
