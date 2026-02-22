#!/usr/bin/env Rscript
source("surveillance/lib/config_paths.R")
paths <- get_paths()
setwd(paths$project_root)

scan_code <- system2(file.path(R.home("bin"), "Rscript"), "scripts/security_scan.R")
if (!identical(scan_code, 0L)) {
  stop("Security scan failed. Refusing to render site.")
}

code <- system2("quarto", c("render", "--to", "html"))
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
