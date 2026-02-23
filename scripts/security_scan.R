#!/usr/bin/env Rscript
source("surveillance/lib/config_paths.R")
paths <- get_paths()
setwd(paths$project_root)

args <- commandArgs(trailingOnly = TRUE)
include_rendered <- "--include-rendered" %in% args

collect_files <- function(include_rendered = FALSE) {
  if (dir.exists(file.path(paths$project_root, ".git")) && nzchar(Sys.which("git"))) {
    git_files <- tryCatch(
      system2("git", c("ls-files", "--cached", "--others", "--exclude-standard"), stdout = TRUE, stderr = FALSE),
      error = function(e) character()
    )
    files <- if (length(git_files) > 0) file.path(paths$project_root, git_files) else character()
  } else {
    files <- character()
  }

  if (length(files) == 0) {
    candidates <- list.files(paths$project_root, recursive = TRUE, full.names = TRUE, include.dirs = FALSE)
    keep <- !grepl("/(data/private|data/public|\\.git|data/private/Rlibs)/", candidates)
    if (!include_rendered) {
      keep <- keep & !grepl("/(_site|reports/public)/", candidates)
    }
    files <- candidates[keep]
  }

  if (include_rendered) {
    rendered_dirs <- file.path(paths$project_root, c("_site", "reports/public"))
    rendered_dirs <- rendered_dirs[dir.exists(rendered_dirs)]
    if (length(rendered_dirs) > 0) {
      rendered <- unlist(
        lapply(rendered_dirs, function(d) list.files(d, recursive = TRUE, full.names = TRUE, include.dirs = FALSE)),
        use.names = FALSE
      )
      files <- unique(c(files, rendered))
    }
  }

  files
}

is_text_target <- function(x) {
  grepl("\\.(R|r|Rmd|rmd|qmd|md|html|tex|yml|yaml|toml|txt|bib|csv)$", x)
}

patterns <- list(
  absolute_path = "(^|[[:space:]'\\\"])(/Users/[^[:space:]'\\\"]+|[A-Za-z]:\\\\\\\\[^[:space:]'\\\"]+)",
  secret_literal = "(?i)(api[_-]?key|secret|token|password)\\s*[:=]\\s*['\"][^'\"]{8,}",
  secret_export = "(?i)^\\s*CPADS_SECRET_KEY\\s*=\\s*[^\\s#]+",
  algolia_admin_write_key = "(?i)(algolia).*(admin|write).*(api[_-]?key)"
)

files <- collect_files(include_rendered = include_rendered)
files <- files[is_text_target(files) & file.exists(files)]

hits <- list()
for (f in files) {
  txt <- tryCatch(readLines(f, warn = FALSE, encoding = "UTF-8"), error = function(e) character())
  if (length(txt) == 0) next
  for (name in names(patterns)) {
    if (basename(f) == "security_scan.R" && name == "absolute_path") {
      next
    }
    idx <- grep(patterns[[name]], txt, perl = TRUE)
    if (length(idx) > 0) {
      rel <- sub(paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", paths$project_root), "/?"), "", f)
      hits[[length(hits) + 1]] <- data.frame(file = rel, line = idx[1], rule = name, stringsAsFactors = FALSE)
    }
  }
}

if (length(hits) > 0) {
  out <- do.call(rbind, hits)
  print(out)
  quit(status = 1)
}

scope <- if (include_rendered) "source + rendered artifacts" else "source files"
cat(sprintf("Security scan passed for %s: no sensitive path or secret literals found.\n", scope))
