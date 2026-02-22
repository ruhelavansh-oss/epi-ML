if (!exists("%||%")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || identical(x, "") || (length(x) == 1 && is.na(x))) y else x
  }
}

locate_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in seq_len(8)) {
    if (file.exists(file.path(current, "surveillance", "lib", "config_paths.R"))) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  normalizePath(start, winslash = "/", mustWork = FALSE)
}

helper_file <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE), error = function(e) "")
project_root_guess <- ""
if (nzchar(helper_file)) {
  helper_dir <- dirname(helper_file)
  candidate_root <- dirname(helper_dir)
  if (file.exists(file.path(candidate_root, "surveillance", "lib", "config_paths.R"))) {
    project_root_guess <- candidate_root
  }
}
if (!nzchar(project_root_guess)) {
  quarto_root <- Sys.getenv("QUARTO_PROJECT_ROOT", "")
  if (nzchar(quarto_root) && file.exists(file.path(quarto_root, "surveillance", "lib", "config_paths.R"))) {
    project_root_guess <- normalizePath(quarto_root, winslash = "/", mustWork = FALSE)
  }
}
if (!nzchar(project_root_guess)) {
  project_root_guess <- locate_project_root()
}

config_file <- file.path(project_root_guess, "surveillance", "lib", "config_paths.R")
if (file.exists(config_file)) {
  source(config_file)
}

safe_paths <- function() {
  if (exists("get_paths")) {
    p <- tryCatch(get_paths(), error = function(e) list())
  } else {
    p <- list()
  }
  root <- normalizePath(project_root_guess, winslash = "/", mustWork = FALSE)

  absolutize <- function(x, fallback, base = root) {
    raw <- x %||% fallback
    is_abs <- grepl("^(/|[A-Za-z]:[/\\\\])", raw)
    if (!is_abs) raw <- file.path(base, raw)
    normalizePath(raw, winslash = "/", mustWork = FALSE)
  }

  project_root <- absolutize(p$project_root, root, base = root)
  list(
    project_root = project_root,
    data_dir = absolutize(
      p$data_dir,
      file.path(project_root, "data", "private"),
      base = project_root
    ),
    output_private_dir = absolutize(
      p$output_private_dir,
      file.path(project_root, "data", "private", "outputs"),
      base = project_root
    ),
    output_public_dir = absolutize(
      p$output_public_dir,
      file.path(project_root, "reports", "public"),
      base = project_root
    ),
    reports_dir = absolutize(
      p$reports_dir,
      file.path(project_root, "reports", "source"),
      base = project_root
    ),
    wrangled_dir = absolutize(
      p$wrangled_dir,
      file.path(project_root, "data", "private", "outputs", "wrangled"),
      base = project_root
    ),
    figures_dir = absolutize(
      p$figures_dir,
      file.path(project_root, "data", "private", "outputs", "figures"),
      base = project_root
    )
  )
}

paths <- safe_paths()

script_catalog <- data.frame(
  label = c(
    "Data Wrangling",
    "Descriptive Statistics",
    "Distribution Tests",
    "Frequentist Inference",
    "Bayesian Inference",
    "Power Design",
    "Logistic Models",
    "Model Comparison",
    "Regression Models",
    "Propensity Scores",
    "Causal Estimators",
    "Treatment Effects",
    "DAG Specification",
    "Meta Synthesis",
    "eBAC Core",
    "eBAC Selection Adjustment (IPW)",
    "eBAC Gender/SMOTE Sensitivity",
    "eBAC Integrations",
    "Figures",
    "Tables",
    "Summary Report"
  ),
  script_rel = c(
    "surveillance/investigation/03_data_wrangling.R",
    "surveillance/investigation/04_descriptive_stats.R",
    "surveillance/investigation/04_distributions.R",
    "surveillance/investigation/05_frequentist.R",
    "surveillance/investigation/05_bayesian.R",
    "surveillance/investigation/05_power_design.R",
    "surveillance/investigation/06_logistic.R",
    "surveillance/investigation/06_model_comparison.R",
    "surveillance/investigation/06_regression.R",
    "surveillance/investigation/07_propensity.R",
    "surveillance/investigation/07_causal_estimators.R",
    "surveillance/investigation/07_treatment_effects.R",
    "surveillance/investigation/07_dag.R",
    "surveillance/investigation/07_meta_synthesis.R",
    "surveillance/ebac/07_ebac.R",
    "surveillance/ebac/07_ebac_ipw.R",
    "surveillance/ebac/07_ebac_gender_smote_sensitivity.R",
    "surveillance/ebac/07_ebac_integrations.R",
    "surveillance/investigation/08_figures.R",
    "surveillance/investigation/08_tables.R",
    "surveillance/investigation/09_report.R"
  ),
  page_href = c(
    "code/data-wrangling.qmd",
    "code/descriptive-statistics.qmd",
    "code/distribution-tests.qmd",
    "code/frequentist-inference.qmd",
    "code/bayesian-inference.qmd",
    "code/power-design.qmd",
    "code/logistic-models.qmd",
    "code/model-comparison.qmd",
    "code/regression-models.qmd",
    "code/propensity-scores.qmd",
    "code/causal-estimators.qmd",
    "code/treatment-effects.qmd",
    "code/dag-specification.qmd",
    "code/meta-synthesis.qmd",
    "code/ebac-core.qmd",
    "code/ebac-selection-adjustment-ipw.qmd",
    "code/ebac-gender-smote-sensitivity.qmd",
    "code/ebac-integrations.qmd",
    "code/figures.qmd",
    "code/tables.qmd",
    "code/final-report.qmd"
  ),
  stringsAsFactors = FALSE
)

script_path <- function(script_rel) {
  normalizePath(file.path(paths$project_root, script_rel), winslash = "/", mustWork = FALSE)
}

relative_page_link <- function(href) {
  sub("^code/", "", href)
}

extract_declared_outputs <- function(lines) {
  pattern <- "['\\\"]([A-Za-z0-9_./-]+\\.(csv|rds|png|pdf|html|txt|md|tex|bib))['\\\"]"
  m <- gregexpr(pattern, lines, perl = TRUE)
  raw <- regmatches(lines, m)
  vals <- unique(gsub("^['\\\"]|['\\\"]$", "", unlist(raw)))
  vals <- vals[!grepl("\\.(R|r|Rmd|rmd|qmd)$", vals)]
  vals <- vals[!grepl("^https?://", vals)]
  vals
}

candidate_paths <- function(name) {
  nm <- gsub("^\\./", "", name)
  unique(c(
    file.path(paths$project_root, nm),
    file.path(paths$wrangled_dir, nm),
    file.path(paths$figures_dir, nm),
    file.path(paths$output_private_dir, nm),
    file.path(paths$output_public_dir, nm),
    file.path(paths$reports_dir, nm),
    file.path(paths$wrangled_dir, basename(nm)),
    file.path(paths$figures_dir, basename(nm)),
    file.path(paths$output_private_dir, basename(nm)),
    file.path(paths$output_public_dir, basename(nm)),
    file.path(paths$output_private_dir, "wrangled", basename(nm)),
    file.path(paths$output_private_dir, "figures", basename(nm)),
    file.path(paths$project_root, "reports", "public", basename(nm)),
    file.path(paths$project_root, "reports", "source", basename(nm))
  ))
}

starts_with_path <- function(path, prefix) {
  p <- normalizePath(path, winslash = "/", mustWork = FALSE)
  pr <- normalizePath(prefix, winslash = "/", mustWork = FALSE)
  startsWith(p, pr)
}

default_expected_path <- function(name) {
  nm <- gsub("^\\./", "", name)
  base <- basename(nm)
  ext <- tolower(sub(".*\\.", "", base))
  has_dir <- grepl("/", nm, fixed = TRUE)
  if (has_dir) {
    return(normalizePath(file.path(paths$project_root, nm), winslash = "/", mustWork = FALSE))
  }

  if (ext %in% c("rds")) {
    return(normalizePath(file.path(paths$wrangled_dir, base), winslash = "/", mustWork = FALSE))
  }
  if (ext %in% c("png", "pdf")) {
    return(normalizePath(file.path(paths$figures_dir, base), winslash = "/", mustWork = FALSE))
  }
  if (ext %in% c("html")) {
    return(normalizePath(file.path(paths$output_public_dir, base), winslash = "/", mustWork = FALSE))
  }
  if (ext %in% c("md", "tex", "bib")) {
    return(normalizePath(file.path(paths$reports_dir, base), winslash = "/", mustWork = FALSE))
  }
  normalizePath(file.path(paths$output_private_dir, base), winslash = "/", mustWork = FALSE)
}

is_private_expected <- function(path) {
  starts_with_path(path, paths$data_dir) || starts_with_path(path, paths$output_private_dir)
}

resolve_output <- function(name) {
  cands <- candidate_paths(name)
  idx <- which(file.exists(cands))
  if (length(idx) == 0) {
    expected <- default_expected_path(name)
    path_label <- if (exists("safe_label_path")) safe_label_path(expected, paths) else expected
    return(list(
      found = FALSE,
      status = ifelse(is_private_expected(expected), "not published", "no"),
      path = path_label,
      size_kb = NA_character_,
      modified = NA_character_
    ))
  }
  fp <- cands[idx[1]]
  info <- file.info(fp)
  path_label <- if (exists("safe_label_path")) safe_label_path(fp, paths) else fp
  list(
    found = TRUE,
    status = "yes",
    path = path_label,
    size_kb = sprintf("%.1f", as.numeric(info$size) / 1024),
    modified = format(info$mtime, "%Y-%m-%d %H:%M:%S")
  )
}

render_script_metadata <- function(script_rel) {
  abs <- script_path(script_rel)
  exists_flag <- file.exists(abs)
  cmd <- if (file.exists(file.path(paths$project_root, basename(script_rel)))) {
    paste("Rscript", basename(script_rel))
  } else {
    paste("Rscript", script_rel)
  }
  n_lines <- if (exists_flag) length(readLines(abs, warn = FALSE, encoding = "UTF-8")) else 0L
  cat("- **Script:** `", script_rel, "`\n", sep = "")
  cat("- **Exists:** `", ifelse(exists_flag, "yes", "no"), "`\n", sep = "")
  cat("- **Lines of code:** `", n_lines, "`\n", sep = "")
  cat("- **Run command:** `", cmd, "`\n", sep = "")
}

render_outputs_table <- function(script_rel) {
  abs <- script_path(script_rel)
  if (!file.exists(abs)) {
    cat("No script file found for output detection.\n")
    return(invisible(NULL))
  }
  lines <- readLines(abs, warn = FALSE, encoding = "UTF-8")
  outputs <- extract_declared_outputs(lines)
  if (length(outputs) == 0) {
    cat("No explicit output filenames detected in this script.\n")
    return(invisible(NULL))
  }
  outputs <- sort(unique(outputs))
  resolved <- lapply(outputs, resolve_output)
  if (any(vapply(resolved, function(x) identical(x$status, "not published"), logical(1)))) {
    cat(
      "Public site note: some artifacts are generated into private output directories ",
      "and are intentionally not published in GitHub Pages.\n\n",
      sep = ""
    )
  }
  cat("| Declared output | Exists | Resolved path | Size (KB) | Last modified |\n")
  cat("|---|---:|---|---:|---|\n")
  for (i in seq_along(outputs)) {
    r <- resolved[[i]]
    cat(
      "| `", outputs[[i]], "` | ",
      r$status, " | ",
      ifelse(is.na(r$path), "-", paste0("`", r$path, "`")), " | ",
      ifelse(is.na(r$size_kb), "-", r$size_kb), " | ",
      ifelse(is.na(r$modified), "-", r$modified), " |\n",
      sep = ""
    )
  }
}

render_script_code <- function(script_rel) {
  abs <- script_path(script_rel)
  if (!file.exists(abs)) {
    cat("Script file not found: `", script_rel, "`\n", sep = "")
    return(invisible(NULL))
  }
  lines <- readLines(abs, warn = FALSE, encoding = "UTF-8")

  # Public display guardrails: remove pure comments and redact local absolute paths.
  lines <- lines[!grepl("^\\s*#", lines)]
  user_home_rx <- paste0("/", "Users", "/[A-Za-z0-9._-]+")
  lines <- gsub(user_home_rx, "PROJECT_ROOT_USER", lines, perl = TRUE)
  lines <- gsub("([A-Za-z]:)?/(Users|home)/[A-Za-z0-9._-]+", "PROJECT_ROOT_USER", lines, perl = TRUE)

  # Compact repeated blank lines after comment stripping.
  keep <- rep(TRUE, length(lines))
  blank <- trimws(lines) == ""
  if (length(lines) > 1) {
    keep[-1] <- !(blank[-1] & blank[-length(blank)])
  }
  lines <- lines[keep]

  cat("<details>\n")
  cat("<summary>Show script</summary>\n\n")
  cat("```r\n")
  cat(paste(lines, collapse = "\n"))
  cat("\n```\n\n")
  cat("</details>\n")
}

render_code_index <- function() {
  cat("| Module page | Source script |\n")
  cat("|---|---|\n")
  for (i in seq_len(nrow(script_catalog))) {
    page <- relative_page_link(script_catalog$page_href[i])
    cat(
      "| [", script_catalog$label[i], "](", page, ") | `",
      script_catalog$script_rel[i], "` |\n",
      sep = ""
    )
  }
}
