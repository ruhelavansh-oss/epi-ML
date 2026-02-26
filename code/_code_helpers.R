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
    value <- x %||% fallback
    is_abs <- grepl("^(/|[A-Za-z]:[/\\\\])", value)
    if (!is_abs) value <- file.path(base, value)
    normalizePath(value, winslash = "/", mustWork = FALSE)
  }

  project_root <- absolutize(p$project_root, root, base = root)
  list(
    project_root = project_root,
    public_data_dir = absolutize(
      p$public_data_dir,
      file.path(project_root, "data", "public"),
      base = project_root
    ),
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
      file.path(project_root, "reports", "drafts"),
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

detect_git_branch <- function(project_root, default = "main") {
  branch <- tryCatch(
    system2(
      "git",
      c("-C", project_root, "rev-parse", "--abbrev-ref", "HEAD"),
      stdout = TRUE,
      stderr = FALSE
    ),
    error = function(e) character(0)
  )
  branch <- trimws(branch[1] %||% "")
  if (!nzchar(branch) || identical(branch, "HEAD")) {
    return(default)
  }
  branch
}

repo_owner <- Sys.getenv("EPIML_GITHUB_OWNER", "ruhelavansh-oss")
repo_name <- Sys.getenv("EPIML_GITHUB_REPO", "epi-ML")
repo_branch <- Sys.getenv("EPIML_GITHUB_BRANCH", "")
if (!nzchar(repo_branch)) {
  repo_branch <- detect_git_branch(paths$project_root, default = "main")
}
github_repo_base <- paste0("https://github.com/", repo_owner, "/", repo_name)
github_blob_base <- paste0(github_repo_base, "/blob/", repo_branch, "/")
github_raw_base <- paste0("https://raw.githubusercontent.com/", repo_owner, "/", repo_name, "/", repo_branch, "/")
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
  matched <- regmatches(lines, m)
  vals <- unique(gsub("^['\\\"]|['\\\"]$", "", unlist(matched)))
  vals <- vals[!grepl("\\.(R|r|Rmd|rmd|qmd)$", vals)]
  # Internal row-level artifacts (RDS) are intentionally excluded from public module tables.
  vals <- vals[!grepl("\\.rds$", tolower(vals))]
  vals <- vals[!grepl("^https?://", vals)]
  # Canonicalize to basename for stable module tables and deduplication.
  unique(basename(vals))
}

candidate_paths <- function(name) {
  nm <- gsub("^\\./", "", name)
  unique(c(
    # Prefer public artifact locations so Site/Repo links resolve when published.
    file.path(paths$public_data_dir, "outputs", nm),
    file.path(paths$public_data_dir, "outputs", "figures", nm),
    file.path(paths$public_data_dir, "outputs", "wrangled", nm),
    file.path(paths$public_data_dir, "outputs", basename(nm)),
    file.path(paths$public_data_dir, "outputs", "figures", basename(nm)),
    file.path(paths$public_data_dir, "outputs", "wrangled", basename(nm)),
    file.path(paths$project_root, nm),
    file.path(paths$output_private_dir, nm),
    file.path(paths$output_private_dir, "figures", nm),
    file.path(paths$output_private_dir, "wrangled", nm),
    file.path(paths$output_private_dir, basename(nm)),
    file.path(paths$output_private_dir, "figures", basename(nm)),
    file.path(paths$output_private_dir, "wrangled", basename(nm)),
    file.path(paths$output_public_dir, nm),
    file.path(paths$output_public_dir, basename(nm)),
    file.path(paths$reports_dir, nm),
    file.path(paths$project_root, "reports", "public", basename(nm)),
    file.path(paths$project_root, "reports", "drafts", basename(nm)),
    file.path(paths$reports_dir, basename(nm))
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
    return(normalizePath(file.path(paths$public_data_dir, "outputs", nm), winslash = "/", mustWork = FALSE))
  }

  if (ext %in% c("png", "pdf")) {
    return(normalizePath(file.path(paths$public_data_dir, "outputs", "figures", base), winslash = "/", mustWork = FALSE))
  }
  if (ext %in% c("html", "md", "tex", "bib")) {
    return(normalizePath(file.path(paths$output_public_dir, base), winslash = "/", mustWork = FALSE))
  }
  normalizePath(file.path(paths$public_data_dir, "outputs", base), winslash = "/", mustWork = FALSE)
}

resolve_output <- function(name) {
  cands <- candidate_paths(name)
  idx <- which(file.exists(cands))
  if (length(idx) == 0) {
    expected <- default_expected_path(name)
    path_label <- if (exists("safe_label_path")) safe_label_path(expected, paths) else expected
    return(list(
      found = FALSE,
      status = "no",
      path = path_label,
      abs_path = expected,
      repo_rel = NA_character_,
      size_kb = NA_character_,
      modified = NA_character_
    ))
  }
  fp <- cands[idx[1]]
  info <- file.info(fp)
  path_label <- if (exists("safe_label_path")) safe_label_path(fp, paths) else fp
  repo_rel <- NA_character_
  if (starts_with_path(fp, paths$project_root)) {
    repo_rel <- sub(paste0("^", normalizePath(paths$project_root, winslash = "/", mustWork = FALSE), "/"), "", fp)
  }
  list(
    found = TRUE,
    status = "yes",
    path = path_label,
    abs_path = fp,
    repo_rel = repo_rel,
    size_kb = sprintf("%.1f", as.numeric(info$size) / 1024),
    modified = format(info$mtime, "%Y-%m-%d")
  )
}

render_script_metadata <- function(script_rel) {
  abs <- script_path(script_rel)
  exists_flag <- file.exists(abs)
  wrapper_rel <- file.path("scripts", "entrypoints", basename(script_rel))
  cmd <- if (file.exists(file.path(paths$project_root, wrapper_rel))) {
    paste("Rscript", wrapper_rel)
  } else if (file.exists(file.path(paths$project_root, basename(script_rel)))) {
    paste("Rscript", basename(script_rel))
  } else {
    paste("Rscript", script_rel)
  }
  n_lines <- if (exists_flag) length(readLines(abs, warn = FALSE, encoding = "UTF-8")) else 0L
  script_alias <- basename(script_rel)
  script_url <- paste0(github_blob_base, script_rel)

  cat_row <- which(script_catalog$script_rel == script_rel)
  page_value <- "\u2014"
  if (length(cat_row) > 0) {
    page_href <- script_catalog$page_href[cat_row[1]]
    page_url <- paste0(github_blob_base, page_href)
    page_value <- paste0("[", script_catalog$label[cat_row[1]], "](", page_url, ")")
  }

  metadata <- data.frame(
    Field = c("Script", "Page", "Exists", "Lines"),
    Value = c(
      paste0("[", script_alias, "](", script_url, ")"),
      page_value,
      ifelse(exists_flag, "yes", "no"),
      as.character(n_lines)
    ),
    stringsAsFactors = FALSE
  )

  cat("<div class=\"module-metadata-table-wrap\">\n")
  print(knitr::kable(metadata, format = "html", escape = FALSE, col.names = c("Field", "Value")))
  cat("</div>\n")
  cat("\n**Run command**\n\n")
  cat("```bash\n", cmd, "\n```\n", sep = "")
}

escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

escape_html_attr <- function(x) {
  x <- escape_html(x)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}

soft_wrap_tokens <- function(x) {
  gsub("([/_.-])", "\\1&#8203;", x, perl = TRUE)
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
  table_rows <- lapply(seq_along(outputs), function(i) {
    r <- resolved[[i]]
    declared <- outputs[[i]]
    declared_alias <- basename(declared)
    ext <- toupper(tools::file_ext(declared_alias))
    if (!nzchar(ext)) ext <- "FILE"

    repo_rel <- r$repo_rel %||% NA_character_

    is_public_artifact <- !is.na(repo_rel) && starts_with_path(r$abs_path, paths$public_data_dir)
    site_value <- "\u2014"
    if (is_public_artifact) {
      # Use site-relative href so links work in localhost preview and published site.
      site_href <- paste0("../", repo_rel)
      site_value <- paste0(
        "<a href=\"",
        site_href,
        "\" title=\"",
        ext,
        " URL\">",
        ext,
        "</a>"
      )
    }

    repo_value <- "\u2014"
    if (is_public_artifact) {
      repo_value <- paste0(
        "<a href=\"",
        github_blob_base,
        repo_rel,
        "\" title=\"",
        ext,
        " URL\">URL</a>"
      )
    }

    copy_btn <- if (!is.na(r$path)) {
      paste0(
        "<button type=\"button\" class=\"copy-path-btn\" data-copy=\"",
        escape_html_attr(r$path),
        "\" title=\"Copy path\"><i class=\"bi bi-clipboard\"></i></button>"
      )
    } else {
      ""
    }
    file_label <- soft_wrap_tokens(escape_html(declared_alias))
    file_value <- paste0(
      "<div class=\"module-file-cell\">",
      "<span class=\"module-file-name\">",
      file_label,
      "</span>",
      copy_btn,
      "</div>"
    )

    data.frame(
      File = file_value,
      True = ifelse(identical(r$status, "yes"), "Y", "N"),
      Site = site_value,
      KB = ifelse(is.na(r$size_kb), "NA", r$size_kb),
      Repo = repo_value,
      MoD = ifelse(is.na(r$modified), "NA", r$modified),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })

  tbl <- do.call(rbind, table_rows)
  cat("<div class=\"module-output-table-wrap\">\n")
  print(knitr::kable(
    tbl,
    format = "html",
    escape = FALSE,
    align = c("l", "c", "c", "c", "c", "c"),
    col.names = c("File Path", "\u2713", "Site", "KB", "Repo", "MoD")
  ))
  cat("</div>\n")
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

  cat("<details class=\"module-script-details\">\n")
  cat("<summary class=\"module-script-summary\">Show script</summary>\n\n")
  code_text <- escape_html(paste(lines, collapse = "\n"))
  cat("<pre class=\"module-script-pre\"><code class=\"language-r module-script-code\">")
  cat(code_text)
  cat("</code></pre>\n\n")
  cat("</details>\n")
}

render_code_index <- function() {
  resolve_script_rel <- function(script_rel) {
    candidates <- c(script_rel, basename(script_rel))
    for (rel in candidates) {
      if (file.exists(file.path(paths$project_root, rel))) return(rel)
    }
    script_rel
  }

  rows <- lapply(seq_len(nrow(script_catalog)), function(i) {
    page_rel <- relative_page_link(script_catalog$page_href[i])
    script_rel <- resolve_script_rel(script_catalog$script_rel[i])
    script_abs <- normalizePath(file.path(paths$project_root, script_rel), winslash = "/", mustWork = FALSE)
    script_label <- basename(script_rel)
    script_url <- paste0(github_blob_base, script_rel)
    copy_value <- if (exists("safe_label_path")) safe_label_path(script_abs, paths) else script_rel

    data.frame(
      Module = paste0("[", script_catalog$label[i], "](", page_rel, ")"),
      Source = paste0("[", script_label, "](", script_url, ")"),
      Path = paste0(
        "<button type=\"button\" class=\"copy-path-btn\" data-copy=\"",
        escape_html_attr(copy_value),
        "\" title=\"Copy path\"><i class=\"bi bi-clipboard\"></i></button>"
      ),
      stringsAsFactors = FALSE
    )
  })

  manifest_rel <- "data/public/outputs_manifest.csv"
  manifest_abs <- normalizePath(file.path(paths$project_root, manifest_rel), winslash = "/", mustWork = FALSE)
  manifest_repo <- paste0(github_blob_base, manifest_rel)
  manifest_site <- paste0("../", manifest_rel)
  manifest_copy <- if (exists("safe_label_path")) safe_label_path(manifest_abs, paths) else manifest_rel

  rows[[length(rows) + 1]] <- data.frame(
    Module = paste0("<a href=\"", manifest_repo, "\">Outputs Manifest</a>"),
    Source = paste0("<a href=\"", manifest_site, "\">outputs_manifest.csv</a>"),
    Path = paste0(
      "<button type=\"button\" class=\"copy-path-btn\" data-copy=\"",
      escape_html_attr(manifest_copy),
      "\" title=\"Copy path\"><i class=\"bi bi-clipboard\"></i></button>"
    ),
    stringsAsFactors = FALSE
  )

  tbl <- do.call(rbind, rows)
  cat("<div class=\"module-index-table-wrap\">\n")
  print(knitr::kable(
    tbl,
    format = "html",
    escape = FALSE,
    align = c("l", "l", "c"),
    col.names = c("Module", "Source", "Path")
  ))
  cat("</div>\n")
}
