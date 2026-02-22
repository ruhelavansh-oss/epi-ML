# Shared path and runtime configuration for CPADS study modules.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x) || identical(x, "")) y else x
}

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in seq_len(6)) {
    if (file.exists(file.path(current, "surveillance", "lib", "config_paths.R"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  ""
}

read_runtime_defaults <- function(project_root) {
  cfg_file <- Sys.getenv("CPADS_RUNTIME_CONFIG", file.path(project_root, "config", "runtime.yml"))
  if (!file.exists(cfg_file)) return(list())
  if (!requireNamespace("yaml", quietly = TRUE)) return(list())

  raw <- tryCatch(yaml::read_yaml(cfg_file), error = function(e) list())
  if (!is.list(raw)) return(list())
  raw
}

get_paths <- function() {
  project_root <- Sys.getenv("CPADS_PROJECT_ROOT", "")
  if (identical(project_root, "")) {
    project_root <- find_project_root()
  }
  if (identical(project_root, "")) {
    stop("Unable to resolve project root. Set CPADS_PROJECT_ROOT before running scripts.")
  }
  project_root <- normalizePath(project_root, winslash = "/", mustWork = FALSE)

  cfg <- read_runtime_defaults(project_root)

  data_dir_default <- cfg$data_dir %||% file.path(project_root, "data", "private")
  raw_default <- cfg$raw_pumf_file %||% file.path(data_dir_default, "CPADS_PUMF.csv")
  out_private_default <- cfg$output_private_dir %||% file.path(project_root, "data", "private", "outputs")
  out_public_default <- cfg$output_public_dir %||% file.path(project_root, "reports", "public")
  reports_default <- cfg$reports_dir %||% file.path(project_root, "reports", "source")

  data_dir <- Sys.getenv("CPADS_DATA_DIR", data_dir_default)
  raw_pumf_file <- Sys.getenv("CPADS_RAW_PUMF_FILE", raw_default)
  output_private_dir <- Sys.getenv("CPADS_OUTPUT_PRIVATE_DIR", out_private_default)
  output_public_dir <- Sys.getenv("CPADS_OUTPUT_PUBLIC_DIR", out_public_default)
  reports_dir <- Sys.getenv("CPADS_REPORTS_DIR", reports_default)

  list(
    project_root = project_root,
    data_dir = normalizePath(data_dir, winslash = "/", mustWork = FALSE),
    raw_pumf_file = normalizePath(raw_pumf_file, winslash = "/", mustWork = FALSE),
    output_private_dir = normalizePath(output_private_dir, winslash = "/", mustWork = FALSE),
    output_public_dir = normalizePath(output_public_dir, winslash = "/", mustWork = FALSE),
    reports_dir = normalizePath(reports_dir, winslash = "/", mustWork = FALSE),
    wrangled_dir = normalizePath(file.path(output_private_dir, "wrangled"), winslash = "/", mustWork = FALSE),
    figures_dir = normalizePath(file.path(output_private_dir, "figures"), winslash = "/", mustWork = FALSE),
    logs_dir = normalizePath(file.path(project_root, "logs"), winslash = "/", mustWork = FALSE),
    secret_key = Sys.getenv("CPADS_SECRET_KEY", "")
  )
}

init_paths <- function(paths = get_paths(), dirs = c("output_private_dir", "wrangled_dir", "figures_dir", "output_public_dir", "reports_dir", "logs_dir")) {
  for (d in dirs) {
    dir.create(paths[[d]], recursive = TRUE, showWarnings = FALSE)
  }
  invisible(paths)
}

assert_required_files <- function(paths = get_paths(), keys = c("raw_pumf_file")) {
  missing <- keys[!vapply(keys, function(k) file.exists(paths[[k]]), logical(1))]
  if (length(missing) > 0) {
    stop(
      "Missing required files: ",
      paste(vapply(missing, function(k) paste0(k, "=", safe_label_path(paths[[k]], paths)), character(1)), collapse = ", ")
    )
  }
  invisible(TRUE)
}

safe_label_path <- function(path, paths = get_paths()) {
  labels <- stats::setNames(
    c(
      "PROJECT_ROOT",
      "PRIVATE_DATA_DIR",
      "PRIVATE_OUTPUT_DIR",
      "PUBLIC_REPORTS_DIR",
      "REPORTS_SOURCE_DIR"
    ),
    c(
      paths$project_root,
      paths$data_dir,
      paths$output_private_dir,
      paths$output_public_dir,
      paths$reports_dir
    )
  )
  out <- normalizePath(path, winslash = "/", mustWork = FALSE)
  for (prefix in names(labels)) {
    if (startsWith(out, prefix)) {
      rel <- sub(paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", prefix), "/?"), "", out)
      return(file.path(labels[[prefix]], rel))
    }
  }
  out
}
