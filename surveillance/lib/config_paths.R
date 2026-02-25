# Shared path and runtime configuration for data study modules.

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
  cfg_file <- Sys.getenv("RUNTIME_CONFIG", file.path(project_root, "config", "runtime.yml"))
  if (!file.exists(cfg_file)) return(list())
  if (!requireNamespace("yaml", quietly = TRUE)) return(list())

  cfg_vals <- tryCatch(yaml::read_yaml(cfg_file), error = function(e) list())
  if (!is.list(cfg_vals)) return(list())
  cfg_vals
}

is_abs_path <- function(path) {
  grepl("^(/|[A-Za-z]:[/\\\\])", path)
}

to_abs_path <- function(path, project_root) {
  if (is_abs_path(path)) return(path)
  file.path(project_root, path)
}

discover_data_file <- function(data_dir, fallback_name = "data.csv") {
  fallback <- file.path(data_dir, fallback_name)
  if (!dir.exists(data_dir)) return(fallback)

  csv_files <- list.files(data_dir, pattern = "[.]csv$", full.names = TRUE, ignore.case = TRUE)
  if (length(csv_files) == 0) return(fallback)

  base <- tolower(basename(csv_files))
  keep <- !grepl("(info|manifest|output|log)", base)
  candidate <- if (any(keep)) csv_files[keep] else csv_files
  info <- file.info(candidate)
  candidate[which.max(info$size)]
}

resolve_data_file <- function(data_file, data_dir) {
  data_file_norm <- normalizePath(data_file, winslash = "/", mustWork = FALSE)
  if (file.exists(data_file_norm)) return(data_file_norm)
  discover_data_file(data_dir = data_dir)
}

get_paths <- function() {
  project_root <- Sys.getenv("PROJECT_ROOT", "")
  if (identical(project_root, "")) {
    project_root <- find_project_root()
  }
  if (identical(project_root, "")) {
    stop("Unable to resolve project root. Set PROJECT_ROOT before running scripts.")
  }
  project_root <- normalizePath(project_root, winslash = "/", mustWork = FALSE)

  cfg <- read_runtime_defaults(project_root)

  data_dir_default <- cfg$data_dir %||% file.path(project_root, "data", "private")
  data_file_default <- cfg$data_file %||% file.path(data_dir_default, "data.csv")
  out_private_default <- cfg$output_private_dir %||% file.path(project_root, "data", "private", "outputs")
  out_public_default <- cfg$output_public_dir %||% file.path(project_root, "reports", "public")
  reports_default <- cfg$reports_dir %||% file.path(project_root, "reports", "drafts")

  data_dir <- to_abs_path(Sys.getenv("DATA_DIR", data_dir_default), project_root)
  data_file <- to_abs_path(Sys.getenv("DATA_FILE", data_file_default), project_root)
  output_private_dir <- to_abs_path(Sys.getenv("OUTPUT_PRIVATE_DIR", out_private_default), project_root)
  output_public_dir <- to_abs_path(Sys.getenv("OUTPUT_PUBLIC_DIR", out_public_default), project_root)
  reports_dir <- to_abs_path(Sys.getenv("REPORTS_DIR", reports_default), project_root)
  data_file <- resolve_data_file(data_file = data_file, data_dir = data_dir)

  list(
    project_root = project_root,
    data_dir = normalizePath(data_dir, winslash = "/", mustWork = FALSE),
    public_data_dir = normalizePath(file.path(project_root, "data", "public"), winslash = "/", mustWork = FALSE),
    data_file = normalizePath(data_file, winslash = "/", mustWork = FALSE),
    output_private_dir = normalizePath(output_private_dir, winslash = "/", mustWork = FALSE),
    output_public_dir = normalizePath(output_public_dir, winslash = "/", mustWork = FALSE),
    reports_dir = normalizePath(reports_dir, winslash = "/", mustWork = FALSE),
    wrangled_dir = normalizePath(file.path(output_private_dir, "wrangled"), winslash = "/", mustWork = FALSE),
    figures_dir = normalizePath(file.path(output_private_dir, "figures"), winslash = "/", mustWork = FALSE),
    logs_dir = normalizePath(file.path(project_root, "logs"), winslash = "/", mustWork = FALSE),
    secret_key = Sys.getenv("SECRET_KEY", "")
  )
}

init_paths <- function(paths = get_paths(), dirs = c("output_private_dir", "wrangled_dir", "figures_dir", "output_public_dir", "reports_dir", "logs_dir")) {
  for (d in dirs) {
    dir.create(paths[[d]], recursive = TRUE, showWarnings = FALSE)
  }
  dir.create(file.path(paths$public_data_dir, "outputs"), recursive = TRUE, showWarnings = FALSE)
  invisible(paths)
}

assert_required_files <- function(paths = get_paths(), keys = c("data_file")) {
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
      "PRIVATE_DATA_DIR",
      "PUBLIC_DATA_DIR",
      "PRIVATE_OUTPUT_DIR",
      "PUBLIC_REPORTS_DIR",
      "REPORTS_DRAFTS_DIR",
      "PROJECT_ROOT"
    ),
    c(
      paths$data_dir,
      paths$public_data_dir,
      paths$output_private_dir,
      paths$output_public_dir,
      paths$reports_dir,
      paths$project_root
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
