# Internal infix helper for defaults.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  if (length(x) == 1 && (is.na(x) || identical(x, ""))) return(y)
  x
}

is_absolute_path <- function(path) {
  grepl("^(/|[A-Za-z]:[/\\\\])", path)
}

#' Find a project root directory
#'
#' Searches upward from `start` for a directory containing a Quarto config
#' and a `data/public` tree.
#'
#' @param start Starting directory.
#' @param max_up Maximum number of parent traversals.
#' @return Absolute path to the detected project root.
#' @export
find_project_root <- function(start = getwd(), max_up = 10L) {
  current <- normalizePath(start, winslash = "/", mustWork = FALSE)

  for (i in seq_len(max_up)) {
    has_quarto <- file.exists(file.path(current, "_quarto.yml"))
    has_public <- dir.exists(file.path(current, "data", "public"))
    has_scripts <- dir.exists(file.path(current, "scripts"))
    if (has_quarto && has_public && has_scripts) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }

  stop(
    "Unable to detect project root. Provide `project_root` explicitly.",
    call. = FALSE
  )
}

#' Resolve standard project paths
#'
#' @param project_root Project root directory. If `NULL`, inferred from the
#'   current working directory.
#' @return Named list of key paths.
#' @export
epiml_paths <- function(project_root = NULL) {
  root <- project_root %||% find_project_root()
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)

  list(
    project_root = root,
    public_data_dir = file.path(root, "data", "public"),
    outputs_dir = file.path(root, "data", "public", "outputs"),
    outputs_manifest = file.path(root, "data", "public", "outputs_manifest.csv"),
    scripts_dir = file.path(root, "scripts")
  )
}
