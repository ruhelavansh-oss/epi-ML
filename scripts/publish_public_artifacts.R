#!/usr/bin/env Rscript
source("surveillance/lib/config_paths.R")
paths <- get_paths()
init_paths(paths)
setwd(paths$project_root)

src_root <- paths$output_private_dir
dst_root <- file.path(paths$public_data_dir, "outputs")
manifest_path <- file.path(paths$public_data_dir, "outputs_manifest.csv")

publish_ext <- c("csv", "pdf", "png", "html", "txt", "md")
ext_pattern <- paste0("\\.(", paste(publish_ext, collapse = "|"), ")$")

if (!dir.exists(src_root)) {
  cat("No private output directory found. Nothing to publish.\n")
  quit(status = 0)
}

all_files <- list.files(src_root, recursive = TRUE, full.names = TRUE, include.dirs = FALSE)
keep <- grepl(ext_pattern, tolower(all_files))
files <- all_files[keep]

if (length(files) == 0) {
  cat("No publishable artifacts found in private outputs.\n")
  quit(status = 0)
}

escape_rx <- function(x) gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", x)
src_rx <- paste0("^", escape_rx(normalizePath(src_root, winslash = "/", mustWork = FALSE)), "/?")

copied <- 0L
meta <- vector("list", length(files))
for (i in seq_along(files)) {
  src <- normalizePath(files[[i]], winslash = "/", mustWork = FALSE)
  rel <- sub(src_rx, "", src)
  dst <- file.path(dst_root, rel)
  dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(src, dst, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
  if (!isTRUE(ok)) next
  copied <- copied + 1L
  info <- file.info(dst)
  meta[[i]] <- data.frame(
    output = rel,
    public_path = file.path("data/public/outputs", rel),
    size_kb = sprintf("%.1f", as.numeric(info$size) / 1024),
    modified = format(info$mtime, "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  )
}

meta <- Filter(Negate(is.null), meta)
if (length(meta) > 0) {
  manifest <- do.call(rbind, meta)
  manifest <- manifest[order(manifest$output), ]
  write.csv(manifest, manifest_path, row.names = FALSE)
}

cat(
  "Published ", copied, " artifact(s) to ",
  safe_label_path(dst_root, paths),
  ".\n",
  sep = ""
)
if (file.exists(manifest_path)) {
  cat("Updated manifest: ", safe_label_path(manifest_path, paths), "\n", sep = "")
}
