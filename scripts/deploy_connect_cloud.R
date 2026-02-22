#!/usr/bin/env Rscript

source("surveillance/lib/config_paths.R")
paths <- get_paths()
setwd(paths$project_root)

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Package 'rsconnect' is required. Install with install.packages('rsconnect').")
}

account_name <- Sys.getenv("RSCONNECT_ACCOUNT", "")
if (!nzchar(account_name)) {
  stop("Set RSCONNECT_ACCOUNT before deploy, e.g., Sys.setenv(RSCONNECT_ACCOUNT = '<account>').")
}

app_name <- Sys.getenv("RSCONNECT_APP_NAME", "epi-ml-site")
app_title <- Sys.getenv("RSCONNECT_APP_TITLE", "EPI-ML Project")
server_name <- Sys.getenv("RSCONNECT_SERVER", "posit.cloud")

cat("Deploying Quarto site with the following settings:\n")
cat("  account:", account_name, "\n")
cat("  app_name:", app_name, "\n")
cat("  app_title:", app_title, "\n")
cat("  server:", server_name, "\n\n")

rsconnect::deployApp(
  appDir = ".",
  appName = app_name,
  appTitle = app_title,
  account = account_name,
  server = server_name,
  appPrimaryDoc = "_quarto.yml",
  launch.browser = FALSE,
  forceUpdate = TRUE
)

cat("Deployment request submitted.\n")
