###############################################################################
# 07_dag.R
# Directed Acyclic Graph (DAG) for the cannabis_any_use -> heavy_drinking_30d
# causal pathway using CPADS PUMF data.
#
# Identifies minimal sufficient adjustment sets and instrumental variables.
# Saves DAG plot to outputs/figures/dag_heavy_drinking.pdf
###############################################################################

options(scipen = 999)
set.seed(42)
source("surveillance/lib/config_paths.R")
paths <- get_paths()
init_paths(paths)

suppressPackageStartupMessages({
  library(dagitty)
  library(ggplot2)
})

cat("=== 07_dag.R: DAG Specification and Analysis ===\n\n")

# ---------------------------------------------------------------------------
# 1. Load data (for reference / summary only)
# ---------------------------------------------------------------------------
dat <- tryCatch(
  readRDS(file.path(paths$wrangled_dir, "cpads_pumf_wrangled.rds")),
  error = function(e) {
    stop("Could not load wrangled CPADS data: ", e$message)
  }
)

cat("Data loaded:", nrow(dat), "observations,", ncol(dat), "variables\n")
cat("Treatment (cannabis_any_use) prevalence:",
    round(mean(dat$cannabis_any_use, na.rm = TRUE) * 100, 1), "%\n")
cat("Outcome  (heavy_drinking_30d)     prevalence:",
    round(mean(dat$heavy_drinking_30d, na.rm = TRUE) * 100, 1), "%\n\n")

# ---------------------------------------------------------------------------
# 2. Define the DAG
# ---------------------------------------------------------------------------
# Causal assumptions:
#   - cannabis_any_use (exposure) may cause heavy_drinking_30d (outcome)
#   - age_group, gender, province_region affect both cannabis use and binge
#     drinking (confounders)
#   - mental_health and physical_health are common causes of both substance
#     use patterns (confounders)
#   - mental_health and physical_health may also be influenced by age and gender
#   - province_region captures regional policy / culture differences
#
# DAG structure:
#   age_group       -> cannabis_any_use, heavy_drinking_30d, mental_health, physical_health
#   gender             -> cannabis_any_use, heavy_drinking_30d, mental_health, physical_health
#   province_region -> cannabis_any_use, heavy_drinking_30d
#   mental_health   -> cannabis_any_use, heavy_drinking_30d
#   physical_health -> cannabis_any_use, heavy_drinking_30d
#   cannabis_any_use -> heavy_drinking_30d

dag <- dagitty::dagitty('
  dag {
    cannabis_any_use [exposure]
    heavy_drinking_30d    [outcome]

    age_group       -> cannabis_any_use
    age_group       -> heavy_drinking_30d
    age_group       -> mental_health
    age_group       -> physical_health

    gender             -> cannabis_any_use
    gender             -> heavy_drinking_30d
    gender             -> mental_health
    gender             -> physical_health

    province_region -> cannabis_any_use
    province_region -> heavy_drinking_30d

    mental_health   -> cannabis_any_use
    mental_health   -> heavy_drinking_30d

    physical_health -> cannabis_any_use
    physical_health -> heavy_drinking_30d

    cannabis_any_use -> heavy_drinking_30d
  }
')

cat("DAG specification:\n")
print(dag)
cat("\n")

# ---------------------------------------------------------------------------
# 3. Identify adjustment sets
# ---------------------------------------------------------------------------
cat("--- Minimal Sufficient Adjustment Sets ---\n")
adj_sets <- dagitty::adjustmentSets(dag, type = "minimal")
if (length(adj_sets) == 0) {
  cat("No adjustment is needed (no open backdoor paths).\n")
} else {
  for (i in seq_along(adj_sets)) {
    cat("  Set", i, ": {", paste(adj_sets[[i]], collapse = ", "), "}\n")
  }
}
cat("\n")

cat("--- All Sufficient Adjustment Sets ---\n")
adj_sets_all <- dagitty::adjustmentSets(dag, type = "all")
for (i in seq_along(adj_sets_all)) {
  cat("  Set", i, ": {", paste(adj_sets_all[[i]], collapse = ", "), "}\n")
}
cat("\n")

# ---------------------------------------------------------------------------
# 4. Check for instrumental variables
# ---------------------------------------------------------------------------
cat("--- Instrumental Variables ---\n")
iv <- tryCatch(
  dagitty::instrumentalVariables(dag, exposure = "cannabis_any_use",
                                  outcome = "heavy_drinking_30d"),
  error = function(e) {
    cat("Error identifying IVs:", e$message, "\n")
    NULL
  }
)

if (is.null(iv) || length(iv) == 0) {
  cat("No instrumental variables identified in this DAG.\n")
  cat("(This is expected given the structure: all covariates are common\n")
  cat(" causes of both exposure and outcome.)\n")
} else {
  cat("Instrumental variable(s) found:\n")
  print(iv)
}
cat("\n")

# ---------------------------------------------------------------------------
# 5. Identify implied conditional independencies
# ---------------------------------------------------------------------------
cat("--- Implied Conditional Independencies (testable implications) ---\n")
impl <- dagitty::impliedConditionalIndependencies(dag)
if (length(impl) == 0) {
  cat("No testable implications (DAG is saturated or nearly so).\n")
} else {
  for (i in seq_along(impl)) {
    cat("  ", format(impl[[i]]), "\n")
  }
}
cat("\n")

# ---------------------------------------------------------------------------
# 6. Plot the DAG
# ---------------------------------------------------------------------------
fig_dir <- paths$figures_dir
# Try ggdag first, fall back to base plot
ggdag_available <- requireNamespace("ggdag", quietly = TRUE)

if (ggdag_available) {
  suppressPackageStartupMessages(library(ggdag))

  # Set coordinates for a balanced layout
  coords <- list(
    x = c(cannabis_any_use = 0, heavy_drinking_30d = 4,
           age_group = 1, gender = 3,
           province_region = 2,
           mental_health = 0.5, physical_health = 3.5),
    y = c(cannabis_any_use = 0, heavy_drinking_30d = 0,
           age_group = 2, gender = 2,
           province_region = 2.5,
           mental_health = 1, physical_health = 1)
  )
  dagitty::coordinates(dag) <- coords

  p <- ggdag(dag, text = FALSE, use_labels = "name") +
    theme_dag() +
    labs(title = "DAG: Cannabis Use -> Heavy Drinking",
         subtitle = "Confounders: age, gender, province, mental & physical health") +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10)
    )

  pdf(file.path(fig_dir, "dag_heavy_drinking.pdf"), width = 10, height = 7)
  print(p)
  dev.off()
  cat("DAG plot saved to:", file.path(fig_dir, "dag_heavy_drinking.pdf"), "(ggdag)\n")

} else {
  # Fallback: base R plot
  coords <- list(
    x = c(cannabis_any_use = 0, heavy_drinking_30d = 4,
           age_group = 1, gender = 3,
           province_region = 2,
           mental_health = 0.5, physical_health = 3.5),
    y = c(cannabis_any_use = 0, heavy_drinking_30d = 0,
           age_group = 2, gender = 2,
           province_region = 2.5,
           mental_health = 1, physical_health = 1)
  )
  dagitty::coordinates(dag) <- coords

  pdf(file.path(fig_dir, "dag_heavy_drinking.pdf"), width = 10, height = 7)
  plot(dag, main = "DAG: Cannabis Use -> Heavy Drinking")
  dev.off()
  cat("DAG plot saved to:", file.path(fig_dir, "dag_heavy_drinking.pdf"), "(base)\n")
}

cat("\n=== 07_dag.R complete ===\n")
