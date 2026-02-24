# epiml

`epiml` is a lightweight R package for reproducibility operations in epidemiological ML projects.

## What it does

- Reads and validates `outputs_manifest.csv` tables.
- Audits whether declared public artifacts are present on disk.
- Builds output manifests from a directory of generated files.
- Runs project workflow steps (`modules`, `publish`, `render`, `readiness`) from R.
- Generates synthetic epidemiology-style tabular data for development/testing in private-data-constrained situations.

## Scientific guardrail

- Synthetic data should be used for development, testing, demos, and CI only.
- Final inferential or policy-facing results must be produced from approved real data with full provenance.
- Synthetic runs should be explicitly labeled as synthetic in outputs and reporting text.

## Install from local source

```r
install.packages("packages/epiml", repos = NULL, type = "source")
```

## Example

```r
library(epiml)

manifest <- read_outputs_manifest(project_root = "/path/to/project")
audit <- audit_public_outputs(project_root = "/path/to/project", manifest = manifest)
summary <- summarize_output_audit(audit)

summary
```

## Synthetic data example

```r
library(epiml)

synthetic_path <- write_synthetic_data(
  path = "data/private/synthetic_study_data.csv",
  n = 8000,
  seed = 2026,
  overwrite = TRUE
)

synthetic_path
```

## Cross-project adaptation

```r
library(epiml)

name_map <- default_synthetic_name_map("generic")
name_map["cannabis_use"] <- "exposure_any"
name_map["bac"] <- "outcome_continuous"

dat <- generate_synthetic_data(
  n = 5000,
  seed = 1,
  name_map = name_map
)
```
