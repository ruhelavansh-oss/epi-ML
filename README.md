# Epidemiological Semiparametric Machine Learning (EML)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.18752220.svg)](https://doi.org/10.5281/zenodo.18752220)

This repository hosts the study and publication framework for a retrospective observational cohort of alcohol and cannabis use in the Canadian Postsecondary Education Alcohol and Drug Use Survey public use microdata file (data 2021-2022 dataset). The repository also includes a reusable reproducibility package under `packages/epiml` for manifest auditing, workflow orchestration, and synthetic-data testing workflows.

## Research Focus

The project estimates both associational and causal-effect targets for:

- **Exposure**: `cannabis_any_use`
- **Primary outcome**: `heavy_drinking_30d`
- **eBAC outcomes**: `ebac_tot`, `ebac_legal`
- **Adjustment covariates**: age group, gender, region, mental-health rating, physical-health rating

## Background Framework

Implemented methods include:

- Survey-weighted prevalence and confidence interval estimation
- Frequentist and Bayesian inference layers
- Power design under SRS and design-effect scenarios
- Logistic and regression model comparison
- Propensity-score weighting, matching, and doubly robust estimation
- Treatment-effect summaries (ATE, ATT, ATC, subgroup contrasts)
- eBAC-domain models, sensitivity checks with SMOTE, and DoubleML random-forest diagnostics

## Repository Structure

```text
surveillance/
  lib/           # shared configuration helpers
  investigation/ # core data study phases
  ebac/          # eBAC-focused scripts
scripts/         # orchestration, rendering, and checks
reports/
  drafts/        # manuscript/report source files
  public/        # rendered report artifacts (gitignored)
data/
  private/       # local initial outputs (gitignored)
  public/        # publishable aggregate artifacts (tracked)
packages/
  epiml/         # reusable reproducibility/audit package
```

## Website and Publication

- Quarto website configuration: `_quarto.yml`
- Public pages: `index.qmd`, `background.qmd`, `initial-results.qmd`, `reproducibility.qmd`, `about.qmd`
- Primary deployment: GitHub Pages (Actions)
- Fallback deployment: Posit Connect Cloud (manifest-based)
- GitHub Pages URL: <https://ruhelavansh-oss.github.io/epi-ML/>

Repository URL: [https://github.com/ruhelavansh-oss/epi-ML](https://github.com/ruhelavansh-oss/epi-ML)

## Reproducibility Workflow

1. Configure local environment variables from `.Renviron.example`.
2. Run module workflow (when regeneration is needed):

```bash
Rscript scripts/run_modules.R
```

This workflow also publishes aggregate artifacts to `data/public/outputs`.

3. Render site and reports:

```bash
Rscript scripts/render_site.R
```

This command refreshes public aggregate artifacts before rendering.

4. Run publish readiness checks:

```bash
Rscript scripts/check_connect_publish_readiness.R
```

Optional dependency bootstrap (local/CI parity):

```bash
Rscript scripts/install_deps.R
```

For full module dependencies (not only render/deploy):

```bash
Rscript scripts/install_deps.R --full
```

For Posit Connect publish dependencies (includes `rsconnect`):

```bash
Rscript scripts/install_deps.R --connect
```

Optional Posit Connect deploy helper:

```bash
RSCONNECT_ACCOUNT=<your-account> Rscript scripts/deploy_connect_cloud.R
```

Package-level reproducibility helpers (optional):

```r
install.packages("packages/epiml", repos = NULL, type = "source")
library(epiml)

manifest <- read_outputs_manifest(project_root = getwd())
audit <- audit_public_outputs(project_root = getwd(), manifest = manifest)
summarize_output_audit(audit)
```

## Citations

`epi-ML` DOI records for `v1.1.1`:

- Current DOI: <https://doi.org/10.5281/zenodo.18752220>
- Legacy DOI record: <https://doi.org/10.5281/zenodo.18750663>

`epi-ML` computational notebook:

```bibtex
@misc{ruhela_2026_18752220,
  author       = {Ruhela, Vansh},
  title        = {Epidemiological Semiparametric Machine Learning},
  month        = feb,
  year         = 2026,
  publisher    = {Zenodo},
  version      = {1.1.1},
  doi          = {10.5281/zenodo.18752220},
  url          = {https://doi.org/10.5281/zenodo.18752220},
}
```

`epiml` package component BibTeX:

```bibtex
@Manual{ruhela_epiml_2026,
  title   = {epiml: Reproducibility Utilities for Epidemiological ML Workflows},
  author  = {Ruhela, Vansh Singh},
  year    = {2026},
  note    = {R package version 0.1.0},
  doi     = {10.5281/zenodo.18752220},
  url     = {https://github.com/ruhelavansh-oss/epi-ML/tree/main/packages/epiml}
}
```

## Data Governance

- Runtime paths are environment-driven to avoid hardcoded machine-specific locations.
- Security/path scans are enforced locally and in CI before deployment.
- Security policy: `SECURITY.md`

## License

This repository is licensed under the Apache License 2.0. See `LICENSE` and `NOTICE`.
