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
epi-ML/
├── [root configuration files]
├── .github/
│   └── workflows/
│       ├── lintr.yml
│       ├── pages.yml
│       ├── r.yml
│       └── security.yml
├── code/
│   ├── _code_helpers.R
│   ├── bayesian-inference.qmd
│   ├── causal-estimators.qmd
│   ├── dag-specification.qmd
│   ├── data-wrangling.qmd
│   ├── descriptive-statistics.qmd
│   ├── distribution-tests.qmd
│   ├── ebac-core.qmd
│   ├── ebac-gender-smote-sensitivity.qmd
│   ├── ebac-integrations.qmd
│   ├── ebac-selection-adjustment-ipw.qmd
│   ├── figures.qmd
│   ├── final-report.qmd
│   ├── frequentist-inference.qmd
│   ├── index.qmd
│   ├── logistic-models.qmd
│   ├── meta-synthesis.qmd
│   ├── model-comparison.qmd
│   ├── power-design.qmd
│   ├── propensity-scores.qmd
│   ├── regression-models.qmd
│   ├── tables.qmd
│   └── treatment-effects.qmd
├── config/
│   └── runtime.yml
├── data/
│   ├── private/
│   └── public/
│       ├── outputs/
│       └── outputs_manifest.csv
├── filters/
│   ├── color-box.lua
│   ├── include-dark.lua
│   └── tools-tabset.lua
├── images/
│   ├── Posit-Logos-2024_horiz-full-color-white-text.svg
│   └── Posit-Logos-2024_horiz-full-color.svg
├── logs/
│   └── emissions/
├── packages/
│   └── epiml/
│       ├── DESCRIPTION
│       ├── NAMESPACE
│       ├── README.md
│       ├── R/
│       ├── inst/
│       ├── man/
│       └── tests/
├── reports/
│   ├── drafts/
│   └── public/
├── scripts/
│   ├── build_emissions_dashboard_data.R
│   ├── check_connect_publish_readiness.R
│   ├── check_output_parity.R
│   ├── check_power_design_render.R
│   ├── deploy_connect_cloud.R
│   ├── generate_manifest.R
│   ├── install_deps.R
│   ├── install_git_hooks.sh
│   ├── preview_site.R
│   ├── publish_public_artifacts.R
│   ├── render_site.R
│   ├── run_modules.R
│   ├── security_scan.R
│   ├── seed_emissions_log_from_public.py
│   ├── track_emissions.py
│   ├── unlock_data_local.sh
│   └── entrypoints/
│       ├── 03_data_wrangling.R
│       ├── 04_descriptive_stats.R
│       ├── 04_distributions.R
│       ├── 05_bayesian.R
│       ├── 05_frequentist.R
│       ├── 05_power_design.R
│       ├── 06_logistic.R
│       ├── 06_model_comparison.R
│       ├── 06_regression.R
│       ├── 07_causal_estimators.R
│       ├── 07_dag.R
│       ├── 07_ebac.R
│       ├── 07_ebac_gender_smote_sensitivity.R
│       ├── 07_ebac_integrations.R
│       ├── 07_ebac_ipw.R
│       ├── 07_meta_synthesis.R
│       ├── 07_propensity.R
│       ├── 07_treatment_effects.R
│       ├── 08_figures.R
│       ├── 08_tables.R
│       └── 09_report.R
├── surveillance/
│   ├── lib/
│   │   └── config_paths.R
│   ├── investigation/
│   │   ├── 03_data_wrangling.R
│   │   ├── 04_descriptive_stats.R
│   │   ├── 04_distributions.R
│   │   ├── 05_bayesian.R
│   │   ├── 05_frequentist.R
│   │   ├── 05_power_design.R
│   │   ├── 06_logistic.R
│   │   ├── 06_model_comparison.R
│   │   ├── 06_regression.R
│   │   ├── 07_causal_estimators.R
│   │   ├── 07_dag.R
│   │   ├── 07_meta_synthesis.R
│   │   ├── 07_propensity.R
│   │   ├── 07_treatment_effects.R
│   │   ├── 08_figures.R
│   │   ├── 08_tables.R
│   │   └── 09_report.R
│   └── ebac/
│       ├── 07_ebac.R
│       ├── 07_ebac_gender_smote_sensitivity.R
│       ├── 07_ebac_integrations.R
│       └── 07_ebac_ipw.R
```

## Website and Publication

- Quarto website configuration: `_quarto.yml`
- Public pages: `index.qmd`, `background.qmd`, `initial-results.qmd`, `reproducibility.qmd`, `about.qmd`
- Primary deployment: GitHub Pages (Actions)
- Fallback deployment: Posit Connect Cloud (manifest-based)
- GitHub Pages URL (ACTIVE): <https://ruhelavansh-oss.github.io/epi-ML/>
- Repository URL: [https://github.com/ruhelavansh-oss/epi-ML](https://github.com/ruhelavansh-oss/epi-ML)

## Reproducibility Workflow

1. Configure local environment variables from `.Renviron.example`.
2. Run module workflow (when regeneration is needed):

```bash
Rscript scripts/run_modules.R
```

This workflow also publishes aggregate artifacts to `data/public/outputs`.

To track compute emissions with CodeCarbon during module runs:

```bash
python3 -m pip install codecarbon
Rscript scripts/run_modules.R --track-emissions
```

Optional upload to the CodeCarbon online dashboard:

```bash
CODECARBON_SAVE_TO_API=1 CODECARBON_EXPERIMENT_ID=<experiment-id> Rscript scripts/run_modules.R --track-emissions
```

Tracked raw logs are written to `logs/emissions/` (gitignored). Sanitized dashboard datasets are published to `data/public/outputs/emissions/`.

3. Render site and reports:

```bash
Rscript scripts/render_site.R
```

This command refreshes public aggregate artifacts before rendering.

4. Preview with a guaranteed fresh render (recommended):

```bash
Rscript scripts/preview_site.R --port 4774
```

This avoids stale website output from `quarto preview` caching behavior.

5. Run publish readiness checks:

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

`epi-ML` DOI records for `1.1.1.1`:

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
  version      = {1.1.1.1},
  doi          = {10.5281/zenodo.18752220},
  url          = {https://doi.org/10.5281/zenodo.18752220},
}
```

`epiml` package component BibTeX:

```bibtex
@Manual{ruhela_epiml_2026,
  title   = {epiml: Reproducibility Utilities for Epidemiological ML Workflows},
  author  = {Ruhela, Vansh},
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
