# Epidemiological Semiparametric Machine Learning (EML)

This repository hosts the study and publication framework for a retrospective observational cohort of alcohol and cannabis use in the Canadian Postsecondary Education Alcohol and Drug Use Survey (CPADS 2021-2022 PUMF).

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
  investigation/ # core CPADS study phases
  ebac/          # eBAC-focused scripts
scripts/         # orchestration, rendering, and checks
reports/
  source/        # manuscript/report source files
  public/        # local rendered report artifacts (gitignored)
data/
  private/       # local-only raw and private outputs (gitignored)
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

3. Render site and reports:

```bash
Rscript scripts/render_site.R
```

4. Run publish readiness checks:

```bash
Rscript scripts/check_connect_publish_readiness.R
```

Optional dependency bootstrap (local/CI parity):

```bash
Rscript scripts/install_deps.R
```

For full analysis-module dependencies (not only render/deploy):

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

## Data Governance

- Raw microdata and row-level private outputs are excluded from version control.
- Runtime paths are environment-driven to avoid hardcoded machine-specific locations.
- Security/path scans are enforced locally and in CI before deployment.
- Security policy: `SECURITY.md`

## License

This repository is licensed under the Apache License 2.0. See `LICENSE` and `NOTICE`.
