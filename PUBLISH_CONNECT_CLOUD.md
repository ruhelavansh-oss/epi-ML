# Posit Connect Cloud Publish Guide (EML)

## Scope

This guide covers publishing the Quarto website in this repository to Posit Connect Cloud using GitHub integration.

## Prerequisites

1. Repository has no pending changes and is pushed to:
   - `https://github.com/ruhelavansh-oss/epi-ML`
2. Private paths are ignored in git:
   - `data/private/**`
   - `.env`
   - `.Renviron`
3. Required project files exist:
   - `_quarto.yml`
   - `manifest.json`

## Pre-Publish Validation

```bash
Rscript scripts/install_deps.R --connect
Rscript scripts/security_scan.R
Rscript scripts/generate_manifest.R
Rscript scripts/check_connect_publish_readiness.R
```

If output files were updated, run the workflow before validation:

```bash
Rscript scripts/run_modules.R
```

## Push to GitHub

```bash
git add .
git commit -m "Prepare EML site for publish"
git push origin main
```

## Deploy on Posit Connect Cloud

1. Sign in to Posit Connect Cloud.
2. Create new content from GitHub repository.
3. Select `ruhelavansh-oss/epi-ML` and the target branch.
4. Set primary document to `_quarto.yml`.
5. Deploy and verify build logs.
6. Optionally enable auto-redeploy on each push.

Optional CLI deployment helper:

```bash
RSCONNECT_ACCOUNT=<your-account> Rscript scripts/deploy_connect_cloud.R
```

## Post-Deploy Verification

1. Open the deployed site and check all pages load correctly.
2. Confirm repository links, navigation, and citations resolve as expected.
3. Validate that no private filesystem references appear in rendered pages.
4. Confirm GitHub Pages fallback remains available:
   - <https://ruhelavansh-oss.github.io/epi-ML/>

## Optional GitHub Pages Custom Domain

After DNS ownership and records are configured, add a `CNAME` file at repository root with:

```text
epi-ml.io
```

Until then, use the default GitHub Pages URL.
