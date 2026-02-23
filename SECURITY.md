# Security Policy

## Scope

This repository publishes a Quarto website and reproducible research workflow for the EML project. Public artifacts must not contain private data, local machine paths, credentials, or row-level confidential outputs.

## Supported Publication Model

- Public content: source code, curated narrative pages, and sanitized outputs.
- Private content: raw microdata, row-level intermediates, and secret material remain outside git.

## Security Controls

1. **Path and secret scanning**
   - `scripts/security_scan.R` runs locally and in CI.
   - CI also runs gitleaks on push and pull requests.
   - `.gitleaks.toml` includes a narrow allowlist for the public Algolia `search-only-api-key` line in `_quarto.yml`; no admin/write keys are allowlisted.
2. **Private path exclusion**
   - `data/private/**` and related local files are gitignored.
   - `manifest.json` generation excludes private and build-only paths.
3. **Deployment checks**
   - `scripts/check_connect_publish_readiness.R` validates publication safety before deploy.

## Reporting a Vulnerability

If you identify a security issue in the repository or deployment process, open a private security advisory on GitHub or contact the repository maintainer directly with:

- issue summary,
- reproducible steps,
- affected files/paths,
- suggested mitigation.

Do not publish sensitive details in a public issue before remediation.
