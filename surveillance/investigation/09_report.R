#!/usr/bin/env Rscript
# =============================================================================
# 09_report.R - Rebuild final HTML report from regenerated outputs
# =============================================================================

options(scipen = 999)
source("surveillance/lib/config_paths.R")
paths <- get_paths()
init_paths(paths)

out_dir <- paths$output_private_dir
report_dir <- paths$output_public_dir
fig_dir <- paths$figures_dir
wrangled_dir <- paths$wrangled_dir
if (!dir.exists(report_dir)) dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

escape_html <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

fmt <- function(x, digits = 3) {
  if (is.na(x)) return("NA")
  if (!is.numeric(x)) return(as.character(x))
  formatC(x, format = "f", digits = digits)
}

fmt_pct <- function(x, digits = 1) {
  if (is.na(x)) return("NA")
  paste0(formatC(100 * x, format = "f", digits = digits), "%")
}

table_html <- function(df, digits = 3) {
  if (is.null(df) || nrow(df) == 0) return("<p><em>No data available.</em></p>")
  cols <- names(df)
  body <- apply(df, 1, function(r) {
    cells <- vapply(seq_along(r), function(i) {
      val <- r[[i]]
      num_val <- suppressWarnings(as.numeric(val))
      if (!is.na(num_val) && is.finite(num_val)) {
        val <- fmt(num_val, digits = digits)
      }
      paste0("<td>", escape_html(val), "</td>")
    }, character(1))
    paste0("<tr>", paste0(cells, collapse = ""), "</tr>")
  })
  head_cells <- paste0("<th>", escape_html(cols), "</th>", collapse = "")
  paste0(
    "<table class='tbl'><thead><tr>", head_cells, "</tr></thead><tbody>",
    paste0(body, collapse = "\n"),
    "</tbody></table>"
  )
}

read_csv_safe <- function(path) {
  if (!file.exists(path)) return(NULL)
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

# ---- Load outputs ----
pumf <- readRDS(file.path(wrangled_dir, "cpads_pumf_wrangled.rds"))
prev <- read_csv_safe(file.path(out_dir, "frequentist_heavy_drinking_prevalence_ci.csv"))
hyp <- read_csv_safe(file.path(out_dir, "frequentist_hypothesis_tests.csv"))
bayes_post <- read_csv_safe(file.path(out_dir, "bayesian_posterior_summaries.csv"))
bayes_bf <- read_csv_safe(file.path(out_dir, "bayesian_bayes_factors.csv"))
power <- read_csv_safe(file.path(out_dir, "power_analysis_summary.csv"))
log_or <- read_csv_safe(file.path(out_dir, "logistic_odds_ratios.csv"))
mod_comp <- read_csv_safe(file.path(out_dir, "model_comparison_summary.csv"))
treat <- read_csv_safe(file.path(out_dir, "treatment_effects_summary.csv"))
causal_comp <- read_csv_safe(file.path(out_dir, "causal_estimator_comparison.csv"))
cate <- read_csv_safe(file.path(out_dir, "cate_subgroup_estimates.csv"))

overall_prev <- prev[prev$variable == "Overall" & prev$level == "All", ]
if (nrow(overall_prev) == 0) overall_prev <- prev[1, ]

can_or <- log_or[log_or$term == "cannabis_any_use", ]
if (nrow(can_or) == 0) can_or <- log_or[1, ]

ate_ipw <- treat[treat$estimand == "ATE" & treat$method == "IPW", ]
if (nrow(ate_ipw) == 0) ate_ipw <- treat[1, ]

overall_tests <- hyp[grepl("_overall$", hyp$test_name), c("test_name", "statistic", "df", "p_value", "sig_nominal")]
gender_prev <- prev[prev$variable == "Gender", c("level", "prev", "ci_lower", "ci_upper")]
age_prev <- prev[prev$variable == "Age Group", c("level", "prev", "ci_lower", "ci_upper")]
region_prev <- prev[prev$variable == "Province/Region", c("level", "prev", "ci_lower", "ci_upper")]
mh_prev <- prev[prev$variable == "Mental Health", c("level", "prev", "ci_lower", "ci_upper")]

to_pct_df <- function(d) {
  if (is.null(d) || nrow(d) == 0) return(d)
  d$prev <- vapply(d$prev, fmt_pct, character(1))
  d$ci_lower <- vapply(d$ci_lower, fmt_pct, character(1))
  d$ci_upper <- vapply(d$ci_upper, fmt_pct, character(1))
  d
}

gender_prev <- to_pct_df(gender_prev)
age_prev <- to_pct_df(age_prev)
region_prev <- to_pct_df(region_prev)
mh_prev <- to_pct_df(mh_prev)

mk_fig <- function(file_name, caption) {
  p <- file.path(fig_dir, file_name)
  if (!file.exists(p)) return("")
  paste0(
    "<figure><img src='../figures/", escape_html(file_name),
    "' alt='", escape_html(caption),
    "'/><figcaption>", escape_html(caption), "</figcaption></figure>"
  )
}

table1_embed <- if (file.exists(file.path(report_dir, "table1.html"))) {
  "<iframe src='table1.html' title='Table 1' class='tbl-frame'></iframe>"
} else {
  "<p><em>Table 1 output not found.</em></p>"
}

report_path <- file.path(report_dir, "09_report.html")
today <- as.character(Sys.Date())

html <- paste0(
"<!DOCTYPE html>
<html lang='en'>
<head>
  <meta charset='utf-8'>
  <meta name='viewport' content='width=device-width, initial-scale=1'>
  <meta name='author' content='Vansh Singh Ruhela'>
  <title>CPADS Analysis Report (Updated Workflow)</title>
  <script src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js'></script>
  <style>
    body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif; margin: 0; color: #1f2937; background: #f8fafc; line-height: 1.45; }
    .wrap { max-width: 1080px; margin: 0 auto; padding: 28px 20px 40px; }
    h1, h2, h3 { color: #0f172a; margin-top: 1.2rem; }
    h1 { margin-top: 0; }
    .meta { color: #475569; margin-bottom: 20px; }
    .card { background: #fff; border: 1px solid #e2e8f0; border-radius: 10px; padding: 16px 18px; margin: 14px 0; }
    .kpi { display: grid; grid-template-columns: repeat(auto-fit, minmax(230px, 1fr)); gap: 10px; margin-top: 10px; }
    .kpi div { background: #f1f5f9; border-radius: 8px; padding: 10px; }
    .tbl { border-collapse: collapse; width: 100%; font-size: 0.95rem; margin: 8px 0 12px; }
    .tbl th, .tbl td { border: 1px solid #cbd5e1; padding: 8px; text-align: left; }
    .tbl th { background: #e2e8f0; }
    figure { margin: 14px 0; }
    figure img { width: 100%; max-width: 980px; border: 1px solid #cbd5e1; border-radius: 8px; background: #fff; }
    figcaption { color: #475569; font-size: 0.9rem; margin-top: 6px; }
    .tbl-frame { width: 100%; min-height: 680px; border: 1px solid #cbd5e1; border-radius: 8px; background: #fff; }
    code { background: #f1f5f9; padding: 1px 4px; border-radius: 4px; }
    ul { margin-top: 0.4rem; }
  </style>
</head>
<body>
  <div class='wrap'>
    <h1>Alcohol Use Among Canadian Postsecondary Students: Updated CPADS Report</h1>
    <div class='meta'><strong>Author:</strong> Vansh Singh Ruhela<br><strong>Generated:</strong> ", escape_html(today), "<br><strong>Data:</strong> CPADS 2021-2022 PUMF (n = ", nrow(pumf), ")</div>

    <div class='card'>
      <h2>Abstract</h2>
      <p>This report was regenerated from refreshed outputs after rerunning Phases 05-08 using corrected coding from the wrangled Phase 03 data. Previous values near 90% are deprecated.</p>
      <div class='kpi'>
        <div><strong>Heavy drinking prevalence (weighted)</strong><br>", fmt_pct(overall_prev$prev[1], 1), " (95% CI ", fmt_pct(overall_prev$ci_lower[1], 1), " to ", fmt_pct(overall_prev$ci_upper[1], 1), ")</div>
        <div><strong>Cannabis effect (logistic OR)</strong><br>OR ", fmt(can_or$OR[1], 2), " (95% CI ", fmt(can_or$OR_lower95[1], 2), " to ", fmt(can_or$OR_upper95[1], 2), ")</div>
        <div><strong>ATE (IPW)</strong><br>", fmt(ate_ipw$estimate[1], 3), " (95% CI ", fmt(ate_ipw$ci_lower[1], 3), " to ", fmt(ate_ipw$ci_upper[1], 3), ")</div>
      </div>
    </div>

    <div class='card'>
      <h2>Background</h2>
      <p>Analyses were run from the current workflow outputs:</p>
      <ul>
        <li>Frequentist inference: survey-weighted prevalence estimates and hypothesis tests.</li>
        <li>Bayesian inference: posterior summaries and Bayes factors.</li>
        <li>Power analysis: one-proportion, two-proportion, and chi-square power/MDE outputs.</li>
        <li>Regression and model comparison: survey-weighted logistic and nested models.</li>
        <li>Causal estimation: naive, G-computation, IPW, AIPW (manual fallback), ATT/ATC/matching and CATE.</li>
      </ul>
    </div>

    <div class='card'>
      <h2>Estimated blood alcohol concentration (eBAC)</h2>
      <p>Following the CPADS 2021-2022 PUMF documentation (Seidl et al., 2000), estimated blood alcohol concentration on the heaviest drinking day is computed using a Widmark-style equation:</p>
      <p>\\[
      \\mathrm{eBAC} = \\frac{G}{r\\,W} - \\beta\\,t,
      \\]</p>
      <p>where \\(G\\) is grams of ethanol consumed, \\(r\\) is the sex-specific distribution factor, \\(W\\) is body weight (kg), \\(\\beta\\) is the ethanol elimination rate, and \\(t\\) is elapsed time since first drink (hours).</p>
      <p><strong>CPADS inputs and unit conversions.</strong> Let \\(D\\) be the maximum number of standard drinks on the heaviest drinking day in the past 30 days (<code>alc13a</code>); \\(H\\) be hours to consume \\(D\\) (<code>alc13b_a</code>); \\(M\\) be minutes to consume \\(D\\) (<code>alc13b_b</code>); \\(W\\) be body weight in kg (<code>demq4</code>); \\(h\\) be height in cm (<code>demq3</code>).</p>
      <p>\\[
      G = 13.6\\,D, \\qquad
      t = H + \\frac{M}{60} = \\frac{60H+M}{60}.
      \\]</p>
      <p><strong>Sex-specific distribution factor.</strong></p>
      <p>\\[
      \\begin{aligned}
      r_{\\mathrm{female}} &= 0.31223 - 0.006446\\,W + 0.004466\\,h, \\\\
      r_{\\mathrm{male}}   &= 0.31608 - 0.004821\\,W + 0.004632\\,h.
      \\end{aligned}
      \\]</p>
      <p><strong>Elimination rate.</strong> \\(\\beta = 0.017\\;\\mathrm{g/dL/hour}\\).</p>
      <p><strong>g/L to g/dL conversion used in CPADS.</strong></p>
      <p>\\[
      \\mathrm{eBAC}_{(\\mathrm{g/dL})}
      =
      \\frac{1}{10}\\left(\\frac{G}{r\\,W} - 0.017\\,t\\right).
      \\]</p>
      <p><strong>Expanded formulas (g/dL).</strong></p>
      <p>Females:</p>
      <p>\\[
      \\mathrm{eBAC}_{(\\mathrm{g/dL})}
      =
      \\frac{1}{10}\\left(
      \\frac{13.6\\,D}{\\left(0.31223 - 0.006446\\,W + 0.004466\\,h\\right)W}
      - 0.017\\,t
      \\right).
      \\]</p>
      <p>Males:</p>
      <p>\\[
      \\mathrm{eBAC}_{(\\mathrm{g/dL})}
      =
      \\frac{1}{10}\\left(
      \\frac{13.6\\,D}{\\left(0.31608 - 0.004821\\,W + 0.004632\\,h\\right)W}
      - 0.017\\,t
      \\right).
      \\]</p>
      <p><strong>Legal limit indicator (0.08 g/dL).</strong></p>
      <p>\\[
      \\mathrm{eBAC}_{\\mathrm{legal}}=
      \\begin{cases}
      0, & \\mathrm{eBAC}_{(\\mathrm{g/dL})} \\le 0.080,\\\\
      1, & \\mathrm{eBAC}_{(\\mathrm{g/dL})} > 0.080.
      \\end{cases}
      \\]</p>
      <p><strong>Domain.</strong> eBAC computations are defined for respondents in-domain for past-30-day alcohol use (<code>alc13a</code>).</p>
    </div>

    <div class='card'>
      <h2>Initial Results: Prevalence and Inference</h2>
      <h3>Prevalence by gender</h3>
      ", table_html(gender_prev, digits = 2), "
      <h3>Prevalence by age group</h3>
      ", table_html(age_prev, digits = 2), "
      <h3>Prevalence by province/region</h3>
      ", table_html(region_prev, digits = 2), "
      <h3>Prevalence by mental health</h3>
      ", table_html(mh_prev, digits = 2), "
      <h3>Overall hypothesis tests</h3>
      ", table_html(overall_tests, digits = 4), "
    </div>

    <div class='card'>
      <h2>Bayesian Initial Results</h2>
      <h3>Posterior summaries</h3>
      ", table_html(bayes_post, digits = 4), "
      <h3>Bayes factors</h3>
      ", table_html(bayes_bf, digits = 4), "
    </div>

    <div class='card'>
      <h2>Power Design</h2>
      ", table_html(power, digits = 4), "
    </div>

    <div class='card'>
      <h2>Regression and Model Comparison</h2>
      <h3>Logistic odds ratios</h3>
      ", table_html(log_or, digits = 4), "
      <h3>Nested model comparison</h3>
      ", table_html(mod_comp, digits = 4), "
    </div>

    <div class='card'>
      <h2>Causal Estimation</h2>
      <h3>Estimator comparison</h3>
      ", table_html(causal_comp, digits = 4), "
      <h3>Treatment effects summary</h3>
      ", table_html(treat, digits = 4), "
      <h3>CATE (first rows)</h3>
      ", table_html(head(cate, 20), digits = 4), "
    </div>

    <div class='card'>
      <h2>Figures</h2>
      ", mk_fig("bayesian_prior_posterior.png", "Bayesian prior vs posterior"),
      mk_fig("bayesian_vs_frequentist_ci.png", "Bayesian vs frequentist confidence/credible intervals"),
      mk_fig("binge_by_demographics.png", "Heavy drinking by demographics"),
      mk_fig("binge_by_mental_health.png", "Heavy drinking by mental health"),
      mk_fig("cate_forest_plot.png", "CATE forest plot"), "
      <p><em>Note:</em> CADS/PHAC external files were not available in this run, so those corresponding outputs were skipped by scripts.</p>
    </div>

    <div class='card'>
      <h2>Table 1 (HTML)</h2>
      ", table1_embed, "
    </div>

    <div class='card'>
      <h2>References</h2>
      <ol>
        <li>Revelle W. <em>psych: Procedures for Psychological, Psychometric, and Personality Research</em>. Northwestern University; 2026. R package version 2.6.1. <a href='https://CRAN.R-project.org/package=psych'>https://CRAN.R-project.org/package=psych</a>. doi:10.32614/CRAN.package.psych.</li>
        <li>Revelle W. <em>The psych package for personality and psychological research (Personality Project R Guide)</em>. 2023. Accessed 2026-02-20. <a href='https://personality-project.org/r/psych/'>https://personality-project.org/r/psych/</a>.</li>
        <li>Lumley T, Gao P, Schneider B. <em>survey: Analysis of Complex Survey Samples</em>. 2025. R package version 4.4-8. <a href='https://CRAN.R-project.org/package=survey'>https://CRAN.R-project.org/package=survey</a>. doi:10.32614/CRAN.package.survey.</li>
        <li>Firke S, Denney B, Haid C, Knight R, Grosser M, Zadra J. <em>janitor: Simple Tools for Examining and Cleaning Dirty Data</em>. 2024. R package version 2.2.1. <a href='https://CRAN.R-project.org/package=janitor'>https://CRAN.R-project.org/package=janitor</a>. doi:10.32614/CRAN.package.janitor.</li>
        <li>Wickham H, Averick M, Bryan J, et al. Welcome to the tidyverse. <em>Journal of Open Source Software</em>. 2019;4(43):1686. doi:10.21105/joss.01686.</li>
        <li>Health Canada. <em>Drug and alcohol use in Canada: Postsecondary students — dashboard data (CPADS.csv)</em>. 2026. Accessed 2026-02-20. <a href='https://health-infobase.canada.ca/src/data/substance-use/cpads/CPADS.csv'>https://health-infobase.canada.ca/src/data/substance-use/cpads/CPADS.csv</a>.</li>
        <li>Health Canada. <em>CPADS 2021-2022 PUMF Data</em>. 2024. Accessed 2026-02-20. <a href='https://open.canada.ca/data/en/dataset/736fa9b2-62e4-4e31-aea4-51869605b363'>https://open.canada.ca/data/en/dataset/736fa9b2-62e4-4e31-aea4-51869605b363</a>.</li>
        <li>Health Canada. <em>Canadian Postsecondary Education Alcohol and Drug Use Survey: Technical notes</em>. 2024. Accessed 2026-02-20. <a href='https://health-infobase.canada.ca/substance-use/reports/cpads/technical-notes.html'>https://health-infobase.canada.ca/substance-use/reports/cpads/technical-notes.html</a>.</li>
      </ol>
    </div>

  </div>
</body>
</html>")

writeLines(html, con = report_path, useBytes = TRUE)
cat("Saved report:", safe_label_path(report_path, paths), "\n")
