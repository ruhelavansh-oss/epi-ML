---
Preliminaries: "Interpretation Guide for Current CPADS Initial Results"
Author: VSR
---

# Interpretation Guide for Current CPADS Initial Results

This file explains what your current outputs mean in plain language, while staying faithful to the exact workflow currently implemented.

## 1. Direct answer to your three headline numbers

### Heavy drinking prevalence (weighted): 45.8% (95% CI 45.2% to 46.5%)
- Source: `PROJECT_ROOT/data/private/outputs/frequentist_heavy_drinking_prevalence_ci.csv`
- Exact estimate: `0.4580988313946399`.
- Interpretation: after applying survey weights (`wtpumf`), the estimated proportion of the target student population with heavy drinking in the past 30 days is about **45.8%**.
- CI meaning: under the survey-weighted inference model used in the script, plausible values for the population prevalence are about **45.2% to 46.5%**.
- This estimate was based on respondents with nonmissing heavy-drinking outcome (`n_unweighted_nonmissing = 31719`).

### Cannabis effect (logistic OR): OR 5.71 (95% CI 5.38 to 6.07)
- Source: `PROJECT_ROOT/data/private/outputs/logistic_odds_ratios.csv`
- Exact estimate for `cannabis_any_use`: OR `5.7103`, CI `5.3761` to `6.0653`.
- Interpretation: in the survey-weighted logistic model, after adjustment for age group, gender, region, and mental health, the **odds** of heavy drinking are estimated to be about **5.7 times** higher among cannabis users vs non-users.
- Important: OR is an **odds ratio**, not a probability ratio. It does **not** mean cannabis users are 5.7 times as likely in probability terms.

### ATE (IPW): 0.394 (95% CI 0.383 to 0.404)
- Source: `PROJECT_ROOT/data/private/outputs/treatment_effects_summary.csv`
- Exact IPW ATE estimate: `0.39366363793148723` (CI `0.3831325775249193` to `0.40419469833805516`).
- Interpretation: this is an **absolute risk difference** estimate.
  - It means the estimated probability of heavy drinking is about **39.4 percentage points higher** under cannabis use than under no cannabis use, averaged over the analysis sample, under model assumptions.
- In potential-outcomes notation:
  \[
  ATE = E[Y(1)] - E[Y(0)]
  \]
  where treatment \(T=1\) is cannabis use and outcome \(Y=1\) is heavy drinking.
- Reconstructing the script’s IPW decomposition gives approximately:
  - \(E[Y(1)] \approx 0.6949\)
  - \(E[Y(0)] \approx 0.3012\)
  - difference \(\approx 0.3937\)

So the ATE is not abstract: it is a **difference in expected prevalence** (about 69.5% vs 30.1% in the IPW pseudo-population constructed by the script).

---

## 2. What is the treatment, what is the outcome?

In your causal scripts (`PROJECT_ROOT/07_propensity.R`, `PROJECT_ROOT/07_causal_estimators.R`, `PROJECT_ROOT/07_treatment_effects.R`):

- **Treatment**: `cannabis_any_use`
  - `1 = yes`
  - `0 = no`

- **Outcome**: `heavy_drinking_30d`
  - `1 = heavy drinking in past 30 days`
  - `0 = no heavy drinking in past 30 days`

- **Measured confounders used for adjustment**:
  - `age_group`
  - `gender`
  - `province_region`
  - `mental_health`
  - `physical_health`

---

## 3. Was there randomization?

Short answer: **No**.

This is observational survey data, not a randomized trial. No script randomly assigns cannabis use. The workflow tries to approximate a randomized comparison by adjustment methods (propensity scores, IPW, matching, g-computation, AIPW), but these are model-based corrections, not true random assignment.

What this implies:
- You can report causal estimates, but only **conditionally** (if assumptions hold).
- Unmeasured confounding can still bias estimates.

---

## 4. What is the sampling design in this workflow?

### Data origin
- CPADS PUMF sample from `PROJECT_ROOT/CPADS_PUMF.csv`.

### How sampling weights are used
- Descriptive/frequentist/logistic scripts use survey weights with:
  - `survey::svydesign(ids = ~1, weights = ~wtpumf, data = ...)`
- So prevalence and survey-logistic estimates are intended as weighted population-style estimates.

### Design complexity note
- Current scripts use `ids = ~1` only (no explicit strata/cluster variables in these calls).
- Power script reports DEFF around `1.4136580928207156`, with effective n around `22437.53` for the heavy-drinking endpoint.

### Important distinction
- The causal scripts (`07_*`) generally use complete-case data and propensity-based weights, but **not** survey weights in the final treatment-effect calculations.
- Therefore, your causal ATE/ATT/ATC results are not the same target as the survey-weighted prevalence/logistic outputs.

---

## 5. What do “IPW”, “matching”, and “subclassification” mean here?

## IPW (Inverse Probability Weighting)
- Fit model for treatment probability: \(\hat e(X)=P(T=1\mid X)\)
- Reweight people so treated and control groups are more comparable on measured covariates.
- In `07_treatment_effects.R`, propensity scores are clipped to `[0.01, 0.99]` before constructing weights.
- ATE, ATT, and ATC are then computed as weighted mean differences.

## Matching
- **ATT nearest-neighbor matching (1:1)**:
  - Each treated unit is paired with a similar control by propensity score.
- **ATE subclassification (10 subclasses)**:
  - Units are grouped into propensity strata; treatment effect aggregated across strata.

## Why these are used
They are attempts to reduce confounding by measured covariates. They do not fix unmeasured confounding.

---

## 6. How to interpret OR vs ATE (and why they are different)

These are different effect scales and come from different modeling setups:

- **OR (5.71)** is on the **odds scale** from survey-weighted logistic regression.
- **ATE (0.394)** is on the **risk-difference scale** from propensity-weighted causal estimation.

They answer different questions:
- OR: multiplicative effect on odds, conditional on covariates in that logistic model.
- ATE: average absolute change in outcome probability under treatment vs no treatment (under causal assumptions).

So OR and ATE are not expected to be numerically similar.

---

## 7. Additional interpretation of major result families

## Frequentist subgroup findings
From `PROJECT_ROOT/data/private/outputs/frequentist_hypothesis_tests.csv`, omnibus tests for gender, age group, region, and mental health are all highly significant after correction. This indicates strong heterogeneity in heavy-drinking prevalence across these demographics.

## Bayesian results
Posterior prevalence under multiple priors is highly stable (around `0.4795` unweighted), indicating the sample is large enough that prior choice has limited influence on that posterior mean.

## Causal-estimator triangulation
From `PROJECT_ROOT/data/private/outputs/causal_estimator_comparison.csv`, ATE estimates cluster around `0.39-0.41` across naive, g-computation, IPW, and manual AIPW. This consistency across estimators is supportive, but not proof of causal validity.

## eBAC-specific findings
From `PROJECT_ROOT/data/private/outputs/ebac_final_key_summary.csv` and related final eBAC outputs:

- Weighted prevalence of `ebac_legal = 1` in the observed eBAC domain is `0.6499` (about 65.0%).
- Weighted mean `ebac_tot` is `0.1431` g/dL.
- Adjusted weighted OR for cannabis on `ebac_legal` is `3.3166`.
- Selection-adjusted IPW OR is `3.3302` (very close to observed-domain OR), suggesting limited change from adjustment for eBAC observability.
- eBAC causal contrasts on the risk-difference scale are around `0.256` to `0.260` (ATE/ATT/ATC).
- DML random-forest estimates are similar in direction and somewhat smaller in magnitude (`ATE = 0.2468`, `ATTE = 0.2425`).
- Gender interaction for cannabis on `ebac_legal` was not strongly supported (`p = 0.170` in the joint interaction test).

Interpretation: cannabis use remains strongly associated with higher eBAC risk indicators across modeling strategies, and the effect is robust to IPW and DML sensitivity checks, but interpretation is limited to respondents with observed eBAC.

---

## 8. What can you claim vs what you should not claim

## Reasonable claims (with caveats)
- Weighted heavy-drinking prevalence in this analyzed CPADS population is around 46%.
- Cannabis use shows a strong positive association with heavy drinking in adjusted survey-logistic analysis.
- Multiple causal estimators in this workflow produce large positive estimated effects (about +39 percentage points).

## Claims that would be too strong
- “This proves cannabis causes heavy drinking by 39.4% in all students.”
- “The OR means probability is 5.71x.”
- “Adjustment fully removes confounding.”

## Required causal assumptions to state explicitly
- Consistency
- Conditional exchangeability (no unmeasured confounding after adjustment)
- Positivity
- Correct specification of propensity/outcome models

---

## 9. Known caveats you should keep visible in writeups

1. `probability_estimates.csv` contains one invalid probability (`P(Alcohol12m | Cannabis12m) > 1`), so that particular computed conditional quantity is not interpretable as a valid probability.
2. Weighted and unweighted analyses are mixed across phases, so interpret each estimate in its own target-population context.
3. CATE table (`cate_subgroup_estimates.csv`) is based on unadjusted subgroup contrasts in the current script.
4. Complete-case analysis can change target population and introduce selection bias if missingness is informative.

---

## 10. One-sentence interpretation you can reuse

In the current CPADS workflow, heavy drinking is estimated at about 45.8% in the weighted population, cannabis use is strongly associated with heavy drinking in adjusted survey-logistic models (OR about 5.71), and propensity-based causal analyses estimate a large absolute risk increase (ATE about +0.394), but causal interpretation remains conditional on standard no-unmeasured-confounding and model-correctness assumptions.
