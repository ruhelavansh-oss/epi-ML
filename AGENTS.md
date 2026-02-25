# Project-Specific Guardrails (Official-Doc Alignment)

## Scope Lock
- Treat both sources as joint constraints:
  - CPADS PUMF technical guidance.
  - Sex-differences commentary on hypothesis-driven interaction analysis.
- Keep CPADS inference explicitly observational.
- Keep prospective randomization artifacts explicitly design-only.

## Power and Gender Rules
- Primary power planning must target interaction terms, not subgroup-only post hoc contrasts.
- Required power targets: 0.80, 0.85, 0.90, 0.95, 0.99, 0.999.
- Near-equal subgroup planning is mandatory for primary design outputs.
- Report observed-imbalance penalties against equal-strata plans.
- Keep transgender/non-binary groups disaggregated when present; do not collapse by default.

## Terminology Rules
- Do not conflate sex and gender in prose or output labels.
- Use "gender (self-reported)" when describing the CPADS grouping variable.
- If sex-at-birth is unavailable or incomplete, state that constraint explicitly.

## Output Contracts
- Preserve legacy power CSV outputs for backward compatibility.
- Publish new interaction-power outputs, randomization blueprints, and official-doc checklist.
- Use flags to separate modes:
  - `analysis_mode = observational`
  - `design_mode = prospective_trial_planning`

## Execution Order
1. `Rscript scripts/run_modules.R`
2. `Rscript scripts/render_site.R`
3. `Rscript scripts/check_connect_publish_readiness.R`

## Quality Gates
- Power monotonicity across target levels.
- Equal-allocation integrity by subgroup.
- Non-negative observed-imbalance penalty where estimable.
- Balanced assignments within randomization blocks.
