# RDD Audit Follow-Up

Date: 2026-03-31

This note records which externally generated RDD follow-up scripts were audited, which were retained, and which were not integrated into the paper.

## Integrated

### `code/25_hausman_rapson_robustness.R`

- Purpose: address temporal-RDD inference concerns by documenting article-level cross-sectional density around each cutoff and by checking alternative clustering.
- Audit outcome: retained and fixed.
- Fixes:
  - added explicit `data.table::uniqueN()` namespacing so the script runs without attaching `data.table`
  - aligned the clustering-table caption with the rows that are actually estimable
- Main findings:
  - local windows are dense, with 961 articles and 188 accounts per day around the 2018 cutoff, and 1,392 articles and 231 accounts per day around the 2020 cutoff
  - the main one-click estimates keep the same sign and similar magnitude under heteroskedasticity-robust, province-clustered, and manual two-way account-by-day clustered inference
- Integrated into manuscript:
  - Research Design
  - main-text Validity discussion
  - SI subsection "Temporal RDD Inference and Clustering"

### `code/26_confound_checks.R`

- Purpose: diagnose whether the July 2020 cutoff is contaminated by CPC-anniversary timing and nearby composition changes.
- Audit outcome: retained and fixed, but interpreted cautiously.
- Fixes:
  - repaired namespace issues and data-table subsetting
  - removed structurally unavailable 2019 `like_rate` placebo
  - rewrote the omnibus-balance covariates to use pre-publication article attributes rather than reads
  - dropped the standalone day-of-week balance table, which is not especially informative in a date-based RD because weekday is mechanically tied to the running variable
- Main findings:
  - July 1, 2019 shows a negative one-click placebo discontinuity and lower reads, so July 1 is not a clean null date
  - party-themed posts account for only 1.1% of pre-cutoff and 1.4% of post-cutoff articles in 2020
  - excluding party-themed posts leaves the 2020 like and zaikan results nearly unchanged
  - hard-propaganda share and reads remain locally continuous at the 2020 cutoff
- Integrated into manuscript:
  - main-text Validity discussion
  - SI subsection "July 1 Calendar Confounds"

### `code/27_covariate_adjusted_rdd.R`

- Purpose: re-estimate the main local RDDs with article-level covariates for precision.
- Audit outcome: retained after narrowing the covariate set.
- Fixes:
  - removed `read_num` from the covariate set because it is not a valid pre-treatment covariate for engagement outcomes
- Main findings:
  - point estimates remain close to the baseline main-text RDDs
- Integrated into manuscript:
  - SI subsection "Covariate-Adjusted Local Specifications"

## Audited But Not Integrated

### `code/28_did_event_study.R`

- Purpose: build a propaganda-versus-service DiD / event-study complement over a longer panel.
- Audit outcome: not integrated.
- Reason:
  - the original specification uses `account_id + year_month` fixed effects on an account-month-family panel, which does not absorb family-specific baseline levels cleanly
  - the current event-study extraction is fragile, and the coefficient pattern is not yet ready for paper use
  - substantively, this design relies on stronger parallel-trends assumptions than the paper's main RDD strategy

### `code/29_power_and_local_rand.R`

- Purpose: add `rdpower` calculations and local-randomization inference.
- Audit outcome: not integrated.
- Reason:
  - the script calls `rdpower` with an incorrect API
  - the local-randomization table expects object elements that are not returned as coded
  - beyond code breakage, local randomization is not the cleanest complement for a daily temporal running variable with mass points unless implemented very carefully

### `code/30_persistence_clustering.R`

- Purpose: expanding-donut persistence checks and July 1 placebo dates.
- Audit outcome: only the July 1 placebo logic was useful, and that evidence is already captured more cleanly through `code/26_confound_checks.R`.
- Reason:
  - the expanding-donut estimates are unstable and often become numerically fragile as the hole widens
  - the broader July 1 placebo pattern is informative as a warning, not as a robustness pass, so it is better handled in the calendar-confound discussion rather than as a standalone appendix battery

## Bottom Line

The follow-up scripts added real value in two places:

1. temporal-RDD inference diagnostics
2. direct July 1 confound diagnostics

They did not yet add paper-ready value in three places:

1. DiD/event-study complements
2. power analysis
3. local-randomization inference

The manuscript now incorporates the first two and explicitly treats the July 2020 design more cautiously as a result.
