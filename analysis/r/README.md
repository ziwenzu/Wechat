# R migration notes

This folder is a clean R scaffold that sits alongside the current Stata workflow.

The main design choice is deliberate: the new R scripts keep the observed data pipeline separate from theory-driven simulation choices. In the current Stata workflow, [`analysis/code/clean.do`](../code/clean.do) mixes raw cleaning with imputation, synthetic engagement shocks, and visibility-treatment calibration. That is fine for prototyping, but it makes it hard to tell later which results come from the data and which come from modeling assumptions.

## Files

- [`analysis/r/01_build_dataset.R`](01_build_dataset.R): reads `articles.csv`, parses dates, creates transparent category mappings, and saves an R-ready dataset.
- [`analysis/r/02_descriptives.R`](02_descriptives.R): creates descriptive tables and two quick plots.
- [`analysis/r/03_rdd.R`](03_rdd.R): runs a baseline daily-aggregated RDD around the 2017 and 2020 interface changes.
- [`analysis/r/_common.R`](_common.R): shared helpers for paths, argument parsing, category mapping, and safe rate calculations.

## Suggested run order

```bash
Rscript analysis/r/01_build_dataset.R
Rscript analysis/r/02_descriptives.R
Rscript analysis/r/03_rdd.R
```

For a smoke test on a smaller slice:

```bash
Rscript analysis/r/01_build_dataset.R --nrows=100000 --output=wechat_posts_clean_smoke.rds
Rscript analysis/r/02_descriptives.R --input=wechat_posts_clean_smoke.rds
Rscript analysis/r/03_rdd.R --input=wechat_posts_clean_smoke.rds
```

## What the current data look like

- `articles.csv` currently contains about 4.29 million posts from 329 official accounts spanning 2013 to 2025.
- The engagement variables are extremely sparse. In the full file, zeros dominate:
  - `read_num`: about 95.4% zero
  - `like_num`: about 96.9% zero
  - `share_num`: about 98.5% zero
  - `look_num`: about 97.4% zero
- Before estimating substantive effects, it is worth checking whether those zeros are true zeros, top-coded scrapes, missing observations recorded as zero, or a mix of all three.

## Category mapping choices

The baseline mapping is intentionally conservative:

- `propaganda`: `意识形态与宣传教育`, `时政与领导活动`
- `public_service`: `公共服务信息`, `社会保障与公共福利`, `应急管理与风险沟通`
- `other`: everything else by default, with `state_governance` broken out as its own intermediate group

This is meant as a transparent starting point, not the final theoretical classification. Once you settle the LLM labeling scheme, the easiest next step is to replace the mapping in [`analysis/r/_common.R`](_common.R).

## Recommended next upgrades

- Replace the rule-based category map with the final LLM-coded labels.
- Add account and calendar fixed effects with [`fixest`](https://lrberge.github.io/fixest/).
- Move from the baseline daily-aggregated RDD to account-day or post-level event-study specifications.
- Save intermediate data in a columnar format if the full workflow starts to feel slow.
