# Code Notes

This folder contains the project’s R-only workflow. Stata code has been removed so that the repository has a single analysis path.

## Files

- [`01_build_dataset.R`](01_build_dataset.R): reads `data/articles.csv`, parses dates, creates transparent category mappings, and saves an R-ready dataset to `data/`.
- [`02_descriptives.R`](02_descriptives.R): creates descriptive tables in `tables/` as LaTeX `.tex` files and saves plots in `figures/` as PDF files, including annual trends for the three main theory-facing content families.
- [`03_rdd.R`](03_rdd.R): runs daily-aggregated RDDs around the 2018 and 2020 interface changes, writes RDD tables to `tables/` as LaTeX `.tex` files, and saves selected figures as PDF files.
- [`_common.R`](_common.R): shared helpers for paths, argument parsing, category mapping, and safe rate calculations.

## Suggested run order

```bash
Rscript code/01_build_dataset.R
Rscript code/02_descriptives.R
Rscript code/03_rdd.R
```

For a smoke test on a smaller slice:

```bash
Rscript code/01_build_dataset.R --nrows=100000 --output=wechat_posts_clean_smoke.rds
Rscript code/02_descriptives.R --input=wechat_posts_clean_smoke.rds
Rscript code/03_rdd.R --input=wechat_posts_clean_smoke.rds
```

## Notes

- Output rule for research deliverables: tables should be written as LaTeX `.tex` files and figures as PDF files.

- `articles.csv` currently contains about 4.29 million posts from 329 official accounts spanning 2013 to 2025.
- The engagement variables are extremely sparse. In the full file, zeros dominate:
  - `read_num`: about 95.4% zero
  - `like_num`: about 96.9% zero
  - `share_num`: about 98.5% zero
  - `look_num`: about 97.4% zero
- Before estimating substantive effects, it is worth checking whether those zeros are true zeros, top-coded scrapes, missing observations recorded as zero, or a mix of all three.

## Category mapping choices

The baseline mapping is intentionally theory-facing:

- `hard_propaganda`: `意识形态与宣传教育`, `时政与领导活动`
- `soft_propaganda`: `经济与发展建设`, `城市形象与文化活动`
- `public_service`: `公共服务信息`, `社会保障与公共福利`, `应急管理与风险沟通`
- `state_governance`: `政策与政务公开`, `社会治理与执法通报`, `群众动员与社会参与`
- `other`: anything left uncoded after label normalization

This is meant as a transparent starting point, not the final word on every boundary case. Once you settle the final LLM labeling scheme, the easiest next step is to replace the mapping in [`_common.R`](_common.R).

## Recommended next upgrades

- Replace the rule-based category map with the final LLM-coded labels.
- Add account and calendar fixed effects with [`fixest`](https://lrberge.github.io/fixest/).
- Move from the baseline daily-aggregated RDD to account-day or post-level event-study specifications.
- Save intermediate data in a columnar format if the full workflow starts to feel slow.
