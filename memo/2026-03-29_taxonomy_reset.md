# Taxonomy Reset Memo

Date: 2026-03-29

## Scope Reset

- All classification-appendix writing added in the previous round has been removed from the manuscript.
- Work from this point forward is analysis-first.
- Findings, rules, and diagnostics are recorded in memo files rather than in `Main.tex`.
- The analysis window is now restricted to `2015-01-01` through `2024-12-31`.

## Fixed Taxonomy

### Detailed labels (10)

1. `意识形态与宣传教育`
2. `时政与领导活动`
3. `公共服务信息`
4. `社会保障与公共福利`
5. `应急管理与风险沟通`
6. `政策与政务公开`
7. `社会治理与执法通报`
8. `群众动员与社会参与`
9. `经济与发展建设`
10. `城市形象与文化活动`

### Higher-order families

- `hard_propaganda`
  - `意识形态与宣传教育`
  - `时政与领导活动`
- `public_service`
  - `公共服务信息`
  - `社会保障与公共福利`
  - `应急管理与风险沟通`
- `state_governance`
  - `政策与政务公开`
  - `社会治理与执法通报`
  - `群众动员与社会参与`
- `soft_propaganda`
  - `经济与发展建设`
  - `城市形象与文化活动`

## Existing Raw Assets

The raw source file is `data/articles.csv`.

Available fields include:

- post identifiers: `id`, `task_id`
- location fields: `province`, `city`
- account field: `public_account_name`
- text metadata: `title`
- existing classifier outputs: `category`, `reason`, `keywords`, `confidence`
- timing fields: `YEAR`, `DATE`
- engagement fields: `read_num`, `like_num`, `share_num`, `look_num`, `comment_count`, `collect_num`, `reward_count`

The old coarse-classification prompt is stored in `code/提示词20251104.txt`.

Important data limitation:

- `articles.csv` does **not** currently include full article body text.
- The available text-side fields are `title`, `category`, `reason`, and `keywords`.
- Unless another text archive is recovered later, discovery sampling, human annotation preparation, and LLM prompt refinement must be designed around those available fields.

## Old Prompt Audit

The 2025-11-04 prompt treated the task as open-ended category induction rather than frozen-taxonomy assignment.

Key features:

- It explicitly encouraged free creation of category names.
- It treated `纪检监察与党风廉政` as a separate category.
- It requested JSON output with `category`, `confidence`, `keywords`, and `reason`.
- It did not enforce the final ten-label taxonomy now required for the project.

Implication:

- The existing `category` field is useful as a rough first-pass label.
- It cannot be used directly as the final analysis variable without a second-round cleaning and consolidation step.

## 2015-2024 Raw Data Audit

Within the restricted window `2015-01-01` to `2024-12-31`:

- rows: `4,207,072`
- years covered: `2015-2024`
- unique public accounts: `329`
- unique provinces: `31`
- unique cities: `329`
- unique raw category strings: `862`

Coverage of auxiliary classification fields is effectively complete:

- missing `reason` share: `0.000001`
- missing `keywords` share: `0.000008`
- missing `confidence` share: `0.000000`

Year counts:

- `2015`: `170,093`
- `2016`: `240,304`
- `2017`: `284,971`
- `2018`: `337,013`
- `2019`: `360,241`
- `2020`: `536,351`
- `2021`: `535,247`
- `2022`: `608,907`
- `2023`: `576,132`
- `2024`: `557,813`

## Current Problem

The current cleaned dataset has **not** yet been collapsed to the frozen ten-label taxonomy.

Diagnostic check before rebuilding:

- current cleaned dataset unique `category` values: `868`
- canonical labels present: `10`
- non-canonical category values still present: `858`

Implication:

- The previous normalization step only handled a small subset of variants.
- A full second-round cleaning pass is still needed.

## High-Level Cleaning Strategy

The next cleaning pass should be row-level rather than raw-label-only.

Reason:

- many rare labels are mixtures, variants, or malformed strings
- ambiguous generic values such as `其他`, `未知`, `无`, and `未分类` exist
- the existing `reason` and `keywords` fields provide additional signal that can resolve cases not recoverable from `category` alone

Planned inputs for the cleaner:

- `category`
- `reason`
- `keywords`
- optionally `title` for residual tie-breaking

Planned outputs:

- `primary_label_10`
- `family_label_4`
- preserved `category_raw`
- preserved old `reason`, `keywords`, `confidence`

## Immediate Next Tasks

1. Build a deterministic taxonomy-cleaning function for `2015-2024`.
2. Verify that the final cleaned analysis variable contains only the ten fixed detailed labels.
3. Generate a `400`-post discovery sample in two rounds:
   - `Round 1 = 200`: time, region, account type, high-frequency label strata
   - `Round 2 = 200`: boundary cases, long-tail labels, mixed posts
4. Write boundary rules, primary-label rules, and optional secondary flags in memo form.
5. Draw a `2,000`-post city-size-weighted annotation sample for later human coding.

## Cleaning Update

Status: completed for the `2015-2024` analysis window.

The cleaned dataset has been rebuilt and now collapses all rows into the frozen ten-label taxonomy.

Final cleaned dataset summary:

- rows: `4,207,072`
- date range: `2015-01-01` to `2024-12-31`
- unique detailed labels after cleaning: `10`
- unique higher-order families after cleaning: `4`

Detailed-label counts after cleaning:

- `经济与发展建设`: `641,160`
- `应急管理与风险沟通`: `544,084`
- `时政与领导活动`: `530,382`
- `公共服务信息`: `524,185`
- `城市形象与文化活动`: `489,484`
- `意识形态与宣传教育`: `470,939`
- `政策与政务公开`: `432,057`
- `社会保障与公共福利`: `274,601`
- `社会治理与执法通报`: `203,340`
- `群众动员与社会参与`: `96,840`

Family counts after cleaning:

- `public_service`: `1,342,870`
- `soft_propaganda`: `1,130,644`
- `hard_propaganda`: `1,001,321`
- `state_governance`: `732,237`

Number of distinct raw labels folded into each cleaned detailed label:

- `社会保障与公共福利`: `251`
- `公共服务信息`: `151`
- `经济与发展建设`: `113`
- `社会治理与执法通报`: `90`
- `城市形象与文化活动`: `81`
- `政策与政务公开`: `55`
- `应急管理与风险沟通`: `44`
- `群众动员与社会参与`: `41`
- `意识形态与宣传教育`: `20`
- `时政与领导活动`: `20`

Illustrative high-frequency remappings:

- `纪检监察与党风廉政 -> 意识形态与宣传教育`
- `公众服务信息 -> 公共服务信息`
- `教育医疗养老 -> 社会保障与公共福利`
- `文化活动与城市形象 -> 城市形象与文化活动`
- `人事任免与干部公示 -> 时政与领导活动`
- `人事任免 -> 时政与领导活动`
- `思想意识与宣传教育 -> 意识形态与宣传教育`
- `宣传教育 -> 意识形态与宣传教育`

Generic low-information labels are now resolved at row level using `reason`, `keywords`, and `title` rather than by raw-label string alone. This affects labels such as:

- `其他`
- `未知`
- `无`
- `未分类`
- `不适用`

## Discovery Sample

Status: completed.

Saved file:

- `data/discovery_sample_400.rds`

Design:

- `Round 1 = 200`
  - fully crossed by `5` time bins and `10` cleaned categories
  - each time-bin-by-category cell contributes `4` posts
- `Round 2 = 200`
  - `50` generic or low-information raw-label cases
  - `50` low-confidence existing-label cases
  - `50` long-tail raw-label cases
  - `50` mixed-domain raw-label cases

Observed sample structure:

- total posts: `400`
- round split: `200 / 200`
- stored fields include:
  - `title`
  - `province`
  - `city`
  - `public_account_name`
  - `publish_date`
  - `category_raw`
  - `reason`
  - `keywords`
  - `confidence`
  - cleaned `category`
  - cleaned `content_family`
  - discovery design fields `discovery_round` and `discovery_stratum`

Interpretation:

- `Round 1` is the structured discovery layer used to stabilize the ten-label taxonomy across time and account contexts.
- `Round 2` is the boundary-testing layer used to identify ambiguous, sparse, or mixed raw labels and refine inclusion/exclusion rules.

## Annotation Sample

Status: completed.

Saved files:

- `data/annotation_sample_2000_internal.rds`
- `data/annotation_sample_2000_blind.rds`

Design:

- city-size-weighted sampling based on the cleaned `2015-2024` corpus
- one minimum draw per city, then integer quota adjustment to an exact total of `2,000`

Observed sample structure:

- total posts: `2,000`
- covered cities: `329`
- per-city quota summary:
  - min: `1`
  - first quartile: `2`
  - median: `5`
  - mean: `6.079`
  - third quartile: `9`
  - max: `25`

Detailed-label composition in the internal sample:

- `经济与发展建设`: `305`
- `城市形象与文化活动`: `264`
- `时政与领导活动`: `256`
- `公共服务信息`: `246`
- `应急管理与风险沟通`: `240`
- `意识形态与宣传教育`: `204`
- `政策与政务公开`: `192`
- `社会保障与公共福利`: `148`
- `社会治理与执法通报`: `97`
- `群众动员与社会参与`: `48`

Family composition in the internal sample:

- `public_service`: `634`
- `soft_propaganda`: `569`
- `hard_propaganda`: `460`
- `state_governance`: `337`

The blind file contains empty annotation fields for later coding:

- `primary_label_ra1`
- `family_label_ra1`
- `primary_label_ra2`
- `family_label_ra2`
- `adjudicated_primary_label`
- `adjudicated_family_label`
- `annotation_notes`
