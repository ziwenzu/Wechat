# Annotation Validation Memo

Date: 2026-03-29

## Scope

This memo records the completed annotation-validation step for the `2,000`-post title-based sample.

The annotation file used for validation is:

- `data/annotation_sample_2000_blind.rds`

This file now contains:

- `primary_label_ra1`
- `family_label_ra1`
- `primary_label_ra2`
- `family_label_ra2`
- `adjudicated_primary_label`
- `adjudicated_family_label`
- `llm_primary_label`
- `llm_family_label`
- `adjudication_rule`

## Working Assumption

The coding task is title-based rather than full-text based.

The validation sample therefore evaluates how well the frozen taxonomy can be assigned from:

- post title
- account and location context
- supporting metadata used during adjudication

## Inter-Coder Reliability

The two RAs completed independent coding for all `2,000` posts.

Observed reliability:

- primary exact agreement: `0.8875`
- family exact agreement: `0.9130`
- primary Cohen's kappa: `0.8726`
- family Cohen's kappa: `0.8811`
- adjudication rate: `0.1125`

Interpretation:

- agreement is high at both levels
- family-level agreement is higher than detailed-label agreement, as expected
- only `11.25%` of cases required adjudication

## Disagreement Structure

Number of primary-label disagreements:

- `225`

Largest disagreement pairs:

- `时政与领导活动` vs `经济与发展建设`: `52`
- `时政与领导活动` vs `意识形态与宣传教育`: `20`
- `时政与领导活动` vs `政策与政务公开`: `12`
- `公共服务信息` vs `应急管理与风险沟通`: `12`
- `时政与领导活动` vs `社会治理与执法通报`: `9`
- `政策与政务公开` vs `经济与发展建设`: `9`

Substantive interpretation:

- the main ambiguities are not random
- they occur where the codebook predicts boundary pressure:
  - leader event vs policy substance
  - leader event vs development framing
  - routine service vs emergency communication
  - policy release vs governance/enforcement

## Adjudication

Adjudication was completed for all `225` disagreement cases and written back into:

- `adjudicated_primary_label`
- `adjudicated_family_label`

The adjudication workflow combined:

- automatic acceptance of coder agreement
- rule-based adjudication for disagreements using the frozen codebook
- title and supporting metadata as tie-breaking evidence

Most common adjudication routes:

- `coder_agreement`: `1,775`
- `raw_prior_break_tie`: `120`
- `emergency_over_service`: `12`
- `development_over_leader`: `10`
- `leader_over_development`: `8`
- `leader_meeting_default`: `7`
- `service_over_non_welfare`: `7`

## LLM Validation Against the Adjudicated Gold Standard

The LLM prediction used for validation is the cleaned corpus label joined back by `id`.

### Detailed 10-label results

- accuracy: `0.8150`
- macro F1: `0.7955`
- evaluation sample: `2,000`

Class-wise highlights:

- strongest F1:
  - `城市形象与文化活动`: `0.9125`
  - `经济与发展建设`: `0.8541`
  - `时政与领导活动`: `0.8437`
  - `应急管理与风险沟通`: `0.8403`
- weaker classes:
  - `群众动员与社会参与`: `0.6598`
  - `政策与政务公开`: `0.7333`
  - `社会保障与公共福利`: `0.7335`
  - `公共服务信息`: `0.7424`

Interpretation:

- the LLM performs reasonably well at the detailed level
- the most difficult categories are exactly those with narrower support or more porous boundaries

### Higher-order family results

- accuracy: `0.8755`
- macro F1: `0.8642`
- evaluation sample: `2,000`

Interpretation:

- the family-level mapping is materially more stable than the detailed-label mapping
- this supports using higher-order families as the main theory-facing categories in the paper

## File Outputs

Generated validation tables:

- `tables/annotation_reliability_summary.tex`
- `tables/llm_vs_gold_detailed_summary.tex`
- `tables/llm_vs_gold_detailed_by_class.tex`
- `tables/llm_vs_gold_detailed_confusion.tex`
- `tables/llm_vs_gold_family_summary.tex`
- `tables/llm_vs_gold_family_by_class.tex`
- `tables/llm_vs_gold_family_confusion.tex`

## Cleanup

Removed intermediate annotation-generation files:

- `code/ra1_annotation.R`
- `code/ra1_annotation_fix_fp.R`
- `code/ra1_annotation_refine.R`
- `code/ra2_annotation.R`
- `data/annotation_sample_2000_internal.rds`

The retained annotation product is:

- `data/annotation_sample_2000_blind.rds`
