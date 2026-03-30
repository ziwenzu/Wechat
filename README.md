# WeChat Media Project

This repository contains code and writing for a WeChat project on how citizens interact with Chinese local-government official accounts. The paper's core question is not simply whether propaganda ``works,'' but how platform design changes the political meaning of engagement.

The current manuscript is being organized as an APSR-style theory and identification paper. The central claim is that official WeChat accounts are mixed channels that bundle public service information with political messaging, and that citizens sort their behavior across different levels of expressive risk.

## Key Working Decisions

- Current data status: the working dataset is prepared for instructional use by the instructor, who had full authority over its design and generation. It is intended for teaching and assessment rather than for publication or academic research.
- Interface interpretation: the sharp decline in `like` from `2019-01` onward is consistent with the `2018-12-21` WeChat redesign that replaced article-bottom `like` with `好看` / later `在看`, not just an obvious downstream coding error.
- Causal interpretation: `2018` should be treated mainly as a rebundling or measurement-regime change because the meaning of the button changes; `2020-06-29` to `2020-07-01` is the cleaner test because `like` returns alongside `在看` and `share`.
- Classification rule: `hard_propaganda`, `soft_propaganda`, and `public_service` are not meant to be an exhaustive topic ontology. They are a theory-facing taxonomy organized by communicative purpose and expressive risk.
- Auxiliary classes: `state_governance` should remain an auxiliary category, while `other` / `uncoded` captures residual or low-confidence cases.
- Measurement workflow: do not rely on pure zero-shot three-way classification. Use finer-grained labels first, then map them into the three main analytic families.
- Validation strategy: use explicit anchors in titles, sections, or source cues such as `学习`, `宣传`, `专栏`, `通知`, `办事`, or department labels as high-precision seed cases; then build a human-coded gold-standard sample; then use the LLM for full-corpus assignment; then use topic modeling only as a residual-topic audit.
- Boundary handling: mixed posts are expected. Each post should receive one primary label based on its main communicative purpose, with optional secondary flags for leadership presence, service content, mobilization, emergency context, and related overlaps.
- Output rule: research tables should be exported as LaTeX `.tex` files only, and research figures should be exported as PDF files only.

## Core Research Question

How do changes in the visibility of engagement reshape citizens' willingness to leave politically legible traces on official WeChat content under authoritarian rule?

## Main Argument

The project treats WeChat as a platform of layered expression rather than a generic social-media site.

- `read` is low-risk consumption
- `like` is low-visibility approval
- `zaikan` / `haokan` is socially legible endorsement
- `share` is the highest-visibility form of amplification

This implies that engagement metrics on official accounts should not be read straightforwardly as support for the regime. They are strategic behavioral traces shaped by both content type and interface design.

## Platform Timeline

Three interface changes organize the theory, but the main causal design centers on 2018 and 2020.

- `2017-05-17`: `看一看` expands discovery and social recommendation. This is mainly a discovery and reach shock, not the cleanest expressive-cost discontinuity.
- `2018-12-21`: the bottom `like` button is replaced by `好看` / later `在看`, rebundling approval with recommendation and making endorsement more socially legible.
- `2020-06-29` to `2020-07-01`: the `like` button returns alongside `zaikan` and `share`, restoring a lower-visibility approval channel inside a layered menu of expression.

## Theoretical Framework

The project combines three strands of theory.

- Official accounts as mixed governance infrastructure: WeChat official accounts are not only propaganda outlets. They also function as service-delivery and information-routing tools.
- Preference falsification and self-censorship: visible expression is a noisy indicator of genuine belief under authoritarian conditions.
- Platform governance and affordances: interface design changes the cost, visibility, and inferred meaning of a click.

The key theoretical move is to extend preference falsification from public speech to platform-mediated behavior. Citizens may consume official content, approve of it privately, and still avoid socially legible endorsement.

## Main Hypotheses

- `H1 (Attention and Utility)`: public service content should attract more routine consumption than either hard or soft propaganda because it offers immediate utility and lower expressive stakes.
- `H2 (Rebundling Visible Approval)`: when the 2018 redesign bundles approval with public recommendation, low-visibility like responses should collapse and observed reactions should be reallocated into more socially legible channels.
- `H3 (Private Substitution)`: when low-visibility likes return in 2020, approval should shift back toward likes rather than rising uniformly across all public-facing reactions.
- `H4 (Political-Risk Gradient)`: these reallocations should be strongest for hard propaganda, then soft propaganda, and weakest for routine public-service content.

## Measurement and Classification Strategy

The classification strategy is theory-driven rather than purely unsupervised. The main families are justified by communicative purpose and expressive risk, not by an attempt to recover the one true topic structure of WeChat posts.

### Main analytic families

- `hard_propaganda`
- `soft_propaganda`
- `public_service`
- `state_governance` as an auxiliary class
- `other` / `uncoded` as residual categories

### Current coding logic

- `hard_propaganda`: ideology, leadership activity, discipline, enforcement, explicitly political mobilization
- `soft_propaganda`: development, city branding, culture, tourism, positive-performance narratives
- `public_service`: service access, welfare, health, weather, traffic, emergency information, instrumental everyday guidance
- `state_governance`: policy notices, routine administrative disclosure, governance updates that are neither strongly service-oriented nor clearly propagandistic

Posts should be assigned according to their primary communicative purpose rather than surface keywords alone. Mixed posts are expected, so the preferred workflow is one primary label plus optional secondary flags.

### Planned workflow

- Start from finer-grained content labels, then map them into the three main theory-facing families used in the paper
- Use explicit anchors from titles, sections, and source cues to identify high-precision seed cases
- Build and validate against a human-coded gold-standard sample
- Use an LLM as a text coder on the full corpus, with deterministic settings and a frozen prompt
- Use `BERTopic` / `STM` plus manual review only as an appendix robustness and residual-topic audit, not as the source of the paper's main categories

## Empirical Design

### Data

- Universe of posts from `329` prefecture-level government WeChat public accounts
- Coverage from `2013-01-21` to `2025-01-06`
- Current full corpus size: `4,287,179` posts

### Main empirical components

- Descriptive analysis of content production and attention across time, space, and content families
- RDD centered on the `2018-12-21` and `2020-07-01` interface changes
- Heterogeneity by content risk, with hard propaganda as the highest-risk category

### Current interpretation of the interface shocks

- `2018` is best interpreted as a rebundling or expressive-reallocation shock because the meaning of the button itself changes
- the `2019` collapse in observed `like` is consistent with that redesign, while the rise in `look` / `public_signal` reflects the activation of a more socially legible reaction channel
- `2020` is the cleaner causal test because it restores a lower-visibility approval channel while keeping more public channels available

## Paper Structure

The manuscript is currently being organized around the following structure.

1. `Introduction`
2. `Theory and Hypotheses`
3. `Setting, Data, and Measurement`
4. `Descriptive Findings`
5. `Research Design`
6. `Causal Findings`
7. `Heterogeneity`
8. `Conclusion`

The LLM + human classification workflow and the topic-model robustness checks are intended to sit mainly in the appendix rather than compete with the main theoretical story.

## Repository Structure

- [`code/`](code): all analysis code in R
- [`figures/`](figures): generated figures saved as PDF files
- [`tables/`](tables): generated tables saved as LaTeX `.tex` files
- [`data/`](data): local-only data location, ignored by git
- [`writing/`](writing): manuscript draft, bibliography, and literature-review materials

Supporting notes about the code workflow live in [`code/README.md`](code/README.md).

## Data Policy

The repository does **not** include the underlying WeChat post data or all derived outputs generated from those data.

Ignored items include:

- raw and processed files in `data/`
- generated tables in `tables/`
- generated figures in `figures/`

This keeps the repository lightweight and avoids uploading large or sensitive research data.

## Output Format Rule

For research workflows in this repository:

- tables should be exported as LaTeX `.tex` files only
- figures should be exported as PDF files only

Avoid using `.txt`, `.csv`, `.png`, or other presentation-output formats for final research tables and figures.

## Analysis Workflow

Run from the repository root:

```bash
Rscript code/01_build_dataset.R
Rscript code/02_descriptives.R
Rscript code/03_rdd.R
```

The scripts do the following:

- `01_build_dataset.R`: reads `data/articles.csv`, parses dates, maps content categories, and creates an R-ready dataset
- `02_descriptives.R`: produces descriptive summaries and yearly plots for the main content families
- `03_rdd.R`: runs baseline daily-aggregated RDDs around the 2018 and 2020 interface changes, plus simple robustness and continuity checks

## Writing Workflow

Compile the manuscript from the repository root with:

```bash
./build_pdf.sh
```

You can also target a specific TeX file:

```bash
./build_pdf.sh writing/Main.tex
./build_pdf.sh Main.tex
```

## Current Status

- The manuscript has been reorganized into an APSR-style flow
- The baseline code now uses theory-facing content families
- Full-sample descriptives and baseline RDD outputs have been regenerated
- The appendix still needs the finalized frozen prompt, validation metrics, and topic-model audit outputs

## Next Priorities

- Finalize the theory-driven codebook for WeChat post classification
- Replace provisional mapping logic with the final LLM-coded labels
- Fill in the appendix materials for prompt design, human validation, and topic-model robustness
- Move from daily aggregates toward richer account-day or post-level specifications if needed
