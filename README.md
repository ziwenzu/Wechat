# WeChat Media Project

This repository contains code and writing for a research project on how Chinese local government WeChat official accounts interact with citizens. The project studies how users respond to different kinds of official posts, with particular attention to reading, liking, sharing, forwarding, and classification of posts into propaganda, public service, and other content.

The current workflow combines three parts:

- manuscript drafting in LaTeX
- legacy data processing and analysis in Stata
- a new R pipeline for transparent data preparation, descriptive analysis, and baseline discontinuity analysis

## Repository structure

- [`analysis/code/`](analysis/code): existing Stata scripts
- [`analysis/r/`](analysis/r): new R scripts for dataset construction, descriptives, and RDD
- [`writing/`](writing): manuscript draft and bibliography
- [`理论背景.md`](理论背景.md): theory notes and hypothesis sketches

## Data policy

The repository does **not** include the underlying WeChat post data or any derived output files generated from those data.

Ignored items include:

- raw and processed files in `analysis/data/`
- generated tables and figures in `analysis/outputs/`
- temporary files in `analysis/temp/`

This keeps the public repository lightweight and avoids uploading large or sensitive research data.

## R workflow

The R scripts were added to make the workflow easier to reproduce and extend.

Run from the repository root:

```bash
Rscript analysis/r/01_build_dataset.R
Rscript analysis/r/02_descriptives.R
Rscript analysis/r/03_rdd.R
```

The scripts do the following:

- `01_build_dataset.R`: reads the source article file, parses dates, maps content categories, and creates an R-ready dataset
- `02_descriptives.R`: produces descriptive summaries and quick plots by content family
- `03_rdd.R`: runs a baseline daily-aggregated regression discontinuity design around the 2017 and 2020 WeChat interface changes

There is also a more detailed note in [`analysis/r/README.md`](analysis/r/README.md).

## Stata workflow

The original Stata scripts remain in place:

- `clean.do`
- `analysis.do`
- `graph.do`

These are useful for comparison while the project transitions toward R.

## Next steps

Planned improvements include:

- replacing provisional rule-based categories with final LLM-based labels
- moving from baseline RDD plots to richer account-day or post-level specifications
- tightening the distinction between observed data processing and theory-driven modeling choices
