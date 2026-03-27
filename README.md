# WeChat Media Project

This repository contains code and writing for a research project on how Chinese local government WeChat official accounts interact with citizens. The project studies how users respond to different kinds of official posts, with particular attention to reading, liking, sharing, forwarding, and classification of posts into propaganda, public service, and other content.

The repository now uses a single R workflow for data preparation, descriptive analysis, and baseline discontinuity analysis.

## Repository structure

- [`code/`](code): all analysis code in R
- [`figures/`](figures): generated figures
- [`tables/`](tables): generated tables and text summaries
- [`data/`](data): local-only data location, ignored by git
- [`writing/`](writing): manuscript draft and bibliography
- [`theory_background.md`](theory_background.md): theory notes and hypothesis sketches

## Data policy

The repository does **not** include the underlying WeChat post data or any derived output files generated from those data.

Ignored items include:

- raw and processed files in `data/`
- generated tables in `tables/`
- generated figures in `figures/`

This keeps the public repository lightweight and avoids uploading large or sensitive research data.

## R workflow

Run from the repository root:

```bash
Rscript code/01_build_dataset.R
Rscript code/02_descriptives.R
Rscript code/03_rdd.R
```

The scripts do the following:

- `01_build_dataset.R`: reads the source article file, parses dates, maps content categories, and creates an R-ready dataset
- `02_descriptives.R`: produces descriptive summaries and saves plots as PDF files
- `03_rdd.R`: runs a baseline daily-aggregated regression discontinuity design around the 2017 and 2020 WeChat interface changes and saves figures as PDF files

There is also a short code note in [`code/README.md`](code/README.md).

## Writing workflow

Compile the manuscript from the repository root with:

```bash
./build_pdf.sh
```

By default this builds `writing/Main.tex` and cleans all LaTeX intermediate files afterward, including `.aux`, `.bcf`, `.log`, `.ptc`, and `_minted-*` directories. You can also pass a specific TeX file:

```bash
./build_pdf.sh writing/Main.tex
./build_pdf.sh Main.tex
```

## Next steps

Planned improvements include:

- replacing provisional rule-based categories with final LLM-based labels
- moving from baseline RDD plots to richer account-day or post-level specifications
- tightening the distinction between observed data processing and theory-driven modeling choices
