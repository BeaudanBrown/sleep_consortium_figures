# SDC Figures Analysis Pipeline

A meta-analysis pipeline that processes sleep-related data and generates forest plots for cognitive and dementia outcomes.

## Overview

This pipeline uses the `targets` R package to orchestrate a reproducible analysis workflow. It:
- Loads cognition and dementia analysis data from Excel files
- Performs meta-analyses on sleep phenotypes (9 clusters) grouped by cognitive domains
- Generates forest plots and summary tables for both association and interaction analyses
- Outputs results as PNGs, PDFs, and CSVs

## Running the Pipeline

### Prerequisites
- R with packages: `targets`, `tarchetypes`, `crew`, `data.table`, `readr`, `ggplot2`, `forestplot`, `readxl`, `patchwork`, `meta`
- Input data files specified in `.env` (via `DATA_DIR2` environment variable)

### Execute

```r
targets::tar_make()
```
