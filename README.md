# Expectations for sequences of generous acts

This repository contains the code, data, and experiment materials for "Expectations of reciprocal generosity are specific to equal relationships" (Open Mind, 2026). 

## Overview

This project examines how social relationships influence people's expectations about sequences of generosity. Across six studies, we test how these expectations vary across different relationship types (equal vs. hierarchical) and explore the underlying mechanisms driving these patterns.

## Preregistration links

  - Study 1: https://osf.io/fxwbh/
  - Study 2: https://osf.io/jvnca/
  - Study 3: https://osf.io/vt4kp
  - Study 4: https://osf.io/dfv24/
  - Study 5: https://osf.io/ag9v2/
  - Study 6: https://osf.io/zn6wh/

## Repository structure

- `analysis/` -- R scripts for statistical analysis and figure generation
- `data/` -- anonymized participant data. See [codebook](data/README.md).
- `experiments/` -- jsPsych browser experiments
- `figures/` -- figure outputs used in the paper. The final figures are in `figures/PDF/`.

## How to reproduce results

### Requirements

- R 4.5.2 (packages managed by `renv`)
- Python 3.13+ (managed by `uv`)

### Set up

**Python (using uv):**
```bash
uv sync
```

**R (using renv):**
```r
# Install renv if not already installed
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Restore packages from renv.lock
renv::restore()
```

### Run analyses

Run each study script and save its console output:

```bash
Rscript -e 'renv::run("analysis/scenario_validation.R")' > analysis/outputs/scenario_validation.txt 2>&1
Rscript -e 'renv::run("analysis/study_1.R")' > analysis/outputs/study_1.txt 2>&1
Rscript -e 'renv::run("analysis/study_2.R")' > analysis/outputs/study_2.txt 2>&1
Rscript -e 'renv::run("analysis/study_3.R")' > analysis/outputs/study_3.txt 2>&1
Rscript -e 'renv::run("analysis/study_4.R")' > analysis/outputs/study_4.txt 2>&1
Rscript -e 'renv::run("analysis/study_5.R")' > analysis/outputs/study_5.txt 2>&1
Rscript -e 'renv::run("analysis/study_6.R")' > analysis/outputs/study_6.txt 2>&1
Rscript -e 'renv::run("analysis/sensitivity_analysis.R")' > analysis/outputs/sensitivity.txt 2>&1
```

Each study script also writes a `.tex` file (e.g., `analysis/stats_study_1.tex`) containing LaTeX macros for every inline statistic reported in the paper. 

### Generate figures

```bash
Rscript -e "rmarkdown::render('analysis/figures.Rmd')"
```
