# Expectations for sequences of generous acts

This project examines how social relationships influence people's expectations about sequences of generosity. The research tests how these expectations vary across different relationship types (equal vs. hierarchical) and explores the underlying mechanisms driving these patterns.

<!-- Preprint: https://osf.io/preprints/psyarxiv/7j6k8_v1 -->

## This repository

This repository contains the code and data for the project. The code is organized into the following folders:

- `analysis/`: contains the code for the analyses
- `data/`: contains the data. See [codebook](data/README.md) for more details. 
- `experiments/`: contains the code for the experiments. 
- `figures/`: contains the figures. The direct figure outputs are in `figures/`. Illustrator-formatted figures for the paper are in `figures/PDF`. 

## How to reproduce results

### Set up Python and R environments

**Python (using uv):**
```bash
uv sync
```

**R:**
```r
# Install renv if not already installed
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Initialize renv (this will restore packages from renv.lock)
renv::restore()
```

### Run analyses

1. **Reproduce main results**:
```bash
Rscript -e 'renv::run("analysis/scenario_validation.R")' > analysis/outputs/scenario_validation.txt 2>&1
Rscript -e 'renv::run("analysis/study_1.R")' > analysis/outputs/study_1.txt 2>&1
Rscript -e 'renv::run("analysis/study_2.R")' > analysis/outputs/study_2.txt 2>&1
Rscript -e 'renv::run("analysis/study_3.R")' > analysis/outputs/study_3.txt 2>&1
Rscript -e 'renv::run("analysis/study_4.R")' > analysis/outputs/study_4.txt 2>&1
Rscript -e 'renv::run("analysis/study_5.R")' > analysis/outputs/study_5.txt 2>&1
Rscript -e 'renv::run("analysis/study_6.R")' > analysis/outputs/study_6.txt 2>&1
```

Text outputs are saved in `analysis/outputs/`.

2. **Generate figures**:
```bash
Rscript -e "rmarkdown::render('analysis/figures.Rmd')"
```

<!-- ## Contact

Email: aliciach@mit.edu -->

