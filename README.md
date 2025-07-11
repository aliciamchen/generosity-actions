# Expectations for sequences of generous acts

This project examines how social relationships influence people's expectations about sequences of generosity. The research tests how these expectations vary across different relationship types (equal vs. hierarchical) and explores the underlying mechanisms driving these patterns.

Preprint: https://osf.io/preprints/psyarxiv/7j6k8_v1

## How to reproduce results

### Set up conda and R environments 

```bash
conda env create -f environment.yml
conda activate generosity-actions
```

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

Direct figure output is saved in `figures/`. The Illustrator-formatted figures for the paper are in `figures/PDF`. 


## Data structure

See [codebook](data/README.md). 

## Contact

Email: aliciach@mit.edu

