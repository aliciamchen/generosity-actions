# Expectations for sequences of generous acts

This project examines how social relationships influence people's expectations about sequences of generosity. The research tests how these expectations vary across different relationship types (equal vs. hierarchical) and explores the underlying mechanisms driving these patterns.

Preprint: TBD

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
cd analysis

Rscript study_1.R
Rscript study_2.R
Rscript study_3.R
Rscript study_4.R
Rscript study_5.R
Rscript study_6.R
```

2. **Generate figures**:
```bash
Rscript -e "rmarkdown::render('figures.Rmd')"
```

## Data structure

See [codebook](data/README.md). 

## Contact

Email: aliciach@mit.edu

