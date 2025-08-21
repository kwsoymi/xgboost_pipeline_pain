# XGBoost ML Pipeline: Pain Model

This repository contains the code used to implement a XGBoost machine learning pipeline to predicting postoperative pain as described in the manuscript *"A Transparent, Uncertainty-Aware XGBoost Pipeline for Clinical Outcome Prediction: Case Study in Pain After Thumb Arthroplasty."*

The workflow is implemented with **R**; the **Quarto** notebook `xgboost_pipeline_pain.qmd` simply orchestrates the complete pipeline for documentation and reproducibility. All steps are standard R scripts that can be run without Quarto:

1.  load and clean registry-derived data
2.  apply inclusion/exclusion criteria and feature engineering
3.  tune hyperparameters for an XGBoost model
4.  evaluate performance and produce plots and tables

## Example Visuals

![`output/readme_image.png`](output/readme_image.png)

![`output/three_way_pain_error_rainfall.png`](output/three_way_pain_error_rainfall.png)

## Prerequisites

-   R \>= 4.3
-   Optional: [Quarto](https://quarto.org/docs/get-started/) or [RStudio](https://posit.co/download/rstudio/) for rendering the notebook. The pipeline scripts can be sourced in any R environment.

## Project structure

```         
├── xgboost_pipeline.qmd          	# Main Quarto notebook
├── data/                               # Processed data and model artifacts
├── helpers/                            # Utility functions
├── models/                             # Training and evaluation scripts
├── pipeline/                           # Data preparation scripts
└── README.md                           # Project overview
```

## Key entry points

-   **Quarto notebook**: `xgboost_pipeline.qmd` orchestrates the full workflow.
-   **Data pipeline**: scripts in [`pipeline/`](pipeline) load raw data, apply inclusion/exclusion criteria and create the `df_transformed` dataset.
-   **Model utilities**: functions in [`models/`](models) tune, train and evaluate XGBoost models. Key functions include `tune_xgb_stepwise()` and `evaluate_xgb_model()`.
-   **Helper functions**: reusable helpers live in [`helpers/`](helpers) and are loaded by the notebook as needed.

## Data requirements

The notebook expects a de‑identified dataset with all predictors used in the manuscript. No patient-level data are included in this repository. After cleaning, exclusions and transformations, the analysis starts from a data frame called `df_transformed` (line 143 in `xgboost_pipeline.qmd`).

An empty template, [`data/df_transformed_template.csv`](data/df_transformed_template.csv), illustrates the required column names. Populate this template with your own observations before running the pipeline. For variable definitions, types, and coding conventions, refer to Supplement Table 1 of the manuscript.

Encode missing values as `NA`; the pipeline and XGBoost handle them internally.

## Environment setup

This project uses [`renv`](https://rstudio.github.io/renv/) for reproducible environments.

``` r
install.packages("renv")
renv::restore()
```

## Running the analysis

### Using Quarto
Render the notebook in RStudio or from the command line:

``` bash
quarto render xgboost_pipeline.qmd -P run_pain_tuning=false
```

Set `run_pain_tuning=true` to enable hyperparameter tuning. Outputs are written to the `data/` and `output/` directories.

### Using base R

All components are regular R scripts. To run the workflow without Quarto, open an R session and source the scripts directly, for example:

``` r
source("pipeline/load_data.R")
source("pipeline/inclusion_exclusion.R")
source("pipeline/data_preprocessing.R")
source("models/comprehensive_evaluation.R")
```

This produces the same models and predictions as the Quarto notebook.

## Citation

Please cite this work as described in [`CITATION.cff`](./CITATION.cff).

## Project status

Submitted - Under review. To do: add link to scientific publication.

## License

This project is released under the MIT License; see [`LICENSE`](LICENSE) for details.
