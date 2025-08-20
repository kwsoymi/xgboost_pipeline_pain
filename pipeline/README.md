# Data pipeline

These scripts prepare the registry-derived dataset prior to modeling.

- `load_data.R` – import raw files and construct the initial data frame.
- `inclusion_exclusion.R` – apply study inclusion and exclusion criteria.
- `data_preprocessing.R` – feature engineering and transformation resulting in `df_transformed`.