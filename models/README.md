# Model scripts

Scripts in this directory build and assess the XGBoost models used in the pain prediction pipeline.

- `xgb_tuning.R` – functions for stepwise and Bayesian hyperparameter tuning.
- `evaluate_xgb_model.R` – utilities for evaluating fitted models on validation data.
- `comprehensive_evaluation.R` – wrapper that trains a final model and generates summaries, plots and tables.
- `XGB_bootstrap_PI_importance.R` – bootstrap procedures for prediction intervals and feature importance.