#' Create Evaluation Table for Tuned Model Only
#'
#' This function generates a summary table of performance metrics
#' (RMSE, MAE, Min/Max Error, and R²) for a tuned XGBoost model,
#' using the `gt` package for elegant display.
#'
#' @param tuned_results A list containing evaluation metrics for the tuned model.
#'        Expected elements: RMSE, MAE, MinError, MaxError, R2.
#' @param percentage_overlap_tuned (Not shown in this table, but can be included separately if needed.)
#' @param model_name A string used for the table title (e.g., "Pain").
#'
#' @return A gt table displaying the performance of the tuned model.
#'
create_model_eval_table_bootstrap <- function(baseline_results, tuned_results,
                                              percentage_overlap_baseline,
                                              percentage_overlap_tuned,
                                              model_name = "Pain") {
  data.frame(
    Metric = c(
      "RMSE (mean ± SD)",
      "MAE (mean ± SD)",
      "MAE (median, IQR)",
      "80%-PI coverage"
    ),
    Default_Model = c(
      sprintf("%.1f ± %.2f", baseline_results$RMSE_mean, baseline_results$RMSE_sd),
      sprintf("%.1f ± %.2f", baseline_results$MAE_mean, baseline_results$MAE_sd),
      sprintf("%.1f (%.1f – %.1f)", baseline_results$MAE_median,
              baseline_results$MAE_IQR[1], baseline_results$MAE_IQR[2]),
      sprintf("%.1f%%", percentage_overlap_baseline)
    ),
    Tuned_Model = c(
      sprintf("%.1f ± %.2f", tuned_results$RMSE_mean, tuned_results$RMSE_sd),
      sprintf("%.1f ± %.2f", tuned_results$MAE_mean, tuned_results$MAE_sd),
      sprintf("%.1f (%.1f – %.1f)", tuned_results$MAE_median,
              tuned_results$MAE_IQR[1], tuned_results$MAE_IQR[2]),
      sprintf("%.1f%%", percentage_overlap_tuned)
    )
  ) %>%
    gt() %>%
    tab_header(
      title = paste(model_name, "Prediction — Test Set Evaluation"),
      subtitle = "Performance Comparison: Default vs. Tuned Model (Bootstrap-based)"
    ) %>%
    cols_label(
      Metric = "Metric",
      Default_Model = "Default Model",
      Tuned_Model = "Tuned Model"
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(everything())
    ) %>%
    tab_options(
      table.font.size = 12,
      heading.title.font.size = 14,
      heading.subtitle.font.size = 12
    ) %>%
    tab_source_note(
      source_note = "RMSE = Root Mean Squared Error, MAE = Mean Absolute Error, IQR = Interquartile Range. Tuned model metrics are based on 1000 bootstrap resamples."
    )
}

#' Create Bootstrap Evaluation Table for Tuned Models
#'
#' Builds a \code{gt} table summarizing bootstrap-based performance
#' metrics for one or two tuned models.  Optionally includes metrics
#' from the corresponding best final model.
#'
#' @param tuned_results  List of bootstrap metrics for the first model.
#' @param percentage_overlap_tuned  Coverage percentage for the first model.
#' @param model_name  Display name for the first model.
#' @param tuned_results_2  (Optional) Metrics for a second model.
#' @param model_name_2  Display name for the second model.
#' @param best_model_metrics  (Optional) Metrics of the final trained model.
#' @param best_model_metrics_2  (Optional) Metrics of the second final model.
#' @return A \code{gt} table comparing bootstrap performance.
create_tuned_model_eval_table_bootstrap <- function(tuned_results,
                                                    percentage_overlap_tuned = NULL,
                                                    model_name = "Pain",
                                                    tuned_results_2 = NULL,
                                                    model_name_2 = "NULL",
                                                    best_model_metrics = NULL,
                                                    best_model_metrics_2 = NULL) {
  extract_metrics <- function(res, best = NULL) {
    c(
      sprintf("%.1f ± %.2f", res$RMSE_mean, res$RMSE_sd),
      sprintf("%.1f ± %.2f", res$MAE_mean, res$MAE_sd),
      if (!is.null(best)) sprintf("%.1f", best$RMSE) else NA,
      if (!is.null(best)) sprintf("%.1f", best$MAE) else NA
    )
  }
  
  metrics <- c(
    "RMSE (mean ± SD)", 
    "MAE (mean ± SD)",
    "Best Model RMSE", 
    "Best Model MAE"
  )
  
  df <- data.frame(Metric = metrics, stringsAsFactors = FALSE)
  df[[model_name]] <- extract_metrics(tuned_results, best_model_metrics)
  
  if (!is.null(tuned_results_2)) {
    df[[model_name_2]] <- extract_metrics(tuned_results_2, best_model_metrics_2)
    title <- "Prediction — Test Set Evaluation (Comparison)"
  } else {
    title <- paste(model_name, "Prediction — Test Set Evaluation")
  }
  
  df %>%
    gt() %>%
    tab_header(
      title = title,
      subtitle = "Bootstrap-based Performance Metrics + Best Model Evaluation"
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(everything())
    ) %>%
    tab_options(
      table.font.size = 12,
      heading.title.font.size = 14,
      heading.subtitle.font.size = 12
    ) %>%
    tab_source_note(
      source_note = "RMSE = Root Mean Squared Error, MAE = Mean Absolute Error. Bootstrapped results based on 1000 iterations. Best model results are from the final trained XGBoost model."
    )
}


#' Compute SHAP Explanations for XGBoost Model (Local & Global)
#'
#' This function computes both local and global SHAP (SHapley Additive exPlanations) values
#' for a tuned XGBoost regression model. It uses the `iml` package to calculate SHAP values
#' for a single observation (local explanation), and the `SHAPforxgboost` package for
#' generating a global summary plot of feature importance across all observations.
#'
#' @param train_data A data.frame or data.table containing the training data used to fit the model.
#'        Should include both predictors and the outcome variable.
#' @param model A trained XGBoost model object (e.g., `tuned_xgb_model_pain`).
#' @param outcome A string specifying the name of the outcome column in `train_data`. Default is `"pain_1y"`.
#' @param exclude_cols A character vector of column names to exclude from the predictors (e.g., patient IDs). Default is `c("fid")`.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{predictor}{An `iml::Predictor` object wrapping the XGBoost model.}
#'   \item{shapley}{A `Shapley` object containing local SHAP values for the first observation.}
#'   \item{shap_values}{A list of global SHAP values and mean absolute SHAP scores from `SHAPforxgboost`.}
#'   \item{shap_long}{Long-format SHAP data prepared for plotting.}
#' }
#'
#' @details
#' This function performs the following steps:
#' \enumerate{
#'   \item Filters complete cases from the dataset.
#'   \item Converts character columns to factors and one-hot encodes all predictors.
#'   \item Computes SHAP values for the first patient using the `iml` package.
#'   \item Computes SHAP values for all patients using `SHAPforxgboost`, and generates a summary plot.
#' }
#' 
compute_shap_explanations <- function(train_data, model, outcome = "pain_1y", exclude_cols = c("fid")) {
  # Prepare the data
  dt <- as.data.table(train_data)
  X_vars <- setdiff(names(dt), c(exclude_cols, outcome))
  
  cat("Original variables:", length(X_vars), "\n")
  
  # Check missing values in remaining columns
  missing_counts <- dt[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = X_vars]
  cat("Missing values in remaining columns:\n")
  print(missing_counts)
  
  # Filter to complete cases for remaining variables
  dt <- dt[!is.na(get(outcome))]  # Ensure outcome is not missing
  complete_case_rows <- complete.cases(dt[, ..X_vars])
  
  cat("Complete cases available:", sum(complete_case_rows), "out of", nrow(dt), "\n")
  
  if (sum(complete_case_rows) == 0) {
    cat("ERROR: No complete cases found!\n")
    return(NULL)
  }
  
  # Filter to complete cases
  dt <- dt[complete_case_rows]
  
  cat("Final data dimensions:", dim(dt), "\n")
  
  # Convert character columns to factor
  char_cols <- names(Filter(is.character, dt[, ..X_vars]))
  if (length(char_cols) > 0) {
    dt[, (char_cols) := lapply(.SD, as.factor), .SDcols = char_cols]
  }
  
  # One-hot encode predictors
  dataX <- model.matrix(~ . - 1, data = dt[, ..X_vars])
  label_y <- dt[[outcome]]
  
  cat("Model matrix dimensions:", dim(dataX), "\n")
  
  # --- Local explanation with `iml` for first patient
  predict_function <- function(model, newdata) {
    predict(model, as.matrix(newdata))
  }
  
  predictor <- Predictor$new(
    model = model,
    data = as.data.frame(dataX),
    y = label_y,
    predict.function = predict_function
  )
  
  shapley <- Shapley$new(
    predictor,
    x.interest = as.data.frame(dataX[1, , drop = FALSE])
  )
  
  cat("Local SHAP values (1st patient):\n")
  print(shapley$results)
  
  # --- Global explanation with SHAPforxgboost
  cat("Calculating global SHAP summary...\n")
  shap_values <- shap.values(xgb_model = model, X_train = dataX)
  shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = dataX)
  
  # Return SHAP objects
  return(list(
    predictor = predictor,
    shapley = shapley,
    shap_values = shap_values,
    shap_long = shap_long
  ))
}



#' Analyze Counterfactual Scenarios
#'
#' Generates alternative predictions for a single patient under
#' predefined intervention scenarios (e.g., improved ROM or grip
#' strength).  Works for pain or function outcomes.
#'
#' @param model        Trained XGBoost model.
#' @param patient_data One-row data frame for the patient.
#' @param train_data   Training data used for imputing missing values.
analyze_counterfactuals <- function(model, patient_data, train_data) {
  
  target_var <- "pain_1y"
  
  # Prepare patient data
  patient_features <- patient_data %>% select(-fid, -all_of(target_var))
  if ("sgrxbilddauer" %in% names(patient_features)) {
    patient_features <- patient_features %>% select(-sgrxbilddauer)
  }
  
  # Handle missing values
  for (col in names(patient_features)) {
    if (is.na(patient_features[[col]])) {
      if (is.numeric(patient_features[[col]])) {
        patient_features[[col]] <- median(train_data[[col]], na.rm = TRUE)
      }
    }
  }
  
  # Baseline prediction
  baseline_pred <- predict(model, as.matrix(patient_features))
  
  # Define scenarios based on outcome type
  scenarios <- list(
    "Improve ROM IP +10°" = function(data) {
      data$romip <- pmin(data$romip + 10, 90); data  # Cap at reasonable max
    },
    "Improve Grip +5kg" = function(data) {
      data$gripstr <- data$gripstr + 5; data
    },
    "Younger by 10 years" = function(data) {
      data$ageexam <- pmax(data$ageexam - 10, 40); data  # Don't go below reasonable minimum
    },
    "No Rest Pain" = function(data) {
      data$painrestnrs <- 0; data
    },
    "All Combined" = function(data) {
      data$romip <- pmin(data$romip + 10, 90)
      data$gripstr <- data$gripstr + 5
      data$ageexam <- pmax(data$ageexam - 10, 40)
      data$painrestnrs <- 0
      data
    }
  )
  
  # Calculate predictions for each scenario
  scenario_predictions <- sapply(scenarios, function(scenario_func) {
    modified_data <- scenario_func(patient_features)
    predict(model, as.matrix(modified_data))
  })
  
  # Create results dataframe
  results <- data.frame(
    Scenario = c("Baseline", names(scenarios)),
    Predicted_Outcome = c(baseline_pred, scenario_predictions)
  )
  
  results$Change_from_Baseline <- results$Predicted_Outcome - baseline_pred
  
  # Add outcome-specific column name
  names(results)[2] <- "Predicted_Pain"
  
  return(results)
}
