#' Main function to run comprehensive three-way evaluation
#'
#' @param train_xgb_pain Training data for pain prediction
#' @param test_xgb_pain Test data for pain prediction  
#' @param retune_pain Whether to retune pain models
#' @param save_plots Whether to save plots to files
#' @param plot_dir Directory to save plots
#' @param winner_selection_pain Method to select as winner for pain model: "Auto", "Baseline", "Bayesian", or "Stepwise"
#' @param winner_selection_function Method to select as winner for function model: "Auto", "Baseline", "Bayesian", or "Stepwise"
#'
#' @return Comprehensive results list with all evaluations
run_comprehensive_evaluation <- function(train_xgb_pain, test_xgb_pain,
                                         retune_pain = FALSE,
                                         save_plots = TRUE,
                                         plot_dir = "output/",
                                         winner_selection_pain = "Auto") {
  
  cat("Starting Comprehensive Three-Way Model Evaluation\n")
  cat("===============================================\n\n")
  
  # Validate winner selection parameters
  valid_selections <- c("Auto", "Baseline", "Bayesian", "Stepwise")
  if (!winner_selection_pain %in% valid_selections) {
    stop("winner_selection_pain must be one of: ", paste(valid_selections, collapse = ", "))
  }

  cat(sprintf("Winner selection - Pain: %s", winner_selection_pain))
  
  # Create directories if they don't exist
  if (!dir.exists("data")) dir.create("data", recursive = TRUE)
  if (save_plots && !dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  # Define file paths for intermediate results
  baseline_results_file <- "data/baseline_models_results.rds"
  comparison_results_file <- "data/three_way_comparison_results.rds"
  learning_curves_file <- "data/learning_curves_results.rds"
  summary_stats_file <- "data/summary_statistics_results.rds"
  significance_tests_file <- "data/significance_tests_results.rds"
  importance_comparison_file <- "data/importance_comparison_results.rds"
  shap_analysis_file <- "data/shap_analysis_results.rds"
  final_results_file <- "data/comprehensive_three_way_evaluation_results.rds"
  
  # Step 1: Define feature sets and default parameters
  X_pain <- setdiff(colnames(train_xgb_pain), c("fid", "pain_1y"))
  
  default_params <- list(
    objective = "reg:squarederror",
    eta = 0.3,
    nrounds = 100,
    max_depth = 6,
    min_split_loss = 0,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  )
  
  cat("Feature sets defined:\n")
  cat(sprintf("- Pain model: %d features\n", length(X_pain)))
  
  # Initialize results structure
  results <- list()
  
  # Step 2: Train and evaluate baseline models
  if (file.exists(baseline_results_file) && !(retune_pain)) {
    cat("Loading existing baseline model results...\n")
    baseline_results <- readRDS(baseline_results_file)
  } else {
    cat("Training baseline models with default parameters...\n")
    baseline_results <- train_baseline_models(
      train_xgb_pain, test_xgb_pain, 
      X_pain, default_params
    )
    # SAVE BASELINE RESULTS
    saveRDS(baseline_results, baseline_results_file)
    cat(sprintf("Baseline results saved to %s\n", baseline_results_file))
  }
  results$baseline <- baseline_results
  
  # Step 3: Run both Bayesian and stepwise hyperparameter tuning
  pain_results_file <- "data/all_tuning_results_pain_1y.rds"
  
  # Check tuning completion status
  tuning_status <- list(pain_complete = FALSE)
  if (file.exists(tuning_status_file)) {
    tuning_status <- readRDS(tuning_status_file)
  }
  
  # PAIN MODEL TUNING - Use retune_pain flag
  force_pain_retune <- retune_pain
  if (force_pain_retune || !tuning_status$pain_complete || !file.exists(pain_results_file)) {
    cat("Running hyperparameter tuning for pain model...\n")
    tryCatch({
      pain_tuned_params <- tune_xgb_hyperparams(
        data = train_xgb_pain,
        target_col = "pain_1y", 
        seed = 199987,
        n_cores = parallel::detectCores() - 1,
        resume = !force_pain_retune,  # Don't resume if we're forcing retune
        method = "both"
      )
      results$pain_tuned_params <- pain_tuned_params
      tuning_status$pain_complete <- TRUE
      saveRDS(tuning_status, tuning_status_file)
      cat("Pain model tuning completed and saved!\n")
    }, error = function(e) {
      cat("Error in pain model tuning:", e$message, "\n")
      cat("Continuing with existing results if available...\n")
    })
  } else {
    cat("Using existing pain model tuning results...\n")
  }
  
  # Step 4: Run three-way comparison (SAVE INTERMEDIATE)
  if (file.exists(comparison_results_file) && !(retune_pain)) {
    cat("Loading existing three-way comparison results...\n")
    comparison_results <- readRDS(comparison_results_file)
  } else {
    cat("\nRunning comprehensive three-way method comparison...\n")
    tryCatch({
      comparison_results <- compare_three_methods(
        train_xgb_pain, test_xgb_pain,
        X_pain, default_params
      )
      
      if (!is.null(comparison_results)) {
        # SAVE COMPARISON RESULTS
        saveRDS(comparison_results, comparison_results_file)
        cat(sprintf("Three-way comparison results saved to %s\n", comparison_results_file))
      } else {
        stop("Three-way comparison returned NULL results")
      }
    }, error = function(e) {
      cat("Error in three-way comparison:", e$message, "\n")
      if (file.exists(comparison_results_file)) {
        cat("Loading previous comparison results...\n")
        comparison_results <- readRDS(comparison_results_file)
      } else {
        stop("No comparison results available and current attempt failed")
      }
    })
  }
  results$comparison <- comparison_results
  
  # Step 5: Generate learning curves
  if (file.exists(learning_curves_file) && !(retune_pain)) {
    cat("Loading existing learning curves...\n")
    learning_curves <- readRDS(learning_curves_file)
  } else {
    cat("\nGenerating learning curves for all three methods...\n")
    tryCatch({
      learning_curves <- generate_three_way_learning_curves(
        train_xgb_pain, test_xgb_pain, 
        X_pain, comparison_results, default_params
      )
      # SAVE LEARNING CURVES
      saveRDS(learning_curves, learning_curves_file)
      cat(sprintf("Learning curves saved to %s\n", learning_curves_file))
    }, error = function(e) {
      cat("Error generating learning curves:", e$message, "\n")
      learning_curves <- list()
    })
  }
  results$learning_curves <- learning_curves
  
  # Step 6: Generate comprehensive plots (these are fast, don't need saving)
  cat("\nGenerating three-way comparison plots...\n")
  plots <- plot_three_way_comparison(comparison_results)
  results$plots <- plots
  
  # Save plots if requested
  if (save_plots && length(plots) > 0) {
    cat("Saving plots to", plot_dir, "...\n")
    for (plot_name in names(plots)) {
      filename <- file.path(plot_dir, paste0("three_way_", plot_name, ".png"))
      tryCatch({
        ggsave(filename, plots[[plot_name]], width = 12, height = 8, dpi = 300)
        cat(sprintf("- Saved %s\n", filename))
      }, error = function(e) {
        cat(sprintf("- Error saving %s: %s\n", filename, e$message))
      })
    }
  }
  
  # Step 7: Generate summary statistics (SAVE INTERMEDIATE)
  if (file.exists(summary_stats_file) && !(retune_pain || retune_function)) {
    cat("Loading existing summary statistics...\n")
    summary_stats <- readRDS(summary_stats_file)
  } else {
    cat("\nGenerating three-way summary statistics...\n")
    tryCatch({
      summary_stats <- generate_three_way_summary(comparison_results)
      # SAVE SUMMARY STATS
      saveRDS(summary_stats, summary_stats_file)
      cat(sprintf("Summary statistics saved to %s\n", summary_stats_file))
    }, error = function(e) {
      cat("Error generating summary statistics:", e$message, "\n")
      summary_stats <- list()
    })
  }
  results$summary_stats <- summary_stats
  
  # Step 8: Statistical significance testing (SAVE INTERMEDIATE)
  if (file.exists(significance_tests_file) && !(retune_pain)) {
    cat("Loading existing significance tests...\n")
    significance_tests <- readRDS(significance_tests_file)
  } else {
    cat("\nPerforming statistical significance tests...\n")
    tryCatch({
      significance_tests <- perform_three_way_significance_tests(comparison_results)
      # SAVE SIGNIFICANCE TESTS
      saveRDS(significance_tests, significance_tests_file)
      cat(sprintf("Significance tests saved to %s\n", significance_tests_file))
    }, error = function(e) {
      cat("Error performing significance tests:", e$message, "\n")
      significance_tests <- list()
    })
  }
  results$significance_tests <- significance_tests
  
  # Step 9: Feature importance comparison (SAVE INTERMEDIATE)
  if (file.exists(importance_comparison_file) && !(retune_pain)) {
    cat("Loading existing importance comparison...\n")
    importance_comparison <- readRDS(importance_comparison_file)
  } else {
    cat("\nComparing feature importance across all three methods...\n")
    tryCatch({
      importance_comparison <- compare_three_way_feature_importance(comparison_results)
      # SAVE IMPORTANCE COMPARISON
      saveRDS(importance_comparison, importance_comparison_file)
      cat(sprintf("Importance comparison saved to %s\n", importance_comparison_file))
    }, error = function(e) {
      cat("Error comparing feature importance:", e$message, "\n")
      importance_comparison <- list()
    })
  }
  results$importance_comparison <- importance_comparison
  
  # Step 9.5: Generate SHAP analysis (SAVE INTERMEDIATE - this can be slow)
  if (file.exists(shap_analysis_file) && !(retune_pain)) {
    cat("Loading existing SHAP analysis...\n")
    shap_analysis <- readRDS(shap_analysis_file)
  } else {
    cat("\nGenerating SHAP analysis for all three methods...\n")
    tryCatch({
      shap_analysis <- generate_comprehensive_shap_analysis(
        comparison_results, train_xgb_pain, X_pain
      )
      # SAVE SHAP ANALYSIS
      saveRDS(shap_analysis, shap_analysis_file)
      cat(sprintf("SHAP analysis saved to %s\n", shap_analysis_file))
    }, error = function(e) {
      cat("Error generating SHAP analysis:", e$message, "\n")
      shap_analysis <- list()
    })
  }
  results$shap_analysis <- shap_analysis
  
  # Step 10: Identify winners and generate recommendations (MODIFIED TO USE CUSTOM SELECTION)
  cat("\nIdentifying winning approaches and generating recommendations...\n")
  winners <- identify_winning_methods(comparison_results, 
                                      winner_selection_pain = winner_selection_pain)
  results$winners <- winners
  
  # Step 11: Generate final comprehensive report (fast, don't need saving)
  cat("\nGenerating final three-way evaluation report...\n")
  final_report <- generate_three_way_final_report(results)
  results$final_report <- final_report
  
  cat("\n" %+% final_report)
  
  # Save comprehensive results (FINAL SAVE)
  saveRDS(results, final_results_file)
  cat(sprintf("\nComprehensive three-way results saved to %s\n", final_results_file))
  
  # Clean up intermediate files if everything completed successfully
  # (Optional - comment out if you want to keep all intermediate files)
  if (length(results) > 5) {  # Basic check that we have substantial results
    cat("\nCleaning up intermediate files...\n")
    intermediate_files <- c(
      baseline_results_file, 
      learning_curves_file, 
      summary_stats_file,
      significance_tests_file,
      importance_comparison_file
      # Note: Keep tuning results and comparison results - they're expensive to recreate
    )
    
    for (file in intermediate_files) {
      if (file.exists(file)) {
        unlink(file)
        cat(sprintf("- Cleaned up %s\n", file))
      }
    }
  }
  
  cat("\n===============================================\n")
  cat("Comprehensive Three-Way Evaluation Completed!\n")
  cat("===============================================\n")
  
  return(results)
}

#' Identify winning methods for each outcome and metric
#'
#' Determines the best performing method for each outcome (pain) based on either
#' automatic selection (lowest MAE) or manual override. Supports custom winner selection
#' to allow researchers to choose specific methods regardless of performance metrics.
#'
#' @param comparison_results Results object from three-way comparison containing detailed_summary
#' @param winner_selection_pain Method to select as winner for pain model: "Auto", "Baseline", "Bayesian", or "Stepwise"
#'
#' @return List containing winning methods for each outcome with performance metrics:
#'   \item{pain}{List with rmse_winner, mae_winner, coverage_winner, overall_winner, selection_method}
#'   
identify_winning_methods <- function(comparison_results, 
                                     winner_selection_pain = "Auto") {
  
  winners <- list()
  
  if (!is.null(comparison_results$detailed_summary)) {
    df <- comparison_results$detailed_summary
    
    # Pain winners
    pain_data <- df[df$Outcome == "Pain", ]
    if (nrow(pain_data) == 3) {
      if (winner_selection_pain == "Auto") {
        # Original automatic selection based on MAE
        pain_overall_winner <- pain_data$Method[which.min(pain_data$MAE_Bootstrap)]
        cat(sprintf("Pain model: Auto selection chose %s (lowest MAE: %.3f)\n", 
                    pain_overall_winner, min(pain_data$MAE_Bootstrap)))
      } else {
        # Manual override
        pain_overall_winner <- winner_selection_pain
        selected_row <- pain_data[pain_data$Method == winner_selection_pain, ]
        if (nrow(selected_row) > 0) {
          cat(sprintf("Pain model: Manual selection of %s (MAE: %.3f)\n", 
                      pain_overall_winner, selected_row$MAE_Bootstrap))
        } else {
          warning(sprintf("Winner selection '%s' not found for pain model, falling back to Auto", 
                          winner_selection_pain))
          pain_overall_winner <- pain_data$Method[which.min(pain_data$MAE_Bootstrap)]
        }
      }
      
      winners$pain <- list(
        rmse_winner = pain_data$Method[which.min(pain_data$RMSE_Bootstrap)],
        mae_winner = pain_data$Method[which.min(pain_data$MAE_Bootstrap)],
        coverage_winner = pain_data$Method[which.min(abs(pain_data$Coverage - 80))],
        overall_winner = pain_overall_winner,
        selection_method = winner_selection_pain
      )
    }
  }
  
  return(winners)
}

#' String concatenation operator
#'
#' Simple binary operator for concatenating strings without separator.
#' Used throughout report generation for building formatted text output.
#'
#' @param x First string
#' @param y Second string
#'
#' @return Concatenated string without separator
`%+%` <- function(x, y) paste0(x, y)

#' Null coalescing operator
#'
#' Returns the right-hand value when the left-hand value is NULL,
#' otherwise returns the left-hand value. Useful for providing
#' default values in parameter extraction and data processing.
#'
#' @param x Primary value to check
#' @param y Default value to use if x is NULL
#'
#' @return x if not NULL, otherwise y
`%||%` <- function(x, y) if (is.null(x)) y else x


#' Train baseline models with default XGBoost parameters
#'
#' Trains baseline models for both pain and function prediction using default XGBoost
#' parameters. These models serve as performance benchmarks for comparison with
#' optimized hyperparameter methods.
#'
#' @param train_pain Training data for pain prediction model
#' @param test_pain Test data for pain prediction model
#' @param X_pain Feature column names for pain model
#' @param default_params List of default XGBoost parameters
#'
#' @return List containing baseline model results:
#'   \item{pain}{Baseline pain model results with evaluation metrics}
#'   \item{function_model}{Baseline function model results with evaluation metrics}
#'   
train_baseline_models <- function(train_pain, test_pain,
                                  X_pain,default_params) {
  
  results <- list()
  
  # Train baseline pain model
  cat("Training baseline pain model...\n")
  baseline_pain_results <- train_and_evaluate_baseline(
    train_data = train_pain,
    test_data = test_pain,
    predictors = X_pain,
    target = "pain_1y",
    params = default_params
  )
  results$pain <- baseline_pain_results
  
  return(results)
}

#' Train and evaluate a single baseline model
#'
#' Trains a single XGBoost model with default parameters, performs alpha calibration
#' for prediction intervals, and conducts bootstrap evaluation. Handles ceiling
#' weighting for function models to account for scale boundaries.
#'
#' @param train_data Training dataset
#' @param test_data Test dataset  
#' @param predictors Vector of predictor column names
#' @param target Target variable column name
#' @param params XGBoost parameter list
#'
#' @return List containing:
#'   \item{model}{Trained XGBoost model object}
#'   \item{evaluation}{Performance metrics (RMSE, MAE, R-squared)}
#'   \item{bootstrap_results}{Bootstrap prediction intervals and importance}
#'   \item{params}{Parameters used for training}
train_and_evaluate_baseline <- function(train_data, test_data, predictors, target, params) {
  
  # Determine ceiling value based on target
  ceiling_val <- if (target == "function_1y") 100 else 10
  
  
  # STEP 1: Calibrate alpha first
  cat(sprintf("Calibrating alpha for %s baseline model...\n", target))
  
    # Pain model without ceiling weighting
    alpha_calibration <- calibrate_alpha(
      dat_train = train_data,
      new_dat = test_data,
      X = predictors,
      target_col = target, 
      grd = params,
      target_coverage = 80,
      ceiling_threshold = NULL,
      ceiling_weight = 1.0
    )

  optimal_alpha <- alpha_calibration$optimal_alpha
  
  # STEP 2: Run final bootstrap with calibrated alpha
  cat(sprintf("Running final bootstrap with alpha = %.2f\n", optimal_alpha))
  
  bootstrap_results <- XGB_bootstrap_PI_importance(
      dat_train = train_data,
      new_dat = test_data,
      X = predictors,
      target_col = target,
      grd = params,
      nb = 500,
      alpha = optimal_alpha,  # Use calibrated alpha
      seed = 199987,
      ceiling_val = ceiling_val
    )
  
  # Train a single model for the model object
  train_matrix <- xgb.DMatrix(
    data = as.matrix(train_data[, predictors, drop = FALSE]),
    label = train_data[[target]]
  )
  
  model <- xgb.train(
    params = params[names(params) != "nrounds"],
    data = train_matrix,
    nrounds = params$nrounds,
    verbose = 0
  )
  
  # Simple evaluation from bootstrap results
  evaluation <- list(
    rmse = bootstrap_results$performance_summary$RMSE_mean,
    mae = bootstrap_results$performance_summary$MAE_mean,
    r_squared = NA  # Not directly available
  )
  
  return(list(
    model = model,
    evaluation = evaluation,
    bootstrap_results = bootstrap_results,
    params = params
  ))
}

#' Run comprehensive three-way comparison of hyperparameter methods
#'
#' Compares baseline, Bayesian optimization, and stepwise grid search methods
#' for both pain and function prediction models. Extracts optimized parameters,
#' trains models, and performs bootstrap evaluation with prediction intervals.
#' Includes intermediate result saving for computational efficiency.
#'
#' @param train_pain Training data for pain prediction
#' @param test_pain Test data for pain prediction
#' @param X_pain Feature names for pain model
#' @param default_params Default XGBoost parameters for baseline
#'
#' @return Comprehensive comparison results list containing:
#'   \item{params_*}{Extracted parameters for each method and outcome}
#'   \item{model_*}{Trained model objects}
#'   \item{eval_*}{Evaluation metrics for each model}
#'   \item{bootstrap_*}{Bootstrap results with prediction intervals}
#'   \item{detailed_summary}{Summary table of all model performances}
compare_three_methods <- function(train_pain, test_pain,
                                  X_pain, default_params) {
  
  results <- list()
  methods <- c("baseline", "bayesian", "stepwise")
  outcomes <- c("pain")  # Keep this as is
  
  # Define intermediate save files for this function
  params_extraction_file <- "data/extracted_parameters.rds"
  models_progress_file <- "data/models_training_progress.rds"
  
  # Load tuning results with error handling
  pain_tuning_file <- "data/all_tuning_results_pain_1y.rds"

  if (!file.exists(pain_tuning_file)) {
    stop("Tuning results not found. Please run hyperparameter tuning first.")
  }
  
  # Load and extract parameters (SAVE INTERMEDIATE)
  if (file.exists(params_extraction_file)) {
    cat("Loading existing parameter extraction...\n")
    param_results <- readRDS(params_extraction_file)
    for (name in names(param_results)) {
      results[[name]] <- param_results[[name]]
    }
  } else {
    cat("Extracting parameters from tuning results...\n")
    
    pain_tuning_results <- readRDS(pain_tuning_file)

    # Extract best parameters for each method with robust error handling
    results$params_baseline_pain <- default_params

    # Extract parameters with multiple fallback strategies
    results$params_bayesian_pain <- extract_params_safely(
      pain_tuning_results, "bayesian", default_params, "pain")
    results$params_stepwise_pain <- extract_params_safely(
      pain_tuning_results, "stepwise", default_params, "pain")
    
    # Save parameter extraction
    param_results <- results[grepl("^params_", names(results))]
    saveRDS(param_results, params_extraction_file)
    cat(sprintf("Parameter extraction saved to %s\n", params_extraction_file))
  }
  
  # Load training progress if it exists
  training_progress <- list()
  if (file.exists(models_progress_file)) {
    training_progress <- readRDS(models_progress_file)
    cat("Loading existing model training progress...\n")
  }
  
  # Train and evaluate all models with checkpointing
  methods <- c("baseline", "bayesian", "stepwise")
  outcomes <- c("pain")
  
  for (outcome in outcomes) {
    train_data <- if (outcome == "pain") train_pain else train_function
    test_data <- if (outcome == "pain") test_pain else test_function
    predictors <- if (outcome == "pain") X_pain else X_function
    target <- if (outcome == "pain") "pain_1y" else "function_1y"
    
    for (method in methods) {
      model_key <- paste0("model_", method, "_", outcome)
      
      # Use 'outcome' instead of 'outcome_name'
      cat(sprintf("Training %s %s model...\n", method, outcome))
      
      tryCatch({
        # Get and clean parameters
        param_key <- if (outcome == "pain") {
          paste0("params_", method, "_pain")
        }
        
        params <- results[[param_key]]
        
        clean_params <- list(
          objective = params$objective %||% "reg:squarederror",
          eta = params$eta %||% 0.3,
          max_depth = params$max_depth %||% 6,
          min_split_loss = params$min_split_loss %||% 0,
          colsample_bytree = params$colsample_bytree %||% 1,
          min_child_weight = params$min_child_weight %||% 1,
          subsample = params$subsample %||% 1
        )
        nrounds <- params$nrounds %||% 100
        
        # Train model
        train_matrix <- xgb.DMatrix(
          data = as.matrix(train_data[, predictors, drop = FALSE]),
          label = train_data[[target]]
        )
        
        test_matrix <- xgb.DMatrix(
          data = as.matrix(test_data[, predictors, drop = FALSE]),
          label = test_data[[target]]
        )
        
        model <- xgb.train(
          params = clean_params,
          data = train_matrix,
          nrounds = nrounds,
          verbose = 0
        )
        
        # Evaluate model
        predictions <- predict(model, test_matrix)
        actual_values <- test_data[[target]]
        
        evaluation <- list(
          rmse = sqrt(mean((actual_values - predictions)^2)),
          mae = mean(abs(actual_values - predictions)),
          r_squared = cor(actual_values, predictions)^2
        )
        
        # Bootstrap evaluation
        bootstrap_params <- clean_params
        bootstrap_params$nrounds <- nrounds
        
        # Add ceiling value based on target
        ceiling_val <- if (target == "function_1y") 100 else 10
        
        cat(sprintf("Calibrating alpha for %s %s model...\n", method, target))
        
          alpha_calibration <- calibrate_alpha(
            dat_train = train_data,
            new_dat = test_data,
            X = predictors,
            target_col = target,
            grd = bootstrap_params,
            target_coverage = 80,
            ceiling_threshold = NULL,
            ceiling_weight = 1.0
          )

        bootstrap_results <- XGB_bootstrap_PI_importance(
            dat_train = train_data,
            new_dat = test_data,
            X = predictors,
            target_col = target,
            grd = bootstrap_params,
            nb = 500,
            alpha = alpha_calibration$optimal_alpha,  # Use calibrated alpha
            seed = 199987,
            ceiling_val = ceiling_val
          )

        # Store results
        results[[paste0("model_", method, "_", outcome)]] <- model
        results[[paste0("eval_", method, "_", outcome)]] <- evaluation
        results[[paste0("bootstrap_", method, "_", outcome)]] <- bootstrap_results
        
        # Save progress
        training_progress[[model_key]] <- list(
          model = model,
          evaluation = evaluation,
          bootstrap_results = bootstrap_results
        )
        saveRDS(training_progress, models_progress_file)
        cat(sprintf("Progress saved for %s %s model\n", method, outcome))
        
      }, error = function(e) {
        cat(sprintf("Error training %s %s model: %s\n", method, outcome, e$message))
        cat("Continuing with next model...\n")
      })
    }
  }
  
  # Create detailed summary
  tryCatch({
    results$detailed_summary <- create_three_way_detailed_summary(results)
  }, error = function(e) {
    cat("Error creating detailed summary:", e$message, "\n")
    results$detailed_summary <- NULL
  })
  
  # Clean up training progress file after successful completion
  if (file.exists(models_progress_file)) {
    unlink(models_progress_file)
    cat("Cleaned up training progress file\n")
  }
  
  return(results)
}


#' Create detailed summary table for three-way model comparison
#'
#' Generates a structured summary table combining bootstrap performance metrics
#' (RMSE, MAE) and prediction interval coverage for all three methods.
#'
#' @param results Results object from compare_three_methods function
#'
#' @return Data frame with columns:
#'   \item{Outcome}{Pain}
#'   \item{Method}{Baseline, Bayesian, or Stepwise}
#'   \item{RMSE_Bootstrap}{Bootstrap mean RMSE}
#'   \item{RMSE_SD}{Bootstrap RMSE standard deviation}
#'   \item{MAE_Bootstrap}{Bootstrap mean MAE}
#'   \item{MAE_SD}{Bootstrap MAE standard deviation}
#'   \item{Coverage}{Prediction interval coverage percentage}
create_three_way_detailed_summary <- function(results) {
  summary_rows <- list()
  methods <- c("baseline", "bayesian", "stepwise")
  outcomes <- c("Pain")
  
  for (outcome in outcomes) {
    # Fix the mapping here:
    outcome_key <- "pain"
    
    for (method in methods) {
      method_title <- tools::toTitleCase(method)
      bootstrap_key <- paste0("bootstrap_", method, "_", outcome_key)
      
      if (!is.null(results[[bootstrap_key]])) {
        bootstrap_data <- results[[bootstrap_key]]
        perf_summary <- bootstrap_data$performance_summary
        
        # Calculate prediction interval coverage
        coverage <- calculate_prediction_intervals(bootstrap_data)
        
        summary_rows[[paste(outcome, method_title, sep = "_")]] <- data.frame(
          Outcome = outcome,
          Method = method_title,
          RMSE_Bootstrap = perf_summary$RMSE_mean,
          RMSE_SD        = perf_summary$RMSE_sd,
          MAE_Bootstrap  = perf_summary$MAE_mean,
          MAE_SD         = perf_summary$MAE_sd,
          R2_Bootstrap   = perf_summary$R2_mean,
          R2_SD          = perf_summary$R2_sd,
          R2_lower       = perf_summary$R2_CI[1],
          R2_upper       = perf_summary$R2_CI[2],
          Coverage = coverage,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  # Combine all rows
  detailed_summary <- do.call(rbind, summary_rows)
  rownames(detailed_summary) <- NULL
  
  return(detailed_summary)
}

#' Generate learning curves for all three hyperparameter methods
#'
#' Creates learning curve plots showing model performance as training data size
#' increases. Applies consistent y-axis scaling for fair comparison across methods
#' and outcomes. Uses optimized parameters from hyperparameter tuning results.
#'
#' @param train_pain Training data for pain prediction
#' @param test_pain Test data for pain prediction
#' @param X_pain Feature names for pain model
#' @param comparison_results Results from three-way comparison containing parameters
#' @param default_params Default parameters for baseline method
#'
#' @return List of ggplot objects for learning curves:
#'   \item{pain_baseline}{Learning curve for baseline pain model}
#'   \item{pain_bayesian}{Learning curve for Bayesian pain model}
#'   \item{pain_stepwise}{Learning curve for stepwise pain model}
generate_three_way_learning_curves <- function(train_pain, test_pain,
                                               X_pain, comparison_results, default_params) {
  
  curves <- list()
  methods <- c("baseline", "bayesian", "stepwise")
  outcomes <- c("pain")
  
  # Define consistent y-axis limits for each outcome
  pain_y_limits <- c(0, 1.75)     # Updated for pain data range

  # Define consistent y-axis breaks
  pain_y_breaks <- seq(0, 1.75, by = 0.2)      # Breaks every 0.2

  for (outcome in outcomes) {
    train_data <- train_pain
    test_data <- test_pain
    predictors <- X_pain
    target <- "pain_1y"
    
    # Set y-axis properties for this outcome
    y_limits <- pain_y_limits
    y_breaks <- pain_y_breaks
    
    for (method in methods) {
      if (outcome == "pain") {
        params_key <- paste0("params_", method, "_pain")
      }
      
      curve_key <- paste(gsub("_model", "", outcome), method, sep = "_")
      
      if (params_key %in% names(comparison_results)) {
        params <- comparison_results[[params_key]]
        
        # Handle different parameter structures
        if (is.list(params) && "best_params" %in% names(params)) {
          params <- params$best_params
        }
        
        # Ensure all required parameters exist with defaults
        param_list <- list(
          objective = params$objective %||% "reg:squarederror",
          eta = params$eta %||% 0.3,
          max_depth = params$max_depth %||% 6,
          min_split_loss = params$min_split_loss %||% 0,
          colsample_bytree = params$colsample_bytree %||% 1,
          min_child_weight = params$min_child_weight %||% 1,
          subsample = params$subsample %||% 1
        )
        
        curves[[curve_key]] <- tryCatch({
          if (exists("plot_learning_curve")) {
            # Generate the learning curve plot
            base_plot <- plot_learning_curve(
              train_data = train_data,
              test_data = test_data,
              predictors = predictors,
              target = target,
              params = param_list,
              nrounds = params$nrounds %||% 100,
              seed = 199987
            )
            
            # Force consistent y-axis formatting by completely rebuilding the plot
            if (!is.null(base_plot) && !is.null(base_plot$plot)) {
              # Extract the data from the original plot
              plot_data <- base_plot$cv_data
              
              # Create a new plot with forced y-axis limits
              new_plot <- ggplot(plot_data, aes(x = Fraction)) +
                geom_line(aes(y = TrainMAE, color = "Training"), linewidth = 1) +
                geom_line(aes(y = TestMAE, color = "Test"), linewidth = 1) +
                geom_line(aes(y = CV_MAE, color = "10x5 Cross-Validation"), linewidth = 1, linetype = "dashed") +
                geom_point(aes(y = TrainMAE, color = "Training"), size = 2) +
                geom_point(aes(y = TestMAE, color = "Test"), size = 2) +
                geom_point(aes(y = CV_MAE, color = "10x5 Cross-Validation"), size = 2) +
                scale_color_manual(
                  name = "Error Type",
                  values = c(
                    "Training" = "#1f77b4",
                    "Test" = "#ff7f0e",
                    "10x5 Cross-Validation" = "gray40"
                  )
                ) +
                scale_y_continuous(
                  limits = y_limits,
                  breaks = y_breaks,
                  expand = expansion(mult = c(0.02, 0.02))
                ) +
                coord_cartesian(ylim = y_limits, clip = "off") +
                labs(
                  x = "Fraction of Training Data Used",
                  y = "Mean Absolute Error (MAE)",
                  title = sprintf("Learning Curve by Training Set Size (%s %s)", 
                                  tools::toTitleCase(gsub("_model", "", outcome)), 
                                  tools::toTitleCase(method)),
                  color = "Error Type"
                ) +
                theme_minimal() +
                theme(
                  legend.position = "bottom",
                  plot.title = element_text(hjust = 0)
                )
              
              # Return the structure expected by other parts of the code
              list(
                cv_data = plot_data,
                plot = new_plot
              )
            } else {
              base_plot  # Return original if extraction fails
            }
          } else {
            cat(sprintf("plot_learning_curve function not found for %s %s\n", method, outcome))
            NULL
          }
        }, error = function(e) {
          cat("Error generating learning curve for", method, outcome, "model:", e$message, "\n")
          NULL
        })
      } else {
        cat("Parameters not found for key:", params_key, "\n")
      }
    }
  }
  
  return(curves)
}

#' Generate summary statistics for three-way method comparison
#'
#' Analyzes detailed comparison results to identify best performing methods
#' for each metric (RMSE, MAE, coverage) and outcome. Provides structured
#' summary of performance rankings across all methods.
#'
#' @param comparison_results Results object from compare_three_methods
#'
#' @return List containing:
#'   \item{pain}{Pain model statistics with best performers and metric values}
generate_three_way_summary <- function(comparison_results) {
  
  summary_stats <- list()
  
  if (!is.null(comparison_results$detailed_summary)) {
    df <- comparison_results$detailed_summary
    
    # Pain model comparison
    pain_data <- df[df$Outcome == "Pain", ]
    if (nrow(pain_data) == 3) {
      baseline_pain <- pain_data[pain_data$Method == "Baseline", ]
      bayesian_pain <- pain_data[pain_data$Method == "Bayesian", ]
      stepwise_pain <- pain_data[pain_data$Method == "Stepwise", ]
      
      # Find best performing method for each metric
      pain_rmse_order <- pain_data[order(pain_data$RMSE_Bootstrap), ]
      pain_mae_order <- pain_data[order(pain_data$MAE_Bootstrap), ]
      pain_coverage_order <- pain_data[order(abs(pain_data$Coverage - 80)), ]
      
      summary_stats$pain <- list(
        best_rmse = pain_rmse_order$Method[1],
        best_mae = pain_mae_order$Method[1],
        best_coverage = pain_coverage_order$Method[1],
        rmse_values = setNames(pain_data$RMSE_Bootstrap, pain_data$Method),
        mae_values = setNames(pain_data$MAE_Bootstrap, pain_data$Method),
        coverage_values = setNames(pain_data$Coverage, pain_data$Method)
      )
    }
  }
  
  return(summary_stats)
}

#' Perform statistical significance tests for three-way comparison
#'
#' Conducts pairwise t-tests and ANOVA to determine if performance differences
#' between hyperparameter methods are statistically significant. Uses absolute
#' errors from bootstrap predictions for paired comparisons.
#'
#' @param comparison_results Results object containing bootstrap predictions for all methods
#'
#' @return List of statistical test results:
#'   \item{pain_baseline_vs_bayesian}{Paired t-test for pain models}
#'   \item{pain_baseline_vs_stepwise}{Paired t-test for pain models}
#'   \item{pain_bayesian_vs_stepwise}{Paired t-test for pain models}
#'   \item{pain_anova}{ANOVA for overall pain model differences}
perform_three_way_significance_tests <- function(comparison_results) {
  
  tests <- list()
  methods <- c("baseline", "bayesian", "stepwise")
  
  # Test for pain models
  pain_errors <- list()
  for (method in methods) {
    bootstrap_key <- paste0("bootstrap_", method, "_pain")
    if (!is.null(comparison_results[[bootstrap_key]])) {
      pain_errors[[method]] <- with(comparison_results[[bootstrap_key]]$predictions,
                                    abs(Actual - Predicted))
    }
  }
  
  if (length(pain_errors) == 3) {
    # Pairwise comparisons
    tests$pain_baseline_vs_bayesian <- t.test(pain_errors$baseline, pain_errors$bayesian, paired = TRUE)
    tests$pain_baseline_vs_stepwise <- t.test(pain_errors$baseline, pain_errors$stepwise, paired = TRUE)
    tests$pain_bayesian_vs_stepwise <- t.test(pain_errors$bayesian, pain_errors$stepwise, paired = TRUE)
    
    # ANOVA for overall difference
    combined_errors <- c(pain_errors$baseline, pain_errors$bayesian, pain_errors$stepwise)
    method_labels <- rep(c("Baseline", "Bayesian", "Stepwise"), 
                         times = sapply(pain_errors, length))
    tests$pain_anova <- aov(combined_errors ~ method_labels)
  }
  
  return(tests)
}

#' Compare feature importance across all three hyperparameter methods
#'
#' Merges feature importance rankings from baseline, Bayesian, and stepwise
#' methods to identify features with consistent or divergent importance
#' across optimization approaches. Calculates importance ranges to highlight
#' features with method-dependent rankings.
#'
#' @param comparison_results Results object containing importance_summary for each method
#'
#' @return List containing:
#'   \item{pain}{Merged importance comparison for pain models with range calculations}
compare_three_way_feature_importance <- function(comparison_results) {
  
  importance_comp <- list()
  methods <- c("baseline", "bayesian", "stepwise")
  
  # Compare pain model feature importance
  pain_importance <- list()
  for (method in methods) {
    bootstrap_key <- paste0("bootstrap_", method, "_pain")
    if (!is.null(comparison_results[[bootstrap_key]])) {
      pain_importance[[method]] <- comparison_results[[bootstrap_key]]$importance_summary
    }
  }
  
  if (length(pain_importance) == 3) {
    # Merge all importance data
    merged_pain <- pain_importance$baseline
    colnames(merged_pain)[colnames(merged_pain) == "MeanGain"] <- "MeanGain_Baseline"
    
    merged_pain <- merge(merged_pain, pain_importance$bayesian[, c("Feature", "MeanGain")], 
                         by = "Feature", suffixes = c("", "_Bayesian"))
    colnames(merged_pain)[colnames(merged_pain) == "MeanGain"] <- "MeanGain_Bayesian"
    
    merged_pain <- merge(merged_pain, pain_importance$stepwise[, c("Feature", "MeanGain")], 
                         by = "Feature", suffixes = c("", "_Stepwise"))
    colnames(merged_pain)[colnames(merged_pain) == "MeanGain"] <- "MeanGain_Stepwise"
    
    # Calculate differences
    merged_pain$Range <- apply(merged_pain[, c("MeanGain_Baseline", "MeanGain_Bayesian", "MeanGain_Stepwise")], 
                               1, function(x) max(x) - min(x))
    
    importance_comp$pain <- merged_pain[order(merged_pain$Range, decreasing = TRUE), ]
  }
  
  return(importance_comp)
}

#' Generate comprehensive visualization plots for three-way comparison
#'
#' Creates multiple plot types including performance bar charts with error bars,
#' prediction interval coverage comparison, and rainfall-style error distribution
#' plots. Rainfall plots combine dot plots, box plots, and density curves for
#' comprehensive error analysis visualization.
#'
#' @param comparison_results Results object from three-way comparison
#'
#' @return List of ggplot objects:
#'   \item{rmse_comparison}{Bar chart comparing RMSE across methods}
#'   \item{mae_comparison}{Bar chart comparing MAE across methods}
#'   \item{coverage_comparison}{Bar chart comparing prediction interval coverage}
#'   \item{pain_error_rainfall}{Rainfall plot of pain prediction errors}
plot_three_way_comparison <- function(comparison_results) {
  
  plots <- list()
  
  if (!is.null(comparison_results$detailed_summary)) {
    df <- comparison_results$detailed_summary
    
    # RMSE comparison plot
    plots$rmse_comparison <- ggplot(df, aes(x = Method, y = RMSE_Bootstrap, fill = Outcome)) +
      geom_col(position = "dodge") +
      geom_errorbar(aes(ymin = RMSE_Bootstrap - RMSE_SD, ymax = RMSE_Bootstrap + RMSE_SD),
                    position = position_dodge(width = 0.9), width = 0.25) +
      labs(title = "RMSE Comparison Across Three Methods",
           x = "Method", y = "RMSE (Bootstrap Mean ± SD)") +
      theme_minimal() +
      scale_fill_viridis_d()
    
    # MAE comparison plot
    plots$mae_comparison <- ggplot(df, aes(x = Method, y = MAE_Bootstrap, fill = Outcome)) +
      geom_col(position = "dodge") +
      geom_errorbar(aes(ymin = MAE_Bootstrap - MAE_SD, ymax = MAE_Bootstrap + MAE_SD),
                    position = position_dodge(width = 0.9), width = 0.25) +
      labs(title = "MAE Comparison Across Three Methods",
           x = "Method", y = "MAE (Bootstrap Mean ± SD)") +
      theme_minimal() +
      scale_fill_viridis_d()
    
    # Coverage comparison plot
    plots$coverage_comparison <- ggplot(df, aes(x = Method, y = Coverage, fill = Outcome)) +
      geom_col(position = "dodge") +
      geom_hline(yintercept = 80, linetype = "dashed", color = "red") +
      labs(title = "Prediction Interval Coverage Comparison",
           x = "Method", y = "Coverage (%)") +
      theme_minimal() +
      scale_fill_viridis_d()
  }
  
  # RAINFALL-STYLE ERROR PLOTS

  # Extract error data for each outcome separately
  methods <- c("baseline", "bayesian", "stepwise")
  method_labels <- c("Baseline", "Bayesian", "Stepwise")
  
  # PAIN ERROR PLOT
  pain_errors <- list()
  for (i in seq_along(methods)) {
    method <- methods[i]
    bootstrap_key <- paste0("bootstrap_", method, "_pain")
    if (!is.null(comparison_results[[bootstrap_key]]$predictions)) {
      pred_data <- comparison_results[[bootstrap_key]]$predictions
      pain_errors[[method_labels[i]]] <- data.frame(
        Method = method_labels[i],
        AbsoluteError = abs(pred_data$Actual - pred_data$Predicted)
      )
    }
  }
  
  if (length(pain_errors) > 0) {
    pain_error_data <- do.call(rbind, pain_errors)
    pain_error_data$Method <- factor(pain_error_data$Method, levels = method_labels)
    
    # First, split data by method
    baseline_data <- pain_error_data[pain_error_data$Method == "Baseline", ]
    bayesian_data <- pain_error_data[pain_error_data$Method == "Bayesian", ]
    stepwise_data <- pain_error_data[pain_error_data$Method == "Stepwise", ]
    
    # Create the plot with manual positioning
    plots$pain_error_rainfall <- ggplot() +
      # Baseline (position 1)
      ggdist::geom_weave(
        data = baseline_data,
        aes(x = 0.8, y = AbsoluteError),  # Fixed x position
        side = "left", 
        dotsize = 0.55,
        justification = 1.1,
        binwidth = 0.2,
        alpha = 1, 
        color = NA,
        fill = "#4682b4"
      ) +
      geom_boxplot(
        data = baseline_data,
        aes(x = 0.8, y = AbsoluteError),  # Fixed x position
        width = 0.08,
        color = "black", 
        fill = "darkgrey", 
        outlier.shape = NA
      ) +
      ggdist::stat_halfeye(
        data = baseline_data,
        aes(x = 0.8, y = AbsoluteError),  # Fixed x position
        side = "right",  # Same side as dots
        width = 0.2,
        alpha = 0.4,
        fill = "#6b8e23",
        color = NA,
        justification = -0.5  # Same as dots for perfect alignment
      ) +
      
      # Bayesian (position 2)
      ggdist::geom_weave(
        data = bayesian_data,
        aes(x = 2.1, y = AbsoluteError),  # Fixed x position
        side = "left", 
        dotsize = 0.55,
        justification = 1.1,
        binwidth = 0.2,
        alpha = 1, 
        color = NA,
        fill = "#4682b4"
      ) +
      geom_boxplot(
        data = bayesian_data,
        aes(x = 2.1, y = AbsoluteError),  # Fixed x position
        width = 0.08,
        color = "black", 
        fill = "darkgrey", 
        outlier.shape = NA
      ) +
      ggdist::stat_halfeye(
        data = bayesian_data,
        aes(x = 2.1, y = AbsoluteError),  # Fixed x position
        side = "right",  # Same side as dots
        width = 0.2,
        alpha = 0.4,
        fill = "#6b8e23",
        color = NA,
        justification = -0.5  # Same as dots for perfect alignment
      ) +
      
      # Stepwise (position 3.5 - further right)
      ggdist::geom_weave(
        data = stepwise_data,
        aes(x = 3.5, y = AbsoluteError),  # Custom position - further right
        side = "left", 
        dotsize = 0.55,
        justification = 1.1,
        binwidth = 0.2,
        alpha = 1, 
        color = NA,
        fill = "#4682b4"
      ) +
      geom_boxplot(
        data = stepwise_data,
        aes(x = 3.5, y = AbsoluteError),  # Custom position
        width = 0.08,
        color = "black", 
        fill = "darkgrey", 
        outlier.shape = NA
      ) +
      ggdist::stat_halfeye(
        data = stepwise_data,
        aes(x = 3.5, y = AbsoluteError),  # Custom position
        side = "right",  # Same side as dots
        width = 0.2,
        alpha = 0.4,
        fill = "#6b8e23",
        color = NA,
        justification = -0.5  # Same as dots for perfect alignment
      ) +
      
      # Manual x-axis labels
      scale_x_continuous(
        breaks = c(0.8, 2, 3.5),
        labels = c("Baseline", "Bayesian", "Stepwise"),
        limits = c(-0.4, 4)
      ) +
      labs(
        title = "**Pain Prediction Error Analysis**<br><span style='font-size:17pt; color:gray60'>Comparing Hyperparameter Optimization Strategies</span>",
        x = "",
        y = "Absolute Error"
      ) +
      theme_fancy +
      theme(
        legend.position = "none",
        plot.title = ggtext::element_markdown(size = 17, face = "bold", hjust = 0)
      )
  }
  
  return(plots)
}

#' Generate final comprehensive evaluation report
#'
#' Creates a formatted text report summarizing the entire three-way evaluation,
#' including method descriptions, performance tables, clinical interpretation,
#' and recommendations. Formats numerical results consistently and highlights
#' best performers with asterisks.
#'
#' @param results Complete results object from run_comprehensive_evaluation
#'
#' @return Character string containing formatted evaluation report with:
#'   - Method descriptions and characteristics
#'   - Performance summary tables for both outcomes
#'   - Clinical interpretation guidelines
#'   - File locations for detailed results
generate_three_way_final_report <- function(results) {
  
  # Helper function to create section dividers
  section_divider <- function(title, char = "=", width = 70) {
    if (title == "") {
      return(paste(rep(char, width), collapse = ""))
    }
    title_line <- paste0(" ", title, " ")
    title_length <- nchar(title_line)
    padding <- max(0, (width - title_length) / 2)
    left_pad <- paste(rep(char, floor(padding)), collapse = "")
    right_pad <- paste(rep(char, ceiling(padding)), collapse = "")
    paste0(left_pad, title_line, right_pad)
  }
  
  # Helper function to format numbers consistently
  format_number <- function(x, digits = 3) {
    if (is.na(x)) return("N/A")
    if (abs(x) < 0.001) return(sprintf("%.4f", x))
    if (abs(x) < 1) return(sprintf("%.3f", x))
    if (abs(x) < 100) return(sprintf("%.2f", x))
    return(sprintf("%.1f", x))
  }
  
  # Start building the report
  report <- ""
  
  # Main header
  report <- report %+% section_divider("COMPREHENSIVE THREE-WAY HYPERPARAMETER EVALUATION REPORT") %+% "\n"
  report <- report %+% "Generated: " %+% format(Sys.time(), "%Y-%m-%d %H:%M:%S") %+% "\n\n"
  
  # Methods evaluated
  report <- report %+% section_divider("METHODS EVALUATED") %+% "\n\n"
  report <- report %+% "Three hyperparameter optimization approaches were compared:\n\n"
  report <- report %+% "  * Baseline (Default Parameters)\n"
  report <- report %+% "    - Standard XGBoost defaults without optimization\n"
  report <- report %+% "    - Serves as performance benchmark\n\n"
  report <- report %+% "  * Bayesian Optimization\n" 
  report <- report %+% "    - Automated probabilistic parameter search\n"
  report <- report %+% "    - Efficient exploration of parameter space\n\n"
  report <- report %+% "  * Stepwise Grid Search\n"
  report <- report %+% "    - Systematic exhaustive parameter exploration\n"
  report <- report %+% "    - Comprehensive coverage of parameter combinations\n\n"
  
  # Outcomes analyzed
  if (!is.null(results$comparison$detailed_summary)) {
    outcomes <- unique(results$comparison$detailed_summary$Outcome)
    report <- report %+% sprintf("OUTCOMES ANALYZED: %s\n\n", paste(outcomes, collapse = ", "))
  }
  
  # Performance Summary with formatted tables
  if (!is.null(results$comparison$detailed_summary)) {
    report <- report %+% section_divider("PERFORMANCE SUMMARY") %+% "\n\n"
    
    df <- results$comparison$detailed_summary
    
    # Pain Model Results
    pain_data <- df[df$Outcome == "Pain", ]
    if (nrow(pain_data) > 0) {
      report <- report %+% "PAIN PREDICTION MODELS:\n"
      report <- report %+% paste(rep("-", 50), collapse = "") %+% "\n"
      report <- report %+% sprintf("%-12s | %-10s | %-10s | %-12s\n", 
                                   "Method", "RMSE", "MAE", "Coverage (%)")
      report <- report %+% paste(rep("-", 50), collapse = "") %+% "\n"
      
      # Find best performers
      best_rmse_idx <- which.min(pain_data$RMSE_Bootstrap)
      best_mae_idx <- which.min(pain_data$MAE_Bootstrap)
      best_coverage_idx <- which.min(abs(pain_data$Coverage - 80))
      
      for (i in 1:nrow(pain_data)) {
        method <- sprintf("%-12s", pain_data$Method[i])
        rmse <- sprintf("%-10s", format_number(pain_data$RMSE_Bootstrap[i]))
        mae <- sprintf("%-10s", format_number(pain_data$MAE_Bootstrap[i]))
        coverage <- sprintf("%-12s", format_number(pain_data$Coverage[i]))
        
        # Mark best performers
        if (i == best_rmse_idx) rmse <- paste0(rmse, " *")
        if (i == best_mae_idx) mae <- paste0(mae, " *")
        if (i == best_coverage_idx) coverage <- paste0(coverage, " *")
        
        report <- report %+% sprintf("%s | %s | %s | %s\n", 
                                     method, rmse, mae, coverage)
      }
      report <- report %+% "\n"
    }
  
  }
  
  # Clinical interpretation
  report <- report %+% section_divider("CLINICAL INTERPRETATION") %+% "\n\n"
  report <- report %+% "METHOD CHARACTERISTICS:\n"
  report <- report %+% "  * Baseline: Raw XGBoost performance with default parameters\n"
  report <- report %+% "  * Bayesian: Automated optimization using probabilistic approach\n"
  report <- report %+% "  * Stepwise: Systematic grid search across parameter space\n\n"
  
  # Footer
  report <- report %+% section_divider("") %+% "\n"
  report <- report %+% "Full results saved in: comprehensive_three_way_evaluation_results.rds\n"
  report <- report %+% "Additional plots and diagnostics available in results object\n"
  report <- report %+% section_divider("") %+% "\n"
  
  return(report)
}

#' Create formatted evaluation table for display
#'
#' Formats the detailed summary results into a publication-ready table with
#' proper formatting for confidence intervals and percentages. Can filter
#' results to specific outcomes or show comprehensive comparison.
#'
#' @param comparison_results Results object from three-way comparison
#' @param outcome_type Filter for specific outcome: "pain" or "both"
#'
#' @return Data frame with formatted columns:
#'   \item{Outcome}{Pain}
#'   \item{Method}{Hyperparameter method name}
#'   \item{RMSE (±SD)}{Formatted RMSE with standard deviation}
#'   \item{MAE (±SD)}{Formatted MAE with standard deviation}
#'   \item{R2 (±SD)}{Formatted R-squared with standard deviation}
#'   \item{Coverage}{Formatted coverage percentage}
create_three_way_evaluation_table <- function(comparison_results, outcome_type = "pain") {
  
  outcome_type <- match.arg(outcome_type, c("pain", "both"))
  
  if (is.null(comparison_results$detailed_summary)) {
    return(NULL)
  }
  
  df <- comparison_results$detailed_summary
  
  if (outcome_type != "both") {
    outcome_filter <- tools::toTitleCase(outcome_type)
    df <- df[df$Outcome == outcome_filter, ]
  }
  
  # Format the table for display
  df$RMSE_Display <- sprintf("%.2f (±%.2f)", df$RMSE_Bootstrap, df$RMSE_SD)
  df$MAE_Display  <- sprintf("%.2f (±%.2f)", df$MAE_Bootstrap,  df$MAE_SD)
  df$R2_Display   <- sprintf("%.2f (±%.2f)", df$R2_Bootstrap,   df$R2_SD)
  df$Coverage_Display <- sprintf("%.1f%%", df$Coverage)
  
  # Select columns for display
  display_df <- df[, c("Outcome", "Method", "RMSE_Display", "MAE_Display", "R2_Display", "Coverage_Display")]
  colnames(display_df) <- c("Outcome", "Method", "RMSE (±SD)", "MAE (±SD)", "R2 (±SD)", "Coverage")
  
  return(display_df)
}

#' Generate comprehensive SHAP analysis for all methods
#'
#' Computes SHAP (SHapley Additive exPlanations) values for feature importance
#' analysis across all three hyperparameter methods and both outcomes. Provides
#' model-agnostic explanations for prediction behavior and feature contributions.
#'
#' @param comparison_results Results object containing trained models
#' @param train_pain Training data for pain models 
#' @param X_pain Feature names for pain models
#'
#' @return Nested list structure:
#'   \item{pain}{SHAP results for pain models by method}
generate_comprehensive_shap_analysis <- function(comparison_results, train_pain, X_pain) {
  
  shap_results <- list()
  methods <- c("baseline", "bayesian", "stepwise")
  outcomes <- c("pain")
  
  for (outcome in outcomes) {
    train_data <- train_pain
    predictors <- X_pain
    target <- "pain_1y"
    
    shap_results[[outcome]] <- list()
    
    for (method in methods) {
      model_key <- paste0("model_", method, "_", outcome)
      
      if (!is.null(comparison_results[[model_key]])) {
        cat(sprintf("Computing SHAP values for %s %s model...\n", method, outcome))
        
        tryCatch({
          shap_data <- compute_shap_explanations(
            train_data = train_data,
            model = comparison_results[[model_key]],
            predictors = predictors,
            outcome = target
          )
          
          shap_results[[outcome]][[method]] <- shap_data
          
        }, error = function(e) {
          cat(sprintf("Error computing SHAP for %s %s: %s\n", method, outcome, e$message))
          shap_results[[outcome]][[method]] <- NULL
        })
      }
    }
  }
  
  return(shap_results)
}

#' Calculate prediction interval coverage from bootstrap results
#'
#' Computes the percentage of actual values that fall within the predicted
#' confidence intervals. Used to assess the reliability of uncertainty
#' quantification in model predictions.
#'
#' @param bootstrap_results Bootstrap results object containing predictions with intervals
#'
#' @return Numeric value representing coverage percentage (0-100), or NA if calculation fails
calculate_prediction_intervals <- function(bootstrap_results) {
  if (is.null(bootstrap_results$predictions)) {
    return(NA)
  }
  
  predictions <- bootstrap_results$predictions
  if (!all(c("Actual", "lb", "ub") %in% colnames(predictions))) {
    return(NA)
  }
  
  within_interval <- (predictions$Actual >= predictions$lb) & (predictions$Actual <= predictions$ub)
  coverage <- mean(within_interval, na.rm = TRUE) * 100
  
  return(coverage)
}


#' Safely extract hyperparameters with fallback strategies
#'
#' Robustly extracts optimized hyperparameters from tuning results with multiple
#' fallback strategies. Handles different result structures and provides default
#' parameters when extraction fails, ensuring evaluation can continue.
#'
#' @param tuning_results Complete tuning results object
#' @param method Method name ("bayesian" or "stepwise")
#' @param default_params Default parameters to use if extraction fails
#' @param model_type Model type for informative error messages ("pain" or "function")
#'
#' @return List of extracted parameters or default_params if extraction fails
extract_params_safely <- function(tuning_results, method, default_params, model_type) {
  
  tryCatch({
    if (method %in% names(tuning_results)) {
      method_results <- tuning_results[[method]]
      
      # Try different parameter locations
      if ("params" %in% names(method_results)) {
        return(method_results$params)
      } else if ("best_params" %in% names(method_results)) {
        return(method_results$best_params)
      } else if ("tuning_results" %in% names(method_results) && 
                 "best_params" %in% names(method_results$tuning_results)) {
        return(method_results$tuning_results$best_params)
      }
    }
    
    # If we get here, extraction failed
    cat(sprintf("Warning: Could not extract %s parameters for %s model, using defaults\n", 
                method, model_type))
    return(default_params)
    
  }, error = function(e) {
    cat(sprintf("Error extracting %s parameters for %s model: %s. Using defaults.\n", 
                method, model_type, e$message))
    return(default_params)
  })
}