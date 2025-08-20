log_file <- "tuning_log.txt"

# Clear the log file by overwriting it with an empty string
cat("", file = log_file)

# Define a simple logging function instead of using sink
log_message <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  full_msg <- sprintf("[%s] %s\n", timestamp, msg)
  cat(full_msg, file = log_file, append = TRUE)
  cat(msg, "\n")  # Also print to console
}

log_message("Enhanced tuning with comparison started")

#' Perform Hyperparameter Tuning for XGBoost with Cross-Validation
#'
#' This function evaluates multiple hyperparameter combinations for XGBoost using cross-validation. 
#' The results are logged, and optional parallelization is used to speed up computations.
#'
#' @param target_col Target column name
#' @param tune_grid Grid of hyperparameters to evaluate
#' @param X Feature matrix
#' @param y Target vector
#' @param folds Cross-validation folds
#' @param sample_weights Optional sample weights
#' @param metric Evaluation metric
#' @param parallel Whether to use parallel processing
#' @param verbose Whether to print progress
#' @param outfile Log file path (unused in this version)
#' @param step Description of the current tuning step
#'
#' @return Data frame with tuning results
run_xgb_tuning <- function(target_col, tune_grid, X, y, folds, sample_weights = NULL, metric = "rmse", 
                           parallel = TRUE, verbose = TRUE, outfile = NULL, step = "") {
  seed <- 199987
  
  # Initialize results data frame
  results <- data.frame()
  
  if (parallel) {
    # Set up parallel backend
    n.cores <- parallel::detectCores() - 1
    my.cluster <- parallel::makeCluster(n.cores, type = "PSOCK")
    doParallel::registerDoParallel(cl = my.cluster)
    
    # Run each row of the tuning grid in parallel
    results <- foreach::foreach(
      i = 1:nrow(tune_grid),
      .combine = rbind,
      .packages = c("xgboost"),
      .options.RNG = seed
    ) %dopar% {
      
      # Convert hyperparameter row to list and add fixed objective
      params <- as.list(tune_grid[i, ])
      if ("objective" %in% names(params)) {
        # Use objective from tune_grid if present
        # params$objective already set
      } else {
        # Fallback to default
        params$objective <- "reg:squarederror"
      }
      nrounds <- tune_grid$nrounds[i]
      
      rep_results <- list()
      fold_index <- 1
      
      # Repeat cross-validation using grouped 5-fold subsets (10x5-fold CV in total)
      # I pick 5 folds at a time 10 times and use those in xgb.cv()
      for (j in seq(1, length(folds), by = 5)) {
        fold_subset <- folds[j:(j + 4)] # Get 5 folds per repetition
        fold_indices <- unlist(fold_subset) 
        
        # Perform CV on the current subset of folds
        xgb_cv <- xgb.cv(
          params = params,
          data = xgb.DMatrix(data = X, label = y, weight = sample_weights, missing = NA),
          nrounds = nrounds,
          folds = fold_subset,
          metrics = c("rmse", "mae"),
          early_stopping_rounds = min(20, nrounds %/% 4),
          verbose = FALSE,
          prediction = TRUE
        )
        
        # Store results
        rep_results[[fold_index]] <- list(
          # Each xgb.cv() returns one evaluation_log which has 'nrounds' number of rows
          # and reports the mean evaluation metrics from all 5 folds
          eval_log = xgb_cv$evaluation_log,
          pred = xgb_cv$pred,
          fold_indices = fold_indices
        )
        
        fold_index <- fold_index + 1
      }
      
      # Combine evaluation logs from all repetitions into one data frame
      all_eval_logs <- lapply(rep_results, function(r) r$eval_log)
      # Stacks eval_logs from each repetition vertically into one large data frame 
      # And saves the repetition each row comes from in a new column
      all_logs_combined <- data.table::rbindlist(lapply(seq_along(all_eval_logs), function(i) {
        df <- all_eval_logs[[i]]
        df$rep <- i
        df
      }))
      
      # For each boosting round (iteration), compute the mean of the 
      # test RMSE and MAE 
      # across the 10 cross-validation repetitions. 
      # Result: one row per boosting round, with mean test RMSE and MAE across reps.
      summary_by_iter <- aggregate(
        cbind(test_rmse_mean, test_mae_mean) ~ iter,
        data = all_logs_combined,
        FUN = mean
      )
      
      # From these boosting rounds, select the one with the lowest median test RMSE
      # This gives the most consistently accurate number of boosting rounds
      # for this particular hyperparameter configuration.
      best_row <- summary_by_iter[which.min(summary_by_iter$test_rmse_mean), ]
      best_iter <- best_row$iter
      best_rmse <- best_row$test_rmse_mean
      best_mae <- best_row$test_mae_mean
      
      
      # Return
      # To the hyperparameter configuration, I append the following metrics:
      cbind(
        tune_grid[i, ],
        best_iteration = best_iter,
        input_nrounds = nrounds,
        rmse = best_rmse,
        mae = best_mae
      )
    }
    
    # Stop the cluster
    parallel::stopCluster(my.cluster)
    
  } else {
    # Sequential processing
    for (i in 1:nrow(tune_grid)) {
      log_message(sprintf("%s: Tuning Iteration %d of %d", step, i, nrow(tune_grid)))
      
      params <- as.list(tune_grid[i, ])
      if ("objective" %in% names(params)) {
        # Use objective from tune_grid if present
        # params$objective already set
      } else {
        # Fallback to default
        params$objective <- "reg:squarederror"
      }
      nrounds <- tune_grid$nrounds[i]
      
      rep_results <- list()
      fold_index <- 1
      
      for (j in seq(1, length(folds), by = 5)) {
        fold_subset <- folds[j:(j + 4)]
        fold_indices <- unlist(fold_subset)
        
        xgb_cv <- xgb.cv(
          params = params,
          data = xgb.DMatrix(data = X, label = y, weight = sample_weights, missing = NA),
          nrounds = nrounds,
          folds = fold_subset,
          metrics = c("mae", "rmse"),
          early_stopping_rounds = min(20, nrounds %/% 4),
          verbose = FALSE,
          prediction = TRUE
        )
        
        rep_results[[fold_index]] <- list(
          eval_log = xgb_cv$evaluation_log,
          pred = xgb_cv$pred,
          fold_indices = fold_indices
        )
        
        fold_index <- fold_index + 1
      }
      
      all_eval_logs <- lapply(rep_results, function(r) r$eval_log)
      all_logs_combined <- do.call(rbind, lapply(seq_along(all_eval_logs), function(i) {
        df <- all_eval_logs[[i]]
        df$rep <- i
        return(df)
      }))
      
      summary_by_iter <- aggregate(
        cbind(test_rmse_mean, test_mae_mean) ~ iter,
        data = all_logs_combined,
        FUN = mean
      )
      
      # Use RMSE to select best iteration (consistent with reg:squarederror objective)
      best_row <- summary_by_iter[which.min(summary_by_iter$test_rmse_mean), ]
      best_iter <- best_row$iter
      best_rmse <- best_row$test_rmse_mean
      best_mae <- best_row$test_mae_mean
      
      results <- rbind(
        results,
        cbind(
          tune_grid[i, ],
          best_iteration = best_iter,
          input_nrounds = nrounds,
          rmse = best_rmse,
          mae = best_mae
        )
      )
    }
  }
  
  return(as.data.frame(results))
}


#' Stepwise Hyperparameter Tuning for XGBoost (Reverted to Original Parameter Ranges)
#'
#' This function performs stepwise hyperparameter tuning for XGBoost, optimizing
#' different sets of hyperparameters in separate stages. Each stage logs its progress
#' and results, ensuring reproducibility and transparency.
#'
#' @param data Data frame containing predictors and the target variable
#' @param target_col A character string specifying the name of the target variable
#' @param seed An integer used to set the random seed for reproducibility
#' @param n_cores Integer specifying the number of CPU cores to use for parallelization
#' @param resume Whether to resume from saved results
#'
#' @return A list of the best hyperparameters obtained through tuning
tune_xgb_stepwise <- function(data, target_col, seed = 199987, 
                              n_cores = parallel::detectCores() - 1,
                              resume = TRUE) {
  # Prepare Data
  predictors <- colnames(data %>% select(-fid, -all_of(target_col)))
  
  X_train <- as.matrix(data %>% select(-fid, -all_of(target_col)))
  y_train <- data[[target_col]]
  
  # Check for existing final results if resume is TRUE
  result_file <- sprintf("data/stepwise_opt_results_%s.rds", target_col)
  if (resume && file.exists(result_file)) {
    cat(sprintf("Loading existing stepwise optimization results for %s\n", target_col))
    return(readRDS(result_file))
  }
  
  # Cross-validation folds - REVERTED TO 10x5 (50 total folds)
  set.seed(seed)
  folds <- my_rep_folds(data, IDvarn = "fid", nreps = 10, nfolds = 5, seed = seed)
  
  # Helper function for running each step with resume capability
  run_step <- function(step_num, grid, step_desc) {
    step_file <- sprintf("data/stepwise_step%d_%s.rds", step_num, target_col)
    
    # Check if this step already exists
    if (resume && file.exists(step_file)) {
      cat(sprintf("Step %d: Loading existing results - %s\n", step_num, step_desc))
      return(readRDS(step_file))
    }
    
    # Run the step if it doesn't exist
    cat(sprintf("Step %d: %s\n", step_num, step_desc))
    
    results <- run_xgb_tuning(
      target_col = target_col,
      tune_grid = grid,
      X = X_train,
      y = y_train,
      folds = folds,
      parallel = (n_cores > 1),
      step = sprintf("Step %d", step_num)
    )
    
    # Save step results
    saveRDS(results, step_file)
    
    return(results)
  }
  
  # Define nrounds sequence - REVERTED TO ORIGINAL RANGES
  nrounds_seq <- if (target_col == "pain_1y") {
    seq(50, 500, by = 50)  # Original: 10 values from 50 to 500
  } else {
    seq(from = 60, to = 300, by = 30)  # Original: 9 values from 60 to 300
  }
  
  # STEP 1: Tuning nrounds and eta - ORIGINAL PARAMETER RANGES
  tune_grid1 <- expand.grid(
    nrounds = nrounds_seq,
    eta = if (target_col == "pain_1y") c(0.02, 0.03, 0.05, 0.07, 0.1, 0.15) else c(0.02, 0.03, 0.05, 0.07, 0.1),
    max_depth = if (target_col == "pain_1y") c(3, 4, 5, 6, 7, 8) else c(3, 4, 5, 6),
    min_split_loss = 0,
    colsample_bytree = 1,
    min_child_weight = if (target_col == "pain_1y") c(1, 2, 3) else c(3, 5, 7),
    subsample = 1,
    objective = "reg:squarederror"
  )
  
  results1 <- run_step(1, tune_grid1, "Tuning nrounds and eta")
  best_row1 <- results1[which.min(results1$rmse), ]
  
  # STEP 2: Tuning max_depth and min_child_weight - ORIGINAL PARAMETER RANGES
  tune_grid2 <- expand.grid(
    eta = best_row1$eta, 
    nrounds = nrounds_seq,
    max_depth = if (target_col == "pain_1y") seq(4, 8, by = 1) else c(3, 4, 5, 6),  
    min_split_loss = 0,
    colsample_bytree = 1,
    min_child_weight = if (target_col == "pain_1y") seq(1, 6, by = 1) else seq(1, 6, by = 1),
    subsample = 1
  )
  
  results2 <- run_step(2, tune_grid2, "Tuning max_depth and min_child_weight")
  best_row2 <- results2[which.min(results2$rmse), ]
  
  # STEP 3: Tuning colsample_bytree and subsample
  tune_grid3 <- expand.grid(
    nrounds = if (target_col == "pain_1y") nrounds_seq else seq(best_row2$best_iteration - 50, best_row2$best_iteration + 50, by = 5),
    eta = best_row2$eta,
    max_depth = best_row2$max_depth,
    min_split_loss = 0,
    colsample_bytree = if (target_col == "pain_1y") c(0.75, 0.775, 0.8, 0.825, 0.85, 0.9, 0.95, 1) else c(0.75, 0.8, 0.85, 0.9, 1),
    min_child_weight = best_row2$min_child_weight,
    subsample = if (target_col == "pain_1y") c(0.85, 0.9, 0.95, 0.975, 1) else c(0.85, 0.9, 0.95, 0.975, 1)
  )
  
  results3 <- run_step(3, tune_grid3, "Tuning colsample_bytree and subsample")
  best_row3 <- results3[which.min(results3$rmse), ]
  
  # STEP 4: Tuning min_split_loss (gamma)
  tune_grid4 <- expand.grid(
    nrounds = nrounds_seq,
    eta = best_row1$eta,
    max_depth = best_row2$max_depth,
    min_split_loss = if (target_col == "pain_1y") c(0, 0.5, 1.0, 1.5) else c(0, 0.25, 0.5, 1.0),
    colsample_bytree = best_row3$colsample_bytree,
    min_child_weight = best_row2$min_child_weight,
    subsample = best_row3$subsample
  )
  
  results4 <- run_step(4, tune_grid4, "Tuning min_split_loss (gamma)")
  best_row4 <- results4[which.min(results4$rmse), ]
  
  # STEP 5: Final tuning (lower eta)
  if (target_col == "pain_1y") {
    fine_nrounds_seq <- seq(from = 50, to = 80, by = 1)
  } else {
    fine_nrounds_seq <- seq(best_row4$best_iteration - 20, best_row4$best_iteration + 20, by = 2)
  }
  
  tune_grid5 <- expand.grid(
    nrounds = fine_nrounds_seq,
    eta = if (target_col == "pain_1y") c(0.03, 0.05, 0.07, 0.08, 0.1, 0.12) else c(0.03, 0.05, 0.07, 0.08, 0.1),
    max_depth = best_row2$max_depth,
    min_split_loss = best_row4$min_split_loss,
    colsample_bytree = if (target_col == "pain_1y") best_row3$colsample_bytree else c(0.7, 0.8, 0.9, 1.0),
    min_child_weight = best_row2$min_child_weight,
    subsample = best_row3$subsample
  )
  
  results5 <- run_step(5, tune_grid5, "Final tuning (lower eta)")
  best_row5 <- results5[which.min(results5$rmse), ]
  
  # STEP 6: Local refinement around best settings
  tune_grid6 <- expand.grid(
    nrounds = pmax(1, seq(best_row5$best_iteration - 10, best_row5$best_iteration + 10, by = 2)),
    eta = round(if (target_col == "pain_1y") best_row5$eta * c(0.9, 1, 1.1) else best_row5$eta * c(0.95, 1, 1.05), 5),
    max_depth = best_row5$max_depth + c(-1, 0, 1),
    min_split_loss = if (target_col == "pain_1y") best_row5$min_split_loss * c(0.5, 1, 1.5) else unique(c(best_row5$min_split_loss, 0.1, 0.5)),
    colsample_bytree = best_row5$colsample_bytree * c(0.95, 1),
    min_child_weight = best_row5$min_child_weight + c(-1, 0, 1),
    subsample = best_row5$subsample * c(0.95, 1)
  ) %>%
    filter(max_depth >= 1, min_child_weight >= 0, colsample_bytree <= 1, subsample <= 1)
  
  results6 <- run_step(6, tune_grid6, "Local refinement around best config")
  best_row6 <- results6[which.min(results6$rmse), ]
  
  # Create final parameter list
  final_params <- list(
    objective = "reg:squarederror",
    eta = best_row6$eta,
    max_depth = best_row6$max_depth,
    min_split_loss = best_row6$min_split_loss,
    colsample_bytree = best_row6$colsample_bytree,
    min_child_weight = best_row6$min_child_weight,
    subsample = best_row6$subsample,
    input_nrounds = best_row6$input_nrounds,
    nrounds = best_row6$best_iteration,
    rmse = best_row6$rmse,
    mae = best_row6$mae,
    best_iter = best_row6$best_iteration
  )
  
  # Save all step results for analysis
  all_results <- list(
    step1 = results1,
    step2 = results2,
    step3 = results3,
    step4 = results4,
    step5 = results5,
    step6 = results6,
    final_params = final_params
  )
  
  # Save the consolidated results
  all_steps_file <- sprintf("data/stepwise_all_steps_%s.rds", target_col)
  saveRDS(all_results, all_steps_file)
  cat(sprintf("All step results saved to %s\n", all_steps_file))
  
  # Save the final parameters
  saveRDS(final_params, result_file)
  cat(sprintf("Final parameters saved to %s\n", result_file))
  
  cat("Stepwise tuning completed successfully!\n")
  cat("Best parameters found:\n")
  for (param_name in names(final_params)) {
    cat(sprintf("  %s: %s\n", param_name, final_params[[param_name]]))
  }
  
  return(final_params)
}


#' Bayesian Optimization of XGBoost Hyperparameters
#'
#' Performs Bayesian hyperparameter tuning for an XGBoost regression model using
#' 10x5-fold cross-validation. The function supports two target variables, each with 
#' tailored parameter bounds and acquisition strategies. Results are saved and reused 
#' if available, unless `resume = FALSE`.
#'
#' @param data A data.frame containing the training data. Must include `fid` and the target column.
#' @param target_col Character. Name of the target column (`"pain_1y"`).
#' @param seed Integer. Random seed for reproducibility (default: 199987).
#' @param iters_n Integer. Number of iterations for Bayesian optimization (default: 50).
#' @param init_points Integer. Number of initial random evaluations (default: 15).
#' @param resume Logical. If `TRUE`, loads existing results from disk if available (default: TRUE).
#' @param early_stopping_rounds Integer. Early stopping parameter passed to `xgb.cv` (default: 20).
#'
#' @return A named list containing the best hyperparameters (`eta`, `max_depth`, etc.),
#' the number of boosting rounds (`best_iter`), and evaluation metrics (`rmse`, `mae`).
#' Returns `NULL` if optimization fails.
bayes_tune_xgb_hyperparams <- function(data, target_col, seed = 199987, 
                                       iters_n = 50, 
                                       init_points = 15,
                                       resume = TRUE,
                                       early_stopping_rounds = 20) {
  # Prepare Data
  predictors <- colnames(data %>% select(-fid, -all_of(target_col)))
  X_train <- as.matrix(data %>% select(-fid, -all_of(target_col)))
  y_train <- data[[target_col]]
  
  cat("Data prepared for Bayesian tuning.\n")
  cat(sprintf("Target: %s, Features: %d, Samples: %d\n", 
              target_col, ncol(X_train), nrow(X_train)))
  
  # Check for missing values
  na_count <- sum(is.na(X_train))
  if (na_count > 0) {
    cat(sprintf("WARNING: Data contains %d missing values.\n", na_count))
  }
  
  # Convert training data to DMatrix
  dtrain <- xgb.DMatrix(data = X_train, label = y_train, missing = NA)
  
  # Cross-validation folds
  set.seed(seed)
  folds <- my_rep_folds(data, IDvarn = "fid", nreps = 10, nfolds = 5, seed = seed)
  cat(sprintf("Created %d cross-validation folds.\n", length(folds)))
  
  # Resume check
  result_file <- sprintf("data/bayesian_opt_results_%s.rds", target_col)
  if (resume && file.exists(result_file)) {
    cat(sprintf("Loading existing Bayesian optimization results for %s\n", target_col))
    return(readRDS(result_file))
  }
  
  # Objective function for Bayesian optimization
  xgb_cv_bayes <- function(eta_raw, max_depth, min_split_loss, colsample_bytree, 
                           min_child_weight, subsample, nrounds) {
    
    tryCatch({
      # Handle log-scaling for eta
      eta <- exp(eta_raw)
      # Parameter bounds checking for pain_1y
      eta <- max(0.01, min(0.1, eta))
      max_depth <- max(3, min(6, as.integer(round(max_depth))))
      min_split_loss <- max(0, min(1.5, min_split_loss))
      colsample_bytree <- max(0.6, min(1.0, colsample_bytree))
      min_child_weight <- max(1, min(6, as.integer(round(min_child_weight))))
      subsample <- max(0.7, min(1.0, subsample))
      nrounds <- max(50, min(250, as.integer(round(nrounds))))
      
      params <- list(
        objective = "reg:squarederror",
        eta = eta,
        max_depth = max_depth,
        min_split_loss = min_split_loss,
        colsample_bytree = colsample_bytree,
        min_child_weight = min_child_weight,
        subsample = subsample
      )
      
      # Use 10x5 CV
      rep_results <- list()
      for (rep_idx in seq(1, length(folds), by = 5)) {
        fold_subset <- folds[rep_idx:(rep_idx + 4)]
        
        cv_result <- xgb.cv(
          params = params,
          data = dtrain,
          nrounds = nrounds,
          folds = fold_subset,
          metrics = "rmse",
          early_stopping_rounds = min(20, nrounds %/% 4),
          verbose = 0,
          prediction = TRUE
        )
        
        rep_results[[length(rep_results) + 1]] <- min(cv_result$evaluation_log$test_rmse_mean)
      }
      
      # Average RMSE across all 10 repetitions
      final_rmse <- mean(unlist(rep_results))
      
      score <- -final_rmse  # Negative because Bayesian maximizes
      
      cat(sprintf("Score: %.4f (RMSE: %.4f) | eta=%.3f, d=%d, g=%.2f, c=%.2f, ch=%d, s=%.2f\n",
                  score, final_rmse, eta, max_depth, min_split_loss, 
                  colsample_bytree, min_child_weight, subsample))
      
      return(list(Score = score, Pred = score))
      
    }, error = function(e) {
      # Return random bad scores to maintain variance
      bad_score <- runif(1, -25, -15)
      cat(sprintf("Error: %s | Returning: %.4f\n", e$message, bad_score))
      return(list(Score = bad_score, Pred = bad_score))
    })
  }
  
  # Search space bounds - wider ranges based on diagnostics
  bounds <- list(
    eta_raw = c(log(0.01), log(0.07)),
    max_depth = c(3, 6),
    min_split_loss = c(0, 1.5),
    colsample_bytree = c(0.6, 1.0),
    min_child_weight = c(1, 6),
    subsample = c(0.7, 1.0),
    nrounds = c(50, 250)
  )
  
  cat("Starting Bayesian Optimization...\n")
  set.seed(seed)
  
  # Define acquisition parameters based on target
  acq_type <- "ucb"
  acq_kappa <- 2.576
  
  # Run optimization with error handling - force sequential for stability
  opt_results <- tryCatch({
    bayesOpt(
      FUN = xgb_cv_bayes,
      bounds = bounds,
      initPoints = init_points,
      iters.n = iters_n,
      acq = acq_type,
      kappa = acq_kappa %||% NULL,  # Use only if not NULL
      gsPoints = 50,
      verbose = 1
    )
  }, error = function(e) {
    cat(sprintf("Bayesian optimization failed: %s\n", e$message))
    return(NULL)
  })
  
  if (is.null(opt_results)) {
    cat("Bayesian optimization failed. Returning NULL.\n")
    return(NULL)
  }

  # Extract best parameters
  best_params <- getBestPars(opt_results)
  
  # Defensive check in case optimization failed silently
  if (is.null(best_params)) {
    cat("Bayesian optimization did not yield any best parameters. Returning NULL.\n")
    return(NULL)
  }
  
  # Ensure correct integer rounding
  best_params$max_depth <- as.integer(round(best_params$max_depth))
  best_params$min_child_weight <- as.integer(round(best_params$min_child_weight))
  best_params$nrounds <- as.integer(round(best_params$nrounds))
  
  # Transform eta_raw if needed
  best_params$eta <- if ("eta_raw" %in% names(best_params)) {
    if (target_col == "pain_1y") exp(best_params$eta_raw) else best_params$eta_raw
  } else {
    best_params$eta
  }
  best_params$eta_raw <- NULL
  
  # Check if optimization succeeded
  if (!is.null(opt_results)) {
    cat("Bayesian optimization succeeded.\n")
  } else {
    cat("Bayesian tuning returned NULL - optimization failed\n")
    return(NULL)
  }
  
  # Final evaluation with all folds (no noise)
  final_cv <- xgb.cv(
    params = list(
      objective = "reg:squarederror",
      eta = best_params$eta,
      max_depth = best_params$max_depth,
      min_split_loss = best_params$min_split_loss,
      colsample_bytree = best_params$colsample_bytree,
      min_child_weight = best_params$min_child_weight,
      subsample = best_params$subsample
    ),
    data = dtrain,
    nrounds = best_params$nrounds,
    folds = folds,  # Use all 50 folds for final evaluation
    metrics = c("rmse", "mae"),
    verbose = 0
  )
  
  best_iter <- which.min(final_cv$evaluation_log$test_rmse_mean)
  
  final_result <- c(
    list(objective = "reg:squarederror"),
    best_params[order(names(best_params))],
    list(
      best_iter = best_iter,
      rmse = min(final_cv$evaluation_log$test_rmse_mean),
      mae = min(final_cv$evaluation_log$test_mae_mean)
    )
  )
  
  # Save results
  saveRDS(opt_results, sprintf("data/bayesian_opt_full_results_%s.rds", target_col))
  saveRDS(final_result, result_file)
  cat("Bayesian optimization completed successfully.\n")
  
  return(final_result)
}


#' Main Hyperparameter Tuning Function with Enhanced Comparison
#'
#' @param data Training data
#' @param target_col Target column name
#' @param seed Random seed
#' @param n_cores Number of cores for parallel processing
#' @param resume Whether to resume from saved results
#' @param method Tuning method: "bayesian", "stepwise", or "both"
#'
#' @return List of tuned hyperparameters (and comparison results if method="both")
tune_xgb_hyperparams <- function(data, target_col, seed = 199987, 
                                 n_cores = parallel::detectCores() - 1, 
                                 resume = TRUE, method = "bayesian") {
  
  # Validate method parameter
  if (!method %in% c("bayesian", "stepwise", "both")) {
    stop("Method must be one of: 'bayesian', 'stepwise', or 'both'")
  }
  
  if (method == "both") {
    # Run both methods and return all results
    results <- list()
    
    # Run Bayesian method
    bayesian_params <- NULL
    tryCatch({
      cat("Running Bayesian hyperparameter tuning...\n")
      bayesian_params <- bayes_tune_xgb_hyperparams(
        data = data,
        target_col = target_col,
        seed = seed,
        iters_n = 70,
        init_points = if (target_col == "pain_1y") 20 else 10,
        resume = resume
      )
      results$bayesian <- list(params = bayesian_params)
      cat("Bayesian tuning completed successfully.\n")
    }, error = function(e) {
      cat("Bayesian tuning failed:", e$message, "\n")
      bayesian_params <- NULL
    })
    
    # Run stepwise method
    stepwise_params <- NULL
    tryCatch({
      cat("Running stepwise hyperparameter tuning...\n")
      stepwise_params <- tune_xgb_stepwise(
        data = data,
        target_col = target_col,
        seed = seed,
        n_cores = n_cores,
        resume = resume
      )
      results$stepwise <- list(params = stepwise_params)
      cat("Stepwise tuning completed successfully.\n")
    }, error = function(e) {
      cat("Stepwise tuning failed:", e$message, "\n")
      stepwise_params <- NULL
    })
    
    # Compare the methods and save the comparison
    if (!is.null(bayesian_params) && !is.null(stepwise_params)) {
      comparison <- data.frame(
        Parameter = c(
          "eta", "max_depth", "min_split_loss", "colsample_bytree",
          "min_child_weight", "subsample", "nrounds", "RMSE", "MAE"
        ),
        Bayesian = c(
          bayesian_params$eta, bayesian_params$max_depth, 
          bayesian_params$min_split_loss, bayesian_params$colsample_bytree,
          bayesian_params$min_child_weight, bayesian_params$subsample, 
          bayesian_params$nrounds, bayesian_params$rmse, bayesian_params$mae
        ),
        Stepwise = c(
          stepwise_params$eta, stepwise_params$max_depth, 
          stepwise_params$min_split_loss, stepwise_params$colsample_bytree,
          stepwise_params$min_child_weight, stepwise_params$subsample, 
          stepwise_params$nrounds, stepwise_params$rmse, stepwise_params$mae
        )
      )
      
      results$comparison <- comparison
      saveRDS(results, sprintf("data/all_tuning_results_%s.rds", target_col))
      
      # Print comparison
      cat("\nComparison of Bayesian and Stepwise Tuning:\n")
      print(comparison)
      
      # Determine which method to return based on RMSE (consistent with objective)
      bayesian_metric <- if (!is.null(bayesian_params$rmse) && !is.na(bayesian_params$rmse)) {
        bayesian_params$rmse
      } else {
        Inf  # Use a high value if RMSE is missing
      }
      
      stepwise_metric <- if (!is.null(stepwise_params$rmse) && !is.na(stepwise_params$rmse)) {
        stepwise_params$rmse
      } else {
        Inf  # Use a high value if RMSE is missing
      }
      
      if (bayesian_metric < stepwise_metric) {
        cat("Bayesian method showed better RMSE. Using Bayesian parameters.\n")
        return(bayesian_params)
      } else {
        cat("Stepwise method showed better RMSE. Using Stepwise parameters.\n")
        return(stepwise_params)
      }
    } else if (!is.null(bayesian_params)) {
      cat("Only Bayesian method succeeded. Using Bayesian parameters.\n")
      return(bayesian_params)
    } else if (!is.null(stepwise_params)) {
      cat("Only Stepwise method succeeded. Using Stepwise parameters.\n")
      return(stepwise_params)
    } else {
      cat("Both methods failed. Using default parameters.\n")
      # Return default parameters
      return(list(
        objective = if (target_col == "pain_1y") "reg:squarederror" else "reg:squarederror",
        eta = 0.03,
        max_depth = 6,
        min_split_loss = 0,
        colsample_bytree = 0.8,
        min_child_weight = 1,
        subsample = 0.8,
        nrounds = 100
      ))
    }
    
  } else if (method == "bayesian") {
    # Call the Bayesian optimization function
    if (target_col == "pain_1y") {
      iters_n <- 70
      init_points <- 20
    } else {
      iters_n <- 70
      init_points <- 10
    }
    
    return(bayes_tune_xgb_hyperparams(
      data = data,
      target_col = target_col,
      seed = seed,
      iters_n = iters_n,
      init_points = init_points,
      resume = resume
    ))
  } else { # method == "stepwise"
    return(tune_xgb_stepwise(
      data = data,
      target_col = target_col,
      seed = seed,
      n_cores = n_cores,
      resume = resume
    ))
  }
}


# Utility function for safe access to list elements
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Log a Message
#'
#' Writes a timestamped message to the tuning log file and echoes it
#' to the console.
#'
#' @param msg Character string to log.
log_message("Enhanced tuning script execution completed")