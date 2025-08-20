#' Bootstrapped Prediction with XGBoost and Feature Importance Estimation
#'
#' This function performs bootstrap resampling with replacement to estimate prediction intervals (PIs) 
#' and feature importance by training multiple XGBoost models on different bootstrap samples.
#' The residual distributions from training and validation sets are combined to adjust prediction intervals.
#' 
#' @param dat_train A data frame containing the training data with predictors and target variable
#' @param new_dat A data frame containing new data for which predictions are required
#' @param X A character vector specifying the names of predictor columns
#' @param target_col A character string specifying the name of the target variable
#' @param grd A list specifying the hyperparameters for the XGBoost model
#' @param nb An integer specifying the number of bootstrap iterations (default: 500)
#' @param alpha A numeric value for the significance level used in PI (default: 0.20)
#' @param seed An integer for reproducibility (default: 199987)
#' @param ceiling_val A numeric value to cap predictions (default: 10)
#' @param ceiling_threshold A numeric value above which observations get additional weight (default: NULL)
#' @param ceiling_weight A numeric multiplier for observations above ceiling_threshold (default: 1.0)
#' @param ncores An integer specifying the number of cores (default: detectCores() - 1)
#'
#' @return A list containing predictions, bootstrap_results, importance_summary, and diagnostics
XGB_bootstrap_PI_importance <- function(dat_train, new_dat, X, target_col, grd, nb = 500, alpha = 0.20, seed = 199987, ceiling_val = 10, ceiling_threshold = NULL, ceiling_weight = 1.0, ncores = parallel::detectCores() - 1) {
  
  n <- nrow(dat_train)
  nd <- nrow(new_dat)
  
  # Generate reproducible seed vector
  set.seed(seed)  # Master seed for reproducibility
  bootstrap_seeds <- sample.int(1000000, nb)
  
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  
  results <- foreach(b = 1:nb, .packages = c("xgboost", "dplyr")) %dopar% {
    # Use the pre-generated seed for this iteration
    set.seed(bootstrap_seeds[b])
    
    # Row-level bootstrap
    boot_indices <- sample(1:n, size = n, replace = TRUE)
    oob_indices <- setdiff(1:n, unique(boot_indices))
    if (length(oob_indices) < 10) {
      return(NULL)  # Skip iteration with too small OOB set
    }
    
    bt_train <- dat_train[boot_indices, ]
    bt_val <- dat_train[oob_indices, ]
    
    # Calculate sample weights for ceiling weighting (NEW SECTION)
    if (!is.null(ceiling_threshold) && ceiling_weight > 1.0) {
      # Identify observations above ceiling threshold
      ceiling_observations <- bt_train[[target_col]] >= ceiling_threshold
      
      # Create weight vector: ceiling_weight for ceiling observations, 1.0 for others
      sample_weights <- ifelse(ceiling_observations, ceiling_weight, 1.0)
      
      cat(sprintf("Bootstrap %d: %d/%d observations above ceiling (%.1f%%), weighted %.1fx\n",
                  b, sum(ceiling_observations), nrow(bt_train), 
                  100 * mean(ceiling_observations), ceiling_weight))
    } else {
      # No ceiling weighting - all observations get weight 1.0
      sample_weights <- rep(1.0, nrow(bt_train))
    }
    
    # Create DMatrix with sample weights
    dtrain <- xgb.DMatrix(
      data = as.matrix(bt_train[, X]), 
      label = bt_train[[target_col]], 
      weight = sample_weights,  # Apply ceiling weights here
      missing = NA
    )
    
    xgb_model <- xgboost(
      params = as.list(grd),
      data = dtrain,
      nrounds = grd$nrounds,
      verbose = 0
    )
    
    imp <- xgb.importance(feature_names = X, model = xgb_model)
    
    preds_train <- predict(xgb_model, as.matrix(bt_train[, X]))
    preds_new <- predict(xgb_model, as.matrix(new_dat[, X]))
    
    train_res <- bt_train[[target_col]] - preds_train
    
    if (nrow(bt_val) > 0) {
      preds_val <- predict(xgb_model, as.matrix(bt_val[, X]))
      val_res <- bt_val[[target_col]] - preds_val
      
      n_bt <- nrow(bt_train)
      y_mat <- matrix(rep(bt_train[[target_col]], each = n_bt), nrow = n_bt)
      preds_mat <- matrix(rep(preds_train, each = n_bt), nrow = n_bt, byrow = FALSE)
      gamma <- sqrt(mean((y_mat - preds_mat)^2))  # New RMSE
      
      mtr <- mean(train_res)
      mvr <- mean(val_res)
      
      ROR_raw <- if (abs(gamma - mtr) > 1e-10) abs(mvr - mtr) / abs(gamma - mtr) else 1.0
      ROR <- min(ROR_raw, 10)  # Cap ROR at a reasonable value
      wf <- min(max(0.632 / (1 - 0.368 * ROR), 0), 1)
      
      train_res_samples <- sample(train_res, size = nd, replace = TRUE)
      val_res_samples <- sample(val_res, size = nd, replace = TRUE)
      eps <- (1 - wf) * train_res_samples + wf * val_res_samples
    } else {
      ROR <- 0
      wf <- 0.632
      eps <- sample(train_res, size = nd, replace = TRUE)
      gamma <- 0
    }
    
    list(
      base_preds = preds_new,
      preds_with_noise = preds_new + eps,
      imp = imp,
      val_size = nrow(bt_val),
      wf = wf,
      ror = ROR,
      gamma = gamma,
      ceiling_obs_count = if (!is.null(ceiling_threshold)) sum(ceiling_observations) else 0  # Track ceiling observations
    )
  }
  
  stopCluster(cl)
  
  # Check OOB filtering and ceiling weighting stats
  total_iterations <- nb
  null_iterations <- sum(sapply(results, is.null))
  cat("Total bootstrap iterations:", total_iterations, "\n")
  cat("Iterations skipped due to small OOB:", null_iterations, " (", 
      round(100 * null_iterations / total_iterations, 1), "%)\n")
  
  # Report ceiling weighting statistics (NEW SECTION)
  if (!is.null(ceiling_threshold) && ceiling_weight > 1.0) {
    valid_results <- Filter(Negate(is.null), results)
    ceiling_counts <- sapply(valid_results, function(x) x$ceiling_obs_count)
    cat(sprintf("Ceiling weighting active: threshold=%.0f, weight=%.1fx\n", 
                ceiling_threshold, ceiling_weight))
    cat(sprintf("Avg ceiling observations per bootstrap: %.1f (%.1f%%)\n",
                mean(ceiling_counts), 100 * mean(ceiling_counts) / n))
  }
  
  results <- Filter(Negate(is.null), results)  # Drop skipped iterations
  actual_nb <- length(results)
  
  base_preds_matrix <- matrix(nrow = actual_nb, ncol = nd)
  ystar <- matrix(nrow = actual_nb, ncol = nd)
  wf_vals <- numeric(actual_nb)
  val_sizes <- numeric(actual_nb)
  ror_vals <- numeric(actual_nb)
  gamma_vals <- numeric(actual_nb)
  
  for (b in 1:actual_nb) {
    base_preds_matrix[b, ] <- results[[b]]$base_preds
    ystar[b, ] <- results[[b]]$preds_with_noise
    val_sizes[b] <- results[[b]]$val_size
    wf_vals[b] <- results[[b]]$wf
    ror_vals[b] <- results[[b]]$ror
    gamma_vals[b] <- results[[b]]$gamma
  }
  
  ystar <- apply(ystar, 2, function(x) pmax(pmin(x, ceiling_val), 0))
  base_preds_matrix <- apply(base_preds_matrix, 2, function(x) pmax(pmin(x, ceiling_val), 0))
  percentiles <- apply(ystar, 2, quantile, probs = c(alpha / 2, 1 - alpha / 2))
  preds <- apply(base_preds_matrix, 2, median)
  
  preds_df <- data.frame(
    fid = new_dat$fid,
    Predicted = preds,
    lb = percentiles[1, ],
    ub = percentiles[2, ],
    Actual = new_dat[[target_col]]
  )
  
  if (!is.null(new_dat[[target_col]])) {
    actual <- new_dat[[target_col]]
    
    # Calculate per-bootstrap performance metrics (for CI and means)
    mae_vals <- apply(base_preds_matrix, 1,
                      function(pred) mean(abs(actual - pred), na.rm = TRUE))
    rmse_vals <- apply(base_preds_matrix, 1,
                       function(pred) sqrt(mean((actual - pred)^2, na.rm = TRUE)))
    r2_vals  <- apply(base_preds_matrix, 1,
                      function(pred) cor(actual, pred, use = "complete.obs")^2)
    MAE_CI  <- quantile(mae_vals, probs = c(0.025, 0.975), na.rm = TRUE)
    RMSE_CI <- quantile(rmse_vals, probs = c(0.025, 0.975), na.rm = TRUE)
    R2_CI   <- quantile(r2_vals, probs = c(0.025, 0.975), na.rm = TRUE)
    
    # Calculate individual prediction SDs
    # For each prediction, calculate SD across bootstrap iterations
    individual_pred_sds <- apply(base_preds_matrix, 2, sd)
    
    # Calculate individual error SDs for each observation
    error_matrix <- sweep(base_preds_matrix, 2, actual, "-")  # prediction errors
    abs_error_matrix <- abs(error_matrix)
    squared_error_matrix <- error_matrix^2
    
    # Mean of individual prediction SDs
    mean_pred_sd <- mean(individual_pred_sds, na.rm = TRUE)
    
    # Alternative: SD of all individual errors pooled together
    all_errors <- as.vector(error_matrix)
    all_abs_errors <- as.vector(abs_error_matrix)
    
    performance_summary <- list(
      RMSE_mean = mean(rmse_vals, na.rm = TRUE),
      RMSE_sd   = sd(all_errors, na.rm = TRUE),
      RMSE_CI   = RMSE_CI,
      MAE_mean  = mean(mae_vals, na.rm = TRUE),
      MAE_sd    = sd(all_abs_errors, na.rm = TRUE),
      MAE_CI    = MAE_CI,
      R2_mean   = mean(r2_vals, na.rm = TRUE),
      R2_sd     = sd(r2_vals, na.rm = TRUE),
      R2_CI     = R2_CI,
      MAE_median = median(mae_vals, na.rm = TRUE),
      MAE_IQR    = quantile(mae_vals, probs = c(0.25, 0.75), na.rm = TRUE)
    )
  } else {
    performance_summary <- NULL
  }
  importance_list <- lapply(results, function(res) res$imp)
  importance_df <- bind_rows(importance_list)
  importance_summary <- importance_df %>%
    group_by(Feature) %>%
    summarise(
      MeanGain = mean(Gain, na.rm = TRUE),
      MeanCover = mean(Cover, na.rm = TRUE),
      MeanFrequency = mean(Frequency, na.rm = TRUE)
    ) %>%
    arrange(desc(MeanGain))
  
  return(list(
    predictions = preds_df, 
    bootstrap_results = ystar,
    base_predictions = base_preds_matrix,
    importance_summary = importance_summary,
    performance_summary = performance_summary,
    diagnostics = list(
      wf_vals = wf_vals,
      val_sizes = val_sizes,
      ror_vals = ror_vals,
      gamma_vals = gamma_vals
    )
  ))
}

#' Empirically calibrate alpha for target coverage
#' 
#' @param dat_train Training data
#' @param new_dat Test data  
#' @param X Feature names
#' @param target_col Target column name
#' @param grd Model parameters
#' @param target_coverage Desired coverage (e.g., 80)
#' @param ceiling_threshold Ceiling threshold (NULL for pain model)
#' @param ceiling_weight Ceiling weight (1.0 for pain model)
#' @return Optimal alpha value
calibrate_alpha <- function(dat_train, new_dat, X, target_col, grd, 
                            target_coverage = 80, ceiling_threshold = NULL, 
                            ceiling_weight = 1.0, seed = 199987) {
  
  # Test alpha values
  alpha_candidates <- c(0.1, 0.15, 0.20, 0.25, 0.30)
  coverage_results <- numeric(length(alpha_candidates))
  
  cat(sprintf("Calibrating alpha for %s model (target coverage: %.1f%%)\n", 
              target_col, target_coverage))
  
  for (i in seq_along(alpha_candidates)) {
    alpha <- alpha_candidates[i]
    
    # Run bootstrap with this alpha
    if (is.null(ceiling_threshold)) {
      # Pain model - no ceiling weighting
      result <- XGB_bootstrap_PI_importance(
        dat_train = dat_train,
        new_dat = new_dat, 
        X = X,
        target_col = target_col,
        grd = grd,
        nb = 500,
        alpha = alpha,  # This is the key parameter
        seed = seed,
        ceiling_val = if(target_col == "pain_1y") 10 else 100
      )
    } else {
      # Function model - with ceiling weighting  
      result <- XGB_bootstrap_PI_importance(
        dat_train = dat_train,
        new_dat = new_dat,
        X = X, 
        target_col = target_col,
        grd = grd,
        nb = 500,
        alpha = alpha,  # This is the key parameter
        seed = seed,
        ceiling_val = 100,
        ceiling_threshold = ceiling_threshold,
        ceiling_weight = ceiling_weight
      )
    }
    
    # Calculate actual coverage
    preds <- result$predictions
    within_interval <- (preds$Actual >= preds$lb) & (preds$Actual <= preds$ub)
    coverage <- mean(within_interval) * 100
    coverage_results[i] <- coverage
    
    cat(sprintf("  Alpha %.2f → Coverage: %.1f%%\n", alpha, coverage))
  }
  
  # Find alpha closest to target coverage
  best_idx <- which.min(abs(coverage_results - target_coverage))
  optimal_alpha <- alpha_candidates[best_idx]
  achieved_coverage <- coverage_results[best_idx]
  
  cat(sprintf("Optimal alpha: %.2f (achieves %.1f%% coverage)\n", 
              optimal_alpha, achieved_coverage))
  
  return(list(
    optimal_alpha = optimal_alpha,
    achieved_coverage = achieved_coverage,
    all_alphas = alpha_candidates,
    all_coverages = coverage_results
  ))
}