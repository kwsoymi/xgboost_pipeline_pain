#' Evaluate and visualize XGBoost model performance
#' 
#' @param model Trained xgboost model (xgb.Booster)
#' @param y_true True outcome vector
#' @param y_pred Vector of predicted values
#' @param pi_lower Vector of lower bounds of prediction intervals (optional)
#' @param pi_upper Vector of upper bounds of prediction intervals (optional)
#' @param label Label used for plots ("Bayesian", "Stepwise")
#' @param save_prefix "pain_1y" to prefix file names
#' @return List with RMSE, MAE, R-squared, coverage, interval width,
#'   calibration slope/intercept with 95% CIs and MACE with 95% CI

evaluate_xgb_model <- function(model, y_true, y_pred, 
                               pi_lower = NULL, pi_upper = NULL,
                               label = "", save_prefix = "") {
  # library(ggplot2)
  # library(scales)
  dir.create("output", showWarnings = FALSE)
  
  # ---- Input Validation ----
  if (length(y_true) != length(y_pred)) {
    stop("Length of y_true and y_pred must be equal")
  }
  if (!is.null(pi_lower) && !is.null(pi_upper)) {
    if (length(y_true) != length(pi_lower) || length(y_true) != length(pi_upper)) {
      stop("Prediction intervals must have same length as y_true")
    }
  }
  
  # ---- Metrics ----
  rmse <- sqrt(mean((y_true - y_pred)^2, na.rm = TRUE))
  mae <- mean(abs(y_true - y_pred), na.rm = TRUE)
  r_squared <- cor(y_true, y_pred, use = "complete.obs")^2
  
  cat(sprintf("Model Performance (%s %s):\n", save_prefix, label))
  cat(sprintf("  RMSE: %.4f\n", rmse))
  cat(sprintf("  MAE: %.4f\n", mae))
  cat(sprintf("  R²: %.4f\n", r_squared))
  
  # ---- Residuals ----
  residuals <- y_true - y_pred
  p1 <- ggplot(data.frame(y_pred, residuals), aes(x = y_pred, y = residuals)) +
    geom_point(alpha = 0.6, size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_smooth(method = "loess", se = TRUE, color = "blue", alpha = 0.3) +
    labs(title = paste(label, "Residual Plot"),
         subtitle = paste("Target:", save_prefix),
         x = "Predicted Values", y = "Residuals") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0),
          plot.subtitle = element_text(hjust = 0))
  ggsave(sprintf("output/%s_residuals_%s.png", save_prefix, tolower(label)), p1, width = 8, height = 6, dpi = 300)
  
    # ---- COMPREHENSIVE CALIBRATION ANALYSIS ----
  cat("\nCalibration Analysis:\n")
  
  # 1. CALIBRATION SLOPE (should be close to 1.0)
  cal_model <- lm(y_true ~ y_pred)
  cal_slope <- coef(cal_model)[2]
  cal_intercept <- coef(cal_model)[1]
  cal_slope_ci <- confint(cal_model)[2, ]
  cal_intercept_ci <- confint(cal_model)[1, ]
  cal_slope_p <- summary(cal_model)$coefficients[2, 4]
  
  cat(sprintf("  Calibration Slope: %.3f (95%% CI: %.3f-%.3f, p=%.3f)\n", 
              cal_slope, cal_slope_ci[1], cal_slope_ci[2], cal_slope_p))
  cat(sprintf("  Calibration Intercept: %.3f (95%% CI: %.3f-%.3f)\n",
              cal_intercept, cal_intercept_ci[1], cal_intercept_ci[2]))
  
  # Interpretation
  if (abs(cal_slope - 1.0) < 0.1) {
    cat("  → Slope interpretation: Good calibration\n")
  } else if (cal_slope < 0.9) {
    cat("  → Slope interpretation: Overfitting (predictions too extreme)\n")
  } else if (cal_slope > 1.1) {
    cat("  → Slope interpretation: Underfitting (predictions too conservative)\n")
  }
  
  # 2. CALIBRATION-IN-THE-LARGE
  mean_obs <- mean(y_true)
  mean_pred <- mean(y_pred)
  cal_large_diff <- mean_pred - mean_obs
  cal_large_test <- t.test(y_pred, y_true, paired = FALSE)
  
  cat(sprintf("  Mean Observed: %.3f\n", mean_obs))
  cat(sprintf("  Mean Predicted: %.3f\n", mean_pred))
  cat(sprintf("  Difference: %.3f (p=%.3f)\n", cal_large_diff, cal_large_test$p.value))
  
  # 3. BINNED CALIBRATION (Decile Analysis) - MACE calculation
  n_bins <- 10
  pred_quantiles <- quantile(y_pred, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)
  bins <- cut(y_pred, breaks = pred_quantiles, include.lowest = TRUE)
  
  # Calculate bin statistics using base R (no dplyr dependency)
  bin_data <- data.frame(y_true = y_true, y_pred = y_pred, bin = bins)
  bin_summary <- aggregate(bin_data[c("y_true", "y_pred")], by = list(bin = bin_data$bin), 
                           FUN = function(x) c(mean = mean(x), n = length(x), se = sd(x)/sqrt(length(x))))
  
  # Extract bin statistics
  bin_means_obs <- bin_summary$y_true[, "mean"]
  bin_means_pred <- bin_summary$y_pred[, "mean"] 
  bin_counts <- bin_summary$y_true[, "n"]
  bin_se_obs <- bin_summary$y_true[, "se"]
  
  # Only use bins with sufficient data
  valid_bins <- bin_counts >= 5
  if (sum(valid_bins) > 0) {
    # Mean Absolute Calibration Error (MACE)
    weighted_errors <- abs(bin_means_obs[valid_bins] - bin_means_pred[valid_bins]) * bin_counts[valid_bins]
    mace <- sum(weighted_errors) / sum(bin_counts[valid_bins])
    cat(sprintf("  Mean Absolute Calibration Error (MACE): %.3f\n", mace))
    
    # Bootstrap CI for MACE
    mace_boot <- boot::boot(
      data = bin_data[, c("y_true", "y_pred")],
      statistic = function(d, idx) {
        y_t <- d$y_true[idx]
        y_p <- d$y_pred[idx]
        pred_q <- quantile(y_p, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)
        b <- cut(y_p, breaks = pred_q, include.lowest = TRUE)
        d2 <- data.frame(y_true = y_t, y_pred = y_p, bin = b)
        s <- aggregate(d2[c("y_true", "y_pred")], by = list(bin = d2$bin),
                       FUN = function(x) c(mean = mean(x), n = length(x)))
        bm_obs <- s$y_true[, "mean"]
        bm_pred <- s$y_pred[, "mean"]
        bc <- s$y_true[, "n"]
        vb <- bc >= 5
        if (sum(vb) > 0) {
          we <- abs(bm_obs[vb] - bm_pred[vb]) * bc[vb]
          return(sum(we) / sum(bc[vb]))
        } else {
          return(NA)
        }
      },
      R = 200
    )
    mace_ci <- quantile(mace_boot$t, probs = c(0.025, 0.975), na.rm = TRUE)
    cat(sprintf("  MACE 95%% CI: %.3f-%.3f\n", mace_ci[1], mace_ci[2]))
    
    # Hosmer-Lemeshow style test for continuous outcomes
    hl_stat <- sum((bin_means_obs[valid_bins] - bin_means_pred[valid_bins])^2 * bin_counts[valid_bins])
    cat(sprintf("  HL-type statistic: %.3f\n", hl_stat))
  } else {
    mace <- NA
    hl_stat <- NA
    cat("  MACE: Cannot calculate (insufficient bin sizes)\n")
  }
  
  # 4. RESIDUAL CALIBRATION ANALYSIS
  residual_pred_cor <- cor(residuals, y_pred, use = "complete.obs")
  cat(sprintf("  Residual-Prediction Correlation: %.3f\n", residual_pred_cor))
  
  if (abs(residual_pred_cor) > 0.1) {
    cat("  → Warning: Systematic residual pattern detected\n")
  }
  
  cat("\n")
  
  # ---- ENHANCED CALIBRATION PLOTS ----
  calib_data <- data.frame(y_pred = y_pred, y_true = y_true)
  
  # Enhanced calibration plot with fitted line and metrics
  p2 <- ggplot(calib_data, aes(x = y_pred, y = y_true)) +
    geom_point(alpha = 0.6, size = 1.5, color = "steelblue") +
    # Add fitted calibration line (red)
    geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.2) +
    # Perfect calibration reference line (blue dashed)
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue", size = 1) +
    # Add LOESS for comparison (gray)
    geom_smooth(method = "loess", se = FALSE, color = "gray", alpha = 0.8, linetype = "dotted") +
    labs(
      title = paste(label, "Enhanced Calibration Plot"),
      subtitle = paste("Target:", save_prefix, 
                       "| Slope:", round(cal_slope, 3), 
                       "| Intercept:", round(cal_intercept, 3),
                       "| MACE:", if(!is.na(mace)) round(mace, 3) else "N/A"),
      x = "Predicted Values", 
      y = "Observed Values",
      caption = "Blue dashed = perfect calibration | Red = fitted line | Gray dotted = LOESS"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0, face = "bold"),
          plot.subtitle = element_text(hjust = 0),
          plot.caption = element_text(hjust = 0, size = 9))
  
  ggsave(sprintf("output/%s_calibration_%s.png", save_prefix, tolower(label)), 
         p2, width = 8, height = 6, dpi = 300)
  
  # ---- Distribution Comparison ----
  dist_data <- data.frame(
    Value = c(y_true, y_pred),
    Type = c(rep("Actual", length(y_true)), rep("Predicted", length(y_pred)))
  )
  
  # Side-by-side histograms
  p5a <- ggplot(dist_data, aes(x = Value, fill = Type)) +
    geom_histogram(bins = 30, alpha = 0.8, color = "white", size = 0.1) +
    facet_wrap(~ Type, ncol = 2, scales = "free_y") +
    scale_fill_manual(values = c("Actual" = "steelblue", "Predicted" = "darkorange"),
                      guide = "none") +  # Remove legend since facet labels are clear
    labs(title = paste(label, "Distribution Comparison"),
         subtitle = paste("Target:", save_prefix, "| Residual-Prediction r =", round(residual_pred_cor, 3)),
         x = "Value", y = "Frequency") +
    theme_minimal(base_size = 18) +
    theme(plot.title = element_text(hjust = 0, face = "bold"),
          plot.subtitle = element_text(hjust = 0),
          strip.text = element_text(size = 12, face = "bold"))
  
  # Save
  ggsave(sprintf("output/%s_distribution_sidebyside_%s.png", save_prefix, tolower(label)),
         p5a, width = 10, height = 6, dpi = 300)
  
  # Combined histogram overlay
  p5b <- ggplot(dist_data, aes(x = Value, fill = Type, color = Type)) +
    geom_histogram(aes(y = ..density..), bins = 30, position = "identity", alpha = 0.4) +
    scale_fill_manual(values = c("Actual" = "steelblue", "Predicted" = "darkorange")) +
    scale_color_manual(values = c("Actual" = "steelblue", "Predicted" = "darkorange")) +
    labs(x = "Value", y = "Density",
         fill = "Outcome", color = "Outcome") +
    theme_minimal(base_size = 18) +
    theme(plot.title = element_text(hjust = 0, face = "bold"),
          plot.subtitle = element_text(hjust = 0),
          legend.position = "none",
          axis.title = element_text(size = 16, face = "bold"),
          axis.text = element_text(size = 14))
  
  ggsave(sprintf("output/%s_distribution_combined_%s.png", save_prefix, tolower(label)),
         p5b, width = 10, height = 6, dpi = 300)
  
  # For pain models, add zero/low value analysis
  if (grepl("pain", save_prefix)) {
    cat("\n*** PAIN ZERO ANALYSIS ***\n")
    cat("Model:", label, "| Target:", save_prefix, "\n")
    
    zero_analysis <- data.frame(
      Category = c("Exactly 0", "≤ 1", "≤ 2", "> 5"),
      Actual_Count = c(
        sum(y_true == 0),
        sum(y_true <= 1), 
        sum(y_true <= 2),
        sum(y_true > 5)
      ),
      Actual_Percent = c(
        round(mean(y_true == 0) * 100, 1),
        round(mean(y_true <= 1) * 100, 1),
        round(mean(y_true <= 2) * 100, 1), 
        round(mean(y_true > 5) * 100, 1)
      ),
      Predicted_Percent = c(
        round(mean(y_pred <= 0.5) * 100, 1),  # "Essentially zero"
        round(mean(y_pred <= 1) * 100, 1),
        round(mean(y_pred <= 2) * 100, 1),
        round(mean(y_pred > 5) * 100, 1)
      )
    )
    
    # Manual table output
    cat("Category    | Actual_Count | Actual_% | Predicted_%\n")
    cat("------------|--------------|----------|------------\n")
    for(i in 1:nrow(zero_analysis)) {
      cat(sprintf("%-11s | %12d | %8.1f | %11.1f\n", 
                  zero_analysis$Category[i], 
                  zero_analysis$Actual_Count[i], 
                  zero_analysis$Actual_Percent[i],
                  zero_analysis$Predicted_Percent[i]))
    }
    cat("*** END PAIN ANALYSIS ***\n")
  }
  
  # ---- Prediction Interval Evaluation ----
  coverage <- NA
  median_width <- NA
  if (!is.null(pi_lower) && !is.null(pi_upper)) {
    in_interval <- y_true >= pi_lower & y_true <= pi_upper
    coverage <- mean(in_interval, na.rm = TRUE) * 100
    median_width <- median(pi_upper - pi_lower, na.rm = TRUE)
    
    cat(sprintf("  PI Coverage: %.2f%%\n", coverage))
    cat(sprintf("  Median PI Width: %.4f\n", median_width))
    
    # Interval width plot
    width_data <- data.frame(width = pi_upper - pi_lower)
    p3 <- ggplot(width_data, aes(x = width)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.8) +
      geom_vline(xintercept = median_width, linetype = "dashed", color = "red", size = 1) +
      labs(title = paste(label, "Prediction Interval Widths"),
           subtitle = paste("Target:", save_prefix, "| Median =", round(median_width, 3)),
           x = "Interval Width", y = "Count") +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0),
        plot.subtitle = element_text(hjust = 0),
        panel.grid.minor = element_blank()
      )
    ggsave(sprintf("output/%s_piwidths_%s.png", save_prefix, tolower(label)), p3, width = 8, height = 6, dpi = 300)
    
    # Coverage visualization
    coverage_data <- data.frame(
      y_true = y_true, y_pred = y_pred,
      pi_lower = pi_lower, pi_upper = pi_upper,
      covered = in_interval
    )
    p4 <- ggplot(coverage_data, aes(x = y_pred, y = y_true)) +
      geom_ribbon(aes(ymin = pi_lower, ymax = pi_upper),
                  fill = "#35B779", alpha = 0.2) +
      geom_point(aes(color = covered), alpha = 0.8, size = 2.5) +
      geom_abline(slope = 1, intercept = 0,
                  linetype = "dashed", color = "grey40") +
      scale_color_manual(
        values = c("TRUE" = "#0073C2", "FALSE" = "#D55E00"),
        labels = c("Inside PI", "Outside PI"), name = NULL
      ) +
      labs(
        title = paste(label, "Prediction Interval Coverage"),
        subtitle = paste("Target:", save_prefix,
                         "| Coverage:", round(coverage, 1), "%"),
        x = "Predicted Value", y = "Observed Value",
        caption = "Dashed line = perfect prediction"
      ) +
      theme_modern +
      theme(
        legend.position = "bottom",
        plot.subtitle = element_text(hjust = 0)
      )
    ggsave(sprintf("output/%s_coverage_%s.png", save_prefix, tolower(label)), p4, width = 8, height = 6, dpi = 300)
  
    # ---- Binned Coverage Analysis ----
    cat("Performing binned coverage analysis...\n")
    
    binned_coverage_summary <- calculate_interval_coverage_by_bin(
      pred = y_pred,
      obs = y_true, 
      lower = pi_lower,
      upper = pi_upper,
      bins = 5,
      method = "quantile"
    )
    
    # Print summary
    print_binned_coverage_summary(binned_coverage_summary, label)
    
    # Generate and save plot
    binned_plot <- plot_binned_coverage(binned_coverage_summary, label, save_prefix)
    if (!is.null(binned_plot)) {
      ggsave(sprintf("output/%s_binned_coverage_%s.png", save_prefix, tolower(label)), 
             binned_plot, width = 10, height = 6, dpi = 300)
    }
    
  } else {
    binned_coverage_summary <- NULL
  }
  
  cat("\n")
  
  # ---- Return results ----
  list(
    label = label,
    save_prefix = save_prefix,
    rmse = rmse,
    mae = mae,
    r_squared = r_squared,
    coverage = coverage,
    median_width = median_width,
    n_observations = length(y_true),
    # Calibration metrics
    calibration_slope = cal_slope,
    calibration_intercept = cal_intercept,
    calibration_intercept_ci = cal_intercept_ci,
    calibration_slope_ci = cal_slope_ci,
    calibration_slope_p = cal_slope_p,
    calibration_in_large = cal_large_diff,
    calibration_in_large_p = cal_large_test$p.value,
    mace = mace,
    mace_ci = if (exists("mace_ci")) mace_ci else c(NA, NA),
    hl_statistic = hl_stat,
    residual_pred_correlation = residual_pred_cor,
    binned_coverage = binned_coverage_summary
  )
}

#' Calculate prediction interval coverage by bins
#' 
#' @param pred Vector of predicted values 
#' @param obs Vector of observed values
#' @param lower Vector of lower bounds of prediction intervals
#' @param upper Vector of upper bounds of prediction intervals
#' @param bins Number of bins (default 5) or custom breaks
#' @param method Method for binning: "quantile" (default), "equal_width", or "custom"
#' @param min_bin_size Minimum observations per bin (default 10)
#' @return Data frame with bin statistics and coverage
calculate_interval_coverage_by_bin <- function(pred, obs, lower, upper, 
                                               bins = 5, method = "quantile", 
                                               min_bin_size = 10) {
  
  # Input validation
  if (length(pred) != length(obs) || length(pred) != length(lower) || length(pred) != length(upper)) {
    stop("All input vectors must have the same length")
  }
  
  # Remove missing values
  complete_cases <- complete.cases(pred, obs, lower, upper)
  if (sum(complete_cases) < min_bin_size) {
    warning("Insufficient complete cases for binned analysis")
    return(NULL)
  }
  
  pred <- pred[complete_cases]
  obs <- obs[complete_cases]
  lower <- lower[complete_cases]
  upper <- upper[complete_cases]
  
  # Create bins based on method
  if (method == "quantile") {
    # Quantile-based bins (equal sample sizes)
    bin_breaks <- quantile(pred, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE)
    bin_cut <- cut(pred, breaks = bin_breaks, include.lowest = TRUE)
  } else if (method == "equal_width") {
    # Equal-width bins
    bin_cut <- cut(pred, breaks = bins, include.lowest = TRUE)
  } else if (method == "custom" && is.numeric(bins)) {
    # Custom breaks provided
    bin_cut <- cut(pred, breaks = bins, include.lowest = TRUE)
  } else {
    stop("Invalid method. Use 'quantile', 'equal_width', or 'custom'")
  }
  
  # Calculate statistics for each bin
  bin_stats <- aggregate(
    data.frame(
      obs = obs,
      pred = pred, 
      lower = lower,
      upper = upper,
      covered = obs >= lower & obs <= upper,
      width = upper - lower
    ),
    by = list(bin = bin_cut),
    FUN = function(x) {
      c(
        n = length(x),
        mean = mean(x, na.rm = TRUE),
        median = median(x, na.rm = TRUE),
        sd = sd(x, na.rm = TRUE)
      )
    }
  )
  
  # Extract and format results
  result_df <- data.frame(
    Bin = levels(bin_cut),
    N = bin_stats$obs[, "n"],
    Mean_Pred = round(bin_stats$pred[, "mean"], 3),
    Mean_Obs = round(bin_stats$obs[, "mean"], 3),
    Coverage = round(bin_stats$covered[, "mean"], 3),
    Mean_Width = round(bin_stats$width[, "mean"], 3),
    Median_Width = round(bin_stats$width[, "median"], 3)
  )
  
  # Add bin ranges for clarity
  bin_ranges <- strsplit(as.character(result_df$Bin), ",")
  result_df$Bin_Min <- as.numeric(gsub("\\(|\\[", "", sapply(bin_ranges, `[`, 1)))
  result_df$Bin_Max <- as.numeric(gsub("\\]|\\)", "", sapply(bin_ranges, `[`, 2)))
  
  # Filter bins with sufficient sample size
  valid_bins <- result_df$N >= min_bin_size
  if (sum(valid_bins) < bins) {
    warning(sprintf("Only %d of %d bins have sufficient sample size (>=%d)", 
                    sum(valid_bins), bins, min_bin_size))
  }
  
  result_df$Valid <- valid_bins
  
  # Calculate overall metrics
  overall_coverage <- mean(obs >= lower & obs <= upper, na.rm = TRUE)
  
  # Test for heterogeneity in coverage across bins
  if (sum(valid_bins) >= 3) {
    coverage_values <- result_df$Coverage[valid_bins]
    coverage_test <- var.test(coverage_values, rep(overall_coverage, sum(valid_bins)))
    heterogeneity_p <- coverage_test$p.value
  } else {
    heterogeneity_p <- NA
  }
  
  # Add summary information
  attr(result_df, "overall_coverage") <- round(overall_coverage, 3)
  attr(result_df, "heterogeneity_p") <- heterogeneity_p
  attr(result_df, "method") <- method
  attr(result_df, "target_coverage") <- 0.8  # Assuming 80% intervals
  
  return(result_df)
}

#' Generate binned coverage plot
#' 
#' @param coverage_df Output from calculate_interval_coverage_by_bin
#' @param label Model label for plot title
#' @param save_prefix File prefix for saving
#' @return ggplot object
plot_binned_coverage <- function(coverage_df, label = "", save_prefix = "") {
  
  if (is.null(coverage_df)) {
    return(NULL)
  }
  
  # Get attributes
  overall_cov <- attr(coverage_df, "overall_coverage") %||% NA
  target_cov <- attr(coverage_df, "target_coverage") %||% 0.8
  hetero_p <- attr(coverage_df, "heterogeneity_p") %||% NA
  
  # Create plot
  p <- ggplot(coverage_df, aes(x = Mean_Pred, y = Coverage)) +
    geom_point(aes(size = N), alpha = 0.8, color = "#4E79A7") +
    geom_line(alpha = 0.6, color = "#4E79A7") +
    geom_hline(yintercept = target_cov, linetype = "dashed",
               color = "#D55E00", linewidth = 0.8) +
    geom_hline(yintercept = overall_cov, linetype = "dotted",
               color = "#0073C2", linewidth = 0.8) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
    scale_size_continuous(name = "Sample\nSize", range = c(2, 8)) +
    labs(
      title = paste(label, "Coverage by Prediction Bins"),
      subtitle = paste0(
        "Target: ", save_prefix,
        " | Overall: ", scales::percent(overall_cov, 0.1),
        if (!is.na(hetero_p)) paste0(" | Heterogeneity p=", round(hetero_p, 3)) else ""
      ),
      x = "Mean Predicted Value",
      y = "Coverage Rate",
      caption = "Dashed = target | Dotted = overall"
    ) +
    theme_modern() +
    theme(
      legend.position = "bottom",
      plot.subtitle = element_text(hjust = 0),
      plot.caption = element_text(hjust = 0, size = 9)
    )
  
  return(p)
}

#' Print binned coverage summary
#' 
#' @param coverage_df Output from calculate_interval_coverage_by_bin
#' @param label Model label
print_binned_coverage_summary <- function(coverage_df, label = "") {
  
  if (is.null(coverage_df)) {
    cat("Binned coverage analysis not available\n")
    return(invisible())
  }
  
  cat(sprintf("\n*** BINNED COVERAGE ANALYSIS (%s) ***\n", label))
  
  # Print table
  cat("Bin Range         | N   | Mean_Pred | Coverage | Mean_Width\n")
  cat("------------------|-----|-----------|----------|----------\n")
  
  for (i in 1:nrow(coverage_df)) {
    bin_range <- sprintf("[%.1f,%.1f%s", 
                         coverage_df$Bin_Min[i], 
                         coverage_df$Bin_Max[i],
                         if (i == nrow(coverage_df)) "]" else ")")
    
    cat(sprintf("%-17s | %3d | %9.3f | %8.1f%% | %10.3f%s\n",
                bin_range,
                coverage_df$N[i],
                coverage_df$Mean_Pred[i],
                coverage_df$Coverage[i] * 100,
                coverage_df$Mean_Width[i],
                if (!coverage_df$Valid[i]) " *" else ""))
  }
  
  # Summary statistics
  overall_cov <- attr(coverage_df, "overall_coverage")
  target_cov <- attr(coverage_df, "target_coverage")
  hetero_p <- attr(coverage_df, "heterogeneity_p")
  
  cat("\nSummary:\n")
  cat(sprintf("  Overall Coverage: %.1f%%\n", overall_cov * 100))
  cat(sprintf("  Target Coverage: %.1f%%\n", target_cov * 100))
  cat(sprintf("  Coverage Range: %.1f%% - %.1f%%\n", 
              min(coverage_df$Coverage[coverage_df$Valid]) * 100,
              max(coverage_df$Coverage[coverage_df$Valid]) * 100))
  
  if (!is.na(hetero_p)) {
    cat(sprintf("  Heterogeneity Test p-value: %.3f\n", hetero_p))
    if (hetero_p < 0.05) {
      cat("  → Significant heterogeneity in coverage across bins\n")
    } else {
      cat("  → No significant heterogeneity in coverage across bins\n")
    }
  }
  
  cat("* = Bin has insufficient sample size\n")
  cat("*** END BINNED COVERAGE ANALYSIS ***\n\n")
}

