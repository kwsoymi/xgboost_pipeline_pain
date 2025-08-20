#' Custom Plotting Themes for ggplot2
#'
#' Provides three ggplot2 themes for consistent styling: 
#' - `theme_fancy`: A clean theme with bold axis titles and no panel grid.
#' - `theme_fancy_multi`: A variation optimized for multi-panel plots with smaller text.
#' - `theme_modern`: A polished theme with larger text, light panel grid, and a bottom-aligned legend.
#'
#' Additionally, includes the function `plot_tuning_results`:
#' - Generates a line plot for visualizing hyperparameter tuning results with options for color grouping and faceting.
#'
theme_fancy <- theme_light() + theme(
  axis.title.x = element_text(size = 14, face = "bold"),
  axis.text.x = element_text(size = 11),
  axis.title.y = element_text(size = 14, face = "bold"),
  axis.text.y = element_text(size = 11),
  panel.grid = element_blank(),
  panel.border = element_blank(),
  axis.line.x = element_line(linewidth = 0.3, linetype = "solid", colour = "darkgrey"),
  axis.line.y = element_line(linewidth = 0.3, linetype = "solid", colour = "darkgrey"),
  axis.ticks = element_line(linewidth = 0.3, linetype = "solid", colour = "darkgrey"),
  plot.title = element_markdown(),
  plot.subtitle = element_markdown(),
  plot.tag = element_markdown()
)

#'
theme_fancy_multi <- theme_light() + theme(
  axis.title.x = element_text(size = 16, face = "bold"),
  axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 16, face = "bold"),
  axis.text.y = element_text(size = 14),
  panel.grid = element_blank(),
  panel.border = element_blank(),
  axis.line.x = element_line(linewidth = 0.3, linetype = "solid", colour = "darkgrey"),
  axis.line.y = element_line(linewidth = 0.3, linetype = "solid", colour = "darkgrey"),
  axis.ticks = element_line(linewidth = 0.3, linetype = "solid", colour = "darkgrey"),
  plot.title = element_blank(),
  plot.subtitle = element_blank(),
  plot.tag = element_text(size = 18, face = "bold"),
  plot.tag.position = c(0.01, 0.01)
)

#'
theme_modern <- theme_light(base_family = "Arial") +  
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12),
    panel.grid.major.y = element_line(color = "lightblue", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(linewidth = 0.3, linetype = "solid", colour = "darkgrey"),
    axis.line.y = element_line(linewidth = 0.3, linetype = "solid", colour = "darkgrey"),
    axis.ticks = element_line(linewidth = 0.3, linetype = "solid", colour = "darkgrey"),
    plot.title = element_text(size = 18, face = "bold", family = "Arial", hjust = 0),
    plot.subtitle = element_text(size = 14, family = "Arial", hjust = 0),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "bottom"
  )


#' Plot model performance summary
#'
#' Creates a combined dot-and-whisker plot summarizing MAE, RMSE, and R²
#' for all methods and both outcomes using bootstrap estimates.
#' MAE and RMSE share one x-axis scale; R² is plotted separately with its own axis.
#'
#' @param results Results object from `run_comprehensive_evaluation()`
#' @return A patchwork ggplot object or NULL if input is missing
plot_model_performance_summary <- function(results) {
  if (is.null(results$comparison$detailed_summary)) return(NULL)
  
  df <- results$comparison$detailed_summary
  if (!all(c("R2_Bootstrap", "R2_lower", "R2_upper") %in% names(df))) {
    df$R2_Bootstrap <- NA_real_
    df$R2_lower <- NA_real_
    df$R2_upper <- NA_real_
    for (i in seq_len(nrow(df))) {
      outcome_key <- "pain"
      method_key <- tolower(df$Method[i])
      boot_key <- paste0("bootstrap_", method_key, "_", outcome_key)
      boot_res <- results$comparison[[boot_key]]
      if (!is.null(boot_res$base_predictions) && !is.null(boot_res$predictions)) {
        actual <- boot_res$predictions$Actual
        preds_mat <- boot_res$base_predictions
        r2_vals <- apply(preds_mat, 1, function(p) cor(actual, p, use = "complete.obs")^2)
        df$R2_Bootstrap[i] <- mean(r2_vals, na.rm = TRUE)
        ci <- stats::quantile(r2_vals, c(0.025, 0.975), na.rm = TRUE)
        df$R2_lower[i] <- ci[1]
        df$R2_upper[i] <- ci[2]
      }
    }
  }
  
  rmse_df <- df %>%
    transmute(Outcome, Method, Metric = "RMSE",
              Value = RMSE_Bootstrap,
              Lower = RMSE_Bootstrap - RMSE_SD,
              Upper = RMSE_Bootstrap + RMSE_SD)
  
  mae_df <- df %>%
    transmute(Outcome, Method, Metric = "MAE",
              Value = MAE_Bootstrap,
              Lower = MAE_Bootstrap - MAE_SD,
              Upper = MAE_Bootstrap + MAE_SD)
  
  r2_df <- df %>%
    transmute(Outcome, Method, Metric = "R2",
              Value = R2_Bootstrap,
              Lower = R2_lower,
              Upper = R2_upper)
  
  plot_df <- bind_rows(rmse_df, mae_df, r2_df) %>%
    mutate(
      Method = factor(Method, levels = c("Baseline", "Stepwise", "Bayesian")),
      Metric = factor(Metric, levels = c("MAE", "RMSE", "R2")),
      x_min = case_when(
        Metric == "R2" ~ 0,
        Metric %in% c("RMSE", "MAE") & Outcome == "Function" ~ 0,
        Metric %in% c("RMSE", "MAE") & Outcome == "Pain" ~ 0
      ),
      x_max = case_when(
        Metric == "R2" ~ 1,
        Metric == "RMSE" & Outcome == "Function" ~ 100,
        Metric == "MAE" & Outcome == "Function" ~ 100,
        Metric == "RMSE" & Outcome == "Pain" ~ 10,
        Metric == "MAE" & Outcome == "Pain" ~ 10
      )
    )
  
  dodge <- position_dodge(width = 0.6)
  df_main <- plot_df %>% filter(Metric %in% c("MAE","RMSE"))
  df_r2   <- plot_df %>% filter(Metric == "R2")
  
  palette <- c("Baseline"="#E15759","Stepwise"="#0072B2","Bayesian"="#009E73")
  
  p_main <- ggplot(df_main, aes(x=Value, y=Method, color=Method)) +
    geom_point(position=dodge, size=3) +
    geom_errorbarh(aes(xmin=Lower, xmax=Upper), height=0.2, position=dodge) +
    geom_blank(aes(x=x_min)) + geom_blank(aes(x=x_max)) +
    facet_grid(Metric ~ Outcome, scales="free_x") +
    scale_x_continuous(labels=scales::label_number(accuracy = 1)) +
    scale_color_manual(values=palette) +
    scale_y_discrete(limits=rev(c("Baseline","Stepwise","Bayesian"))) +
    labs(x=NULL, y=NULL, color="Method") +
    theme_minimal() +
    theme(legend.position="none",
          strip.text.y=element_text(face="bold", size=20, angle=90),
          plot.title=element_text(face="bold", size=17, hjust=0, family="Arial"))
  
  p_r2 <- ggplot(df_r2, aes(x = Value, y = Method, color = Method)) +
    geom_point(position = dodge, size = 3) +
    geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2, position = dodge) +
    geom_blank(aes(x = 0)) +
    geom_blank(aes(x = 1)) +
    facet_grid(Metric ~ Outcome, scales = "free_x") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_color_manual(values = palette) +
    scale_y_discrete(limits = rev(c("Baseline", "Stepwise", "Bayesian"))) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      legend.position = "none",
      strip.text.y = element_text(face = "bold", size = 20, angle = 90),
      strip.text.x = element_blank()
    )
  
  
  return(p_main / p_r2 + plot_layout(heights = c(0.65, 0.35)) +
           plot_annotation(
             title = "Model Performance Summary",
             theme = theme(
               plot.title = element_text(face = "bold", size = 17, hjust = 0, family = "Arial")
             )
           ))
  
}


#' Plot model performance without R2 facet
#'
#' Generates a dot-and-whisker plot of MAE and RMSE only.
#'
#' @param results Results object from `run_comprehensive_evaluation()`
#' @param outcome Either "Pain" or "Function"
#' @return A ggplot object
plot_model_performance_by_outcome_no_r2 <- function(results, outcome = "Pain") {
  if (is.null(results$comparison$detailed_summary)) {
    return(NULL)
  }

outcome <- match.arg(outcome)
df <- results$comparison$detailed_summary
df <- df[df$Outcome == outcome, ]

if (!all(c("R2_Bootstrap", "R2_lower", "R2_upper") %in% names(df))) {
  df$R2_Bootstrap <- NA_real_
  df$R2_lower <- NA_real_
  df$R2_upper <- NA_real_
  
  for (i in seq_len(nrow(df))) {
    outcome_key <-"pain" 
    method_key <- tolower(df$Method[i])
    boot_key <- paste0("bootstrap_", method_key, "_", outcome_key)
    boot_res <- results$comparison[[boot_key]]
    if (!is.null(boot_res$base_predictions) && !is.null(boot_res$predictions)) {
      actual <- boot_res$predictions$Actual
      preds_mat <- boot_res$base_predictions
      r2_vals <- apply(preds_mat, 1, function(p) cor(actual, p, use = "complete.obs")^2)
      df$R2_Bootstrap[i] <- mean(r2_vals, na.rm = TRUE)
      ci <- quantile(r2_vals, c(0.025, 0.975), na.rm = TRUE)
      df$R2_lower[i] <- ci[1]
      df$R2_upper[i] <- ci[2]
    }
  }
}
rmse_df <- df %>%
  transmute(Metric = "RMSE", Method, Value = RMSE_Bootstrap,
            Lower = RMSE_Bootstrap - RMSE_SD,
            Upper = RMSE_Bootstrap + RMSE_SD)

mae_df <- df %>%
  transmute(Metric = "MAE", Method, Value = MAE_Bootstrap,
            Lower = MAE_Bootstrap - MAE_SD,
            Upper = MAE_Bootstrap + MAE_SD)

r2_df <- df %>%
  transmute(Metric = "R2", Method, Value = R2_Bootstrap,
            Lower = R2_lower, Upper = R2_upper)

plot_df <- bind_rows(rmse_df, mae_df, r2_df) %>%
  mutate(
    Method = factor(Method, levels = c("Baseline", "Stepwise", "Bayesian")),
    Metric = factor(
      Metric,
      levels = c("MAE", "RMSE", "R2"),
      labels = c("Mean Absolute Error", "Root Mean Squared Error", "R-squared")
    ),
    x_min = 0,
    x_max = case_when(
      Metric == "R-squared" ~ 1,
      Metric %in% c("Root Mean Squared Error", "Mean Absolute Error") ~ 10
    ),
    label = sprintf("%.1f [%.1f, %.1f]", Value, Lower, Upper)
  )

dodge <- position_dodge(width = 0.6)
palette <- c(
  "Baseline" = "#D55E00",  # orange-red
  "Stepwise" = "#0072B2",   # blue
  "Bayesian" = "#009E73"    # bluish green
)

df_main <- plot_df %>% filter(Metric %in% c("Mean Absolute Error", "Root Mean Squared Error"))
df_r2   <- plot_df %>% filter(Metric == "R-squared")

nudge   <- 0.03 * max(df_main$x_max, na.rm = TRUE)

p_main <- ggplot(df_main, aes(x = Value, y = Method, color = Method)) +
  geom_point(position = dodge, size = 3) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2, position = dodge) +
  geom_blank(aes(x = x_min)) +
  geom_blank(aes(x = x_max)) +
  geom_text(aes(label = label, x = Upper), hjust = -0.1, size = 5, color = "black", show.legend = FALSE) +
  facet_grid(Metric ~ ., scales = "free_x") +
  scale_color_manual(values = palette) +
  scale_y_discrete(limits = rev(c("Baseline", "Stepwise", "Bayesian"))) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(
    legend.position = "none",
    strip.text.y = element_text(face = "bold", size = 22, angle = 90),
    plot.title = element_text(face = "bold", size = 17, hjust = 0, family = "Arial")
  )

p_r2 <- ggplot(df_r2, aes(x = Value, y = Method, color = Method)) +
  geom_point(position = dodge, size = 3) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2, position = dodge) +
  geom_blank(aes(x = 0)) +
  geom_blank(aes(x = 1)) +
  geom_text(aes(label = label, x = Upper), hjust = -0.1, size = 3, color = "black", show.legend = FALSE) +
  facet_grid(Metric ~ ., scales = "free_x") +
  scale_x_continuous(limits = c(0, 1)) +
  scale_color_manual(values = palette) +
  scale_y_discrete(limits = rev(c("Baseline", "Stepwise", "Bayesian"))) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text.y = element_text(face = "bold", size = 26, angle = 90),
    strip.text.x = element_blank()
  )

return(p_main / p_r2 + plot_layout(heights = c(0.65, 0.35)) +
         plot_annotation(
           title = paste("Model Performance (Bootstrapped):", outcome, "Outcome"),
           theme = theme(
             plot.title = element_text(face = "bold", size = 17, hjust = 0, family = "Arial")
           )
         ))
}

#' Convert a PNG file into a ggplot object
#'
#' @param path Path to PNG file
#' @return ggplot object displaying the image
png_to_plot <- function(path) {
  img <- png::readPNG(path)
  ggplot() +
    annotation_custom(grid::rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc")),
                      -Inf, Inf, -Inf, Inf) +
    theme_void()
}

#' Create multi-panel plots for the manuscript
#'
#' Uses existing plots to assemble figures with alphabetical tags.
#'
#' @param results Comprehensive evaluation results object
#' @param output_dir Directory containing and saving plots
#'
#' @return Invisible list of saved file paths
create_manuscript_panel_plots <- function(results, output_dir = "output") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  panel_paths <- list()
  
  # Performance vs error rainfall
  perf_plot <- plot_model_performance_by_outcome_no_r2(results, "Pain")
  rainfall_plot <- plot_three_way_comparison(results$comparison)$pain_error_rainfall +
    theme_fancy_multi
  p1 <- perf_plot + rainfall_plot +
    patchwork::plot_annotation(tag_levels = "A") &
    (theme_fancy_multi +
       theme(
         legend.position = "none",
         strip.background = element_blank(),
         strip.text = element_text(face = "bold", color = "black")
       ))
  file1 <- file.path(output_dir, "panel_pain_performance_rainfall.png")
  ggsave(file1, p1, width = 12, height = 6, dpi = 300)
  panel_paths$performance_rainfall <- file1
  
  # Distribution vs calibration
  dist_plot <- png_to_plot(file.path(output_dir, "pain_bayesian_distribution_combined_bayesian.png")) +
    theme_fancy_multi
  calib_plot <- png_to_plot(file.path(output_dir, "pain_bootstrap_calibration.png")) +
    theme_fancy_multi
  p2 <- dist_plot + calib_plot +
    patchwork::plot_annotation(tag_levels = "A") & theme_fancy_multi
  file2 <- file.path(output_dir, "panel_distribution_calibration.png")
  ggsave(file2, p2, width = 12, height = 6, dpi = 300)
  panel_paths$distribution_calibration <- file2
  
  # Patient prediction lines
  line_high <- png_to_plot(file.path(output_dir, "patient_prediction_pain_line_high.png")) + theme_fancy_multi
  line_norm <- png_to_plot(file.path(output_dir, "patient_prediction_pain_line.png")) + theme_fancy_multi
  p3 <- line_high + line_norm +
    patchwork::plot_annotation(tag_levels = "A") & theme_fancy_multi
  file3 <- file.path(output_dir, "panel_patient_lines.png")
  ggsave(file3, p3, width = 12, height = 6, dpi = 300)
  panel_paths$patient_lines <- file3
  
  # Feature importance and SHAP
  fi_plot <- png_to_plot(file.path(output_dir, "feature_importance_pain.png")) + theme_fancy_multi
  shap_water <- png_to_plot(file.path(output_dir, "shap_pain_waterfall.png")) + theme_fancy_multi
  p4 <- fi_plot + shap_water +
    patchwork::plot_annotation(tag_levels = "A") & theme_fancy_multi
  file4 <- file.path(output_dir, "panel_feature_importance_shap.png")
  ggsave(file4, p4, width = 12, height = 6, dpi = 300)
  panel_paths$feature_shap <- file4
  
  invisible(panel_paths)
}



#' Plot model performance for a single outcome
#'
#' Creates a dot-and-whisker plot of MAE, RMSE, and R² for one outcome only,
#' with text labels showing the point estimate and confidence interval.
#'
#' @param results Results object from `run_comprehensive_evaluation()`
#' @param outcome "Pain"
#' @return A ggplot object or NULL if input is missing
plot_model_performance_by_outcome <- function(results, outcome = "Pain") {
  if (is.null(results$comparison$detailed_summary)) {
    return(NULL)
  }
  
  outcome <- match.arg(outcome)
  df <- results$comparison$detailed_summary
  df <- df[df$Outcome == outcome, ]
  
  if (!all(c("R2_Bootstrap", "R2_lower", "R2_upper") %in% names(df))) {
    df$R2_Bootstrap <- NA_real_
    df$R2_lower <- NA_real_
    df$R2_upper <- NA_real_
    
    for (i in seq_len(nrow(df))) {
      outcome_key <- "pain"
      method_key <- tolower(df$Method[i])
      boot_key <- paste0("bootstrap_", method_key, "_", outcome_key)
      boot_res <- results$comparison[[boot_key]]
      if (!is.null(boot_res$base_predictions) && !is.null(boot_res$predictions)) {
        actual <- boot_res$predictions$Actual
        preds_mat <- boot_res$base_predictions
        r2_vals <- apply(preds_mat, 1, function(p) cor(actual, p, use = "complete.obs")^2)
        df$R2_Bootstrap[i] <- mean(r2_vals, na.rm = TRUE)
        ci <- quantile(r2_vals, c(0.025, 0.975), na.rm = TRUE)
        df$R2_lower[i] <- ci[1]
        df$R2_upper[i] <- ci[2]
      }
    }
  }
  
  rmse_df <- df %>%
    transmute(Metric = "RMSE", Method, Value = RMSE_Bootstrap,
              Lower = RMSE_Bootstrap - RMSE_SD,
              Upper = RMSE_Bootstrap + RMSE_SD)
  
  mae_df <- df %>%
    transmute(Metric = "MAE", Method, Value = MAE_Bootstrap,
              Lower = MAE_Bootstrap - MAE_SD,
              Upper = MAE_Bootstrap + MAE_SD)
  
  r2_df <- df %>%
    transmute(Metric = "R2", Method, Value = R2_Bootstrap,
              Lower = R2_lower, Upper = R2_upper)
  
  plot_df <- bind_rows(rmse_df, mae_df, r2_df) %>%
    mutate(
      Method = factor(Method, levels = c("Baseline", "Bayesian", "Stepwise")),
      Metric = factor(
        Metric,
        levels = c("MAE", "RMSE", "R2"),
        labels = c("Mean Absolute Error", "Root Mean Squared Error", "R-squared")
      ),
      x_min = 0,
      x_max = case_when(
        Metric == "R-squared" ~ 1,
        Metric %in% c("Root Mean Squared Error", "Mean Absolute Error") & outcome == "Pain" ~ 10
      ),
      label = sprintf("%.1f [%.1f, %.1f]", Value, Lower, Upper)
    )
  
  dodge <- position_dodge(width = 0.6)
  palette <- c(
    "Baseline" = "#D55E00",  # orange-red
    "Stepwise" = "#0072B2",   # blue
    "Bayesian" = "#009E73"    # bluish green
  )
  
  df_main <- plot_df %>% filter(Metric %in% c("Mean Absolute Error", "Root Mean Squared Error"))
  df_r2   <- plot_df %>% filter(Metric == "R-squared")
  
  nudge   <- 0.03 * max(df_main$x_max, na.rm = TRUE)
  
  p_main <- ggplot(df_main, aes(x = Value, y = Method, color = Method)) +
    geom_point(position = dodge, size = 3) +
    geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2, position = dodge) +
    geom_blank(aes(x = x_min)) +
    geom_blank(aes(x = x_max)) +
    geom_text(aes(label = label, x = Upper), hjust = -0.1, size = 5, color = "black", show.legend = FALSE) +
    facet_grid(Metric ~ ., scales = "free_x") +
    scale_color_manual(values = palette) +
    scale_y_discrete(limits = rev(c("Baseline", "Stepwise", "Bayesian"))) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(legend.position = "none") +
    theme(
      legend.position = "none",
      strip.text.y = element_text(face = "bold", size = 22, angle = 90),
      plot.title = element_text(face = "bold", size = 17, hjust = 0, family = "Arial")
    )
  
  p_r2 <- ggplot(df_r2, aes(x = Value, y = Method, color = Method)) +
    geom_point(position = dodge, size = 3) +
    geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2, position = dodge) +
    geom_blank(aes(x = 0)) +
    geom_blank(aes(x = 1)) +
    geom_text(aes(label = label, x = Upper), hjust = -0.1, size = 3, color = "black", show.legend = FALSE) +
    facet_grid(Metric ~ ., scales = "free_x") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_color_manual(values = palette) +
    scale_y_discrete(limits = rev(c("Baseline", "Stepwise", "Bayesian"))) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(legend.position = "none") +
    theme(
      legend.position = "none",
      strip.text.y = element_text(face = "bold", size = 26, angle = 90),
      strip.text.x = element_blank()
    )
  
  return(p_main / p_r2 + plot_layout(heights = c(0.65, 0.35)) +
           plot_annotation(
             title = paste("Model Performance (Bootstrapped):", outcome, "Outcome"),
             theme = theme(
               plot.title = element_text(face = "bold", size = 17, hjust = 0, family = "Arial")
             )
           ))
}

#' Plot Bootstrapped Feature Importance
#'
#' Generates a feature importance plot from an aggregated summary of bootstrapped XGBoost importance scores.
#'
#' This function reformats a feature importance summary (with `MeanGain`, `MeanCover`, and `MeanFrequency`)
#' to the expected structure for `xgb.plot.importance` and creates a visualization.
#'
#' @param importance_summary A data frame containing the aggregated feature importance with columns:
#' @param title Plot title (e.g., "Feature Importance (Pain Model)").
#'   `Feature`, `MeanGain`, `MeanCover`, and `MeanFrequency`.
#'
#' @return A ggplot object displaying feature importance using `xgb.plot.importance`.
plot_importance_summary <- function(importance_summary, title = "Feature Importance") {
  importance_summary <- importance_summary %>%
    dplyr::mutate(Feature = dplyr::recode(Feature, !!!predictor_labels))
  ggplot(importance_summary, aes(x = reorder(Feature, MeanGain), y = MeanGain)) +
    geom_col() +
    coord_flip() +
    labs(x = "Feature", y = "Mean Gain", title = title) +
    theme_minimal()
}

#' Create Bootstrap Data Frame for Plotting
#'
#' Combines predictions, prediction intervals, and baseline values into a structured data frame for visualization.
#'
#' If the specified baseline variable is missing from the test dataset, it is merged from an external test data frame.
#'
#' @param final_test_data A data frame containing the test data.
#' @param test_xgb_pain A data frame containing the original test data for merging the baseline variable if needed.
#' @param reduced_results A list containing model predictions from the reduced model.
#' @param bootstrap_results_reduced A list containing bootstrapped prediction intervals from the reduced model.
#' @param baseline_var A string specifying the name of the baseline variable (default is `"painactivitiesnrs"`).
#'
#' @return A data frame with columns:
#'   - `Predicted`: Model predictions
#'   - `Lower`: Lower bound of the prediction interval
#'   - `Upper`: Upper bound of the prediction interval
#'   - `Baseline`: Actual baseline value for comparison
#'   - `fid`: Unique identifier for each test observation
create_bootstrap_df <- function(final_test_data, test_xgb_pain = test_xgb_pain, reduced_results, bootstrap_results_reduced,
                                baseline_var = "painactivitiesnrs") {
  library(dplyr)
  
  # Check if the baseline variable exists; if not, merge it.
  if (!baseline_var %in% colnames(final_test_data)) {
    final_test_data_pred <- final_test_data %>%
      left_join(test_xgb_pain %>% select(fid, all_of(baseline_var)), by = "fid")
  } else {
    final_test_data_pred <- final_test_data
  }
  
  # Create the bootstrap dataframe for plotting.
  bootstrap_df <- data.frame(
    Predicted = reduced_results$predictions,
    Lower = bootstrap_results_reduced$predictions$lb,
    Upper = bootstrap_results_reduced$predictions$ub,
    Baseline = final_test_data_pred[[baseline_var]],
    fid = final_test_data_pred$fid
  )
  
  return(bootstrap_df)
}

#' Plot Predictions vs. Baseline Variable with Prediction Intervals
#'
#' This function creates a ggplot object visualizing predicted values, prediction intervals,
#' and the baseline variable from a provided bootstrap data frame. The input data frame must contain
#' the following columns: \code{Predicted}, \code{Lower}, \code{Upper}, \code{Baseline}, and \code{fid}.
#'
#' @param bootstrap_df A data frame containing the columns: \code{Predicted}, \code{Lower}, \code{Upper},
#'   \code{Baseline}, and \code{fid}.
#' @param scale_type A string specifying whether the plot is for "pain" or "function". Determines axis scaling.
#' @param plot_title A string specifying the plot title. Defaults to an appropriate title based on `scale_type`.
#' @param plot_subtitle A string specifying the plot subtitle. Defaults to "Comparison of Baseline Scores and Predicted Values (with 80%-PI)".
#' @param y_label A string specifying the y-axis label. Defaults to "Outcome".
#'
#' @return A ggplot object visualizing the predicted values, prediction intervals, and the baseline variable.
#'
plot_predictions_vs_baseline <- function(bootstrap_df,
                                         scale_type = "pain",  # Default to pain, can be changed to "function"
                                         plot_title = NULL,
                                         plot_subtitle = "Comparison of <span style='color:#0073C2;'>Baseline Scores</span> and <span style='color:#D55E00;'>Predicted Values</span> (with 80%-PI)",
                                         y_label = "Outcome") {
  
  # Define scale settings based on the type of outcome
  if (scale_type == "pain") {
    y_limits <- c(-0.25, 10.0)
    y_breaks <- seq(0, 10, by = 2)
    default_title <- "Pain Predictions"
  } else if (scale_type == "function") {
    y_limits <- c(-2.5, 100.0)
    y_breaks <- seq(0, 100, by = 20)
    default_title <- "Function Predictions"
  } else {
    stop("Invalid scale_type. Use 'pain' or 'function'.")
  }
  
  # If no custom title is provided, use the default based on scale_type
  if (is.null(plot_title)) {
    plot_title <- default_title
  }
  
  p <- ggplot(bootstrap_df, aes(x = 1:nrow(bootstrap_df), y = Predicted)) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0, color = "gray", linewidth = 0.8) +
    geom_point(aes(color = "Predicted"), size = 7) +  
    geom_point(aes(y = Baseline, color = "Baseline"), shape = 4, size = 6, stroke = 1.8) +
    scale_y_continuous(limits = y_limits, breaks = y_breaks) +
    scale_color_manual(
      values = c("Predicted" = "#D55E00", "Baseline" = "#0073C2")
    ) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = "Participants",
      y = y_label
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 36, hjust = 0),
      plot.subtitle = element_markdown(size = 24, hjust = 0),
      axis.title.y = element_text(size = 18, face = "bold"),
      axis.text.y = element_text(size = 18),
      axis.text.x = element_blank(),
      axis.title.x = element_text(size = 18, face = "bold", margin = margin(t = -20)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      text = element_text(family = "sans")
    )
  
  return(p)
}

#' Plot Baseline and Predicted Outcome for a Specific Patient
#'
#' This function creates a horizontal error bar plot showing the predicted outcome level (with
#' its prediction interval) alongside a baseline value for a selected patient.
#' The patient can be selected by either providing a patient ID (as stored in the `fid` column)
#' or by specifying the row index in the bootstrap dataframe.
#'
#' @param bootstrap_df A data frame containing at least the following columns:
#'   \code{Predicted}, \code{Lower}, \code{Upper}, \code{Baseline}, and \code{fid}.
#' @param patient_identifier Either a patient ID (if \code{identifier_type = "id"}) or a numeric index (if \code{identifier_type = "index"}) indicating the patient to plot.
#' @param identifier_type A string indicating whether \code{patient_identifier} is a patient ID ("id") or a row index ("index"). Defaults to "id".
#' @param scale_type A string specifying whether the plot is for "pain" or "function". Determines axis scaling and title.
#' 
#' @return A ggplot object displaying the plot for the selected patient.
#'
plot_patient_prediction <- function(bootstrap_df, patient_identifier, identifier_type = "id",
                                    scale_type = "pain") {
  
  # Define scale settings based on outcome type
  if (scale_type == "pain") {
    x_limits <- c(-0.75, 10.0)
    x_breaks <- seq(0, 10, by = 2)
    base_title <- "Baseline and Predicted Pain for Patient"
  } else if (scale_type == "function") {
    x_limits <- c(-7.5, 100.0)
    x_breaks <- seq(0, 100, by = 10)
    base_title <- "Baseline and Predicted Function for Patient"
  } else {
    stop("Invalid scale_type. Use 'pain' or 'function'.")
  }
  
  # Filter the patient data based on the specified identifier type
  if (identifier_type == "id") {
    patient_data <- bootstrap_df %>% filter(fid == as.character(patient_identifier))
    plot_id <- as.character(patient_identifier)
  } else if (identifier_type == "index") {
    if (is.numeric(patient_identifier) && patient_identifier <= nrow(bootstrap_df)) {
      patient_data <- bootstrap_df[patient_identifier, , drop = FALSE]
      plot_id <- bootstrap_df$fid[patient_identifier]
    } else {
      stop("Invalid index provided for patient_identifier.")
    }
  } else {
    stop("identifier_type must be either 'id' or 'index'.")
  }
  
  # Create the plot for the selected patient
  p <- ggplot(patient_data, aes(x = Predicted, y = "Prediction")) +
    geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2, color = "#949073", size = 1.2) +
    geom_point(color = "#007589", size = 8, alpha = 0.8) +
    geom_point(aes(x = Baseline), color = "#D55E00", shape = 4, size = 6, stroke = 2) +
    scale_x_continuous(limits = x_limits, breaks = x_breaks) +
    labs(
      title = paste(base_title, plot_id),
      x = "Value",
      y = NULL
    ) +
    theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    theme_modern +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
  
  return(p)
}


#' Plot Gradient Bar for a Specific Patient Using Identifier
#'
#' This function selects a single patient's data from a bootstrap data frame (using either a patient ID
#' or a row index) and then creates a gradient bar plot that displays the prediction interval (with error bars)
#' and marks both the predicted value and a baseline measurement.
#'
#' @param bootstrap_df A data frame containing at least the following columns:
#'   \code{Predicted}, \code{lb} (or \code{Lower}), \code{ub} (or \code{Upper}), \code{Baseline} (or an equivalent baseline variable), and \code{fid}.
#' @param patient_identifier Either a patient ID (if \code{identifier_type = "id"}) or a numeric index (if \code{identifier_type = "index"}) indicating the patient to plot.
#' @param identifier_type A string indicating whether \code{patient_identifier} is a patient ID ("id") or a row index ("index"). Defaults to "id".
#' @param scale_type A string specifying whether the plot is for "pain" or "function". Determines axis scaling and title.
#'
#' @return A ggplot object representing the gradient bar plot for the selected patient.
#'
plot_patient_gradient_prediction <- function(bootstrap_df, patient_identifier, identifier_type = "id",
                                             scale_type = "pain") {
  
  # Define x-axis limits, breaks, and base title based on scale_type
  if (scale_type == "pain") {
    x_limits <- c(0, 10)
    x_breaks <- seq(0, 10, by = 2)
    base_title <- "Baseline and Predicted Pain for Patient"
  } else if (scale_type == "function") {
    x_limits <- c(-7.5, 100)
    x_breaks <- seq(0, 100, by = 10)
    base_title <- "Baseline and Predicted Function for Patient"
  } else {
    stop("Invalid scale_type. Use 'pain' or 'function'.")
  }
  
  # Filter the patient data based on the specified identifier type
  if (identifier_type == "id") {
    patient_data <- bootstrap_df %>% filter(fid == as.character(patient_identifier))
    plot_id <- as.character(patient_identifier)
  } else if (identifier_type == "index") {
    if (is.numeric(patient_identifier) && patient_identifier <= nrow(bootstrap_df)) {
      patient_data <- bootstrap_df[patient_identifier, , drop = FALSE]
      plot_id <- bootstrap_df$fid[patient_identifier]
    } else {
      stop("Invalid index provided for patient_identifier.")
    }
  } else {
    stop("identifier_type must be either 'id' or 'index'.")
  }
  
  # Ensure we have a valid patient
  if (nrow(patient_data) == 0) {
    stop("Error: No matching patient found in bootstrap_df.")
  }
  
  # Call the existing plot_gradient_prediction function, assuming it takes a single-patient data frame.
  p <- plot_gradient_prediction(patient_data, patient_id = plot_id, scale_type = scale_type)
  
  # Update the title
  # p <- p + labs(title = paste(base_title, plot_id))
  
  return(p)
}



#' Plot Gradient Bar with Prediction Interval and Baseline Marker for a Patient
#'
#' This function creates a gradient bar plot that displays the prediction interval (with error bars)
#' and marks both the predicted value and a baseline measurement for a specific patient.
#'
#' @param patient_data A data frame for a single patient containing at least the following columns:
#'   \code{Predicted}, \code{Lower}, \code{Upper}, and \code{Baseline}.
#' @param patient_id A character or numeric identifier for the patient (used in the plot title).
#' @param scale_type A string specifying whether the plot is for "pain" or "function". Determines axis scaling and title.
#'
#' @return A ggplot object representing the gradient bar plot with prediction interval and baseline marker.
#' 
plot_gradient_prediction <- function(patient_data, patient_id, scale_type = "pain") {
  
  # Define x-axis limits, breaks, and base title based on scale_type
  if (scale_type == "pain") {
    x_limits <- c(0, 10)
    x_breaks <- seq(0, 10, by = 2)
    base_title <- "Baseline and Predicted Pain for Patient"
    gradient_range <- seq(-0.05, 10.05, length.out = 100)  # Slightly extend range for smooth color transition
  } else if (scale_type == "function") {
    x_limits <- c(-7.5, 100)
    x_breaks <- seq(0, 100, by = 10)
    base_title <- "Baseline and Predicted Function for Patient"
    gradient_range <- seq(-7.5, 100, length.out = 100)
  } else {
    stop("Invalid scale_type. Use 'pain' or 'function'.")
  }
  
  # Create a data frame for the gradient bar.
  gradient_data <- data.frame(
    x = gradient_range,
    y = 1
  )
  
  # Build the plot.
  p <- ggplot() +
    # Gradient bar using geom_tile.
    geom_tile(
      data = gradient_data,
      aes(x = x, y = 1, fill = x),
      height = 0.05
    ) +
    scale_fill_gradientn(
      colors = c("#d2ffae", "#cdea8f", "#bf952b", "#bb8219", "#ac4600", "#9e0101"),
      guide = "none"
    ) +
    # Border around the gradient.
    geom_rect(
      aes(xmin = x_limits[1], xmax = x_limits[2], ymin = 1 - 0.025, ymax = 1 + 0.025),
      color = "#CCCCCC",
      fill = NA,
      size = 1.1
    ) +
    # Error bar representing the prediction interval.
    geom_errorbarh(
      data = patient_data,
      aes(xmin = Lower, xmax = Upper, y = 1),
      height = 0.01,
      color = "#949073",
      size = 1.2
    ) +
    # Predicted value marker (circle).
    geom_point(
      data = patient_data,
      aes(x = Predicted, y = 1),
      color = "#007589",
      size = 12,
      shape = 16
    ) +
    # Baseline value marker (triangle).
    geom_point(
      data = patient_data,
      aes(x = Baseline, y = 1),
      color = "#D55E00",
      shape = 17,
      size = 12,
      stroke = 0
    ) +
    # Adjust the x-axis.
    scale_x_continuous(
      limits = x_limits,
      breaks = x_breaks,
      labels = x_breaks,
      name = NULL
    ) +
    # Title and theme settings.
    labs(
      title = paste(base_title, patient_id),
      y = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      axis.text.x = element_text(size = 12, face = "bold")
    )
  
  return(p)
}


#' Plot Learning Curve by Training Set Size
#'
#' This function generates a learning curve that shows how model performance (in terms of
#' mean absolute error, MAE) changes as the training set size increases. The function trains an
#' XGBoost model on increasing fractions of the training data and evaluates performance on both
#' the training subset and a separate test dataset.
#'
#' @param train_data A data frame containing the training data. Must include a target variable.
#' @param test_data A data frame containing the test data with the same predictors and target variable.
#' @param predictors A character vector of predictor variable names.
#' @param target A string specifying the target variable name. Defaults to "pain_1y".
#' @param params A list of model parameters for XGBoost.
#' @param nrounds An integer specifying the number of boosting rounds.
#' @param fractions A numeric vector of fractions of the training data to use. Defaults to seq(0.1, 1.0, by = 0.1).
#' @param seed An integer to set the random seed for reproducibility.
#'
#' @return A list with two elements:
#'   \item{learning_data}{A data frame containing the fraction of training data used and corresponding training and test MAE}
#'   \item{plot}{A ggplot object displaying the learning curve.}
#'
plot_learning_curve <- function(train_data, test_data, predictors, target = "pain_1y",
                                params, nrounds, fractions = seq(0.1, 1.0, by = 0.1),
                                seed = 199987) {
  
  learning_data <- data.frame(Fraction = fractions,
                              TrainMAE = NA,
                              TestMAE = NA,
                              CV_MAE = NA)
  
  set.seed(seed)
  for (i in seq_along(fractions)) {
    frac <- fractions[i]
    sample_size <- floor(nrow(train_data) * frac)
    
    sub_train <- train_data %>% sample_n(sample_size)
    X <- as.matrix(sub_train %>% select(all_of(predictors)))
    y <- sub_train[[target]]
    
    # DMatrix for training and test
    dtrain <- xgb.DMatrix(data = X, label = y)
    dtest <- xgb.DMatrix(data = as.matrix(test_data %>% select(all_of(predictors))),
                         label = test_data[[target]])
    
    # Train model
    model <- xgboost(data = dtrain, params = params, nrounds = nrounds, verbose = 0)
    
    # Predict and calculate train/test MAE
    pred_train <- predict(model, dtrain)
    pred_test <- predict(model, dtest)
    learning_data$TrainMAE[i] <- mean(abs(y - pred_train))
    learning_data$TestMAE[i] <- mean(abs(test_data[[target]] - pred_test))
    
    # 10x5 Cross-validation
    folds <- my_rep_folds(sub_train, IDvarn = "fid", nreps = 10, nfolds = 5, seed = seed)
    cv_results <- lapply(seq(1, length(folds), by = 5), function(j) {
      fold_subset <- folds[j:(j + 4)]
      cv <- xgb.cv(
        params = params,
        data = dtrain,
        nrounds = nrounds,
        folds = fold_subset,
        metrics = "mae",
        early_stopping_rounds = 10,
        verbose = 0
      )
      min(cv$evaluation_log$test_mae_mean)
    })
    learning_data$CV_MAE[i] <- mean(unlist(cv_results), na.rm = TRUE)
  }
  
  # Plot
  p <- ggplot(learning_data, aes(x = Fraction)) +
    geom_line(aes(y = TrainMAE, color = "Train MAE"), linewidth = 1) +
    geom_line(aes(y = TestMAE, color = "Test MAE"), linewidth = 1) +
    geom_line(aes(y = CV_MAE, color = "10x5 Cross-Validation MAE"), linetype = "dashed", linewidth = 1) +
    geom_point(aes(y = TrainMAE, color = "Train MAE"), size = 2) +
    geom_point(aes(y = TestMAE, color = "Test MAE"), size = 2) +
    geom_point(aes(y = CV_MAE, color = "10x5 Cross-Validation MAE"), size = 2) +
    labs(
      title = "Learning Curve by Training Set Size",
      x = "Fraction of Training Data Used",
      y = "Mean Absolute Error (MAE)",
      color = "Error Type"
    ) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
    scale_color_manual(values = c(
      "Train MAE" = "#1f77b4",
      "Test MAE" = "#d62728",
      "10x5 Cross-Validation MAE" = "gray40"
    )) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0, face = "bold", size = 14),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 11)
    )
  
    return(list(cv_data = learning_data, plot = p))
    
}



#' Plot Baseline and Predicted Outcome for a Specific Patient
#'
#' This function creates a vertical plot showing the predicted outcome level (with
#' its prediction interval) alongside a baseline value for a selected patient.
#' The patient can be selected by either providing a patient ID (fid) or by specifying the row index.
#'
#' @param bootstrap_df A data frame containing at least the following columns:
#'   \code{Predicted}, \code{Lower}, \code{Upper}, \code{Baseline}, and \code{fid}.
#' @param bootstrap_results A matrix containing bootstrap sample predictions.
#' @param patient_identifier Either a patient ID (if \code{identifier_type = "id"}) or a numeric index (if \code{identifier_type = "index"}) indicating the patient to plot.
#' @param identifier_type A string indicating whether \code{patient_identifier} is a patient ID ("id") or a row index ("index"). Defaults to "id".
#' @param scale_type A string specifying whether the plot is for "pain" or "function". Determines axis scaling and labels.
#'
#' @return A ggplot object displaying the plot for the selected patient.
#'
plot_patient_prediction_line <- function(bootstrap_df, bootstrap_results, 
                                         patient_identifier, identifier_type = "id", 
                                         scale_type = "pain") {
  
  # Define y-axis limits, breaks, labels, and titles based on scale_type
  if (scale_type == "pain") {
    y_limits <- c(0, 10)
    y_breaks <- seq(0, 10, by = 2)
    base_title <- "Pain Progression for an Individual Patient"
    y_label <- "Pain (NRS 0-10)"
  } else if (scale_type == "function") {
    y_limits <- c(-2.5, 100)
    y_breaks <- seq(0, 100, by = 20)
    base_title <- "Function Progression for an Individual Patient"
    y_label <- "Function Score (bMHQ score 0-100)"
  } else {
    stop("Invalid scale_type. Use 'pain' or 'function'.")
  }
  
  # Filter the patient data
  if (identifier_type == "id") {
    patient_data <- bootstrap_df %>% filter(fid == as.character(patient_identifier))
    plot_id <- as.character(patient_identifier)
  } else if (identifier_type == "index") {
    if (is.numeric(patient_identifier) && patient_identifier <= nrow(bootstrap_df)) {
      patient_data <- bootstrap_df[patient_identifier, , drop = FALSE]
      plot_id <- bootstrap_df$fid[patient_identifier]
    } else {
      stop("Invalid index provided for patient_identifier.")
    }
  } else {
    stop("identifier_type must be either 'id' or 'index'.")
  }
  
  # Ensure the patient exists in bootstrap_df
  if (nrow(patient_data) == 0) {
    stop("Error: No matching patient found in bootstrap_df.")
  }
  
  # Extract bootstrapped predictions for this patient
  patient_col_idx <- which(bootstrap_df$fid == patient_identifier)
  predicted_samples <- bootstrap_results[, patient_col_idx]
  
  # Calculate median from bootstrap samples
  median_prediction <- median(predicted_samples, na.rm = TRUE)
  
  # Round numeric values
  patient_data <- patient_data %>%
    mutate(
      Predicted = round(median_prediction, 1),
      Lower = round(Lower, 1),
      Upper = round(Upper, 1),
      Baseline = round(Baseline, 1)
    )
  
  # Generate density distribution
  density_data <- density(predicted_samples)
  
  # Scale density for prediction interval range
  density_df <- data.frame(
    scaled_x = (density_data$x - min(density_data$x)) / (max(density_data$x) - min(density_data$x)) * 
      (patient_data$Upper - patient_data$Lower) + patient_data$Lower,
    density = density_data$y
  )
  
  # Normalize density for proper scaling
  max_density <- max(density_df$density)
  density_df$density <- density_df$density / max_density * 0.3  # Adjust width
  
  # Define timepoints for visualization
  timepoints <- data.frame(
    Timepoint = factor(c("Baseline", "1 Year"), levels = c("Baseline", "1 Year")),
    Outcome = c(patient_data$Baseline, patient_data$Predicted)
  )
  
  # Define the shaded "cone" area
  cone_data <- data.frame(
    Timepoint = factor(c("Baseline", "1 Year", "1 Year", "Baseline"), levels = c("Baseline", "1 Year")),
    Outcome = c(patient_data$Baseline, patient_data$Lower, patient_data$Upper, patient_data$Baseline)
  )
  
  # Prepare density plot data
  density_plot_data <- data.frame(
    x_left = rep(as.numeric(factor("1 Year", levels = c("Baseline", "1 Year"))), length(density_df$density)),
    x_right = rep(as.numeric(factor("1 Year", levels = c("Baseline", "1 Year"))), length(density_df$density)) + density_df$density,
    y = density_df$scaled_x
  )
  
  # Data for the transition line
  transition_line <- data.frame(
    Timepoint = c(1, 2),
    Outcome = c(patient_data$Baseline, patient_data$Predicted)
  )
  
  # Create the plot
  p <- ggplot() +
    
    # Dotted line from Baseline to Predicted score
    geom_line(data = transition_line, aes(x = Timepoint, y = Outcome), 
              linetype = "dashed", size = 1, color = "#35B779", alpha = 0.4) +
    
    # Shaded cone from baseline to prediction interval bounds
    geom_polygon(data = cone_data, aes(x = Timepoint, y = Outcome, group = 1), fill = "#35B779", alpha = 0.2) +
    
    geom_segment(data = density_plot_data, 
                 aes(x = x_left, xend = x_right, y = y, yend = y),
                 color = "#35B779", alpha = 0.3, size = 1) +
    
    # Error bars for prediction interval
    geom_errorbar(data = patient_data, aes(x = factor("1 Year"), ymin = Lower, ymax = Upper), 
                  width = 0.05, size = 1, color = "black") +
    
    # Predicted value point
    geom_point(data = patient_data, aes(x = factor("1 Year"), y = Predicted), 
               color = "#D55E00", size = 7, alpha = 1) +
    
    # Baseline value point
    geom_point(data = patient_data, aes(x = factor("Baseline"), y = Baseline), 
               color = "#007589", size = 7, alpha = 0.9) +
    
    # Add text labels
    geom_text(data = patient_data, aes(x = factor("1 Year"), y = Upper, label = Upper), vjust = 0.4, hjust = 0, size = 7, nudge_x = 0.1) +
    geom_text(data = patient_data, aes(x = factor("1 Year"), y = Lower, label = Lower), vjust = 0.4, hjust = 0, size = 7, nudge_x = 0.1) +
    geom_text(data = patient_data, aes(x = factor("1 Year"), y = Predicted, label = Predicted), vjust = 0.4, hjust = 0, size = 7, fontface = "bold", nudge_x = 0.1) +
    
    # Formatting axes and labels
    scale_x_discrete(expand = c(0.1, 0)) +
    scale_y_continuous(limits = y_limits, breaks = y_breaks) +
    labs(
      title = base_title,
      subtitle = "<span style='color:#0073C2;'>Baseline Score</span> and <span style='color:#D55E00;'>Predicted Value</span>, along with<br>its <b>80%-PI</b> and <span style='color:#35B779;'>Distribution of Bootstrapped Predictions</span>.",
      x = "Timepoint",
      y = y_label
    ) +
    
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
      plot.subtitle = ggtext::element_markdown(hjust = 0.5, size = 14),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black")
    )
  
  return(p)
}


#' For Shiny App
#' Plot a patient's predicted outcome against their baseline value
#'
#' This function creates a transition chart from baseline to predicted outcome
#' using a density-scaled prediction interval as in the original visual style.
#'
#' @param df A one-row data frame containing Predicted, Lower, Upper, Baseline, and bootstrap_samples.
#' @param scale_type Either 'pain' or 'function' to adjust axis limits and labels.
#'
#' @return A ggplot object.
#' Plot a patient's predicted outcome against their baseline value
#'
#' This function creates a transition chart from baseline to predicted outcome
#' using a density-scaled prediction interval as in the original visual style.
#'
#' @param df A one-row data frame containing Predicted, Lower, Upper, Baseline, and bootstrap_samples.
#' @param scale_type Either 'pain' or 'function' to adjust axis limits and labels.
#'
#' @return A ggplot object.
plot_patient_prediction_from_input <- function(df, scale_type = "pain") {
  # Set axis settings and labels based on outcome type
  if (scale_type == "pain") {
    y_limits <- c(0, 10)
    y_breaks <- seq(0, 10, 2)
    base_title <- "Pain Progression for an Individual Patient"
    y_label <- "Pain (NRS 0-10)"
  } else {
    y_limits <- c(-2.5, 100)
    y_breaks <- seq(0, 100, 20)
    base_title <- "Function Progression for an Individual Patient"
    y_label <- "Function Score (bMHQ score 0-100)"
  }
  
  # Return early if any required values are missing
  required_cols <- c("Predicted", "Lower", "Upper", "Baseline", "bootstrap_samples")
  if (any(!required_cols %in% names(df))) {
    stop("Missing required columns in input data frame.")
  }
  if (any(is.na(df[, required_cols]))) {
    stop("Missing values detected in prediction or interval components.")
  }
  
  # Timepoint labels
  timepoints <- factor(c("Baseline", "1 Year"), levels = c("Baseline", "1 Year"))
  
  # Create transition and cone data
  df_plot <- data.frame(Timepoint = timepoints, Outcome = c(df$Baseline, df$Predicted))
  cone_data <- data.frame(
    Timepoint = factor(c("Baseline", "1 Year", "1 Year", "Baseline"), levels = timepoints),
    Outcome = c(df$Baseline, df$Lower, df$Upper, df$Baseline)
  )
  
  # Generate density from bootstrapped predictions
  bootstrap_vec <- unlist(df$bootstrap_samples)
  if (length(bootstrap_vec) == 0 || all(is.na(bootstrap_vec))) {
    stop("bootstrap_samples are missing or empty.")
  }
  density_vals <- density(bootstrap_vec)
  
  # Scale density x-axis properly (avoid recycling mismatch)
  scaled_range <- as.numeric(df$Upper) - as.numeric(df$Lower)
  scaled_x <- (density_vals$x - min(density_vals$x)) / (max(density_vals$x) - min(density_vals$x)) * scaled_range + as.numeric(df$Lower)
  
  density_df <- data.frame(
    scaled_x = scaled_x,
    density = density_vals$y
  )
  
  max_density <- max(density_df$density)
  density_df$density <- density_df$density / max_density * 0.3
  
  density_plot_data <- data.frame(
    x_left = rep(2, length(density_df$density)),
    x_right = rep(2, length(density_df$density)) + density_df$density,
    y = density_df$scaled_x
  )
  
  df$Predicted <- round(df$Predicted, 1)
  df$Lower <- round(df$Lower, 1)
  df$Upper <- round(df$Upper, 1)
  
  df$PredictedLabel <- as.character(df$Predicted)
  df$LowerLabel <- as.character(df$Lower)
  df$UpperLabel <- as.character(df$Upper)
  
  # Plot
  ggplot() +
    geom_line(data = df_plot, aes(x = as.numeric(Timepoint), y = Outcome), 
              linetype = "dashed", size = 1, color = "#35B779", alpha = 0.4) +
    geom_polygon(data = cone_data, aes(x = as.numeric(Timepoint), y = Outcome, group = 1), 
                 fill = "#35B779", alpha = 0.2) +
    geom_segment(data = density_plot_data, 
                 aes(x = x_left, xend = x_right, y = y, yend = y),
                 color = "#35B779", alpha = 0.3, size = 1) +
    geom_errorbar(data = df, aes(x = 2, ymin = Lower, ymax = Upper), 
                  width = 0.05, size = 1, color = "black") +
    geom_point(data = df, aes(x = 2, y = Predicted), 
               color = "#D55E00", size = 7) +
    geom_point(data = df, aes(x = 1, y = Baseline), 
               color = "#007589", size = 7) +
    geom_text(data = df, aes(x = 2, y = Upper, label = UpperLabel), vjust = 0.4, hjust = -1.5, size = 7) +
    geom_text(data = df, aes(x = 2, y = Lower, label = LowerLabel), vjust = 0.4, hjust = -1.5, size = 7) +
    geom_text(data = df, aes(x = 2, y = Predicted, label = PredictedLabel), vjust = 0.4, hjust = -1.5, size = 7, fontface = "bold") +
    scale_x_continuous(breaks = c(1, 2), labels = c("Baseline", "1 Year"), expand = c(0.1, 0)) +
    scale_y_continuous(limits = y_limits, breaks = y_breaks) +
    labs(
      title = base_title,
      subtitle = "<span style='color:#0073C2;'>Baseline Score</span> and <span style='color:#D55E00;'>Predicted Value</span>,<br>along with its <b>80%-PI</b> and <span style='color:#35B779;'>uncertainty distribution</span>.",
      x = "Timepoint",
      y = y_label
    ) +
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
      plot.subtitle = ggtext::element_markdown(hjust = 0.5, size = 14),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black")
    )
}


#' Generate comprehensive diagnostic plots for all models in evaluation results
#'
#' @param comprehensive_results Results from run_comprehensive_evaluation()
#' @param train_xgb_pain Training data for pain model
#' @param test_xgb_pain Test data for pain model  
#' @param train_xgb_function Training data for function model
#' @param test_xgb_function Test data for function model
#' @param output_dir Directory to save plots (default: "output")
#'
#' @return List of generated file paths
generate_all_diagnostic_plots <- function(comprehensive_results, 
                                          train_xgb_pain, test_xgb_pain,
                                          output_dir = "output") {
  
  # Generate and save the rainfall error plots
  plots <- plot_three_way_comparison(comprehensive_results$comparison)
  
  # Save the rainfall plots
  if (!is.null(plots$pain_error_rainfall)) {
    ggsave("output/three_way_pain_error_rainfall.png", 
           plots$pain_error_rainfall, width = 11, height = 8, dpi = 300)
    cat("Saved pain error rainfall plot\n")
  }
  
  # Model performance summary plots
  perf_plot <- plot_model_performance_summary(comprehensive_results)
  ggsave(file.path(output_dir, "model_performance_summary.png"),
         perf_plot, width = 8, height = 6, dpi = 300)
  
  perf_pain <- plot_model_performance_by_outcome(comprehensive_results, "Pain")
  if (!is.null(perf_pain)) {
    ggsave(file.path(output_dir, "model_performance_pain.png"),
           perf_pain, width = 8, height = 6, dpi = 300)
  }
  
  cat("Saved model performance summary plots\n")
  
  cat("Generating comprehensive diagnostic plots for all 6 models...\n")
  
  # Create output directory
  dir.create(output_dir, showWarnings = FALSE)
  
  # Define feature set
  X_pain <- setdiff(colnames(train_xgb_pain), c("fid", "pain_1y"))
  
  methods <- c("baseline", "bayesian", "stepwise")
  outcomes <- list(
    list(name = "pain", data = test_xgb_pain, features = X_pain, target = "pain_1y")
    )
  
  generated_files <- c()
  
  # Generate diagnostic plots for all 6 combinations
  for (outcome in outcomes) {
    for (method in methods) {
      
      model_key <- paste0("model_", method, "_", outcome$name)
      bootstrap_key <- paste0("bootstrap_", method, "_", outcome$name)
      
      if (!is.null(comprehensive_results$comparison[[model_key]])) {
        
        cat(sprintf("Evaluating %s %s model...\n", method, outcome$name))
        
        # Get model and data
        model <- comprehensive_results$comparison[[model_key]]
        test_data <- outcome$data
        features <- outcome$features
        target <- outcome$target
        
        # Generate predictions
        test_matrix <- xgb.DMatrix(
          data = as.matrix(test_data[, features, drop = FALSE]),
          label = test_data[[target]]
        )
        predictions <- predict(model, test_matrix)
        
        # Get prediction intervals if available
        pi_lower <- NULL
        pi_upper <- NULL
        bootstrap_data <- comprehensive_results$comparison[[bootstrap_key]]
        if (!is.null(bootstrap_data) && !is.null(bootstrap_data$predictions)) {
          pi_lower <- bootstrap_data$predictions$lb
          pi_upper <- bootstrap_data$predictions$ub
        }
        
        # Call evaluate_xgb_model to generate all diagnostic plots
        tryCatch({
          eval_results <- evaluate_xgb_model(
            model = model,
            y_true = test_data[[target]],
            y_pred = predictions,
            pi_lower = pi_lower,
            pi_upper = pi_upper,
            label = tools::toTitleCase(method),
            save_prefix = paste0(gsub("_model", "", outcome$name), "_", method)
          )
          
          # Track generated files
          prefix <- paste0(gsub("_model", "", outcome$name), "_", method)
          method_lower <- tolower(tools::toTitleCase(method))
          new_files <- c(
            file.path(output_dir, paste0(prefix, "_residuals_", method_lower, ".png")),
            file.path(output_dir, paste0(prefix, "_calibration_", method_lower, ".png")),
            file.path(output_dir, paste0(prefix, "_piwidths_", method_lower, ".png")),
            file.path(output_dir, paste0(prefix, "_coverage_", method_lower, ".png")),
            file.path(output_dir, paste0(prefix, "_distribution_combined_", method_lower, ".png"))
          )
          generated_files <- c(generated_files, new_files)
          
          cat(sprintf("✓ Successfully generated plots for %s %s model\n", method, outcome$name))
          
        }, error = function(e) {
          cat(sprintf("✗ Error generating plots for %s %s model: %s\n", method, outcome$name, e$message))
        })
      } else {
        cat(sprintf("⚠ Model not found: %s\n", model_key))
      }
    }
  }
  
  # List actually generated files
  output_files <- list.files(output_dir, pattern = "*.png", full.names = TRUE)
  if (length(output_files) > 0) {
    cat(sprintf("\n✓ Generated %d diagnostic plot files:\n", length(output_files)))
    for (file in output_files) {
      cat(sprintf("  - %s\n", basename(file)))
    }
  } else {
    cat("\n⚠ No plot files were generated - check for errors above\n")
  }
  
  return(output_files)
}


#' Generate comprehensive model interpretation plots
#'
#' This function creates all model interpretation plots including tree visualization,
#' SHAP analysis, learning curves, and counterfactual analysis
#'
#' @param comprehensive_results Results from run_comprehensive_evaluation()
#' @param train_xgb_pain Training data for pain model
#' @param save_plots Whether to save plots to files
#' @param output_dir Directory to save plots
#'
#' @return List of interpretation plots and results
generate_model_interpretation <- function(comprehensive_results, 
                                          train_xgb_pain, 
                                          save_plots = TRUE,
                                          output_dir = "output/") {
  
  cat("Generating comprehensive model interpretation analysis...\n")
  
  if (save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Extract winning models
  winning_methods <- comprehensive_results$winners
  pain_winner <- winning_methods$pain$overall_winner %>% tolower()
  
  pain_winning_model <- comprehensive_results$comparison[[paste0("model_", pain_winner, "_pain")]]
  
  interpretation_results <- list()
  
  # 1. Learning Curves
  cat("Creating learning curves...\n")
  interpretation_results$learning_curves <- list(
    pain = create_three_way_learning_curves(comprehensive_results, "pain")
  )
  
  interpretation_results$winner_iteration_curves <-
    create_winner_iteration_learning_curves(comprehensive_results)
  
  # 2. Tree Visualizations
  cat("Creating tree visualizations...\n")
  interpretation_results$tree_plots <- list(
    pain = create_tree_visualization(pain_winning_model, "Pain", pain_winner)
  )
  
  # 3. SHAP Analysis
  cat("Performing SHAP analysis...\n")
  interpretation_results$shap_analysis <- list(
    pain = create_shap_analysis(pain_winning_model, train_xgb_pain, "pain")
  )
  
  # 4. Counterfactual Analysis
  cat("Performing counterfactual analysis...\n")
  interpretation_results$counterfactual_analysis <- list(
    pain = create_counterfactual_analysis(pain_winning_model, train_xgb_pain)
  )
  
  # Save plots if requested
  if (save_plots) {
    save_interpretation_plots(interpretation_results, output_dir)
  }
  
  cat("Model interpretation analysis completed!\n")
  return(interpretation_results)
}

#' Create three-way learning curve comparison
#'
#' @param comprehensive_results Results containing learning curves
#' @param outcome_type "pain"
#'
#' @return Combined plot object
create_three_way_learning_curves <- function(comprehensive_results, outcome_type) {
  
    plots <- list(
      comprehensive_results$learning_curves$pain_baseline$plot +
        ggtitle("Pain: Baseline") +
        coord_cartesian(ylim = c(0, 1.75)),
      
      comprehensive_results$learning_curves$pain_bayesian$plot +
        ggtitle("Pain: Bayesian") +
        coord_cartesian(ylim = c(0, 1.75)),
      
      comprehensive_results$learning_curves$pain_stepwise$plot +
        ggtitle("Pain: Stepwise") +
        coord_cartesian(ylim = c(0, 1.75))
    )

  return(gridExtra::grid.arrange(grobs = plots, ncol = 3))
}

#' Create tree visualization
#'
#' @param model XGBoost model object
#' @param outcome_name Name for the outcome (for titles)
#' @param method_name Name of the method used
#'
#' @return Tree plot
create_tree_visualization <- function(model, outcome_name, method_name) {
  
  # Note: xgb.plot.tree() doesn't return a ggplot object, so we capture it differently
  cat(sprintf("Creating tree visualization for %s %s model...\n", outcome_name, method_name))
  
  # Create a wrapper that can be called later
  tree_plot_function <- function() {
    xgb.plot.tree(model = model, trees = 1)
  }
  
  return(tree_plot_function)
}

#' Create SHAP analysis
#'
#' @param model XGBoost model object
#' @param train_data Training data
#' @param outcome_type "pain"
#'
#' @return List with SHAP objects and plots
create_shap_analysis <- function(model, train_data, outcome_type = "pain") {
  
  outcome_type <- match.arg(outcome_type)
  
  # Determine target column
  target_col <- "pain_1y"
  
  shap_values <- shapviz(
    model,
    X_pred = as.matrix(train_data %>% select(-fid, -all_of(target_col))),
    X = train_data %>% select(-fid, -all_of(target_col))
  )
  
  feature_names <- colnames(shap_values$S)
  matched <- intersect(names(predictor_labels), feature_names)
  colnames(shap_values$S)[match(matched, feature_names)] <- predictor_labels[matched]
  if (!is.null(shap_values$X)) {
    colnames(shap_values$X)[match(matched, colnames(shap_values$X))] <- predictor_labels[matched]
  }
  shap_values$feature_names <- colnames(shap_values$S)
  
  shap_plots <- list(
    summary = function() {
      sv_importance(shap_values, kind = "beeswarm", max_display = 15) +
        theme_classic() +
        theme(panel.grid = element_blank(),
              plot.caption = ggtext::element_markdown(hjust = 0)) +
        labs(
          caption = "**Abbreviations.** **IP**, Interphalangeal; **ROM**, Range of motion;
**bMHQ**, Brief Michigan \nHand Questionnaire; **ADL**, Activities of Daily Living;
**MCP**, Metacarpophalangeal; \n**VAS**, Visual analogue scale."
        )
    },
    waterfall = function() sv_waterfall(
      shap_values,
      row_id = ifelse(outcome_type == "pain", 12, 6)
    )
  )
  
  return(list(
    shap_values = shap_values,
    plots = shap_plots
  ))
}

#' Create counterfactual analysis
#'
#' @param model XGBoost model object
#' @param train_data Training data
#' @param outcome_type "pain"
#'
#' @return List with counterfactual results and plots
create_counterfactual_analysis <- function(model, train_data, outcome_type = "pain") {
  
  outcome_type <- match.arg(outcome_type)
  
  # Check if analyze_counterfactuals function exists
  if (!exists("analyze_counterfactuals")) {
    cat("Warning: analyze_counterfactuals function not found. Skipping counterfactual analysis.\n")
    return(list(
      results = NULL,
      plots = NULL,
      message = "analyze_counterfactuals function not available"
    ))
  }
  
  # Select patient for analysis
  patient_data <- train_data[1, ]
  target_col <- "pain_1y"
  
  cat(sprintf("=== COUNTERFACTUAL ANALYSIS: PAIN MODEL ===\n", toupper(outcome_type)))
  cat("Patient ID:", patient_data$fid, "\n")
  cat("Actual outcome:", patient_data[[target_col]], "\n")
  
  cat("Current values - ROM IP:", patient_data$romip, "°, Grip:", patient_data$gripstr,
      "kg, Age:", patient_data$ageexam, ", Rest Pain:", patient_data$painrestnrs, "\n\n")
  
  # Perform counterfactual analysis
  tryCatch({
    counterfactual_results <- analyze_counterfactuals(
      model, 
      patient_data, 
      train_data
    )
    
    # Create counterfactual plot
    counterfactual_plot <- plot_counterfactual_results(
      counterfactual_results,
      patient_data$fid
    )
    
    # Generate summary
    summary_text <- generate_counterfactual_summary(counterfactual_results)
    
    return(list(
      results = counterfactual_results,
      plot = counterfactual_plot,
      summary = summary_text,
      patient_id = patient_data$fid
    ))
    
  }, error = function(e) {
    cat("Error in counterfactual analysis:", e$message, "\n")
    return(list(
      results = NULL,
      plot = NULL,
      error = e$message
    ))
  })
}

#' Plot counterfactual results
#'
#' @param results Counterfactual analysis results
#' @param patient_id Patient identifier
#'
#' @return ggplot object
plot_counterfactual_results <- function(results, patient_id = "") {
  
  # Remove baseline row for plotting changes
  plot_data <- results[-1, ]
  
  # For pain: negative change is good (pain reduction)
  plot_data$Beneficial <- plot_data$Change_from_Baseline < 0
  y_label <- "Change in Predicted Pain"
  title <- "Counterfactual Analysis: Impact on Predicted Pain"
  
  ggplot(plot_data, aes(x = reorder(Scenario, Change_from_Baseline), y = Change_from_Baseline)) +
    geom_col(aes(fill = Beneficial)) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
    labs(title = title,
         subtitle = paste("Patient ID:", patient_id),
         x = "Intervention Scenario",
         y = y_label) +
    scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "darkred"), 
                      name = "Effect", labels = c("Beneficial", "Harmful")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip()
}

#' Generate counterfactual summary
#'
#' @param results Counterfactual analysis results
#' @param outcome_type "pain"
#'
#' @return Summary text
generate_counterfactual_summary <- function(results) {
  
  best_intervention <- results[-1, ][which.min(results[-1, ]$Change_from_Baseline), ]
  summary <- sprintf("Most Beneficial Intervention: %s (%.2f point reduction in pain)",
                     best_intervention$Scenario, abs(best_intervention$Change_from_Baseline))
  
  return(summary)
}

#' Save interpretation plots to files
#'
#' @param interpretation_results Results from generate_model_interpretation()
#' @param output_dir Directory to save plots
#'
save_interpretation_plots <- function(interpretation_results, output_dir) {
  
  cat("Saving interpretation plots...\n")
  
  # Save learning curves
  if (!is.null(interpretation_results$learning_curves$pain)) {
    ggsave(file.path(output_dir, "learning_curves_pain.png"), 
           interpretation_results$learning_curves$pain, 
           width = 15, height = 5, dpi = 300)
  }
  
  if (!is.null(interpretation_results$winner_iteration_curves$pain)) {
    ggsave(file.path(output_dir, "winner_learning_curve_pain.png"),
           interpretation_results$winner_iteration_curves$pain +
             theme(plot.title = element_text(hjust = 0)),
           width = 8, height = 5, dpi = 300)
  }
  
  # Save counterfactual plots
  if (!is.null(interpretation_results$counterfactual_analysis$pain$plot)) {
    ggsave(file.path(output_dir, "counterfactual_pain.png"), 
           interpretation_results$counterfactual_analysis$pain$plot, 
           width = 10, height = 6, dpi = 300)
  }
  
  # Save SHAP plots
  if (!is.null(interpretation_results$shap_analysis$pain)) {
    shap_pain <- interpretation_results$shap_analysis$pain
    if (!is.null(shap_pain$plots$summary)) {
      ggsave(file.path(output_dir, "shap_pain_summary.png"),
             shap_pain$plots$summary(),
             width = 8, height = 6, dpi = 300)
    }
    if (!is.null(shap_pain$plots$waterfall)) {
      ggsave(file.path(output_dir, "shap_pain_waterfall.png"),
             shap_pain$plots$waterfall(),
             width = 8, height = 6, dpi = 300)
    }
  }
  
  if (!is.null(interpretation_results$winner_iteration_curves$pain)) {
    ggsave(file.path(output_dir, "winner_learning_curve_pain.png"),
           interpretation_results$winner_iteration_curves$pain +
             theme(plot.title = element_text(hjust = 0)),
           width = 8, height = 5, dpi = 300)
  }

  # xgb.plot.tree(pain_winning_model, trees = 1)

  cat("Interpretation plots saved to:", output_dir, "\n")
}


#' Extract calibration curves from existing bootstrap results (without binning)
#'
#' This function takes the output from XGB_bootstrap_PI_importance and creates
#' bootstrap calibration curves with confidence bands using actual prediction values
#' instead of binned summaries for maximum precision
#'
#' @param bootstrap_result Output from XGB_bootstrap_PI_importance function
#' @param confidence_level Confidence level for bands (default: 0.95)
#' @param model_name Name for plot labels
#' @param save_path Optional path to save plot
#' @param smooth_method Smoothing method for trends ("loess" or "gam", default: "loess")
#' @param n_points Number of points to evaluate confidence bands at (default: 50)
#' @param scale_type A string specifying whether the plot is for "pain" or "function".
#'   Determines axis scaling.
#'   
#' @return List with calibration plot and statistics
create_calibration_from_bootstrap <- function(bootstrap_result,
                                              confidence_level = 0.95,
                                              model_name = "Model",
                                              save_path = NULL,
                                              smooth_method = "loess",
                                              n_points = 50,
                                              scale_type = "pain",
                                              remove_grid = FALSE,
                                              remove_titles = FALSE,
                                              remove_caption = FALSE) {
  
  # Extract the data we need from your existing bootstrap results
  predictions_df <- bootstrap_result$predictions
  bootstrap_matrix <- bootstrap_result$base_predictions  # Use base predictions without noise
  y_true <- predictions_df$Actual
  y_pred_median <- predictions_df$Predicted
  
  cat("Creating calibration curves from", nrow(bootstrap_matrix), "existing bootstrap samples...\n")
  cat("Using actual values (no binning) for maximum precision\n")
  
  # Create a data frame with all actual prediction-observation pairs
  original_data <- data.frame(
    predicted = y_pred_median,
    observed = y_true
  )
  
  # Calculate bootstrap confidence bands using smoothed curves
  n_bootstrap <- nrow(bootstrap_matrix)
  
  # Define axis limits and breaks based on outcome type
  if (scale_type == "pain") {
    axis_limits <- c(0, 10)
    axis_breaks <- seq(0, 10, by = 2)
  } else if (scale_type == "function") {
    axis_limits <- c(0, 100)
    axis_breaks <- seq(0, 100, by = 20)
  } else {
    stop("Invalid scale_type. Use 'pain' or 'function'.")
  }

  # Define evaluation points across the prediction range
  pred_range <- range(y_pred_median, na.rm = TRUE)
  eval_points <- seq(pred_range[1], pred_range[2], length.out = n_points)
  
  # Calculate smoothed calibration curves for each bootstrap iteration
  bootstrap_curves <- matrix(nrow = n_bootstrap, ncol = n_points)
  
  cat("Computing bootstrap calibration curves...\n")
  for (b in 1:n_bootstrap) {
    bootstrap_predictions <- bootstrap_matrix[b, ]
    
    # Create data frame for this bootstrap iteration
    boot_data <- data.frame(
      predicted = bootstrap_predictions,
      observed = y_true
    )
    
    # Fit smooth curve to this bootstrap sample
    tryCatch({
      if (smooth_method == "loess") {
        smooth_fit <- loess(observed ~ predicted, data = boot_data, span = 0.75)
        bootstrap_curves[b, ] <- predict(smooth_fit, newdata = data.frame(predicted = eval_points))
      } else if (smooth_method == "gam") {
        # Requires mgcv package
        smooth_fit <- mgcv::gam(observed ~ s(predicted), data = boot_data)
        bootstrap_curves[b, ] <- predict(smooth_fit, newdata = data.frame(predicted = eval_points))
      }
    }, error = function(e) {
      # Fallback to linear interpolation if smoothing fails
      bootstrap_curves[b, ] <<- approx(boot_data$predicted, boot_data$observed, 
                                       xout = eval_points, rule = 2)$y
    })
  }
  
  # Calculate confidence bands
  alpha <- 1 - confidence_level
  lower_quantile <- alpha / 2
  upper_quantile <- 1 - alpha / 2
  
  confidence_bands <- data.frame(
    predicted = eval_points,
    lower_band = apply(bootstrap_curves, 2, quantile, lower_quantile, na.rm = TRUE),
    upper_band = apply(bootstrap_curves, 2, quantile, upper_quantile, na.rm = TRUE)
  )
  
  # Fit original smooth curve
  if (smooth_method == "loess") {
    original_smooth <- loess(observed ~ predicted, data = original_data, span = 0.75)
    confidence_bands$original_smooth <- predict(original_smooth, newdata = data.frame(predicted = eval_points))
  } else if (smooth_method == "gam") {
    original_smooth <- mgcv::gam(observed ~ s(predicted), data = original_data)
    confidence_bands$original_smooth <- predict(original_smooth, newdata = data.frame(predicted = eval_points))
  }
  
  # Create the plot with actual data points
  p <- ggplot() +
    # Individual observation points (with some transparency due to potential overlap)
    geom_point(data = original_data, aes(x = predicted, y = observed), 
               alpha = 0.4, size = 1, color = "gray60") +
    
    # Confidence band ribbon
    geom_ribbon(data = confidence_bands, 
                aes(x = predicted, ymin = lower_band, ymax = upper_band), 
                alpha = 0.3, fill = "steelblue") +
    
    # Perfect calibration line
    geom_abline(slope = 1, intercept = 0, 
                linetype = "dashed", color = "red", size = 1) +
    
    # Smoothed calibration curve
    geom_line(data = confidence_bands, 
              aes(x = predicted, y = original_smooth), 
              color = "darkblue", size = 1.5) +
    
    # Formatting
    labs(
      title = "Bootstrap Calibration Curve with Confidence Bands",
      subtitle = paste(model_name, "| Confidence Level:", 
                       paste0(confidence_level * 100, "%"), 
                       "| Method:", tools::toTitleCase(smooth_method)),
      x = "Predicted Values",
      y = "Observed Values",
      caption = paste("Based on", n_bootstrap, "bootstrap samples |",
                      "Individual observations shown as points |",
                      "Red dashed line = perfect calibration")
    ) +
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(hjust = 0, face = "bold", size = 18),
      plot.subtitle = element_text(hjust = 0, size = 16),
      plot.caption = element_text(hjust = 0, size = 14),
      legend.position = "right",
      axis.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 14)
    ) +
    (if (remove_grid) theme(panel.grid = element_blank()) else theme()) +
    (if (remove_titles) labs(title = NULL, subtitle = NULL) else labs()) +
    (if (remove_caption) labs(caption = NULL) else labs()) +
    scale_x_continuous(limits = axis_limits, breaks = axis_breaks) +
    scale_y_continuous(limits = axis_limits, breaks = axis_breaks) +
    coord_equal(ratio = 1)  # Equal scaling for better calibration assessment
  
  # Save plot if path provided
  if (!is.null(save_path)) {
    ggsave(save_path, p, width = 10, height = 8, dpi = 300)
    cat("Bootstrap calibration curve saved to:", save_path, "\n")
  }
  
  # Calculate calibration statistics using the smoothed curve
  calibration_stats <- list(
    calibration_slope = coef(lm(original_smooth ~ predicted, 
                                data = confidence_bands))[2],
    calibration_intercept = coef(lm(original_smooth ~ predicted, 
                                    data = confidence_bands))[1],
    mean_absolute_error = mean(abs(original_data$observed - original_data$predicted)),
    smoothed_mae = mean(abs(confidence_bands$original_smooth - confidence_bands$predicted), na.rm = TRUE),
    coverage_within_bands = calculate_pointwise_coverage(original_data, confidence_bands),
    n_bootstrap_samples = n_bootstrap,
    n_observations = nrow(original_data)
  )
  
  cat("Bootstrap Calibration Statistics (No Binning):\n")
  cat(sprintf("  Calibration Slope (smoothed): %.3f\n", calibration_stats$calibration_slope))
  cat(sprintf("  Raw MAE: %.3f\n", calibration_stats$mean_absolute_error))
  cat(sprintf("  Smoothed MAE: %.3f\n", calibration_stats$smoothed_mae))
  cat(sprintf("  Bootstrap samples used: %d\n", calibration_stats$n_bootstrap_samples))
  cat(sprintf("  Total observations: %d\n", calibration_stats$n_observations))
  
  return(list(
    plot = p,
    confidence_bands = confidence_bands,
    original_data = original_data,
    calibration_stats = calibration_stats,
    bootstrap_curves = bootstrap_curves
  ))
}

#' Calculate pointwise coverage for unbinned data
#' 
#' @param original_data Data frame with predicted and observed values
#' @param confidence_bands Data frame with confidence bands
#' 
#' @return Coverage percentage
calculate_pointwise_coverage <- function(original_data, confidence_bands) {
  
  # For each observation, interpolate the confidence bands at that prediction level
  within_bands <- sapply(1:nrow(original_data), function(i) {
    pred_val <- original_data$predicted[i]
    obs_val <- original_data$observed[i]
    
    # Find closest evaluation points
    closest_idx <- which.min(abs(confidence_bands$predicted - pred_val))
    
    # Check if observation falls within bands at that prediction level
    lower_bound <- confidence_bands$lower_band[closest_idx]
    upper_bound <- confidence_bands$upper_band[closest_idx]
    
    return(obs_val >= lower_bound & obs_val <= upper_bound)
  })
  
  coverage <- mean(within_bands, na.rm = TRUE) * 100
  return(coverage)
}



#' Plot Distribution Comparisons Between Training and Testing Sets
#'
#' This function compares variable distributions in training and test datasets
#' using density-normalized histograms with a clean, publication-friendly design.
#'
#' @param train_df A data frame with training data.
#' @param test_df A data frame with test data.
#' @param vars A character vector of variable names to compare.
#' @param bins Number of histogram bins (default: 30).
#'
#' @return A ggplot2 object showing overlaid histograms for each variable.
#' @export
plot_train_test_distributions <- function(train_df, test_df, vars, bins = 30, model_type = NULL) {
  # Ensure outcome variables are never plotted
  vars <- setdiff(vars, c("pain_1y"))
  
  stopifnot(all(vars %in% names(train_df)), all(vars %in% names(test_df)))
  
  combined <- dplyr::bind_rows(
    dplyr::mutate(dplyr::select(train_df, dplyr::all_of(vars)), Set = "Train"),
    dplyr::mutate(dplyr::select(test_df, dplyr::all_of(vars)), Set = "Test")
  )
  
  long_df <- tidyr::pivot_longer(combined, cols = -Set, names_to = "Variable", values_to = "Value")
  
  var_labels <- c(
    ageexam = "Age at examination, years",
    sex = "Sex",
    employment = "Currently employed",
    kapandji_grouped = "Thumb opposition",
    rommcp = "MCP joint range of motion",
    romip = "IP joint range of motion",
    pinchstr = "Key pinch strength, kg",
    gripstr = "Grip strength, kg",
    painrestnrs = "Pain at rest (0-10 scale)",
    painactivitiesnrs = "Pain during ADL (0-10 scale)",
    eq5d5lvas = "Quality of life VAS (0-100)",
    eq5d5l = "EQ-5D-5L utility index (0–1)",
    bmhq = "Hand function (Brief MHQ score, 0-100)",
    expectation_category = "Expectation fulfillment",
    domhandtreated = "Dominant hand affected",
    isDeQuervainTR = "Concurrent de Quervain release",
    surgeon_expertise = "Surgeon experience level (1–5 scale)"
  )
  
  long_df <- long_df %>%
    dplyr::mutate(Variable = dplyr::recode(Variable, !!!var_labels))
  
  
  # Construct dynamic title
  title_text <- if (!is.null(model_type)) {
    glue::glue("Train vs Test Distributions: {model_type} Model Predictors")
  } else {
    "Train vs Test Distributions"
  }
  
  ggplot2::ggplot(long_df, ggplot2::aes(x = Value, color = Set, fill = Set)) +
    ggplot2::geom_histogram(
      bins = bins,
      position = "identity",
      alpha = 0.3,
      ggplot2::aes(y = ..density..)
    ) +
    ggplot2::facet_wrap(~ Variable, scales = "free", ncol = 3) +
    ggplot2::scale_fill_manual(values = c("Train" = "#F28E2B", "Test" = "#4E79A7")) +
    ggplot2::scale_color_manual(values = c("Train" = "#F28E2B", "Test" = "#4E79A7")) +
    ggplot2::labs(
      title = title_text,
      x = "Value",
      y = "Density",
      fill = "Dataset",
      color = "Dataset"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    theme(
      strip.text = element_text(face = "bold", color = "black", size = 18),
      plot.title = element_text(face = "bold", size = 17, hjust = 0),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}


#' Generate and Save Train vs Test Distribution Plots for a Model
#'
#' @param model_type Character. Either "Pain" or "Function".
#' @param train_df Training dataset.
#' @param test_df Testing dataset.
#' @param top_vars Character vector of top variable names.
#'
#' @return Saved plot to the output directory. Invisibly returns the plot object.
generate_and_save_distribution_plot <- function(model_type, train_df, test_df, top_vars,
                                                remove_titles = FALSE, remove_grid = FALSE) {

  filename <- glue::glue("output/train_test_distribution_{tolower(model_type)}.png")
  
  plot <- plot_train_test_distributions(
    train_df = train_df,
    test_df = test_df,
    vars = top_vars,
    model_type = model_type
  )
  
  if (remove_titles) {
    plot <- plot + labs(title = NULL, subtitle = NULL)
  }
  
  if (remove_grid) {
    plot <- plot + theme(panel.grid = element_blank())
  }
  
  ggplot2::ggsave(filename, plot, width = 10, height = 7, dpi = 300)
  invisible(plot)
}

#' Generate bootstrap calibration plots for all methods
#'
#' Loops through each hyperparameter method (baseline, bayesian, stepwise)
#' and both outcomes to create calibration curves using existing bootstrap
#' results. Plots are saved in the specified output directory with filenames
#' `<outcome>_<method>_bootstrap_calibration.png`.
#'
#' @param comprehensive_results Comprehensive evaluation results object
#' @param output_dir Directory to save the calibration plots
#'
#' @return Character vector of saved file paths
generate_bootstrap_calibration_plots <- function(comprehensive_results,
                                                 output_dir = "output") {
  
  methods <- c("baseline", "bayesian", "stepwise")
  outcomes <- c("pain")
  generated <- c()
  
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  for (outcome in outcomes) {
    for (method in methods) {
      boot_key <- paste0("bootstrap_", method, "_", outcome)
      bootstrap_result <- comprehensive_results$comparison[[boot_key]]
      
      if (!is.null(bootstrap_result)) {
        model_name <- paste(tools::toTitleCase(outcome), "Model -",
                            tools::toTitleCase(method))
        file_name <- file.path(output_dir,
                               paste0(outcome, "_", method,
                                      "_bootstrap_calibration.png"))
        
        create_calibration_from_bootstrap(
          bootstrap_result = bootstrap_result,
          model_name = model_name,
          save_path = file_name,
          smooth_method = "loess"
        )
        
        generated <- c(generated, file_name)
      }
    }
  }
  
  if (length(generated) > 0) {
    cat(sprintf("\n✓ Generated %d bootstrap calibration plots:\n", length(generated)))
    for (f in generated) cat(sprintf("  - %s\n", basename(f)))
  } else {
    cat("\n⚠ No bootstrap calibration plots were generated\n")
  }
  
  return(generated)
}


#' Create learning curve plots for the winning methods
#'
#' This helper pulls the pre-computed learning curves from
#' `comprehensive_results$learning_curves` for the methods that were
#' selected as overall winners.  The curves show model performance as the
#' fraction of training data increases.
#'
#' @param comprehensive_results Comprehensive evaluation results containing the
#'   winners and learning curve objects
#'
#' @return List with ggplot objects for pain and function winners
create_winner_iteration_learning_curves <- function(comprehensive_results) {
  
  winning_methods <- comprehensive_results$winners
  pain_winner <- winning_methods$pain$overall_winner %>% tolower()
  
  pain_key <- paste0("pain_", pain_winner)
  
  pain_curve <- comprehensive_results$learning_curves[[pain_key]]
  
  result <- list(
    pain = if (!is.null(pain_curve)) pain_curve$plot else NULL
  )
  
  return(result)
}