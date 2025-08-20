#' Prepare Data for Summary Tables
#'
#' Creates grouped/factor variables needed for baseline and missing outcome tables.
#'
#' @param df A data.frame containing the transformed dataset.
#' @return Data frame with additional factor variables for table creation.
prepare_table_data <- function(df) {
  df %>%
    mutate(
      kapandji_grouped = case_when(
        kapandji >= 8 ~ "Good (8-10)",
        kapandji >= 6 ~ "Fair (6-7)",
        kapandji >= 4 ~ "Poor (4-5)",
        TRUE ~ "Very Poor (0-3)"
      ),
      kapandji_grouped = factor(
        kapandji_grouped,
        levels = c("Very Poor (0-3)", "Poor (4-5)", "Fair (6-7)", "Good (8-10)")
      ),
      expectation_category = case_when(
        expectationgoal == 1 ~ "Not at all",
        expectationgoal == 2 ~ "A little",
        expectationgoal == 3 ~ "Partially",
        expectationgoal == 4 ~ "Very much",
        expectationgoal == 5 ~ "Completely",
        TRUE ~ NA_character_
      ),
      expectation_category = factor(
        expectation_category,
        levels = c("Not at all", "A little", "Partially", "Very much", "Completely")
      ),
      sex = factor(sex, levels = c(0, 1), labels = c("Female", "Male")),
      employment = factor(employment, levels = c(0, 1), labels = c("No", "Yes")),
      domhandtreated = factor(domhandtreated, levels = c(0, 1), labels = c("No", "Yes")),
      isDeQuervainTR = factor(isDeQuervainTR, levels = c(0, 1), labels = c("No", "Yes"))
    )
}

#' Create Baseline Table (Table 1)
#'
#' Generates the stacked gtsummary table summarising baseline characteristics.
#'
#' @param df A data.frame with variables prepared via `prepare_table_data()`.
#' @return A `gt` table object.
create_baseline_table <- function(df) {
  df_for_table <- prepare_table_data(df)
  n_total <- nrow(df)
  
  demo_table <- df_for_table %>%
    select(ageexam, sex, employment) %>%
    tbl_summary(
      statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)"),
      digits = all_continuous() ~ 1,
      missing = "no",
      label = list(
        ageexam ~ "Age at examination, years",
        sex ~ "Sex",
        employment ~ "Currently employed"
      )
    ) %>%
    add_n(statistic = "{N_miss}", col_label = "**Missing**", last = TRUE)
  
  hand_func_table <- df_for_table %>%
    select(kapandji_grouped, rommcp, romip, pinchstr, gripstr) %>%
    tbl_summary(
      statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)"),
      digits = all_continuous() ~ 1,
      missing = "no",
      label = list(
        kapandji_grouped ~ "Thumb opposition ability (Kapandji 0\u201310, grouped)",
        rommcp ~ "MCP joint range of motion, degrees",
        romip ~ "IP joint range of motion, degrees",
        pinchstr ~ "Key pinch strength, kg",
        gripstr ~ "Grip strength, kg"
      )
    ) %>%
    add_n(statistic = "{N_miss}", col_label = "**Missing**", last = TRUE)
  
  patient_rep_table <- df_for_table %>%
    select(painrestnrs, painactivitiesnrs, eq5d5lvas, eq5d5l, bmhq, expectation_category) %>%
    tbl_summary(
      statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)"),
      digits = all_continuous() ~ 1,
      missing = "no",
      label = list(
        painrestnrs ~ "Pain at rest (0-10 scale)",
        painactivitiesnrs ~ "Pain during ADL (0-10 scale)",
        eq5d5lvas ~ "Quality of life VAS (0-100)",
        eq5d5l ~ "EQ-5D-5L utility index (0\u20131)",
        bmhq ~ "Hand function (Brief MHQ score, 0\u2013100)",
        expectation_category ~ "Surgical expectation fulfillment"
      )
    ) %>%
    add_n(statistic = "{N_miss}", col_label = "**Missing**", last = TRUE)
  
  surgical_table <- df_for_table %>%
    select(domhandtreated, isDeQuervainTR, surgeon_expertise) %>%
    tbl_summary(
      statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)"),
      digits = all_continuous() ~ 1,
      missing = "no",
      label = list(
        domhandtreated ~ "Dominant hand affected",
        isDeQuervainTR ~ "Concurrent de Quervain release",
        surgeon_expertise ~ "Surgeon experience level (1\u20135 scale)"
      )
    ) %>%
    add_n(statistic = "{N_miss}", col_label = "**Missing**", last = TRUE)
  
  tbl_stack(
    list(demo_table, hand_func_table, patient_rep_table, surgical_table),
    group_header = c("Demographics", "Hand Function Measures", "Patient-Reported Outcomes", "Surgical Factors")
  ) %>%
    modify_caption(glue::glue("**Table 1. Patient Characteristics at Baseline (N = {n_total})**"))  %>%
    as_gt() %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_row_groups()
    )
}

#' Create Tables by Outcome Missingness with Effect Sizes
#'
#' Builds a supplementary table comparing baseline characteristics of patients
#' with and without missing pain or function outcomes, including effect sizes.
#'
#' @param df A data.frame processed with `prepare_table_data()`.
#' @return A `gt` table summarizing pain outcome missingness.
create_missing_outcome_tables <- function(df) {
  df_for_table <- prepare_table_data(df)
  
  # Helper function to calculate Cohen's d for continuous variables
  cohens_d <- function(x, group) {
    group1 <- x[group == levels(as.factor(group))[1]]
    group2 <- x[group == levels(as.factor(group))[2]]
    
    group1 <- group1[!is.na(group1)]
    group2 <- group2[!is.na(group2)]
    
    if (length(group1) == 0 | length(group2) == 0) return(NA)
    
    n1 <- length(group1)
    n2 <- length(group2)
    
    mean1 <- mean(group1)
    mean2 <- mean(group2)
    
    sd1 <- sd(group1)
    sd2 <- sd(group2)
    
    pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
    
    d <- (mean1 - mean2) / pooled_sd
    return(d)
  }
  
  # Helper function to calculate effect size for categorical variables (Cramer's V)
  cramers_v <- function(x, group) {
    tryCatch({
      tbl <- table(x, group)
      if (any(dim(tbl) < 2)) return(NA)
      
      chi2 <- chisq.test(tbl)$statistic
      n <- sum(tbl)
      k <- min(dim(tbl)) - 1
      
      v <- sqrt(chi2 / (n * k))
      return(as.numeric(v))
    }, error = function(e) NA)
  }
  
  # Pain missing analysis
  pain_missing_df <- df_for_table %>%
    mutate(missing_status = if_else(is.na(pain_1y), "Missing", "Observed")) %>%
    select(
      missing_status, ageexam, sex, employment, kapandji_grouped,
      rommcp, romip, pinchstr, gripstr, painrestnrs, painactivitiesnrs,
      eq5d5lvas, eq5d5l, bmhq, expectation_category,
      domhandtreated, isDeQuervainTR, surgeon_expertise
    )
  
  # Calculate effect sizes for pain table
  continuous_vars <- c("ageexam", "rommcp", "romip", "pinchstr", "gripstr", 
                      "painrestnrs", "painactivitiesnrs", "eq5d5lvas", "eq5d5l", 
                      "bmhq", "surgeon_expertise")
  
  categorical_vars <- c("sex", "employment", "kapandji_grouped", "expectation_category",
                       "domhandtreated", "isDeQuervainTR")
  
  pain_effect_sizes <- list()
  
  # Calculate Cohen's d for continuous variables
  for (var in continuous_vars) {
    if (var %in% names(pain_missing_df)) {
      pain_effect_sizes[[var]] <- cohens_d(pain_missing_df[[var]], 
                                          pain_missing_df$missing_status)
    }
  }
  
  # Calculate Cramer's V for categorical variables
  for (var in categorical_vars) {
    if (var %in% names(pain_missing_df)) {
      pain_effect_sizes[[var]] <- cramers_v(pain_missing_df[[var]], 
                                           pain_missing_df$missing_status)
    }
  }
  
  pain_tbl <- pain_missing_df %>%
    tbl_summary(
      by = missing_status,
      statistic = list(
        all_continuous() ~ "{mean} ({sd})",
        all_categorical() ~ "{n} ({p}%)"
      ),
      digits = all_continuous() ~ 1,
      missing = "no",
      label = list(
        ageexam ~ "Age at examination, years",
        sex ~ "Sex",
        employment ~ "Currently employed",
        kapandji_grouped ~ "Thumb opposition ability (Kapandji 0–10, grouped)",
        rommcp ~ "MCP joint range of motion, degrees",
        romip ~ "IP joint range of motion, degrees",
        pinchstr ~ "Key pinch strength, kg",
        gripstr ~ "Grip strength, kg",
        painrestnrs ~ "Pain at rest (0-10 scale)",
        painactivitiesnrs ~ "Pain during ADL (0-10 scale)",
        eq5d5lvas ~ "Quality of life VAS (0-100)",
        eq5d5l ~ "EQ-5D-5L utility index (0–1)",
        bmhq ~ "Hand function (Brief MHQ score, 0–100)",
        expectation_category ~ "Surgical expectation fulfillment",
        domhandtreated ~ "Dominant hand affected",
        isDeQuervainTR ~ "Concurrent de Quervain release",
        surgeon_expertise ~ "Surgeon experience level (1–5 scale)"
      )
    ) %>%
    add_p(test = list(isDeQuervainTR ~ "fisher.test")) %>%
    add_difference() %>%
    modify_table_body(
      ~.x %>%
        mutate(
          effect_size = case_when(
            variable == "ageexam" ~ sprintf("%.2f", pain_effect_sizes[["ageexam"]]),
            variable == "sex" & row_type == "label" ~ sprintf("%.2f", pain_effect_sizes[["sex"]]),
            variable == "employment" ~ sprintf("%.2f", pain_effect_sizes[["employment"]]),
            variable == "kapandji_grouped" & row_type == "label" ~ sprintf("%.2f", pain_effect_sizes[["kapandji_grouped"]]),
            variable == "rommcp" ~ sprintf("%.2f", pain_effect_sizes[["rommcp"]]),
            variable == "romip" ~ sprintf("%.2f", pain_effect_sizes[["romip"]]),
            variable == "pinchstr" ~ sprintf("%.2f", pain_effect_sizes[["pinchstr"]]),
            variable == "gripstr" ~ sprintf("%.2f", pain_effect_sizes[["gripstr"]]),
            variable == "painrestnrs" ~ sprintf("%.2f", pain_effect_sizes[["painrestnrs"]]),
            variable == "painactivitiesnrs" ~ sprintf("%.2f", pain_effect_sizes[["painactivitiesnrs"]]),
            variable == "eq5d5lvas" ~ sprintf("%.2f", pain_effect_sizes[["eq5d5lvas"]]),
            variable == "eq5d5l" ~ sprintf("%.2f", pain_effect_sizes[["eq5d5l"]]),
            variable == "bmhq" ~ sprintf("%.2f", pain_effect_sizes[["bmhq"]]),
            variable == "expectation_category" & row_type == "label" ~ sprintf("%.2f", pain_effect_sizes[["expectation_category"]]),
            variable == "domhandtreated" ~ sprintf("%.2f", pain_effect_sizes[["domhandtreated"]]),
            variable == "isDeQuervainTR" ~ sprintf("%.2f", pain_effect_sizes[["isDeQuervainTR"]]),
            variable == "surgeon_expertise" & row_type == "label" ~ sprintf("%.2f", pain_effect_sizes[["surgeon_expertise"]]),
            TRUE ~ NA_character_
          )
        )
    ) %>%
    modify_header(effect_size ~ "**Effect Size**") %>%
    modify_caption("**Supplementary Table. Baseline Characteristics by Pain Outcome Missingness**") %>%
    as_gt(include = everything())
  
  pain_tbl
}