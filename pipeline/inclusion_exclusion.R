#' Filter Patients with Valid Surgery Timing
#'
#' This function filters the dataset to retain only patients who:
#' - Have a recorded surgery date (`sgopdt`).
#' - Had surgery at least 10 months ago (ensuring enough time for follow-up).
#'
#' @param df Data frame containing patient records.
#' @return Filtered data frame with recent or missing surgeries removed.
filter_surgery_date_valid <- function(df) {
  initial_count <- n_distinct(df$fid)
  date_10_months_ago <- Sys.Date() %m-% months(10)
  
  excluded_recent_surgery <- df %>%
    filter(is.na(sgopdt) | sgopdt > date_10_months_ago) %>%
    distinct(fid)
  
  cat("Excluded due to no surgery date or surgery <10 months ago (n =", nrow(excluded_recent_surgery), "):\n")
  
  df <- df %>%
    filter(!fid %in% excluded_recent_surgery$fid)
  
  final_count <- n_distinct(df$fid)
  cat("After filtering by surgery date:", final_count, "patients (", initial_count - final_count, " excluded)\n")
  
  return(df)
}

#' Exclude Patients Missing Both 1-Year Outcomes
#'
#' This function excludes patients who are missing both:
#' - Pain score at 1-year (`paintaetnrs`), and
#' - Function score at 1-year (`bmhq`)
#'
#' These patients provide no usable outcome for model training or evaluation.
#'
#' @param df Data frame containing patient records.
#' @return Filtered data frame with dual-missing outcome patients removed.
exclude_dual_missing_outcomes <- function(df) {
  initial_count <- n_distinct(df$fid)
  
  patients_missing_outcomes <- df %>%
    filter(event == 5 & is.na(paintaetnrs) & is.na(bmhq)) %>%
    distinct(fid)
  
  cat("Excluded due to missing both pain and bmhq at 1-year (n =", nrow(patients_missing_outcomes), "):\n")
  
  df <- df %>%
    filter(!fid %in% patients_missing_outcomes$fid)
  
  final_count <- n_distinct(df$fid)
  cat("After excluding dual-missing outcomes:", final_count, "patients (", initial_count - final_count, " excluded)\n")
  
  return(df)
}


#' Exclude Dropout Patients
#'
#' This function removes patients who dropped out before completing follow-ups.  
#' - Adjusts dropout indicators (`dropoutyn___1`, `dropoutyn___2`) based on dropout timing.  
#' - Patients marked as "dropout" after 12 months are retained.  
#' - Patients with `dropoutyn___2 == 1` or `dropoutyn___1 == 2` are removed.  
#' - Dropout-related columns are removed after filtering.
#'
#' @param df Data frame containing patient records.
#' @return Filtered data frame with dropout patients removed.
# 2. Exclude Dropouts
exclude_dropouts <- function(df) {
  initial_count <- n_distinct(df$fid)
  
  df <- df %>%
    mutate(
      droplast6wo = if_else(droplast6wo >= 12, NA_real_, droplast6wo),
      dropoutyn___2 = if_else(droplast6wo >= 12, 0, dropoutyn___2),
      dropoutdt = if_else(droplast6wo >= 12, NA, dropoutdt),
      dropteillast6wo = if_else(dropteillast6wo >= 12, NA_real_, dropteillast6wo),
      dropoutyn___1 = if_else(dropteillast6wo >= 12, 0, dropoutyn___1),
      dropteil = if_else(dropteillast6wo >= 12, NA, dropteil),
      dropoutyn___1 = if_else(dropteil == 2 & dropoutyn___1 == 1, 2, dropoutyn___1)
    ) %>%
    filter(dropoutyn___2 != 1 | is.na(dropoutyn___2)) %>%
    filter(dropoutyn___1 != 2 | is.na(dropoutyn___1)) %>%
    select(-dropoutyn___2, -droplast6wo, -dropoutyn___1, -dropteillast6wo, -dropteil, -dropoutdt)
  
  final_count <- n_distinct(df$fid)
  cat("Patients excluded (exclude_dropouts):", initial_count - final_count, "\n")
  
  return(df)
}

#' Filter Patients with Touch Prosthesis Only
#'
#' This function retains only patients who received the "Touch" prosthesis (`sgimp == 2`).  
#' - Removes any patients with a different or unspecified prosthesis.  
#' - Drops the `sgimp` column after filtering.
#'
#' @param df Data frame containing patient records.
#' @return Filtered data frame with only Touch prosthesis patients.
# 3. Filter Only Touch Prosthesis
filter_touch_prosthesis <- function(df) {
  initial_count <- n_distinct(df$fid)
  
  df <- df %>%
    filter(sgimp == 2) %>%
    select(-sgimp)
  
  final_count <- n_distinct(df$fid)
  cat("Patients excluded (filter_touch_prosthesis):", initial_count - final_count, "\n")
  
  return(df)
}

#' Filter Only Primary Osteoarthritis (OA) Patients
#'
#' This function retains only patients diagnosed with primary OA (`mhdiagnose == 1`).  
#' - Excludes patients with other conditions.  
#' - Drops the `mhdiagnose` column after filtering.
#'
#' @param df Data frame containing patient records.
#' @return Filtered data frame with only primary OA patients.
# 4. Filter Only Primary OA
filter_primary_oa <- function(df) {
  initial_count <- n_distinct(df$fid)
  
  df <- df %>%
    filter(mhdiagnose == 1) %>%
    select(-mhdiagnose)
  
  final_count <- n_distinct(df$fid)
  cat("Patients excluded (filter_primary_oa):", initial_count - final_count, "\n")
  
  return(df)
}

#' Filter Patients Who Provided Full Consent
#'
#' This function keeps only patients who gave full consent (`gconsentCDP == 2`).  
#' - Patients with partial or missing consent are excluded.
#'
#' @param df Data frame containing patient records.
#' @return Filtered data frame with only fully consenting patients.
# 5. Filter Patients Who Gave Full Consent
filter_full_consent <- function(df) {
  initial_count <- n_distinct(df$fid)
  
  df <- df %>%
    filter(gconsentCDP == 2)
  
  final_count <- n_distinct(df$fid)
  cat("Patients excluded (filter_full_consent):", initial_count - final_count, "\n")
  
  return(df)
}

#' Utility Function: Count Unique Patients
#'
#' This function counts and prints the number of unique patients (`fid`) in the dataset.
#'
#' @param df Data frame containing patient records.
#' @return Number of unique patients.
# Utility Function: Count Unique FIDs (Optional)
count_unique_fid <- function(df) {
  n <- n_distinct(df$fid)
  cat("Unique FIDs:", n, "\n")
  return(n)
}