#' Convert Columns to Expected Data Types
#'
#' Converts specific columns in a data frame to their appropriate data types, ensuring consistency 
#' for downstream processing. Handles numeric, character, and date conversions.
#'
#' @param df A data frame containing the raw data.
#'
#' @return A data frame with updated column types.
#'
#' @details
#' - Ensures `event` is numeric.
#' - Converts `sgopoth`, `sgopothtxt`, and `mhdiagnose` to character for consistent processing.
#' - Parses `imxrdtprae` as a date (`%Y-%m-%d`) and handles potential parsing errors.
#' - Converts `dropoutyn___1` to integer.
#' - Converts `gender` to integer (`1 = Male, 2 = Female`).
convert_column_types <- function(df) {
  df <- df %>%
    mutate(
      event = as.numeric(event),
      sgopoth = as.character(sgopoth),
      sgopothtxt = as.character(sgopothtxt),
      mhdiagnose = as.character(mhdiagnose),
      imxrdtprae = as.Date(imxrdtprae, format = "%Y-%m-%d"),
      dropoutyn___1 = as.integer(dropoutyn___1),
      gender = as.numeric(as.character(gender)) # Use column names directly
    )
  return(df)
}

#' Copy Values from Specific Events to Other Timepoints
#'
#' Copies values from specific events (e.g., Baseline) to other timepoints for the same patient (`regid`).
#' Ensures consistent information across all event timepoints.
#'
#' @param df A data frame with longitudinal patient data, including `event` and `regid` columns.
#' @param columns_event_0 Character vector of column names to copy from event 0.
#' @param columns_event_1 Character vector of column names to copy from event 1.
#'
#' @return A data frame with updated columns containing copied values.
#'
#' @details
#' - Uses `group_by(regid)` and `mutate()` to copy values efficiently.
#' - Copies only if `event == 0` (Baseline) or `event == 1` is available.
#' - Ensures safe copying when multiple records exist per patient.
#' - Missing values remain `NA` if event 0 or 1 is not found.
copy_event_info <- function(df, columns_event_0, columns_event_1) {
  df <- df %>%
    group_by(regid) %>%
    mutate(
      across(all_of(columns_event_0), ~ if_else(event != 0, first(.x[event == 0], default = NA), .x)),
      across(all_of(columns_event_1), ~ if_else(event != 1, first(.x[event == 1], default = NA), .x))
    ) %>%
    ungroup()
  return(df)
}

#' Apply General Data Transformations
#'
#' This function applies multiple transformations to clean and structure the data,
#' including type conversions, filtering, and renaming columns.
#'
#' @param df A data frame containing the raw data to be transformed.
#'
#' @return A transformed data frame.
#'
#' @details
#' - Converts `gender` to `sex` and removes the original column.
#' - Filters rows to include only specific `event` values (Baseline and 1-year follow-up).
#' - Converts numeric scores for pain to integers for consistency.
#' - Assumes `event` includes the specified values.
#'
apply_data_transformations <- function(df) {
  df <- df %>%
    mutate(event = as.integer(event)) %>%
    mutate(sex = as.integer(gender)) %>%
    select(-gender) %>%
    mutate(
      paintaetnrs = as.integer(paintaetnrs),
      painruhenrs = as.integer(painruhenrs)
    ) %>%
    filter(!event %in% c(2, 3, 6, 7, 8, 9, 10, 0))  # Keep 1 (Baseline) and 5 (1-year post-op)
  return(df)
}

#' Create New Variables for Analysis
#'
#' This function derives new variables from existing ones, recodes specific fields,
#' and drops redundant columns after transformation.
#'
#' @param df A data frame containing preprocessed data.
#'
#' @return A data frame with new variables and updated columns.
#'
#' @details
#' - Recodes `sgopothtxt` to identify surgeries related to De Quervain's Tendon Release.
#' - Creates `domhandtreated` to indicate whether the dominant hand was treated.
#' - Drops intermediate columns (`sgopothtxt`, `domhand`, `mhside`) after creating new variables.
#'
create_new_variables <- function(df) {
  df <- df %>%
    mutate(
      sgopothtxt = if_else(
        str_detect(
          sgopothtxt, 
          regex("Ringbandspaltung.*|RBS.*|A1-De Quervain's Tendon Release|A1 De Quervain's Tendon Release|Ringbandspaltunr Daumen.*", ignore_case = TRUE)
        ),
        "De Quervain's Tendon Release",
        sgopothtxt
      ),
      isDeQuervainTR = if_else(sgopothtxt == "De Quervain's Tendon Release", 1, 0)
    ) %>%
    select(-sgopothtxt, -sgopoth) %>%
    mutate(
      domhandtreated = if_else((mhside == 1 & domhand == 1) | (mhside == 0 & domhand == 0), 1, 0)
    ) %>%
    select(-domhand, -mhside)
  return(df)
}

#' Recode Surgeon Expertise
#'
#' This function recodes surgeon IDs into human-readable labels and categorizes their expertise levels.
#'
#' @param df A data frame containing surgeon IDs (`sgoperateur`).
#'
#' @return A data frame with surgeon expertise levels and recoded surgeon labels.
#'
#' @details
#' - Maps specific surgeon IDs to their corresponding names (e.g., `401879 -> "HED"`).
#' - Assigns expertise levels based on surgeon names (e.g., "HED" -> 5, "Other" -> 1).
#' - Removes intermediate columns (`sgoperateur`).
#'
recode_surgeon_expertise <- function(df) {
  df <- df %>%
    mutate(
      sgoperateur = case_when(
        sgoperateur == 401879 ~ "HED",
        sgoperateur == 402709 ~ "SST",
        sgoperateur == 893025 ~ "BROM",
        sgoperateur == 802436 ~ "REVA",
        sgoperateur == 981357 ~ "STXE",
        sgoperateur == 1004438 ~ "SCHGER",
        sgoperateur == 999999 ~ "Other",
        TRUE ~ as.character(sgoperateur)
      ),
      sgoperateur = if_else(is.na(sgoperateur) & fid %in% c(3164075, 4133729), "HED", sgoperateur),
      surgeon_expertise = case_when(
        sgoperateur %in% c("HED", "SST") ~ 5,
        sgoperateur == "SCHGER" ~ 3,
        sgoperateur == "Other" ~ 1,
        sgoperateur %in% c("REVA", "STXE", "BROM") ~ 2,
        TRUE ~ NA_real_
      )
    ) %>%
    select(-sgoperateur, -sgoperateur2)
  return(df)
}

#' Apply Final Transformations to Dataset
#'
#' Cleans, renames, and transforms the dataset for analysis. 
#' Extracts 1-year outcome data, merges them, and applies one-hot encoding to categorical variables.
#'
#' @param df A data frame containing cleaned patient data.
#'
#' @return A fully transformed data frame ready for modeling.
#'
#' @details
#' - Renames variables for clarity (`beruf -> employment`, `erwartreason -> treatmentgoal`, etc.).
#' - Extracts and merges 1-year outcome data (`pain_1y`).
#' - One-hot encodes `treatmentgoal` while handling missing values explicitly.
#' - Ensures meaningful encoding for `goal_pain_relief` based on `goal_pain_relief_at_rest/adl`.
final_transformations <- function(df) {
  df <- df %>%
    rename(
      employment = beruf,
      treatmentgoal = erwartreason,
      expectationgoal = erwartausmass,
      painrestnrs = painruhenrs,
      painactivitiesnrs = paintaetnrs,
      eq5d5l = eq5d5l_indexDE
    )
  
  # Extract 1-year data and merge back
  one_year_data <- df %>%
    filter(event == 5) %>%
    select(fid, painactivitiesnrs) %>%
    rename(pain_1y = painactivitiesnrs)
  
  df <- df %>%
    left_join(one_year_data, by = "fid") %>%
    filter(event != 5)  # Remove rows where event == 5
  
  # Drop columns that are no longer needed
  df <- df %>%
    select(
      -event, -regid, -redcap_event_name, -pid, -brthdt, -commentgen,
      -identifikation_complete, -timepoint, -examdt, -arztformular_complete,
      -sgopdt, -mhvorop, -imxrdtprae, -promdt, -patientenformular_complete,
      -gconsentCDP, -gconsent, -promstatus, -ageop, -agec, -touch_nickel, -revtouch_nickel, -sgrxbilddauer
    )
  
  # One-hot encode the 'treatmentgoal' variable
  df$treatmentgoal <- factor(df$treatmentgoal, exclude = NULL)  # Retain NA levels
  treatment_matrix <- model.matrix(~treatmentgoal - 1, df)
  
  # Define a mapping for renaming columns to more meaningful names
  column_map <- c(
    "treatmentgoal1" = "goal_thumb_mobility",
    "treatmentgoal2" = "goal_thumb_appearance",
    "treatmentgoal3" = "goal_daily_activities",
    "treatmentgoal4" = "goal_leisure_activities",
    "treatmentgoal5" = "goal_pain_relief",
    "treatmentgoal6" = "goal_improve_strength",
    "treatmentgoal7" = "goal_return_to_work",
    "treatmentgoal8" = "goal_pain_relief_at_rest",
    "treatmentgoal9" = "goal_pain_relief_adl"
  )
  
  # # EQ-5D-5L domain mapping (standardized health questionnaire)
  # eq5d5l_column_map <- c(
  #   "eq5d5l01" = "eq5d_mobility",          # Walking about
  #   "eq5d5l02" = "eq5d_selfcare",          # Washing or dressing myself  
  #   "eq5d5l03" = "eq5d_usual_activities",  # Usual activities (work, study, housework, family, leisure)
  #   "eq5d5l04" = "eq5d_pain_discomfort",   # Pain or discomfort
  #   "eq5d5l05" = "eq5d_anxiety_depression" # Anxiety or depression
  # )
  
  # # Rename EQ-5D-5L columns in the dataset
  # colnames(df) <- dplyr::recode(colnames(df), !!!eq5d5l_column_map)
  
  # Rename one-hot encoded columns
  colnames(treatment_matrix) <- dplyr::recode(colnames(treatment_matrix), !!!column_map)
  
  # Combine one-hot encoded variables with the original dataframe
  df <- cbind(df, treatment_matrix)
  
  # Update 'goal_pain_relief' based on 'goal_pain_relief_at_rest' or 'goal_pain_relief_adl'
  df <- df %>%
    mutate(
      goal_pain_relief = if_else(
        goal_pain_relief_at_rest == 1 | goal_pain_relief_adl == 1,
        1,
        goal_pain_relief
      )
    ) %>%
    # Remove unnecessary columns
    select(-treatmentgoalNA, -treatmentgoal, -goal_pain_relief_at_rest, -goal_pain_relief_adl)
  
  # Convert binary indicators to factor or integer (choose ONE style)
  df <- df %>%
    mutate(
      employment = as.integer(employment),
      isDeQuervainTR = as.integer(isDeQuervainTR),
      domhandtreated = as.integer(domhandtreated)
    )
  
  # Convert labelled columns to numeric
  df <- df %>%
    mutate(across(where(is.labelled), ~ as.numeric(.)))
  
  return(df)
}
