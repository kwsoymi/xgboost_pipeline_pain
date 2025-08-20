#' Load and Install Required Packages
#'
#' This function ensures that all specified packages are installed and loaded into the R environment. 
#' If a package is not installed, it will be installed with dependencies before being loaded.
#'
#' @param packages A character vector of package names to load.
#' @return None. Packages are loaded into the global environment.
load_libraries <- function(packages) {
  # Installiere alle fehlenden Pakete vorab
  new_pkgs <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)
  
  for (pkg in packages) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
}



#' Count Unique `fid` for Event 1
#'
#' This helper function counts and prints the number of unique `fid` values
#' where `event == 1` in the dataset.
#'
#' @param data A data frame containing the columns `event` and `fid`.
#' @return The original dataset (unchanged). The count is printed to the console.
count_unique_fid <- function(data) {
  unique_count <- data %>%
    filter(event == 1) %>%
    summarise(unique_fid_count = n_distinct(fid)) %>%
    pull(unique_fid_count)
  print(unique_count)
  return(data)
}

#' Split Dataset into Features and Labels
#'
#' This function takes a dataset and separates it into a matrix of features 
#' and a vector of labels based on the specified label column. 
#' It prepares data for machine learning tasks by ensuring numerical representation.
#'
#' @param dataset A data frame or tibble containing the dataset to be split.
#' @param label_col A string specifying the name of the column to be used as labels.
#' @return A list with two elements:
#'   \describe{
#'     \item{data}{A numeric matrix of features (all columns except the label column).}
#'     \item{labels}{A vector of labels (values from the specified label column).}
#'   }
split_data <- function(dataset, label_col) {
  data <- dataset %>%
    select(-all_of(label_col)) %>%
    data.matrix()
  labels <- dataset %>%
    pull(all_of(label_col))
  list(data = data, labels = labels)
}


#' Generate Summary Statistics for Dataset
#'
#' Computes summary statistics including unique `fid` count, binary variable percentages, 
#' and mean values for all numeric columns.
#'
#' @param data A data frame containing columns such as `fid`, `employment`, `isDeQuervainTR`, and `domhandtreated`, 
#'   along with numeric variables for summarization.
#' @return A data frame containing the following summary statistics:
#'   \describe{
#'     \item{fid_count}{Number of unique `fid` values in the dataset.}
#'     \item{employment_freq_1}{Percentage of rows where `employment == 1`.}
#'     \item{isDeQuervainTR_freq_1}{Percentage of rows where `isDeQuervainTR == 1`.}
#'     \item{domhandtreated_freq_1}{Percentage of rows where `domhandtreated == 1`.}
#'     \item{Means of Numeric Variables}{Mean values for all numeric variables in the dataset.}
#'   }
#' @note Excludes certain categorical columns from the output to focus on relevant numeric variables.
generate_summary_stats <- function(data) {
  data %>%
    summarise(
      fid_count = n_distinct(fid),  # Count distinct `fid` values
      employment_freq_1 = mean(employment == 1, na.rm = TRUE) * 100,  # % Employment == 1
      isDeQuervainTR_freq_1 = mean(isDeQuervainTR == 1, na.rm = TRUE) * 100,  # % isDeQuervainTR == 1
      domhandtreated_freq_1 = mean(domhandtreated == 1, na.rm = TRUE) * 100,  # % domhandtreated == 1
      across(where(is.numeric), ~ mean(.x, na.rm = TRUE))  # Mean for all numeric columns
    ) %>%
    select(-any_of(c("fid", "employment", "expectationgoal", 
                     "goal_thumb_mobility", "goal_daily_activities", 
                     "goal_leisure_activities", "goal_pain_relief", 
                     "goal_improve_strength", "goal_return_to_work")))
}


#' Split Dataset by Availability of Target
#'
#' Separates a dataset into observations with and without the target
#' variable present.  Useful for preparing training and hold‑out sets.
#'
#' @param data        Input data frame.
#' @param target_col  Column containing the target variable.
#' @param exclude_col Column to drop from both returned data frames.
#' @return List with elements \code{included} and \code{excluded}.
filter_target_dataset <- function(data, target_col, exclude_col = NULL) {
  if (!rlang::quo_is_null(rlang::enquo(exclude_col))) {
    included <- data %>%
      select(-{{ exclude_col }}) %>%
      filter(!is.na({{ target_col }}))
    
    excluded <- data %>%
      select(-{{ exclude_col }}) %>%
      filter(is.na({{ target_col }}))
  } else {
    included <- data %>% filter(!is.na({{ target_col }}))
    excluded <- data %>% filter(is.na({{ target_col }}))
  }
  
  list(included = included, excluded = excluded)
}
