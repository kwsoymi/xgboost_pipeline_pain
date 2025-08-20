#' Create Repeated Cross-Validation Folds for XGBoost
#'
#' This function creates subject-wise repeated cross-validation folds, ensuring 
#' that each unique subject (identified by an ID column) appears in only one fold 
#' per repetition, preventing data leakage. The resulting folds are formatted for 
#' compatibility with `xgb.cv`.
#'
#' @param dat_train A data frame containing the training data.
#' @param IDvarn A character string specifying the column name of the unique 
#'   identifier for subjects (e.g., patient ID).
#' @param nreps An integer specifying the number of repetitions. Must be positive. Defaults to 5.
#' @param nfolds An integer specifying the number of folds. Must be greater than 1. Defaults to 5.
#' @param seed An integer used to set the random seed for reproducibility. Defaults to 49789.
#' 
#' @return A list of cross-validation folds, where each element is a vector of row indices 
#'   corresponding to that fold, formatted for use in `xgb.cv`. The list includes folds 
#'   across all repetitions.
#'
my_rep_folds <- function(dat_train, IDvarn, nreps = 10, nfolds = 5, seed = 49789) {
  # Ensure groupdata2 package is installed
  if (!requireNamespace("groupdata2", quietly = TRUE)) {
    stop("The 'groupdata2' package is required but not installed. Please install it with install.packages('groupdata2').")
  }
  
  # Validate inputs
  if (!IDvarn %in% colnames(dat_train)) {
    stop("The column specified by 'IDvarn' is not found in 'dat_train'.")
  }
  
  if (!is.numeric(nreps) || nreps <= 0) {
    stop("'nreps' must be a positive integer.")
  }
  
  if (!is.numeric(nfolds) || nfolds <= 1) {
    stop("'nfolds' must be an integer greater than 1.")
  }
  
  # Create repeated folds
  myRepFolds <- purrr::map(
    1:nreps,
    function(rep_num) {
      # Set random seed for reproducibility
      set.seed(seed + rep_num)
      
      # Create folds for this repetition
      train_folds <- groupdata2::fold(dat_train, k = nfolds, id_col = IDvarn)
      
      # Convert folds to row indices for each fold
      lapply(1:nfolds, function(i) which(train_folds$.folds == i))
    }
  )
  
  # Combine all repetitions into a single list
  folds <- unlist(myRepFolds, recursive = FALSE)
  
  return(folds)
}