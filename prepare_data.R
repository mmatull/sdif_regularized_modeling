# =====================================================================================================================================
# File: prepare_data.R
# =====================================================================================================================================
# Description:
# This script contains functions to:
# - create_contrasts_matrix 
# - create_design_matrix
# - split_data
#
# Create contrast matrices for categorical features
# Create a design matrix from a dataset and contrast specifications
# Stratified Train-Test Split with Optional Cross-Validation and Weighting
#
# Author: mmatull
# Date: 2025-03-20
# =====================================================================================================================================



# =====================================================================================================================================
# Create contrast matrices for categorical features
# =====================================================================================================================================
#'
#' This function generates contrast matrices for specified categorical features,
#' optionally excluding specified contrasts and controlling centering behavior.
#'
#' @param features A character vector of categorical variable names.
#' @param data A data frame containing the categorical variables.
#' @param contrasts_exclude A named list where each entry specifies which contrasts to exclude for a given feature.
#' @param sparse_matrix Logical; if TRUE, returns sparse matrices, otherwise dense matrices.
#' @param center_contrasts Numeric; 1 centers the contrast matrix by subtracting the middle row (middle level as intercept),
#'                         if 2, returns the original MASS::contr.sdif matrix without centering (grand mean as intercept).
#'
#' @return A named list of contrast matrices, where each element corresponds to a categorical variable.
#'
#' @examples
#' data <- data.frame(Group = factor(c("A", "B", "C")), Category = factor(c("X", "Y", "X")))
#' # With centering (default)
#' create_contrasts_matrix(c("Group", "Category"), data)
#' # Without centering
#' create_contrasts_matrix(c("Group", "Category"), data, center_contrasts = FALSE)
#'
#' @export
create_contrasts_matrix <- function(features, data, contrasts_exclude = list(), sparse_matrix = FALSE, contrast_type = 1) {
  purrr::map(set_names(features), function(var) {
    
    # Generate contrast matrix for variable 'var'
    levels_count <- nlevels(data[[var]])
    contrasts <- MASS::contr.sdif(levels_count, contrasts = TRUE, sparse = sparse_matrix)
    
    if (contrast_type == 1) {
      # Center contrast matrix by subtracting the middle level row (reference for intercept)
      # Note: floor(levels_count / 2) is used; for odd levels, consider ceiling(levels_count / 2)
      center_row <- floor(levels_count / 2)
      final_contrasts <- Matrix::Matrix(contrasts - matrix(contrasts[center_row, , drop = FALSE],
                                                           nrow = nrow(contrasts),
                                                           ncol = ncol(contrasts),
                                                           byrow = TRUE),
                                        sparse = sparse_matrix)
    } else if (contrast_type == 2) {
      # Use the original contrast matrix without centering
      final_contrasts <- Matrix::Matrix(contrasts, sparse = sparse_matrix)
    } else {
      stop("Invalid contrast_type. Use 1 for centered or 2 for original contrasts.")
    }
    
    # Exclude specified contrasts for variable 'var' if provided
    if (var %in% names(contrasts_exclude)) {
      final_contrasts <- final_contrasts[, !colnames(final_contrasts) %in% contrasts_exclude[[var]], drop = FALSE]
    }
    
    final_contrasts
  })
}



# =====================================================================================================================================
# Create a design matrix from a dataset and contrast specifications
# =====================================================================================================================================
#'
#' This function generates a design matrix for regression modeling,
#' based on specified contrasts for categorical variables.
#'
#' @param data A data frame containing the variables used in the model.
#' @param contrasts A named list specifying contrast matrices for categorical variables.
#' @param sparse_matrix Logical; if TRUE, returns a sparse matrix, otherwise a dense matrix.
#' 
#' @return A design matrix with the specified contrasts applied, without the intercept or original factor columns.
#' If only one column remains, a dummy column is added.
#' 
#' @examples
#' data <- data.frame(Group = factor(c("A", "B", "C")), Value = c(1, 2, 3))
#' contrasts <- list(Group = MASS::contr.sdif(3))
#' create_design_matrix(data, contrasts)
#' 
#' @export
create_design_matrix <- function(data, contrasts, sparse_matrix = FALSE) {
  
  # Construct the formula string for the design matrix, including all contrast variables
  formula_str <- paste0("~ 1 + ", paste(names(contrasts), collapse = " + "))
  
  # Generate the design matrix, either as a sparse or dense matrix based on the parameter
  design_matrix <- if (sparse_matrix) {
    Matrix::sparse.model.matrix(as.formula(formula_str), data = data, contrasts.arg = contrasts)
  } else {
    model.matrix(as.formula(formula_str), data = data, contrasts.arg = lapply(contrasts, as.matrix))
  }
  
  # Remove intercept and original factor columns
  keep_cols <- setdiff(colnames(design_matrix), c("(Intercept)", names(contrasts)))
  design_matrix <- design_matrix[, keep_cols, drop = FALSE]
  
  return(design_matrix)
}


# =====================================================================================================================================
# Stratified Train-Test Split with Optional Cross-Validation and Weighting
# =====================================================================================================================================
#'
#' This function performs a stratified train-test split on the dataset, where the split is based on weighted target values. 
#' Optionally, it also supports cross-validation (CV) splitting for model training.
#'
#' @param data A data frame containing the dataset to split.
#' @param target_col A string specifying the column name of the target variable.
#' @param weight_col A string specifying the column name of the weights for each observation.
#' @param test_size A numeric value between 0 and 1 specifying the proportion of data to be used for the test set (default: 0.2).
#' @param seed An integer to set the random seed for reproducibility (default: 42).
#' @param cv_folds An integer specifying the number of folds for cross-validation. If NULL, cross-validation is not performed (default: NULL).
#'
#' @return A list containing:
#'   - `train_index`: Indices for the training data.
#'   - `test_index`: Indices for the test data.
#'   - `cv_folds`: A list of cross-validation folds, if `cv_folds` is provided; otherwise, NULL.
#'
#' @examples
#' split_result <- split_data(my_data, target_col = "Outcome", weight_col = "Weight", test_size = 0.3, cv_folds = 5)
#'
#' @export
split_data <- function(data, target_col, weight_col, test_size = 0, seed = 42, cv_folds = NULL) {
  
  # Check if target_col and weight_col are provided
  if (missing(target_col) || missing(weight_col)) {
    stop("Error: target_col and weight_col must be specified.")
  }
  
  # Ensure the columns exist in the data frame
  if (!target_col %in% colnames(data)) {
    stop(paste("Error: target_col", target_col, "does not exist in the data frame."))
  }
  if (!weight_col %in% colnames(data)) {
    stop(paste("Error: weight_col", weight_col, "does not exist in the data frame."))
  }
  
  # Set the random seed for reproducibility
  set.seed(seed)
  
  # Create a weighted target variable for stratification
  weighted_target <- data[[target_col]] / data[[weight_col]]
  
  # Perform stratified train-test split based on the weighted target variable
  train_index <- caret::createDataPartition(weighted_target, p = 1 - test_size, list = FALSE)[, 1]
  
  # Create cross-validation folds if specified
  if (!is.null(cv_folds)) {
    folds <- caret::createFolds(weighted_target[train_index], k = cv_folds, list = TRUE)
  } else {
    folds <- NULL
  }
  
  # Return the indices for train-test split and CV folds (if applicable)
  list(train_index = train_index, test_index = setdiff(seq_len(nrow(data)), train_index), cv_folds = folds)
}
