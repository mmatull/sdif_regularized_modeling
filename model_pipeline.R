# =====================================================================================================================================
# File: model_pipeline.R
# =====================================================================================================================================
# Description:
# This script contains functions to:
# - run_model_pipeline
# - run_model_pipeline_relax
#
# Run Model Pipeline for Stratified Train-Test Split and Deviance Calculation
# Run Relaxed Model for Risk Factor Calculation
#
# Author: mmatull
# Date: 2025-03-20
# =====================================================================================================================================



# =====================================================================================================================================
# Run Model Pipeline for Stratified Train-Test Split and Deviance Calculation
# =====================================================================================================================================
#'
#' This function runs a full model pipeline including stratified train-test split, contrast and design matrix creation, 
#' fitting a model using glmnet, and calculating deviances for both train and test splits. The model can use Poisson or Tweedie distribution.
#'
#' @param features A vector of feature names to be used as explanatory variables in the model.
#' @param data A data frame containing the full dataset.
#' @param distribution A string specifying the distribution for the model fitting (either "poisson" or "tweedie").
#' @param target_col A string specifying the column name of the target variable.
#' @param weight_col A string specifying the column name of the weight variable.
#' @param offset_col A string specifying the column name for the offset, which must be on the response scale (optional, default is NULL).
#' @param tweedie_power A numeric value specifying the power parameter for the Tweedie distribution (default: 1.5).
#' @param contrasts_exclude A vector of variables to be excluded from the contrast matrix (default: empty).
#' @param sparse_matrix A boolean indicating whether to return a sparse matrix (default: FALSE).
#' @param test_size A numeric value specifying the proportion of data to be used for the test split (default: 0.2).
#' @param seed An integer seed for reproducibility (default: 42).
#' @param cv_folds The number of cross-validation folds (default: NULL, no cross-validation).
#'
#' @return A list containing model components such as contrasts, fitted model, risk factors, predictions, and deviances.
#'
#' @export
run_model_pipeline <- function(features, data, distribution,
                               target_col, weight_col, offset_col = NULL, tweedie_power = 1.5,
                               contrasts_exclude = character(0), sparse_matrix = FALSE,
                               test_size = 0.2, seed = 42, cv_folds = NULL) {
  
  # Check if 'features', 'data', 'distribution', 'target_col', and 'weight_col' are provided correctly
  if (missing(features) || !is.vector(features)) {
    stop("'features' must be a vector of feature names.")
  }
  if (missing(data) || !is.data.frame(data)) {
    stop("'data' must be a data frame containing the full dataset.")
  }
  if (missing(distribution) || !distribution %in% c("poisson", "tweedie")) {
    stop("'distribution' must be either 'poisson' or 'tweedie'.")
  }
  if (missing(target_col) || !target_col %in% colnames(data)) {
    stop("'target_col' must be a valid column name in the 'data' dataframe.")
  }
  if (missing(weight_col) || !weight_col %in% colnames(data)) {
    stop("'weight_col' must be a valid column name in the 'data' dataframe.")
  }
  
  # Ensure the columns exist in the data frame
  if (!target_col %in% colnames(data)) {
    stop(paste("Error: target_col", target_col, "does not exist in the data frame."))
  }
  if (!weight_col %in% colnames(data)) {
    stop(paste("Error: weight_col", weight_col, "does not exist in the data frame."))
  }
  
  
  # Perform stratified train-test split
  split <- split_data(data, target_col, weight_col, test_size, seed, cv_folds)
  
  # Create contrast and design matrices
  contrN <- create_contrasts_matrix(features, data, contrasts_exclude, sparse_matrix)
  dataD <- create_design_matrix(data, contrN, sparse_matrix)
  
  # Fit the model using glmnet
  fitted_model_train <- glmnet::glmnet(
    x = dataD[split$train_index, , drop = FALSE],
    y = data[[target_col]][split$train_index] / data[[weight_col]][split$train_index],
    weights = if (distribution %in% c("tweedie") & !is.null(offset_col))
                data[[weight_col]][split$train_index]*data[[offset_col]][split$train_index]^(p-1)
              else 
                data[[weight_col]][split$train_index],
    offset = if (!is.null(offset_col)) get_family(distribution)$linkfun(data[[offset_col]][split$train_index]) else NULL, # offset on link scale
    family = if (distribution %in% c("poisson", "binomial")) distribution # use built-in family if possible
              else get_family(distribution),
    standardize = FALSE,
    intercept = TRUE,
    alpha = 1,
    nlambda = 100,
    lambda.min.ratio = 1e-06,  # Smaller value to obtain null model
    trace.it = 1
  )
  
  # Calculate dummy encoded risk factors
  risk_results <- calculate_dummy_encoded_risk_factors(
    fitted_model_train, 
    contrN, 
    data[split$train_index, ], 
    weight_col, 
    colnames(dataD)
  )
  
  # Predict on full dataset (train + test)
  preds_full <- predict(fitted_model_train, 
                        newx = dataD, 
                        newoffset = if (!is.null(offset_col)) get_family(distribution)$linkfun(data[[offset_col]]) else NULL, 
                        type = "response")
  
  # Compute deviance for training split
  deviance_train <- calculate_deviance_per_s(
    y = data[[target_col]][split$train_index] / data[[weight_col]][split$train_index],
    mu_matrix = preds_full[split$train_index, , drop = FALSE],
    weights = if (distribution %in% c("tweedie") & !is.null(offset_col))
                data[[weight_col]][split$train_index]*data[[offset_col]][split$train_index]^(p-1)
              else 
                data[[weight_col]][split$train_index],
    family = if(distribution == "tweedie") "tweedie" else "poisson",
    tweedie_power = tweedie_power
  )
  
  # Compute deviance for test split
  deviance_test <- calculate_deviance_per_s(
    y = data[[target_col]][split$test_index] / data[[weight_col]][split$test_index],
    mu_matrix = preds_full[split$test_index, , drop = FALSE],
    weights = if (distribution %in% c("tweedie") & !is.null(offset_col))
                data[[weight_col]][split$test_index]*data[[offset_col]][split$test_index]^(p-1)
              else 
                data[[weight_col]][split$test_index],
    family = if(distribution == "tweedie") "tweedie" else "poisson",
    tweedie_power = tweedie_power
  )
  
  # Return the results as a list
  list(
    contrasts = contrN,
    fitted_model_train = fitted_model_train,
    base_level = risk_results$base_level,
    risk_factors = risk_results$risk_factors,
    preds_full = preds_full,
    preds_train = preds_full[split$train_index, , drop = FALSE],
    preds_test = preds_full[split$test_index, , drop = FALSE],
    deviance_train = deviance_train,
    deviance_test = deviance_test,
    split = split
  )
}



# =====================================================================================================================================
# Run Relaxed Model for Risk Factor Calculation
# =====================================================================================================================================
#' 
#' This function trains a relaxed model using glmnet, calculates risk factors, and computes deviance for both train and test splits.
#' It supports different distributions and calculates deviance based on the fitted model's predictions.
#'
#' @param pipeline_output A list containing the results from the model pipeline, including fitted model and other related components.
#' @param features A vector of feature names to be used in the model.
#' @param data A data frame containing the full dataset.
#' @param distribution A string specifying the distribution for the model fitting .we have to set this globally because the param distribution is not available in the environment context when glmnet performs the relaxed fit
#' @param target_col A string specifying the column name of the target variable (default: ".target").
#' @param weight_col A string specifying the column name of the weight variable (default: ".weights").
#' @param offset_col A string specifying the column name for the offset, which must be on the response scale (optional, default is NULL).
#' @param tweedie_power A numeric value specifying the power parameter for the Tweedie distribution (default: 1.5).
#' @param lambda_index A numeric value specifying the index for the lambda parameter in the fitted model (default: NULL).
#' @param gamma A numeric value for the gamma parameter used in the relaxation process (default: 1).
#' @param split_index An optional list containing indices for train-test split (default: NULL).
#' @param contrasts_exclude A vector of variables to exclude from the contrast matrix (default: empty).
#' @param sparse_matrix A boolean indicating whether to return a sparse matrix (default: FALSE).
#'
#' @return A list containing model components such as contrasts, fitted models, risk factors, predictions, and deviances.
#'
#' @examples
#' run_model_pipeline_relax(pipeline_output, features, data, "poisson")
#'
#' @export

run_model_pipeline_relax <- function(pipeline_output, features, data, distribution,
                                     target_col, weight_col, offset_col = NULL, 
                                     tweedie_power = 1.5, lambda_index = NULL, gamma = 1, split_index = NULL,
                                     contrasts_exclude = character(0), sparse_matrix = FALSE) {
  
  if (lambda_index < 0) {  
    stop("Error: The lambda index must be greater than 0.")
  }
  
  # Check if 'features', 'data', 'distribution', 'target_col', and 'weight_col' are provided correctly
  if (missing(features) || !is.vector(features)) {
    stop("'features' must be a vector of feature names.")
  }
  if (missing(data) || !is.data.frame(data)) {
    stop("'data' must be a data frame containing the full dataset.")
  }
  if (missing(distribution) || !distribution %in% c("poisson","binomial","negative_binomial","gamma","tweedie")) {
    stop("'distribution' must be one of 'poisson', 'binomial', 'negative_binomial', 'gamma', or 'tweedie'.")
  }
  if (missing(target_col) || !target_col %in% colnames(data)) {
    stop("'target_col' must be a valid column name in the 'data' dataframe.")
  }
  if (missing(weight_col) || !weight_col %in% colnames(data)) {
    stop("'weight_col' must be a valid column name in the 'data' dataframe.")
  }
  
  # Ensure the columns exist in the data frame
  if (!target_col %in% colnames(data)) {
    stop(paste("Error: target_col", target_col, "does not exist in the data frame."))
  }
  if (!weight_col %in% colnames(data)) {
    stop(paste("Error: weight_col", weight_col, "does not exist in the data frame."))
  }
  
  # Create contrast and design matrix
  contrN <- create_contrasts_matrix(features, data, contrasts_exclude, sparse_matrix)
  dataD <- create_design_matrix(data, contrN, sparse_matrix)
  
  #If split_index is NULL, use all rows, else use train
  indices_to_use <- if (!is.null(split_index)) split_index$train_index else seq_len(nrow(data))
  
  # Train model
  relaxed_model <- glmnet::glmnet(
    x = dataD[indices_to_use, , drop = FALSE],
    y = data[[target_col]][indices_to_use] / data[[weight_col]][indices_to_use],
    weights = if (distribution %in% c("tweedie") & !is.null(offset_col))
                data[[weight_col]][indices_to_use]*data[[offset_col]][indices_to_use]^(p-1)
              else 
                data[[weight_col]][indices_to_use],
    offset = if (!is.null(offset_col)) get_family(distribution)$linkfun(data[[offset_col]][indices_to_use]) else NULL,
    family = if (distribution %in% c("poisson", "binomial")) distribution # use built-in family if possible
    else get_family(distribution),
    standardize = FALSE,
    intercept = TRUE,
    alpha = 1,
    lambda = pipeline_output$fitted_model_train$lambda[lambda_index+1], # the lambda-index starts at 1, the "s" starts at 0
    nlambda = 1,
    lambda.min.ratio = 1e-06,
    trace.it = 1,
    #Greatly reduce the convergence tolerance (thresh) and increase iterations (maxit) to force the exact solution regardless of the start value.
    #Even with exact lambda, rounding errors or differences in the optimization path initialization can cause minimal deviations. 
    thresh = 1e-10, maxit = 1e5,
    gamma = gamma,
    relax = T
  )
  
  # Extract risk factors
  risk_results <- calculate_dummy_encoded_risk_factors(
    relaxed_model$relaxed, 
    contrN, 
    data, 
    weight_col, 
    colnames(dataD)
  )
  
  # Predict on full data
  preds_full <- predict(relaxed_model$relaxed, 
                        newx = dataD, 
                        newoffset = if (!is.null(offset_col)) get_family(distribution)$linkfun(data[[offset_col]]) else NULL, 
                        type = "response")
  
  # Compute deviance for train split
  deviance_train <- calculate_deviance_per_s(
    y = data[[target_col]][indices_to_use] / data[[weight_col]][indices_to_use],
    mu_matrix = preds_full[indices_to_use, , drop = FALSE],
    weights = if (distribution %in% c("tweedie") & !is.null(offset_col))
                data[[weight_col]][indices_to_use]*data[[offset_col]][indices_to_use]^(p-1)
              else 
                data[[weight_col]][indices_to_use],
    family = if(distribution == "tweedie") "tweedie" else "poisson",
    tweedie_power = tweedie_power
  )
  
  # if split_index is NULL or split_index$test_index is an empty list, ie test_size == 0
  if (length(split_index$test_index) > 0) {
    
    preds_test <- preds_full[split_index$test_index, , drop = FALSE]
    
    deviance_test <- calculate_deviance_per_s(
      y = data[[target_col]][split_index$test_index] / data[[weight_col]][split_index$test_index],
      mu_matrix = preds_full[split_index$test_index, , drop = FALSE],
      weights = if (distribution %in% c("tweedie") & !is.null(offset_col))
                  data[[weight_col]][split_index$test_index]*data[[offset_col]][split_index$test_index]^(p-1)
                else 
                  data[[weight_col]][split_index$test_index],
      family = if(distribution == "tweedie") "tweedie" else "poisson",
      tweedie_power = tweedie_power
    )
  } else {
    preds_test <- NULL
    deviance_test <- NULL
  }
  
  list(
    contrasts = contrN,
    relaxed_model = relaxed_model$relaxed,
    initial_model = relaxed_model,
    base_level = risk_results$base_level,
    risk_factors = risk_results$risk_factors,
    preds_full = preds_full,
    preds_train = preds_full[indices_to_use, , drop = FALSE],
    preds_test = preds_test,
    deviance_train = deviance_train,
    deviance_test = deviance_test,
    split = split
  )
}


