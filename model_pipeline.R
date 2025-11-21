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
# Date: 2025-10-06
# =====================================================================================================================================



# =====================================================================================================================================
# Run Model Pipeline for Stratified Train-Test Split and Deviance Calculation
# =====================================================================================================================================
#'
#' This function runs a full model pipeline including stratified train-test split, contrast and design matrix creation, 
#' fitting a model using glmnet, and calculating deviances for both train and test splits. The model can use Poisson or Tweedie distribution.
#'
#' @param data A data frame containing the full dataset.
#' @param distribution A string specifying the distribution for the model fitting (either "poisson" or "tweedie").
#' @param features A vector of feature names to be used as explanatory variables in the model.
#' @param interactions A vector of interaction terms (e.g. c(“Var1:Var2”), optional).
#' @param target_col A string specifying the column name of the target variable.
#' @param weight_col A string specifying the column name of the weight variable.
#' @param offset_col A string specifying the column name for the offset, which must be on the response scale (optional, default is NULL).
#' @param tweedie_power A numeric value specifying the power parameter for the Tweedie distribution (default: 1.5).
#' @param contrasts_exclude A vector of variables to be excluded from the contrast matrix (default: empty).
#' @param sparse_matrix A boolean indicating whether to return a sparse matrix (default: FALSE).
#' @param contrast_type Numeric; 1 centers the contrast matrix by subtracting the middle row (middle level as intercept),
#'                         if 2, returns the original MASS::contr.sdif matrix without centering (grand mean as intercept).
#' @param test_size A numeric value specifying the proportion of data to be used for the test split (default: 0.2).
#' @param seed An integer seed for reproducibility (default: 42).
#' @param cv_folds The number of cross-validation folds (default: NULL, no cross-validation).
#'
#' @return A list containing model components such as contrasts, fitted model, risk factors, predictions, and deviances.
#'
#' @export
run_model_pipeline <- function(data, distribution, features, interactions = NULL,
                               target_col, weight_col, offset_col = NULL, tweedie_power = 1.5,
                               contrasts_exclude = character(0), sparse_matrix = FALSE, contrast_type = 1,
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
  print(paste("Perform stratified train-test split:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  split_index <- split_data(data, target_col, weight_col, test_size, seed, cv_folds)
  
  # Create contrast and design matrices
  print(paste("Create contrast and design matrices:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  contrN <- create_contrasts_matrix(features, data, contrasts_exclude, sparse_matrix, contrast_type = contrast_type)
  dataD <- create_design_matrix(data, contrN, interactions = interactions, sparse_matrix)
  
  # Fit the model using glmnet
  print(paste("Fit the model:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  fitted_model_train <- glmnet::glmnet(
    x = dataD[split_index$train_index, , drop = FALSE],
    y = data[[target_col]][split_index$train_index] / data[[weight_col]][split_index$train_index],
    weights = if (distribution %in% c("tweedie") & !is.null(offset_col))
      data[[weight_col]][split_index$train_index]*data[[offset_col]][split_index$train_index]^(tweedie_power-1)
    else 
      data[[weight_col]][split_index$train_index],
    offset = if (!is.null(offset_col)) get_family(distribution)$linkfun(data[[offset_col]][split_index$train_index]) else NULL, # offset on link scale
    family = if (distribution %in% c("poisson", "binomial")) distribution # use built-in family if possible
    else get_family(distribution, variance_power = tweedie_power),
    standardize = FALSE,
    intercept = TRUE,
    alpha = 1,
    nlambda = 100,
    lambda.min.ratio = 1e-06,  # Smaller value to obtain null model
    trace.it = 1
  )
  
  # Calculate dummy encoded risk factors
  print(paste("Calculate risk factors:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  risk_results <- calculate_dummy_encoded_risk_factors(
    glmnet_model = fitted_model_train,
    data = data[split_index$train_index, ], 
    weight_col = weight_col,
    data_structure = data[0, features, drop = FALSE],
    contrasts = contrN,
    features = features,
    interactions = interactions
  )
  
  # Predict on full dataset (train + test)
  print(paste("Predict on full dataset:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  preds_full <- predict(fitted_model_train, 
                        newx = dataD, 
                        newoffset = if (!is.null(offset_col)) get_family(distribution)$linkfun(data[[offset_col]]) else NULL, 
                        type = "response")
  
  # Compute deviance for training split
  print(paste("Compute deviance:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  deviance_train <- calculate_deviance_per_s(
    y = data[[target_col]][split_index$train_index] / data[[weight_col]][split_index$train_index],
    mu_matrix = preds_full[split_index$train_index, , drop = FALSE],
    weights = if (distribution %in% c("tweedie") & !is.null(offset_col))
      data[[weight_col]][split_index$train_index]*data[[offset_col]][split_index$train_index]^(tweedie_power-1)
    else 
      data[[weight_col]][split_index$train_index],
    family = if(distribution == "tweedie") "tweedie" else "poisson",
    tweedie_power = tweedie_power
  )
  
  # Compute deviance for test split
  deviance_test <- calculate_deviance_per_s(
    y = data[[target_col]][split_index$test_index] / data[[weight_col]][split_index$test_index],
    mu_matrix = preds_full[split_index$test_index, , drop = FALSE],
    weights = if (distribution %in% c("tweedie") & !is.null(offset_col))
      data[[weight_col]][split_index$test_index]*data[[offset_col]][split_index$test_index]^(tweedie_power-1)
    else 
      data[[weight_col]][split_index$test_index],
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
    preds_train = preds_full[split_index$train_index, , drop = FALSE],
    preds_test = preds_full[split_index$test_index, , drop = FALSE],
    deviance_train = deviance_train,
    deviance_test = deviance_test,
    split_index = split_index
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
#' @param data A data frame containing the full dataset.
#' @param features A vector of feature names to be used in the model.
#' @param interactions A vector of interaction terms (e.g. c(“Var1:Var2”), optional).
#' @param distribution A string specifying the distribution for the model fitting (either "poisson" or "tweedie").
#' @param target_col A string specifying the column name of the target variable (default: ".target").
#' @param weight_col A string specifying the column name of the weight variable (default: ".weights").
#' @param offset_col A string specifying the column name for the offset, which must be on the response scale (optional, default is NULL).
#' @param gamma A numeric value for the gamma parameter used in the relaxation process (default: 0 is the unpenalized fit).
#' @param contrasts_exclude A vector of variables to exclude from the contrast matrix (default: empty).
#' @param sparse_matrix A boolean indicating whether to return a sparse matrix (default: FALSE).
#' @param mode A string specifying the fitting mode: "normal" fits all lambda values, "partial_fit" fits only selected lambda values (default: "normal").
#' @param lambda_index A vector of lambda indices (0-based) to use when mode="partial_fit". Must be provided when using partial_fit mode.
#'
#' @return A list containing model components such as contrasts, fitted models, risk factors, predictions, and deviances.
#'
#' @examples
#' # Normal mode - fit all lambda values
#' run_model_pipeline_relax(pipeline_output, features, data, "poisson")
#' 
#' # partial_fit mode - fit only specific lambda values
#' run_model_pipeline_relax(pipeline_output, features, data, "poisson", mode = "partial_fit", lambda_index = c(0, 5, 10))
#'
#' @export

run_model_pipeline_relax <- function(pipeline_output, data, features, interactions = NULL, distribution,
                                     target_col, weight_col, offset_col = NULL, 
                                     gamma = 0, 
                                     contrasts_exclude = character(0), sparse_matrix = FALSE,
                                     mode = c("normal","partial_fit"), lambda_index = NULL) {
  
  # =====================================================================
  # Input Validation
  # =====================================================================
  
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
  
  # Ensure the columns exist in the data frame (double check for safety)
  if (!target_col %in% colnames(data)) {
    stop(paste("Error: target_col", target_col, "does not exist in the data frame."))
  }
  if (!weight_col %in% colnames(data)) {
    stop(paste("Error: weight_col", weight_col, "does not exist in the data frame."))
  }
  
  # Check if partial_fit mode is used correctly
  mode <- match.arg(mode)
  if (mode == "partial_fit") {
    if (is.null(lambda_index)) {
      stop("lambda_index must not be NULL when mode is 'partial_fit'")
    }
    # Validate lambda_index range (0-based indexing)
    n_lambda <- length(pipeline_output$fitted_model_train$lambda)
    if (any(lambda_index < 0 | lambda_index > n_lambda - 1)) {
      stop(sprintf("lambda_indices out of valid range (0 to %d)", n_lambda-1))
    }
  }
  
  # =====================================================================
  # Data Preparation
  # =====================================================================
  
  # Create design matrix from data using contrasts
  dataD <- create_design_matrix(data, pipeline_output$contrasts, interactions = interactions, sparse_matrix)
  
  # Determine which indices to use for model training
  # If split_index is NULL, use all rows, else use train indices only
  indices_to_use <- if (!is.null(pipeline_output$split_index)) pipeline_output$split_index$train_index else seq_len(nrow(data))
  
  # =====================================================================
  # Model Selection for Relaxed Fitting
  # =====================================================================
  
  # Determine which model to use for relax fitting based on mode
  model_for_relax <- if (mode == "partial_fit") {
    # Use subset of the original model with only selected lambda values
    subset_glmnet_by_lambda(pipeline_output$fitted_model_train, lambda_index)
  } else {
    # Use the full original model with all lambda values
    pipeline_output$fitted_model_train
  }
  
  # =====================================================================
  # Relaxed Model Fitting
  # =====================================================================
  
  # Train relaxed model using glmnet::relax.glmnet
  # This performs relaxed lasso by refitting unpenalized models on the selected variables
  relaxed_model <- glmnet::relax.glmnet(model_for_relax,
                                        x = dataD[indices_to_use, , drop = FALSE],
                                        y = data[[target_col]][indices_to_use] / data[[weight_col]][indices_to_use],
                                        weights = if (distribution %in% c("tweedie") & !is.null(offset_col))
                                          # For Tweedie with offset: adjust weights by offset^(power-1)
                                          data[[weight_col]][indices_to_use]*data[[offset_col]][indices_to_use]^(tweedie_power-1)
                                        else
                                          # Standard case: use weights as provided
                                          data[[weight_col]][indices_to_use],
                                        offset = if (!is.null(offset_col)) 
                                          # Transform offset to link scale if provided
                                          get_family(distribution)$linkfun(data[[offset_col]][indices_to_use]) 
                                        else 
                                          NULL,
                                        gamma = gamma,  # Relaxation parameter 
                                        trace.it = 1    # Show progress during fitting
  )
  
  # =====================================================================
  # Risk Factor Calculation
  # =====================================================================
  
  # Extract risk factors from the relaxed model coefficients
  # This calculates relative risk factors for each variable level
  risk_results <- calculate_dummy_encoded_risk_factors(
    glmnet_model = relaxed_model$relaxed,
    data = data[indices_to_use, , drop = FALSE],
    weight_col = weight_col,
    data_structure = data[0, features, drop = FALSE],
    contrasts = pipeline_output$contrasts,
    features = features,
    interactions = interactions
  )
  
  # =====================================================================
  # Prediction and Deviance Calculation
  # =====================================================================
  
  # Generate predictions on the full dataset using the relaxed model
  preds_full <- predict(relaxed_model$relaxed, 
                        newx = dataD, 
                        newoffset = if (!is.null(offset_col)) 
                          # Transform offset to link scale for prediction
                          get_family(distribution)$linkfun(data[[offset_col]]) 
                        else 
                          NULL, 
                        type = "response")  # Return predictions on response scale
  
  # Compute deviance for training split
  # Deviance measures goodness of fit - lower is better
  deviance_train <- calculate_deviance_per_s(
    y = data[[target_col]][indices_to_use] / data[[weight_col]][indices_to_use],
    mu_matrix = preds_full[indices_to_use, , drop = FALSE],
    weights = if (distribution %in% c("tweedie") & !is.null(offset_col))
      # For Tweedie with offset: adjust weights by offset^(power-1)
      data[[weight_col]][indices_to_use]*data[[offset_col]][indices_to_use]^(tweedie_power-1)
    else 
      # Standard case: use weights as provided
      data[[weight_col]][indices_to_use],
    family = if(distribution == "tweedie") "tweedie" else "poisson",
    tweedie_power = tweedie_power
  )
  
  # =====================================================================
  # Test Set Evaluation (if available)
  # =====================================================================
  
  # Check if test split exists and is non-empty
  # If split_index is NULL or split_index$test_index is an empty list, i.e., test_size == 0
  if (length(pipeline_output$split_index$test_index) > 0) {
    
    test_index <- pipeline_output$split_index$test_index
    
    # Extract predictions for test set
    preds_test <- preds_full[test_index, , drop = FALSE]
    
    # Compute deviance for test split
    # This provides an unbiased estimate of model performance
    deviance_test <- calculate_deviance_per_s(
      y = data[[target_col]][test_index] / data[[weight_col]][test_index],
      mu_matrix = preds_full[test_index, , drop = FALSE],
      weights = if (distribution %in% c("tweedie") & !is.null(offset_col))
        # For Tweedie with offset: adjust weights by offset^(power-1)
        data[[weight_col]][test_index]*data[[offset_col]][test_index]^(tweedie_power-1)
      else 
        # Standard case: use weights as provided
        data[[weight_col]][test_index],
      family = if(distribution == "tweedie") "tweedie" else "poisson",
      tweedie_power = tweedie_power
    )
  } else {
    # No test set available
    preds_test <- NULL
    deviance_test <- NULL
  }
  
  # =====================================================================
  # Return Results
  # =====================================================================
  
  # Return comprehensive results list
  list(
    contrasts = pipeline_output$contrasts,                                # Contrast matrix for categorical variables
    relaxed_model = relaxed_model$relaxed,                                # Fitted relaxed glmnet model
    initial_model = relaxed_model,                                        # Full relaxed.glmnet object (includes original model)
    base_level = risk_results$base_level,                                 # Base levels for risk factor calculation
    risk_factors = risk_results$risk_factors,                             # Calculated risk factors for each variable
    preds_full = preds_full,                                              # Predictions on full dataset
    preds_train = preds_full[indices_to_use, , drop = FALSE],             # Predictions on training set
    preds_test = preds_test,                                              # Predictions on test set (NULL if no test set)
    deviance_train = deviance_train,                                      # Training deviance for model selection
    deviance_test = deviance_test,                                        # Test deviance for model evaluation
    split_index = pipeline_output$split_index                             # Train/test split information
  )
}

