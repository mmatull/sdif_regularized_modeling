# =====================================================================================================================================
# File: util.R
# =====================================================================================================================================
# Description:
# This script contains functions to:
# - predict_dummy_encoded_risk_factor
# - calculate_dummy_encoded_risk_factors
# - calculate_deviance_per_s
# - get_family
# - export_risk_factors_to_excel
#
# Convert Sum-to-Differences (SDIF) Encoding to Dummy Encoding 
# Calculates risk factors for categorical variables that have been converted to a dummy-encoded format after applying Sum-to-Differences 
#   (SDIF) encoding.
# Calculate Deviance for Poisson and Tweedie Distributions
# Get a GLM Family Object Based on the Specified Distribution
# Export risk model coefficients to Excel
# 
#
# Author: mmatull
# Date: 2025-04-21
# =====================================================================================================================================


# =====================================================================================================================================
# Convert Sum-to-Differences (SDIF) Encoding to Dummy Encoding 
# =====================================================================================================================================
#'
#' This function transforms sum-to-differences (SDIF) encoded categorical variables into 
#' dummy encoding by applying a trained glmnet model to the respective contrast matrix. 
#' It ensures compatibility with a model trained on dummy-encoded data.
#'
#' @param glmnet_model A trained glmnet model used for prediction.
#' @param contrasts A named list of contrast matrices for categorical variables (SDIF encoded).
#' @param target A character string specifying the target categorical variable for conversion.
#' @param train_cols A character vector specifying the column order of the training design matrix.
#'
#' @return A matrix of dummy-encoded predictions.
#'
#' @examples
#' # Assuming `glmnet_model` is trained on dummy-encoded data and `contrasts` contains SDIF encodings
#' predict_dummy_encoded_risk_factor(glmnet_model, contrasts, "Group", train_cols)
#'
#' @export
predict_dummy_encoded_risk_factor <- function(glmnet_model, contrasts, target, train_cols) {
  all_vars <- names(contrasts)
  
  # Extract target matrix and set column names
  target_matrix <- Matrix(as.matrix(contrasts[[target]]), sparse = TRUE)
  colnames(target_matrix) <- paste0(target, colnames(target_matrix))
  
  # Create zero matrices for remaining variables
  zero_matrices <- map(setdiff(all_vars, target), ~ {
    zero_mat <- Matrix(0, nrow = nrow(target_matrix), ncol = ncol(contrasts[[.x]]), sparse = TRUE)
    colnames(zero_mat) <- paste0(.x, colnames(contrasts[[.x]]))
    zero_mat
  })
  
  # Combine matrices efficiently
  final_matrix <- do.call(cbind, c(list(target_matrix), zero_matrices))
  
  # Reorder columns to match training design matrix
  final_matrix <- final_matrix[, train_cols, drop = FALSE]
  
  # Predict dummy encoding, no offset!
  predict(glmnet_model, newx = final_matrix, newoffset = rep(0,nrow(final_matrix)), type = "link")
}



# =====================================================================================================================================
# Calculates risk factors for categorical variables that have been converted to a dummy-encoded format 
# after applying Sum-to-Differences (SDIF) encoding.
# =====================================================================================================================================
#'
#' This function calculates the risk factors for each categorical variable, 
#' using a fitted model and a list of contrast matrices. 
#' It computes the standard, link, and dummy-encoded risk factors, adjusting 
#' for the most frequent level of each categorical variable.
#'
#' @param glmnet_model A fitted model (e.g., glmnet) used to predict the risk factors.
#' @param contrasts_list A named list of contrast matrices for each categorical variable (SDIF).
#' @param data A data frame containing the full dataset with original categorical variables.
#' @param weight_col A string indicating the column name for the weights in the data.
#' @param design_matrix_cols A vector of column names corresponding to the design matrix used for training.
#'
#' @return A list containing the computed risk factors:
#'   - `risk_factors`: A named list with each categorical variable's risk factors (standard, link, and dummy encoded).
#'   - `base_level`: The base level risk factor, adjusted for all variables.
#'
#' @examples
#' calculate_dummy_encoded_risk_factors(glmnet_model, contrasts_list, data, "weight", design_matrix_cols)
#'
#' @export
calculate_dummy_encoded_risk_factors <- function(glmnet_model, contrasts_list, data, weight_col, design_matrix_cols) {
  # Iterate over each contrast matrix in the list of contrasts
  risk_factors <- imap(contrasts_list, function(contr_mat, var_name) {
    
    # Convert Sum-to-Differences (SDIF) Encoding to Dummy Encoding 
    risk_factor_link <- predict_dummy_encoded_risk_factor(glmnet_model, contrasts_list, var_name, train_cols = design_matrix_cols)
    
    # Identify the level with the maximum weight (most frequent level) in the data
    max_level <- data %>%
      group_by(across(all_of(var_name))) %>%  # Group by the variable of interest
      summarise(total_weight = sum(.data[[weight_col]]), .groups = "drop") %>%  # Calculate the total weight for each level
      slice_max(total_weight, n = 1) %>%  # Select the level with the highest weight
      mutate(level_number = as.integer(.data[[var_name]]))  # Convert the level to a numeric identifier
    
    # Get the risk factor corresponding to the most frequent level, which will serve as the new baseline.
    risk_factor_standard <- risk_factor_link[max_level$level_number, , drop = FALSE]
    
    # Recenter the risk factor link by subtracting the baseline risk factor, effectively setting the most frequent category level as the reference.
    risk_factor_link_dummy_encoded <- sweep(risk_factor_link, 2, risk_factor_standard, "-")
    
    # Set row names of the dummy-encoded matrix to the factor levels
    rownames(risk_factor_link_dummy_encoded) <- levels(data[[var_name]])
    
    # Return the computed risk factors for the variable (standard, link, dummy encoded)
    list(
      risk_factor_standard = risk_factor_standard,
      risk_factor_link_sdif_encoded = risk_factor_link,
      risk_factor_link_dummy_encoded = risk_factor_link_dummy_encoded
    )
  })
  
  # Calculate the new base level for the model, adjusting for all variables
  base_level_original <- glmnet_model$a0  # Base level from the fitted model
  base_level <- base_level_original +
    reduce(map(risk_factors, "risk_factor_standard"), `+`) -  # Sum the org. risk factors of the new base level
    length(risk_factors) * base_level_original  # Adjust by subtracting the effect of the base level
  
  # delete risk_factor_standard, since we dont need it
  risk_factors <- lapply(risk_factors, function(x) {
    x$risk_factor_standard <- NULL
    return(x)
  })
  
  # Return the computed risk factors and base level
  list(
    risk_factors = risk_factors,
    base_level = base_level
  )
}



# =====================================================================================================================================
# Calculate Deviance for Poisson and Tweedie Distributions
# =====================================================================================================================================
#'
#' This function calculates the deviance for either Poisson or Tweedie distributions, 
#' depending on the specified family. It supports weighted observations and a Tweedie power parameter for 
#' flexible modeling of the Tweedie distribution.
#'
#' @param y A numeric vector of observed values.
#' @param mu_matrix A matrix of predicted means (one column per model) for each observation.
#' @param weights A numeric vector of weights, default is equal weight (1) for each observation.
#' @param family A string specifying the distribution for the deviance calculation, either "poisson" or "tweedie".
#' @param tweedie_power A numeric value specifying the Tweedie power parameter, relevant only if family is "tweedie" (default: 1.5).
#'
#' @return A numeric vector of deviances for each column of the `mu_matrix` (corresponding to each model).
#'
#' @examples
#' y <- c(5, 10, 15)
#' mu_matrix <- matrix(c(4, 8, 12), nrow = 3, ncol = 1)
#' calculate_deviance_per_s(y, mu_matrix, family = "poisson")
#' calculate_deviance_per_s(y, mu_matrix, family = "tweedie", tweedie_power = 1.5)
#'
#' @export
calculate_deviance_per_s <- function(y, mu_matrix, weights = rep(1, length(y)), 
                                     family = c("poisson", "tweedie"), tweedie_power = 1.5) {
  # Match the family argument to one of the specified values
  family <- match.arg(family)
  
  if (family == "poisson") {
    # Compute deviance for Poisson distribution
    y_mat <- matrix(y, nrow = length(y), ncol = ncol(mu_matrix))  # Reshape y for matrix operations
    term <- ifelse(y_mat == 0, 0, y_mat * log(y_mat / mu_matrix)) - (y_mat - mu_matrix)  # Deviance formula for Poisson
    deviances <- 2 * colSums(weights * term)  # Sum over the columns (models)
  } else {
    # Compute deviance for Tweedie distribution
    deviances <- colSums(weights * tweedie::tweedie.dev(y, mu_matrix, power = tweedie_power))  # Tweedie deviance
  }
  
  # Set column names of the deviance result to match the column names of mu_matrix
  names(deviances) <- colnames(mu_matrix)
  
  return(deviances)
}


# =====================================================================================================================================
# Get a GLM Family Object Based on the Specified Distribution
# =====================================================================================================================================
#'
#' This function returns a generalized linear model (GLM) family object based on the selected 
#' distribution. It supports Poisson, binomial, negative binomial, gamma, and Tweedie distributions.
#'
#' @param distribution A character string specifying the distribution. Options include:
#'   - "poisson" (Poisson distribution with log link)
#'   - "binomial" (Binomial distribution with logit link)
#'   - "negative_binomial" (Negative binomial distribution with log link)
#'   - "gamma" (Gamma distribution with log link)
#'   - "tweedie" (Tweedie distribution with a specified variance power)
#' @param variance_power A numeric value specifying the variance power for the Tweedie distribution (default: 1.5).
#' @param theta A numeric value for the dispersion parameter of the negative binomial distribution (default: 2).
#'
#' @return A GLM family object corresponding to the specified distribution.
#'
#' @examples
#' get_family("poisson")
#' get_family("negative_binomial", theta = 1.5)
#' get_family("tweedie", variance_power = 1.8)
#'
#' @export
get_family <- function(distribution = c("poisson","binomial","negative_binomial","gamma","tweedie"),
                       variance_power = 1.5,
                       theta = 2){
  
  # Match the distribution argument to one of the specified values
  distribution <- match.arg(distribution)
  
  # Select the appropriate GLM family based on the specified distribution
  family <- switch(distribution,
                   poisson = poisson(link = "log"),
                   binomial = binomial(link = "logit"),
                   negative_binomial = MASS::negative.binomial(theta = theta, link = "log"),
                   gamma = Gamma(link = "log"),
                   tweedie = statmod::tweedie(var.power = variance_power, link.power = 0),
                   distribution)
  
  return(family)
}



# =====================================================================================================================================
# Export risk model coefficients to Excel
# =====================================================================================================================================
#' Export risk model coefficients to Excel
#'
#' @param model The modeling object with risk factors
#' @param s Numeric index for coefficient selection (e.g., 0 for "s0", 1 for "s1", etc.)
#' @param type Either "response" or "link" for the output type
#' @param file_path File path for Excel output (optional)
#' @param file_name Name of the Excel file (optional)
#'
#' @return Invisible, exports data to Excel
#' @export
#'
#' @examples
#' export_risk_factors_to_excel(model_n, 0, "response")
export_risk_factors_to_excel <- function(model, s, type = "response", 
                                         file_path = NULL, file_name = NULL) {
  # Convert numeric s to "s0", "s1", etc.
  s_name <- paste0("s", s)
  
  # Validate parameters
  if (!s_name %in% colnames(model$base_level)) {
    stop(paste("Invalid s value. Available values are:", 
               paste(gsub("s", "", colnames(model$base_level)), collapse = ", ")))
  }
  
  if (!type %in% c("response", "link")) {
    stop("Type must be either 'response' or 'link'")
  }
  
  # Default path and name, if not specified
  if (is.null(file_path)) {
    file_path <- getwd()
  }
  if (is.null(file_name)) {
    file_name <- paste0("risk_factors_", s, "_", type, ".xlsx")
  }
  
  # Create full file path
  full_path <- file.path(file_path, file_name)
  
  # function for converting from link to response
  get_family <- get_family(distribution)$linkinv
  
  # Prepare result data
  results <- list()
  
  # Extract base level for the selected s
  base_level <- model$base_level[1, s_name]
  
  # Iterate over risk factors 
  for (factor_name in names(model$risk_factors)) {
    # Extract dummy-encoded data
    dummy_data <- model$risk_factors[[factor_name]]$risk_factor_link_dummy_encoded
    
    # Extract values for the selected s
    s_values <- dummy_data[, s_name, drop = FALSE]
    
    # Iterate over all feature expressions
    for (i in 1:nrow(s_values)) {
      feature <- factor_name
      expression <- rownames(s_values)[i]
      coef_value <- s_values[i, 1]
      
      # Convert for response type, if needed
      if (type == "response") {
        coef_value <- get_family(coef_value)
      }
      
      # Store result
      results[[length(results) + 1]] <- list(
        Feature = feature,
        Expression = expression,
        Coefficient = coef_value
      )
    }
  }
  
  # Add base level
  base_coef <- base_level
  if (type == "response") {
    base_coef <- get_family(base_coef)
  }
  
  results[[length(results) + 1]] <- list(
    Feature = "Base Level",
    Expression = "Base",
    Coefficient = base_coef
  )
  
  # Convert to dataframe
  df <- do.call(rbind, lapply(results, as.data.frame))
  
  # Export to Excel
  wb <- createWorkbook()
  addWorksheet(wb, "Risk Factors")
  writeData(wb, "Risk Factors", df)
  
  # Formatting
  setColWidths(wb, "Risk Factors", cols = 1:3, widths = c(15, 20, 15))
  
  # Save
  saveWorkbook(wb, full_path, overwrite = TRUE)
  
  cat(paste0("File successfully saved at: ", full_path, "\n"))
  
  return(invisible(df))
}