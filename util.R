# =====================================================================================================================================
# File: util.R
# =====================================================================================================================================
# Description:
# This script contains functions to:
# - create_term_frame
# - find_baseline_level_name
# - predict_dummy_encoded_risk_factor
# - calculate_dummy_encoded_risk_factors
# - calculate_deviance_per_s
# - get_family
# - export_risk_factors_to_excel
# - calculate_feature_exposure
#
# Convert Sum-to-Differences (SDIF) Encoding to Dummy Encoding 
# Calculates risk factors for categorical variables that have been converted to a dummy-encoded format after applying Sum-to-Differences 
#   (SDIF) encoding.
# Calculate Deviance for Poisson and Tweedie Distributions
# Get a GLM Family Object Based on the Specified Distribution
# Export risk model coefficients to Excel
# Calculate exposure for features including interactions
# 
#
# Author: mmatull
# Date: 2025-10-06
# =====================================================================================================================================


# =====================================================================================================================================
# Find Baseline Level with Maximum Weight
# =====================================================================================================================================
#'
#' This function identifies the most frequent level (for a main effect) or the most frequent
#' combination of levels (for an interaction) in a dataset, based on a specified weight column.
#' It is typically used to determine a stable baseline or reference category for modeling.
#' The function handles both single variable names and interaction terms (e.g., "Var1:Var2").
#'
#' @param data A `data.frame` or `tibble` containing the variables and the weight column.
#' @param var_name A character string specifying the target variable or interaction term.
#'   Interaction terms must be separated by a colon (e.g., `"Area"` or `"Area:Power"`).
#' @param weight_col A character string with the name of the column containing the weights (e.g., Exposure).
#'
#' @return A single character string representing the name of the most frequent level or
#'   level combination. For interactions, the levels are joined by a colon (e.g., `"C"` or `"C:9"`).
#'
#' @examples
#' # Create sample data
#' sample_data <- data.frame(
#'   Area = factor(rep(c("A", "B", "C"), times = 4)),
#'   Power = factor(rep(c(8, 9), times = 6)),
#'   Exposure = c(10, 20, 100, 15, 25, 5, 12, 18, 150, 8, 22, 4)
#' )
#'
#' # Example 1: Find the baseline for a main effect ("Area")
#' # The most frequent level is "C" (weight 100 + 150 = 250)
#' # find_baseline_level_name(sample_data, "Area", "Exposure")
#'
#' # Example 2: Find the baseline for an interaction ("Area:Power")
#' # The most frequent combination is "C:9" (weight 150)
#' # find_baseline_level_name(sample_data, "Area:Power", "Exposure")
#'
#' @export
find_baseline_level_name <- function(data, var_name, weight_col) {
  
  # Split the `var_name` by the colon to extract the individual variable names.
  # This works for both a single variable "Var" and an interaction "Var1:Var2".
  vars_to_group_by <- str_split(var_name, ":", simplify = TRUE)[1, ]
  
  # Find the combination of levels with the highest total weight.
  # The `group_by` statement with `across` dynamically handles one or more column names.
  max_level_info <- data %>%
    group_by(across(all_of(vars_to_group_by))) %>%
    summarise(total_weight = sum(.data[[weight_col]], na.rm = TRUE), .groups = "drop") %>%
    slice_max(order_by = total_weight, n = 1, with_ties = FALSE)
  
  # Construct the final name for the baseline level.
  if (length(vars_to_group_by) > 1) {
    # Case 1: Interaction. Join the level names from the individual columns with a colon.
    baseline_name <- max_level_info %>%
      unite("name", all_of(vars_to_group_by), sep = ":") %>%
      pull(name)
  } else {
    # Case 2: Main effect. Extract the single level name.
    baseline_name <- as.character(max_level_info[[vars_to_group_by]])
  }
  
  return(baseline_name)
}



# =====================================================================================================================================
# Create a Data Frame for a Specific Model Term
# =====================================================================================================================================
#'
#' This function builds a data frame for a specific model term (either a main effect or an interaction).
#' It is designed to work from the structure of a data frame (i.e., a 0-row version) to create a
#' complete grid of all factor combinations for the specified term. All other columns in the
#' data frame are filled with their first factor level as a default value. This is useful for
#' creating new data for prediction, which can then be passed to `create_design_matrix`.
#'
#' @param data_structure A data frame with 0 rows but with the complete structure (column names,
#'   types, and factor levels) of the original data. All columns are expected to be factors.
#' @param term A character string specifying the model term, e.g., `'Region'` for a main effect
#'   or `'Produkt:Region'` for an interaction.
#'
#' @return A new data frame with rows for each level (for a main effect) or combination of levels
#'   (for an interaction) of the specified term. Row names are set to the factor levels or their
#'   combinations (separated by ":").
#'
#' @examples
#' # Define the structure of some mock data
#' data <- data.frame(
#'   Region = factor(c("North", "South", "East", "West")),
#'   Product = factor(c("A", "B", "C")),
#'   Product2 = factor(c("X", "Y"))
#' )
#' data_structure <- data[0, ]
#'
#' # 1. Create a frame for a main effect
#' main_effect_frame <- create_term_frame(data_structure, "Region")
#' print(main_effect_frame)
#' # Row names will be: "North", "South", "East", "West"
#'
#' # 2. Create a frame for an interaction
#' interaction_frame <- create_term_frame(data_structure, "Product:Region")
#' print(interaction_frame)
#' # Row names will be: "A:North", "B:North", "C:North", "A:South", etc.
#'
#' @export
create_term_frame <- function(data_structure, term) {
  # Check if the term is an interaction (contains a colon)
  if (grepl(":", term)) {
    # --- LOGIC FOR INTERACTIONS ---
    term_columns <- strsplit(term, ":")[[1]]
    level_list <- lapply(data_structure[term_columns], levels)
    base_df <- do.call(expand.grid, c(level_list, list(stringsAsFactors = FALSE)))
    
    # Convert to factors with original levels
    for (i in seq_along(term_columns)) {
      col_name <- term_columns[i]
      original_levels <- levels(data_structure[[col_name]])
      base_df[[col_name]] <- factor(base_df[[col_name]], levels = original_levels)
    }
    
    # Create row names by combining factor levels with ":"
    row_names <- apply(base_df, 1, function(row) {
      paste(row, collapse = ":")
    })
    
  } else {
    # --- LOGIC FOR MAIN EFFECTS ---
    term_columns <- term
    factor_levels <- levels(data_structure[[term_columns]])
    base_df <- data.frame(
      factor_levels, 
      stringsAsFactors = FALSE
    )
    names(base_df) <- term_columns
    
    # Convert to factor with original levels
    original_levels <- levels(data_structure[[term_columns]])
    base_df[[term_columns]] <- factor(base_df[[term_columns]], levels = original_levels)
    
    # Row names are just the factor levels
    row_names <- factor_levels
  }
  
  # --- COMMON LOGIC TO FILL OTHER COLUMNS AND ORDER ---
  # Fill remaining columns with their first factor level as a default
  other_columns <- setdiff(names(data_structure), term_columns)
  if (length(other_columns) > 0) {
    for (col_name in other_columns) {
      original_levels <- levels(data_structure[[col_name]])
      first_level <- original_levels[1]
      base_df[[col_name]] <- factor(rep(first_level, nrow(base_df)), levels = original_levels)
    }
  }
  
  # Ensure the final data frame has the same column order as the original structure
  term_df <- base_df[, names(data_structure)]
  
  # Set the row names
  rownames(term_df) <- row_names
  
  return(term_df)
}



# =====================================================================================================================================
# Isolate and Predict the Effect of a Single Model Term
# =====================================================================================================================================
#'
#' This function calculates the isolated effect of a single model term (main effect or interaction)
#' from a set of SDIF-encoded variables. It works by first creating a full design matrix,
#' then setting all columns not related to the target term to zero. Finally, it uses a
#' trained glmnet model to predict the effect based on this modified matrix.
#'
#' @param glmnet_model A trained glmnet model used for prediction.
#' @param contrasts A named list of contrast matrices for categorical variables (SDIF encoded).
#' @param data_structure A data.frame defining the structure for which the design matrix is created.
#' @param interactions An optional formula or specification for interaction terms.
#' @param var_name A character string specifying the target variable or interaction term (e.g., "Group" or "Group:Age").
#'
#' @return A matrix of predictions representing the isolated link-scale contribution of the specified term.
#'
#' @examples
#' # Assuming `glmnet_model` is a trained model and `contrasts` contains SDIF encodings.
#' # Let `my_data` be the structure for prediction.
#'
#' # Predict the effect of the main term "Group"
#' # predict_dummy_encoded_risk_factor(glmnet_model, contrasts, my_data, interactions, "Group")
#'
#' # Predict the effect of an interaction term "Group:Age"
#' # predict_dummy_encoded_risk_factor(glmnet_model, contrasts, my_data, interactions, "Group:Age")
#'
#' @export
predict_dummy_encoded_risk_factor <- function(glmnet_model, contrasts, data_structure, interactions = NULL, var_name) {
  
  # Create a data frame that defines the terms of the model
  term_frame <- create_term_frame(data_structure = data_structure, var_name)
  
  # Create the full design matrix using the specified contrasts and interactions
  term_frame_d <- create_design_matrix(term_frame, contrasts, interactions = interactions, sparse_matrix = TRUE)
  
  # Identify all columns in the design matrix that correspond to the target term (var_name)
  # This handles both main effects (e.g., "Group") and interactions (e.g., "Group:Age")
  pattern <- grepl(if(grepl(":", var_name)) gsub(":", "[^:]*:", var_name) else paste0("^", var_name, "[^:]*$"), colnames(term_frame_d))
  
  # Zero out all other terms to isolate the effect of the target variable
  term_frame_d[, !pattern] <- 0
  
  # Predict the isolated effect using the trained model; no offset is applied.
  preds <- predict(glmnet_model, newx = term_frame_d, newoffset = rep(0, nrow(term_frame_d)), type = "link")
  
  return(preds)
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
#' @param data A data frame containing the full dataset with original categorical variables.
#' @param weight_col A string indicating the column name for the weights in the data.
#' @param data_structure A data structure containing information about the categorical variables.
#' @param contrasts A list of contrast matrices for each categorical variable (SDIF encoding).
#' @param features A vector of main effect feature names to process.
#' @param interactions A vector of interaction effect names to process (optional, default: NULL).
#'
#' @return A list containing the computed risk factors:
#'   - `risk_factors`: A named list with each categorical variable's risk factors (standard, link, and dummy encoded).
#'   - `base_level`: The base level risk factor, adjusted for all variables.
#'
#' @examples
#' calculate_dummy_encoded_risk_factors(glmnet_model, data, "weight", data_structure, contrasts, features, interactions)
#'
#' @export
calculate_dummy_encoded_risk_factors <- function(glmnet_model, data, weight_col, data_structure, contrasts, features, interactions = NULL) {
  
  # Combine main effects and interaction effects into a single list for processing
  features_list <- c(features, interactions)
  
  # Iterate over each feature (main effects and interactions) to calculate risk factors
  risk_factors <- map(features_list, function(var_name) {
    
    # Convert Sum-to-Differences (SDIF) Encoding to Dummy Encoding using the fitted model
    risk_factor_link <- predict_dummy_encoded_risk_factor(glmnet_model, contrasts, data_structure, interactions = interactions, var_name)
    
    # Find the level name with the highest weight (works for both main effects and interaction effects)
    baseline_level_name <- find_baseline_level_name(data, var_name, weight_col)
    
    # Select the risk factor for this baseline level directly by name.
    # This approach is more robust than using row numbers.
    risk_factor_standard <- risk_factor_link[baseline_level_name, , drop = FALSE]
    
    # Recenter the risk factor link by subtracting the baseline risk factor, effectively setting the most frequent category level as the reference.
    risk_factor_link_dummy_encoded <- sweep(risk_factor_link, 2, risk_factor_standard, "-")
    
    # Return the computed risk factors for the variable (standard, link, dummy encoded)
    list(
      risk_factor_standard = risk_factor_standard,
      risk_factor_link_sdif_encoded = risk_factor_link,
      risk_factor_link_dummy_encoded = risk_factor_link_dummy_encoded
    )
  })
  # Assign names to the risk factors list using the features list
  names(risk_factors) <- features_list
  
  # Calculate the new base level for the model, adjusting for all variables
  base_level_original <- glmnet_model$a0  # Extract the original intercept from the fitted model
  base_level <- base_level_original +
    reduce(map(risk_factors, "risk_factor_standard"), `+`) -  # Add the sum of all baseline risk factors
    length(risk_factors) * base_level_original  # Subtract the original intercept effect for each variable
  
  # Remove risk_factor_standard from the output as it's no longer needed
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



# =====================================================================================================================================
# Subset glmnet Object by Lambda Indices
# =====================================================================================================================================
#'
#' This helper function creates a subset of a fitted glmnet object containing only the 
#' specified lambda values. This is useful for speeding up relaxed fitting by reducing 
#' the number of lambda values that need to be processed.
#'
#' The function maintains the structure and class of the original glmnet object while
#' only keeping the coefficients, intercepts, and other components corresponding to 
#' the selected lambda indices.
#'
#' @param glmnet_fit A fitted glmnet object (output from glmnet::glmnet)
#' @param lambda_indices A vector of lambda indices (0-based) to extract from the original model.
#'                       These will be converted to 1-based indexing internally.
#'
#' @return A modified glmnet object containing only the specified lambda values, maintaining
#'         the same structure and class as the input object.
#'
#' @details
#' The function handles both standard glmnet fits and special cases like multinomial 
#' or grouped lasso where coefficients are stored as lists rather than matrices.
#' 
#' All relevant components are subset consistently:
#' - Intercepts (a0)
#' - Coefficients (beta) 
#' - Degrees of freedom (df)
#' - Lambda values (lambda)
#' - Deviance ratios (dev.ratio)
#' - Model dimensions (dim)
#'
#' @examples
#' # Create subset with first, middle, and last lambda values
#' fit_subset <- subset_glmnet_by_lambda(original_fit, c(0, 10, 25))
#'
#' @keywords internal

subset_glmnet_by_lambda <- function(glmnet_fit, lambda_indices) {
  
  # =====================================================================
  # Index Conversion and Input Validation
  # =====================================================================
  
  # Convert from 0-based to 1-based indexing for R
  # The calling function uses 0-based indices (consistent with glmnet conventions)
  # but R uses 1-based indexing for array access
  lambda_indices <- lambda_indices + 1
  
  # Validate that input is a proper glmnet object
  if (!inherits(glmnet_fit, "glmnet")) {
    stop("glmnet_fit must be a glmnet object")
  }
  
  # Check that lambda indices are within valid range
  
  
  # =====================================================================
  # Object Preparation
  # =====================================================================
  
  # Create a copy of the original object to avoid modifying the input
  # This preserves all attributes and structure of the original glmnet object
  modified_fit <- glmnet_fit
  
  # =====================================================================
  # Subset Core Model Components
  # =====================================================================
  
  # Subset intercept terms (a0)
  # For glmnet, a0 contains the intercept for each lambda value
  modified_fit$a0 <- glmnet_fit$a0[lambda_indices]
  
  # Subset coefficient matrix/list (beta)
  # The structure depends on the type of glmnet model:
  if (is.list(glmnet_fit$beta)) {
    # Multinomial or grouped lasso case:
    # Beta is a list where each element is a coefficient matrix for one response class
    # Each matrix has dimensions [features x lambda_values]
    modified_fit$beta <- lapply(glmnet_fit$beta, function(beta_mat) {
      beta_mat[, lambda_indices, drop = FALSE]
    })
  } else {
    # Standard case (Gaussian, binomial, Poisson, etc.):
    # Beta is a single sparse matrix with dimensions [features x lambda_values]
    modified_fit$beta <- glmnet_fit$beta[, lambda_indices, drop = FALSE]
  }
  
  # =====================================================================
  # Subset Model Metadata
  # =====================================================================
  
  # Subset degrees of freedom for each lambda
  # df tracks the number of non-zero coefficients at each lambda value
  modified_fit$df <- glmnet_fit$df[lambda_indices]
  
  # Subset the actual lambda values
  # These are the regularization parameters used in the penalty
  modified_fit$lambda <- glmnet_fit$lambda[lambda_indices]
  
  # Subset deviance ratios
  # dev.ratio measures the proportion of deviance explained by the model
  # (similar to R-squared but for GLMs)
  modified_fit$dev.ratio <- glmnet_fit$dev.ratio[lambda_indices]
  
  # =====================================================================
  # Update Model Dimensions
  # =====================================================================
  
  # Update the dimension information
  # dim[1] is number of features (unchanged)
  # dim[2] is number of lambda values (now reduced)
  modified_fit$dim[2] <- length(lambda_indices)
  
  # =====================================================================
  # Return Modified Object
  # =====================================================================
  
  # Return the subset glmnet object
  # This object can be used with relax.glmnet() and will only process
  # the selected lambda values, significantly reducing computation time
  return(modified_fit)
}



# =====================================================================================================================================
# Helper function to calculate exposure for features and interactions
# =====================================================================================================================================
#' Calculate exposure for features including interactions
#'
#' This helper function calculates exposure sums for both regular features and interaction terms.
#' For interaction terms (containing ":"), it combines the values of individual features.
#' For regular features, it uses standard grouping.
#'
#' @param feature_name Character. The name of the feature (can include interactions like "X1:X2")
#' @param exposure_df A data frame containing the exposure data
#' @param exposure_col Character. The name of the column containing exposure values
#' @param categories Character vector. Optional. The expected categories to ensure consistent output
#'
#' @return A named numeric vector with exposure sums by category
#'
#' @examples
#' \dontrun{
#' # For regular feature
#' exp_sum <- calculate_feature_exposure("region", claims_data, "premium")
#' 
#' # For interaction
#' exp_sum <- calculate_feature_exposure("region:age_group", claims_data, "premium")
#' }
calculate_feature_exposure <- function(feature_name, exposure_df, exposure_col, categories = NULL) {
  
  # Check if it's an interaction (contains ":")
  if (grepl(":", feature_name)) {
    # Split the interaction into individual features
    individual_features <- strsplit(feature_name, ":")[[1]]
    
    # Check if all individual features exist in the data
    missing_features <- setdiff(individual_features, names(exposure_df))
    if (length(missing_features) > 0) {
      warning(paste("Features not found in exposure_df:", paste(missing_features, collapse = ", ")))
      return(numeric(0))
    }
    
    # Create interaction categories by pasting together the values
    interaction_categories <- apply(
      exposure_df[, individual_features, drop = FALSE], 
      1, 
      paste, 
      collapse = ":"
    )
    
    # Calculate exposure sum for each interaction category
    exposure_sum <- tapply(
      exposure_df[[exposure_col]], 
      interaction_categories, 
      sum, 
      na.rm = TRUE
    )
    
  } else {
    # Standard calculation for non-interaction features
    if (feature_name %in% names(exposure_df)) {
      exposure_sum <- tapply(
        exposure_df[[exposure_col]], 
        exposure_df[[feature_name]], 
        sum, 
        na.rm = TRUE
      )
    } else {
      warning(paste("Feature not found in exposure_df:", feature_name))
      return(numeric(0))
    }
  }
  
  # If categories are provided, ensure all categories are present
  if (!is.null(categories)) {
    missing_cats <- setdiff(categories, names(exposure_sum))
    if (length(missing_cats) > 0) {
      # Add missing categories with 0 exposure
      missing_exposure <- setNames(rep(0, length(missing_cats)), missing_cats)
      exposure_sum <- c(exposure_sum, missing_exposure)
      # Reorder according to provided categories
      exposure_sum <- exposure_sum[categories]
    }
  }
  
  return(exposure_sum)
}
