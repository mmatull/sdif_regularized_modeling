# =====================================================================================================================================
# File: plot.R
# =====================================================================================================================================
# Description:
# This script contains functions to:
# - summarize_risk_factor_levels_per_s
# - plot_risk_factors_all
# - plot_deviance_train_test
# - plot_risk_factors_compare_model
# - plot_feature_predictions_comparison
# - plot_all_feature_predictions_comparison
# - plot_all_risk_factors_for_feature_slider
# - plot_all_features_slider
# - save_all_plots_grid
# - create_risk_factor_dashboard
# - plot_actual_vs_predicted_slider
# - plot_all_features_actual_vs_predicted
#
# Summarize Risk Factors Overview
# Plot Risk Factors and Exposure for All Features
# Plot Train and Test Deviance Over Iterations
# Plot Risk Factors and Exposure for All Features
# Compare Risk Factors Across Models with Exposure Data
# Plot Feature Predictions Comparison Between Regularized and Unregularized Models
# Plot risk factors with exposure by category with interactive slider
# Create interactive risk factor plots for multiple features
# Save Multiple Plots as Interactive Dashboard
# Create risk factor dashboard with global slider for all plots
# Plot actual vs predicted values by category with interactive slider
# Plot actual vs predicted values for all features with interactive sliders
# 
#
# Author: mmatull
# Date: 2025-10-06
# =====================================================================================================================================




# =====================================================================================================================================
# Summarize Risk Factors Overview
# =====================================================================================================================================
#'
#' This function generates an overview of the number of unique values for the 'risk_factor_link_dummy_encoded' 
#' across various features.
#' It calculates the unique counts for each column in the risk factor matrices related to this specific risk factor 
#' and returns the results in a list.
#'
#' @param pipeline_output A list containing the results from the model pipeline, including risk factor information.
#'
#' @return A list where each entry corresponds to a feature (e.g., "Area_categorical", "VehPower_categorical", "VehAge_categorical") 
#'         and contains the number of unique values per feature for the 'risk_factor_link_dummy_encoded'.
#'
#' @examples
#' summarize_risk_factor_levels_per_s(pipeline_output)
#'
#' @export

summarize_risk_factor_levels_per_s <- function(pipeline_output) {
  # Only for 'risk_factor_link_dummy_encoded'
  feature_names <- names(pipeline_output$risk_factors)
  
  # List to store the summary
  summary_list <- list()
  
  # Loop over each feature and calculate unique counts for 'risk_factor_link_dummy_encoded'
  for(feat in feature_names) {
    mat <- pipeline_output$risk_factors[[feat]]$risk_factor_link_dummy_encoded
    
    # Calculate the number of unique values per column
    unique_counts <- apply(mat, 2, function(x) length(unique(x)))
    
    # Store the unique counts
    summary_list[[feat]] <- unique_counts
  }
  
  # Convert summary list to a data frame for better printing
  summary_df <- do.call(rbind, summary_list)
  rownames(summary_df) <- feature_names
  
  # Return the summary list
  return(summary_df )
}



# =====================================================================================================================================
# Plot Risk Factors and Exposure for All Features
# =====================================================================================================================================
#'
#' This function generates a series of plots for each risk factor in the provided model output. 
#' It compares the risk factor values and exposure across categories, using a bar plot for exposure and a line plot for the risk factors. 
#' The function uses Plotly for interactive visualization.
#'
#' @param pipeline_output A list containing the results from the model pipeline, including risk factor information.
#' @param exposure_df A data frame containing exposure data with categories and associated values.
#' @param column_index An integer specifying the column index for the risk factor to be used in the plot.
#' @param exposure_col A string indicating the column name in the exposure_df that contains the exposure values.
#'
#' @return A list of Plotly plots for each feature's risk factor and exposure comparison.
#'
#' @examples
#' plot_risk_factors_all(pipeline_output, exposure_data, 2, "ExposureColumn")
#'
#' @export

plot_risk_factors_all <- function(pipeline_output, exposure_df, column_index,
                                  exposure_col ) {
  
  # Check if column_index is valid (greater than or equal to 0)
  if (column_index < 0) {
    stop("Error: The column index must be greater equal 0.")
  }
  
  # Initialize an empty list to store the plots
  plots <- list()
  
  # Loop through each feature in the risk factors list
  for (feature in names(pipeline_output$risk_factors)) {
    
    # Get the risk factor matrix for the current feature
    risk_factor_matrix <- pipeline_output$risk_factors[[feature]]$risk_factor_link_dummy_encoded
    
    # Skip if the matrix is null or not a matrix
    if (is.null(risk_factor_matrix) || !is.matrix(risk_factor_matrix)) {
      next
    }
    
    # Get the column name and values for the specified column index
    col_name <- colnames(risk_factor_matrix)[column_index + 1]
    risk_values <- risk_factor_matrix[, column_index + 1]
    categories <- rownames(risk_factor_matrix)
    
    # Calculate the exposure using the helper function
    exposure_sum <- calculate_feature_exposure(feature, exposure_df, exposure_col)
    
    # Create a data frame for the exposure by category
    if (length(exposure_sum) > 0) {
      exposure_by_category <- data.frame(
        Category = names(exposure_sum),
        Exposure = as.numeric(exposure_sum)
      )
    } else {
      # If no exposure data available, create empty data frame
      exposure_by_category <- data.frame(
        Category = character(0),
        Exposure = numeric(0)
      )
    }
    
    # Create a data frame for plotting, combining risk factor values and categories
    plot_data <- data.frame(
      Category = categories,
      RiskFactor = risk_values
    )
    
    # Merge the exposure data with the plot data based on the Category
    plot_data <- merge(
      plot_data,
      exposure_by_category,
      by.x = "Category",
      by.y = "Category",
      all.x = TRUE
    )
    
    # Replace any NA values in Exposure with 0
    plot_data$Exposure[is.na(plot_data$Exposure)] <- 0
    
    # Order the data by category
    plot_data <- plot_data[order(factor(plot_data$Category, levels = categories)), ]
    
    # Create the plot using Plotly
    p <- plot_ly() %>%
      # Add a bar plot for the exposure data
      add_trace(
        data = plot_data,
        x = ~Category, 
        y = ~Exposure,
        type = "bar",  # Bar plot for exposure
        name = "Exposure",  # Legend label for exposure
        marker = list(color = "rgba(219, 64, 82, 0.7)"),  # Color for the exposure bars
        yaxis = "y2",  # Use the secondary Y-axis for exposure
        hoverinfo = "text",  # Show hover information
        text = ~paste("Category:", Category, "<br>Exposure:", format(Exposure, big.mark = ".", decimal.mark = ",")),
        textposition = "none"  # Do not display the text on the bars
      ) %>%
      # Add a line plot for the risk factor values
      add_trace(
        data = plot_data,
        x = ~Category, 
        y = ~RiskFactor,
        type = "scatter",  # Scatter plot for the risk factors
        mode = "lines+markers",  # Lines and markers for the risk factors
        name = "Risk Factor",  # Legend label for risk factors
        line = list(color = "rgb(31, 119, 180)", width = 3),  # Line style for risk factor
        marker = list(color = "rgb(31, 119, 180)", size = 8),  # Marker style for risk factor
        hoverinfo = "text",  # Show hover information
        text = ~paste("Category:", Category, "<br>Risk Factor:", format(RiskFactor, digits = 5, nsmall = 5))  # Text displayed on hover
      ) %>%
      # Set layout options for the plot
      layout(
        title = paste("Risk Factors and Exposure for", feature, "-", col_name),  # Plot title
        xaxis = list(
          title = feature,  # X-axis label
          tickangle = -90,  # Rotate the x-axis labels
          tickfont = list(size = 12),  # Font size for the x-axis labels
          categoryorder = "array",  # Order the categories numerically
          categoryarray = categories  
        ),
        yaxis = list(
          title = "Risk Factor",  # Left Y-axis title
          titlefont = list(color = "rgb(31, 119, 180)"),  # Left Y-axis font color
          tickfont = list(color = "rgb(31, 119, 180)"),  # Left Y-axis tick color
          side = "left",  # Position on the left
          zeroline = TRUE  # Display zero line
        ),
        yaxis2 = list(
          title = "Exposure",  # Right Y-axis title
          titlefont = list(color = "rgb(219, 64, 82)"),  # Right Y-axis font color
          tickfont = list(color = "rgb(219, 64, 82)"),  # Right Y-axis tick color
          side = "right",  # Position on the right
          overlaying = "y",  # Overlay on the primary Y-axis
          zeroline = FALSE  # Remove the zero line for the right Y-axis
        ),
        legend = list(
          orientation = "h",  # Horizontal legend
          xanchor = "center",  # Center the legend
          x = 0.5,  # Position the legend in the middle
          y = 1.1  # Position the legend above the plot
        ),
        margin = list(t = 80, r = 80, l = 80, b= 80),  # Set plot margins
        hoverlabel = list(
          bgcolor = "white",  # Set background color for hover labels
          font = list(family = "Arial", size = 12)  # Font settings for hover labels
        ),
        hovermode = "closest"  # Display the hover info for the closest data point
      )
    
    # Add the plot to the list of plots for each feature
    plots[[feature]] <- list(plot = p)
  }
  
  # Return the list of plots
  return(plots)
}



# =====================================================================================================================================
# Plot Train and Test Deviance Over Iterations with Percentage Improvement
# =====================================================================================================================================
#'
#' This function creates a plot to compare the deviance values for both training and test datasets over iterations. 
#' It uses Plotly to generate an interactive plot with two y-axes: one for the train deviance and one for the test deviance.
#' Additionally, it can display the percentage improvement in deviance, where 100% represents the maximum observed improvement
#' from the initial deviance value.
#'
#' @param deviance_train A numeric vector containing the deviance values for the training dataset.
#' @param deviance_test A numeric vector containing the deviance values for the test dataset.
#' @param show_percentage Logical, indicating whether to show percentage improvement instead of absolute values. Default is FALSE.
#'
#' @return A Plotly plot object showing the deviance for both train and test datasets, with interactive hover information and separate y-axes.
#'
#' @examples
#' # Sample data
#' train_dev <- c(1210603, 1209320, 1208348, 1207612, 1206880, 1206071, 1205400, 1204866, 1204462, 1204156)
#' test_dev <- c(1201911, 1201945, 1201885, 1201911, 1201945, 1201945, 1201911, 1201885, 1201866, 1201851)
#' 
#' # Show absolute deviance values
#' plot_deviance_train_test(train_dev, test_dev)
#' 
#' # Show percentage improvement
#' plot_deviance_train_test(train_dev, test_dev, show_percentage = TRUE)
#'
#' @export

plot_deviance_train_test <- function(deviance_train, deviance_test, show_percentage = FALSE) {
  
  # Determine the number of steps based on the length of the training data
  n_steps <- length(deviance_train)
  
  # Get initial and minimum values for both datasets
  train_initial <- deviance_train[1]
  train_min <- min(deviance_train)
  test_initial <- deviance_test[1]  # Assuming first value is the initial value
  test_min <- min(deviance_test)
  
  # Calculate the maximum possible improvement for each dataset
  max_train_improvement <- train_initial - train_min
  max_test_improvement <- test_initial - test_min
  
  # Calculate actual improvement at each step
  train_total_improvement <- train_initial - deviance_train
  test_total_improvement <- test_initial - deviance_test
  
  # Calculate step-by-step improvement (difference to previous step)
  train_step_improvement <- c(0, diff(train_total_improvement))  # First value is 0 (no previous step)
  test_step_improvement <- c(0, diff(test_total_improvement))  # First value is 0 (no previous step)
  
  # Calculate percentage improvement if requested
  if (show_percentage) {
    # For train data: convert to percentage improvement (0% = no improvement, 100% = maximum improvement)
    train_percent <- train_total_improvement  / max_train_improvement * 100
    
    # For test data: convert to percentage improvement (0% = no improvement, 100% = maximum improvement)
    test_percent <- test_total_improvement  / max_test_improvement * 100
    
    # Step-by-step percentage improvement
    train_step_percent <- c(0, diff(train_percent))  # First value is 0%
    test_step_percent <- c(0, diff(test_percent))  # First value is 0%    
    
    # Use percentage values for plotting
    y_train <- train_percent
    y_test <- rev(test_percent)
    
    # Y-axis labels for percentage view
    train_axis_title <- "Train Deviance Improvement (%)"
    test_axis_title <- "Test Deviance Improvement (%)"
    
    # Hover text for percentage view
    hover_train <- paste(
      "Iteration:", 0:(n_steps - 1), 
      "<br>Train Deviance:", round(deviance_train, 1),
      "<br>Total Improvement:", round(train_percent, 1), "%",
      "<br>Absolute Total:", round(train_total_improvement, 1),
      "<br>Step Improvement:", round(train_step_percent, 1), "%",
      "<br>Absolute Step:", round(train_step_improvement, 1)
    )
    
    hover_test <- paste(
      "Iteration:", rev(0:(n_steps - 1)), 
      "<br>Test Deviance:", round(rev(deviance_test), 1),
      "<br>Total Improvement:", round(rev(test_percent), 1), "%",
      "<br>Absolute Total:", round(rev(test_total_improvement), 1),
      "<br>Step Improvement:", round(rev(test_step_percent), 1), "%",
      "<br>Absolute Step:", round(rev(test_step_improvement), 1)
    )
  } else {
    # Use absolute deviance values for plotting
    y_train <- deviance_train
    y_test <- rev(deviance_test)
    
    # Y-axis labels for absolute view
    train_axis_title <- "Train Deviance"
    test_axis_title <- "Test Deviance"
    
    # Hover text for absolute view
    hover_train <- paste(
      "Iteration:", 0:(n_steps - 1), 
      "<br>Train Deviance:", round(deviance_train, 1)
    )
    
    hover_test <- paste(
      "Iteration:", rev(0:(n_steps - 1)), 
      "<br>Test Deviance:", round(rev(deviance_test), 1)
    )
  }
  
  # Create a data frame for the plot
  df <- data.frame(
    Step = 0:(n_steps - 1),          # Iteration steps for x-axis
    Train = y_train,                 # Train values (absolute or percentage)
    Test = y_test,                   # Test values (absolute or percentage)
    Test_Step = rev(0:(n_steps - 1)),# Reversed test iteration steps for hover text
    Hover_Train = hover_train,       # Hover text for train data
    Hover_Test = hover_test          # Hover text for test data
  )
  
  # Define a uniform font size for plot elements
  font_size <- 12
  
  # Plot title based on view type
  plot_title <- if(show_percentage) {
    "<b>Train vs. Test Deviance Improvement (%)</b>"
  } else {
    "<b>Train vs. Test Deviance</b>"
  }
  
  # Y-axis range for percentage view (ensure 0-100% with some padding)
  y_range <- if(show_percentage) {
    list(
      yaxis = list(range = c(-5, 105)),
      yaxis2 = list(range = c(-5, 105))
    )
  } else {
    list()  # Empty list for default behavior
  }
  
  # Create the plot using Plotly
  p <- plot_ly() %>%
    # Add trace for training data on the primary Y-axis (left side)
    add_trace(
      data = df,
      x = ~Step,                         # x-axis: Iteration step
      y = ~Train,                        # y-axis: Train values
      type = 'scatter',                  # Scatter plot
      mode = 'lines+markers',            # Plot lines and markers
      name = 'Train',                    # Label for the trace
      line = list(color = 'blue', width = 2),   # Line style (blue)
      marker = list(color = 'blue', size = 8),  # Marker style (blue)
      hoverinfo = 'text',                # Info shown on hover
      text = ~Hover_Train,               # Hover text
      yaxis = "y"                        # Primary Y-axis
    ) %>%
    # Add trace for test data on the secondary Y-axis (right side)
    add_trace(
      data = df,
      x = ~Step,                         # x-axis: Iteration step
      y = ~Test,                         # y-axis: Test values
      type = 'scatter',                  # Scatter plot
      mode = 'lines+markers',            # Plot lines and markers
      name = 'Test',                     # Label for the trace
      line = list(color = 'red', width = 2),    # Line style (red)
      marker = list(color = 'red', size = 8),   # Marker style (red)
      hoverinfo = 'text',                # Info shown on hover
      text = ~Hover_Test,                # Hover text
      yaxis = "y2"                       # Secondary Y-axis
    ) %>%
    # Layout settings for both Y-axes and overall plot appearance
    layout(
      title = list(
        text = plot_title,               # Title of the plot
        font = list(size = font_size + 2) # Title font size
      ),
      xaxis = list(
        title = "Iteration",             # Label for x-axis
        zeroline = FALSE,                # Remove zero line
        titlefont = list(size = font_size), # Font size for x-axis title
        tickfont = list(size = font_size)  # Font size for x-axis ticks
      ),
      yaxis = list(
        title = train_axis_title,        # Label for left Y-axis
        side = "left",                   # Place on left side
        showgrid = TRUE,                 # Show grid lines
        zeroline = FALSE,                # Remove zero line
        titlefont = list(size = font_size), # Font size for Y-axis title
        tickfont = list(size = font_size),  # Font size for Y-axis ticks
        range = if(show_percentage) c(-5, 105) else NULL  # Range for percentage view
      ),
      yaxis2 = list(
        title = test_axis_title,         # Label for right Y-axis
        side = "right",                  # Place on right side
        overlaying = "y",                # Overlay on the primary Y-axis
        showgrid = FALSE,                # No grid for the second Y-axis
        zeroline = FALSE,                # Remove zero line
        ticklen = 4,                     # Tick length
        tickwidth = 1,                   # Tick width
        tickcolor = "#000",              # Tick color (black)
        titlefont = list(size = font_size), # Font size for Y-axis title
        tickfont = list(size = font_size),  # Font size for Y-axis ticks
        range = if(show_percentage) c(-5, 105) else NULL  # Range for percentage view
      ),
      legend = list(
        x = 1.1,                           # Position of the legend (right edge)
        y = 1.1,                           # Position of the legend (top edge)
        xanchor = 'right',               # Anchor legend to right edge
        yanchor = 'top',                 # Anchor legend to top edge
        yref = "paper", 
        font = list(size = font_size)    # Font size for legend
      ),
      margin = list(r = 75, l = 75, t = 50, b = 50), # Margin settings
      font = list(size = font_size)      # Global font size
    )
  
  # Return the plot object
  return(p)
}



# =====================================================================================================================================
# Compare Risk Factors Across Models with Exposure Data
# =====================================================================================================================================
#'
#' This function creates comparative plots of risk factors from two models, incorporating exposure data.
#' The function extracts risk factors, aligns them by category, and visualizes them alongside exposure values.
#'
#' @param pipe_regularized A list containing risk factors and model results from the first model (regularized).
#' @param pipe_unregularized A list containing risk factors and model results from the second model (unregularized).
#' @param data A data frame containing exposure data.
#' @param exposure_col A string specifying the column name representing exposure values.
#' @param lambda_index A numeric value specifying the index for the lambda parameter in the fitted model .
#'
#' @return A list of Plotly plots comparing the risk factors and exposure across models.
#'
#' @examples
#' plot_risk_factors_compare_model(result1, result2, data, "exposure", 0)
#'
#' @export
plot_risk_factors_compare_model <- function(pipe_regularized, pipe_unregularized, data, exposure_col, lambda_index) {
  
  # Create the lambda parameter name (e.g. “s5” for lambda_index = 5)
  target_lambda <- paste0("s", lambda_index)
  
  plots <- list()
  
  # Collect all features from both models
  all_features <- unique(c(
    names(pipe_regularized$risk_factors),
    names(pipe_unregularized$risk_factors)
  ))
  
  for (feature in all_features) {
    
    # Extract risk factor matrices for each model
    risk_factor_matrix1 <- pipe_regularized$risk_factors[[feature]]$risk_factor_link_dummy_encoded %||% NULL
    risk_factor_matrix2 <- pipe_unregularized$risk_factors[[feature]]$risk_factor_link_dummy_encoded %||% NULL
    
    # Skip features not present in either model
    if ((is.null(risk_factor_matrix1) || !is.matrix(risk_factor_matrix1)) && 
        (is.null(risk_factor_matrix2) || !is.matrix(risk_factor_matrix2))) {
      next
    }
    
    # Extract all unique categories across both models
    categories1 <- if (!is.null(risk_factor_matrix1)) rownames(risk_factor_matrix1) else character(0)
    categories2 <- if (!is.null(risk_factor_matrix2)) rownames(risk_factor_matrix2) else character(0)
    if (!setequal(categories1, categories2)) {
      stop(
        sprintf(
          "Category mismatch between models:\nOnly in model1: %s\nOnly in model2: %s",
          paste(setdiff(categories1, categories2), collapse = ", "),
          paste(setdiff(categories2, categories1), collapse = ", ")
        )
      )
    }
    
    # original factor level/names for proper sorted x-axis
    all_categories <- rownames(risk_factor_matrix1)
    
    # Check if target lambda exists in model 1
    risk_values1 <- NULL
    col_name1 <- NULL
    if (!is.null(risk_factor_matrix1)) {
      available_lambdas1 <- colnames(risk_factor_matrix1)
      if (target_lambda %in% available_lambdas1) {
        col_index1 <- which(colnames(risk_factor_matrix1) == target_lambda)
        col_name1 <- colnames(risk_factor_matrix1)[col_index1]
        risk_values1 <- risk_factor_matrix1[, col_index1]
        names(risk_values1) <- rownames(risk_factor_matrix1)
      } else {
        warning(paste("Lambda parameter", target_lambda, "not found in regularized model for feature", feature, 
                      ". Available lambdas:", paste(available_lambdas1, collapse = ", ")))
      }
    }
    
    # Check if target lambda exists in model 2
    risk_values2 <- NULL
    col_name2 <- NULL
    if (!is.null(risk_factor_matrix2)) {
      available_lambdas2 <- colnames(risk_factor_matrix2)
      if (target_lambda %in% available_lambdas2) {
        col_index2 <- which(colnames(risk_factor_matrix2) == target_lambda)
        col_name2 <- colnames(risk_factor_matrix2)[col_index2]
        risk_values2 <- risk_factor_matrix2[, col_index2]
        names(risk_values2) <- rownames(risk_factor_matrix2)
      } else {
        warning(paste("Lambda parameter", target_lambda, "not found in unregularized model for feature", feature, 
                      ". Available lambdas:", paste(available_lambdas2, collapse = ", ")))
      }
    }
    
    # Only plot if both models have the target lambda (this is a comparison function!)
    if (is.null(risk_values1) || is.null(risk_values2)) {
      if (is.null(risk_values1) && is.null(risk_values2)) {
        warning(paste("Lambda parameter", target_lambda, "not found in either model for feature", feature, ". Skipping this feature."))
      } else if (is.null(risk_values1)) {
        warning(paste("Lambda parameter", target_lambda, "not found in regularized model for feature", feature, ". Cannot compare - skipping this feature."))
      } else {
        warning(paste("Lambda parameter", target_lambda, "not found in unregularized model for feature", feature, ". Cannot compare - skipping this feature."))
      }
      next
    }
    
    # Aggregate exposure data by category
    exposure_sum <- calculate_feature_exposure(feature, data, exposure_col)
    
    exposure_by_category <- data.frame(
      Category = names(exposure_sum),
      Exposure = as.numeric(exposure_sum)
    )
    
    # Create data frame for plotting
    plot_data <- data.frame(
      Category = all_categories,
      RiskFactor1 = if (!is.null(risk_values1)) risk_values1[match(all_categories, names(risk_values1))] else NA,
      RiskFactor2 = if (!is.null(risk_values2)) risk_values2[match(all_categories, names(risk_values2))] else NA,
      stringsAsFactors = FALSE
    )
    
    # Merge exposure data
    plot_data <- merge(plot_data, exposure_by_category, by = "Category", all.x = TRUE, sort = FALSE)
    plot_data$Exposure[is.na(plot_data$Exposure)] <- 0
    
    p <- plot_ly() %>%
      add_trace(
        data = plot_data,
        x = ~Category, 
        y = ~Exposure,
        type = "bar",
        name = "Exposure",
        marker = list(color = "rgba(219, 64, 82, 0.7)"),
        yaxis = "y2",
        hoverinfo = "text",
        text = ~paste("Category:", Category, 
                      "<br>Exposure:", format(Exposure, big.mark = ".", decimal.mark = ",")),
        textposition = "none"
      )
    
    # Since this is a comparison function, we need both models to have the target lambda
    # Create traces for both models (both are guaranteed to exist at this point)
    model1_name <- paste("Regularized Model", if(!is.null(col_name1)) paste("-", col_name1) else "")
    p <- p %>%
      add_trace(
        data = plot_data,
        x = ~Category, 
        y = ~RiskFactor1,
        type = "scatter",
        mode = "lines+markers",
        name = model1_name,
        line = list(color = "rgb(31, 119, 180)", width = 3),
        marker = list(color = "rgb(31, 119, 180)", size = 8),
        hoverinfo = "text",
        text = ~paste("Category:", Category, 
                      "<br>Risk Factor (R. Model):", 
                      format(RiskFactor1, digits = 5, nsmall = 5))
      )
    
    model2_name <- paste("Unregularized Model", if(!is.null(col_name2)) paste("-", col_name2) else "")
    p <- p %>%
      add_trace(
        data = plot_data,
        x = ~Category, 
        y = ~RiskFactor2,
        type = "scatter",
        mode = "lines+markers",
        name = model2_name,
        line = list(color = "rgb(44, 160, 44)", width = 3, dash = "dash"),
        marker = list(color = "rgb(44, 160, 44)", size = 8, symbol = "triangle-up"),
        hoverinfo = "text",
        text = ~paste("Category:", Category, 
                      "<br>Risk Factor (U. Model):", 
                      format(RiskFactor2, digits = 5, nsmall = 5))
      )
    
    # Set layout options for the plot
    p <- p %>%
      layout(
        title = paste("Risk Factors and Exposure for", feature),
        xaxis = list(
          title = feature,
          tickangle = -90,
          tickfont = list(size = 12),
          categoryorder = "array",
          categoryarray = plot_data$Category
        ),
        yaxis = list(
          title = "Risk Factor",
          titlefont = list(color = "rgb(31, 119, 180)"),
          tickfont = list(color = "rgb(31, 119, 180)"),
          side = "left",
          zeroline = TRUE
        ),
        yaxis2 = list(
          title = "Exposure",
          titlefont = list(color = "rgb(219, 64, 82)"),
          tickfont = list(color = "rgb(219, 64, 82)"),
          side = "right",
          overlaying = "y",
          zeroline = FALSE
        ),
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = 0.5,
          y = 1.1
        ),
        margin = list(t = 80, r = 80, l = 80, b= 80),
        hoverlabel = list(
          bgcolor = "white",
          font = list(family = "Arial", size = 12)
        ),
        hovermode = "closest"
      )
    
    plots[[feature]] <- list(plot = p)
  }
  
  return(plots)
}



# =====================================================================================================================================
# Plot Feature Predictions Comparison Between Regularized and Unregularized Models (Enhanced Version)
# =====================================================================================================================================
#'
#' This function creates an interactive plotly visualization comparing how regularized
#' and unregularized models predict across different levels of a specific feature.
#' It shows actual values, predicted values from both models, and exposure (sample weights)
#' for each feature level. Now supports interaction features (e.g., "X1:X2").
#'
#' @param pipe_regularized An object containing the regularized model information with fitted model results
#' @param pipe_unregularized An object containing the unregularized model information
#' @param data A data frame containing the features and target variable
#' @param feature_name Character string with the name of the feature to plot (supports interactions like "X1:X2")
#' @param target_col Character string with the name of the target/response column
#' @param weight_col Character string with the name of the weight/exposure column
#' @param lambda_index A numeric value specifying the index for the lambda parameter in the fitted model.
#' @param train_or_test Character string indicating whether to use training or test data ("train" or "test")
#'
#' @return A plotly object showing the comparison between actual values, regularized model predictions,
#'         and unregularized model predictions across different levels of the specified feature
#'
#' @examples
#' \dontrun{
#' # Create comparison plot for a single feature using training data
#' plot_feature_predictions_comparison(
#'   pipe_regularized = my_regularized_model,
#'   pipe_unregularized = my_unregularized_model,
#'   data = my_data,
#'   feature_name = "age_group",
#'   target_col = "loss_ratio",
#'   weight_col = "premium",
#'   lambda_index = 5,
#'   train_or_test = "train"
#' )
#' 
#' # Create comparison plot for an interaction feature
#' plot_feature_predictions_comparison(
#'   pipe_regularized = my_regularized_model,
#'   pipe_unregularized = my_unregularized_model,
#'   data = my_data,
#'   feature_name = "X1:X2",
#'   target_col = "loss_ratio",
#'   weight_col = "premium",
#'   lambda_index = 5,
#'   train_or_test = "train"
#' )
#' }
plot_feature_predictions_comparison <- function(pipe_regularized, pipe_unregularized, data, feature_name, target_col, weight_col = NULL, lambda_index, train_or_test = "train") {
  
  # Validate train_or_test parameter
  if(!train_or_test %in% c("train", "test")) {
    stop("train_or_test parameter must be either 'train' or 'test'")
  }
  
  # Create the target lambda parameter name (e.g. "s5" for lambda_index = 5)
  target_lambda <- paste0("s", lambda_index)
  
  # Set train_or_test-specific variables based on the train_or_test parameter
  index_field <- paste0(train_or_test, "_index")
  preds_field_reg <- paste0("preds_", train_or_test)
  preds_field_unreg <- paste0("preds_", train_or_test)
  
  # Check if target lambda exists in regularized model predictions
  available_lambdas_reg <- colnames(pipe_regularized[[preds_field_reg]])
  if (!target_lambda %in% available_lambdas_reg) {
    stop(paste("Lambda parameter", target_lambda, "not found in regularized model predictions.", 
               "Available lambdas:", paste(available_lambdas_reg, collapse = ", ")))
  }
  
  # Check if target lambda exists in unregularized model predictions
  available_lambdas_unreg <- colnames(pipe_unregularized[[preds_field_unreg]])
  if (!target_lambda %in% available_lambdas_unreg) {
    stop(paste("Lambda parameter", target_lambda, "not found in unregularized model predictions.", 
               "Available lambdas:", paste(available_lambdas_unreg, collapse = ", ")))
  }
  
  # Get column indices for both models
  col_index_reg <- which(colnames(pipe_regularized[[preds_field_reg]]) == target_lambda)
  col_index_unreg <- which(colnames(pipe_unregularized[[preds_field_unreg]]) == target_lambda)
  
  # Select appropriate data based on train_or_test parameter
  selected_data <- data[pipe_regularized$split_index[[index_field]],]
  
  # =====================================================================================================================================
  # ENHANCED FEATURE HANDLING: Support for both single features and interactions
  # =====================================================================================================================================
  
  # 1. Check if it's an interaction feature
  if (grepl(":", feature_name)) {
    # Split the interaction feature name at the colon to get individual variables
    vars <- unlist(strsplit(feature_name, ":"))
    
    # Create the new column name for the interaction
    interaction_col_name <- paste(vars, collapse = "_")
    
    # Create the new interaction column in the dataset if it doesn't exist yet
    if (!(interaction_col_name %in% names(selected_data))) {
      selected_data <- selected_data %>%
        mutate(
          !!interaction_col_name := paste(!!!syms(vars), sep = ":")
        )
    }
    
    # Set the grouping column name to the new interaction column
    grouping_column_name <- interaction_col_name
    
    # For interactions, we need to get the feature values from the created column
    feature_values <- selected_data[[grouping_column_name]]
    
  } else {
    # If it's not an interaction, use the original feature name
    grouping_column_name <- feature_name
    
  }
  
  # =====================================================================================================================================
  # CALCULATIONS: Actual and predicted values per feature level
  # =====================================================================================================================================
  
  # Calculate actual values per feature level
  # This aggregates the target variable weighted by the weight column for each feature level
  actual_values <- selected_data %>%
    group_by(feature_level = .data[[grouping_column_name]]) %>%
    summarise(
      actual_rate = sum(.data[[target_col]]) / sum(.data[[weight_col]]),
      exposure = sum(.data[[weight_col]]),
      .groups = "drop"
    )
  
  # Calculate predicted values from pipe_regularized (regularized model)
  # This aggregates the model predictions for each feature level
  pred1_values <- selected_data %>%
    mutate(
      prediction = pipe_regularized[[preds_field_reg]][, col_index_reg]
    ) %>%
    group_by(feature_level = .data[[grouping_column_name]]) %>%
    summarise(
      pred_rate = sum(prediction * .data[[weight_col]]) / sum(.data[[weight_col]]),
      .groups = "drop"
    )
  
  # Calculate predicted values from pipe_unregularized (unregularized model)
  pred2_values <- selected_data %>%
    mutate(
      prediction = pipe_unregularized[[preds_field_unreg]][, col_index_unreg]
    ) %>%
    group_by(feature_level = .data[[grouping_column_name]]) %>%
    summarise(
      pred_rate = sum(prediction * .data[[weight_col]]) / sum(.data[[weight_col]]),
      .groups = "drop"
    )
  
  # Combine all data for plotting
  plot_data <- actual_values %>%
    left_join(pred1_values, by = "feature_level") %>%
    rename(pred_rate_reg = pred_rate) %>%
    left_join(pred2_values, by = "feature_level") %>%
    rename(pred_rate_unreg = pred_rate)
  
  # Create train_or_test label for title (more readable)
  train_or_test_label <- ifelse(train_or_test == "train", "Training", "Test")
  
  # =====================================================================================================================================
  # PLOTLY VISUALIZATION: Interactive plot with four components
  # =====================================================================================================================================
  
  # Create plotly visualization with four components:
  # 1. Exposure as bar chart on secondary y-axis
  # 2. Actual values as dashed line
  # 3. Regularized model predictions as solid line
  # 4. Unregularized model predictions as solid line with triangle markers
  p <- plot_ly() %>%
    # Exposure as bar chart on secondary y-axis
    add_trace(
      data = plot_data,
      x = ~feature_level,
      y = ~exposure,
      type = "bar",
      name = "Exposure",
      marker = list(color = "rgba(219, 64, 82, 0.7)"),
      yaxis = "y2",
      hoverinfo = "text",
      text = ~paste("Level:", feature_level, 
                    "<br>Exposure:", format(exposure, big.mark = ",", decimal.mark = ".")),
      textposition = "none"     
    ) %>%
    # Actual values - NOW WITH DASHED LINE
    add_trace(
      data = plot_data,
      x = ~feature_level,
      y = ~actual_rate,
      type = "scatter",
      mode = "lines+markers",
      name = "Actual Rates",
      line = list(color = "rgb(31, 119, 180)", width = 3, dash = "dash"),  # Dashed line for actual values
      marker = list(color = "rgb(31, 119, 180)", size = 8),
      hoverinfo = "text",
      text = ~paste("Level:", feature_level, 
                    "<br>Actual Rate:", format(actual_rate, digits = 5, nsmall = 5))
    ) %>%
    # Regularized model predictions - SOLID LINE
    add_trace(
      data = plot_data,
      x = ~feature_level,
      y = ~pred_rate_reg,
      type = "scatter",
      mode = "lines+markers",
      name = paste("Regularized Model"),
      line = list(color = "rgb(44, 160, 44)", width = 3),  # Solid line for regularized model
      marker = list(color = "rgb(44, 160, 44)", size = 8),
      hoverinfo = "text",
      text = ~paste("Level:", feature_level, 
                    "<br>Regularized Pred:", format(pred_rate_reg, digits = 5, nsmall = 5))
    ) %>%
    # Unregularized model predictions - SOLID LINE
    add_trace(
      data = plot_data,
      x = ~feature_level,
      y = ~pred_rate_unreg,
      type = "scatter",
      mode = "lines+markers",
      name = paste("Unregularized Model"),
      line = list(color = "rgb(214, 39, 40)", width = 3),  # Solid line for unregularized model
      marker = list(color = "rgb(214, 39, 40)", size = 8, symbol = "triangle-up"),  # Triangle markers for differentiation
      hoverinfo = "text",
      text = ~paste("Level:", feature_level, 
                    "<br>Unregularized Pred:", format(pred_rate_unreg, digits = 5, nsmall = 5))
    ) %>%
    # Layout configuration for plot appearance and interactivity
    layout(
      title = paste("Prediction for", feature_name, "-", train_or_test_label, "Data (", target_lambda, ")"),
      xaxis = list(
        title = feature_name,
        tickangle = -90,  # Rotate x-axis labels for better readability
        tickfont = list(size = 12),
        categoryorder = "array",
        categoryarray = plot_data$feature_level
      ),
      yaxis = list(
        title = "Rate",
        titlefont = list(color = "rgb(31, 119, 180)"),
        tickfont = list(color = "rgb(31, 119, 180)"),
        side = "left",
        zeroline = TRUE
      ),
      yaxis2 = list(
        title = "Exposure",
        titlefont = list(color = "rgb(219, 64, 82)"),
        tickfont = list(color = "rgb(219, 64, 82)"),
        side = "right",
        overlaying = "y",
        zeroline = FALSE
      ),
      legend = list(
        orientation = "h",  # Horizontal legend
        x = 0.5,
        y = 1.1,
        xanchor = "center"
      ),
      margin = list(t = 80, r = 80, l = 80, b = 80),
      hoverlabel = list(
        bgcolor = "white",
        font = list(family = "Arial", size = 12)
      ),
      hovermode = "closest"
    )
  
  return(p)
}



# =====================================================================================================================================
# Plot All Features Prediction Comparison Between Regularized and Unregularized Models
# =====================================================================================================================================
#'
#' This function generates comparison plots for all features in the model,
#' showing how regularized and unregularized models predict across different levels
#' of each feature. It returns a list of plotly visualizations.
#'
#' @param pipe_regularized An object containing the regularized model information
#' @param pipe_unregularized An object containing the unregularized model information
#' @param data A data frame containing the features and target variable
#' @param target_col Character string with the name of the target/response column
#' @param weight_col Character string with the name of the weight/exposure column
#' @param lambda_index A numeric value specifying the index for the lambda parameter in the fitted model.
#' @param train_or_test Character string indicating whether to use training or test data ("train" or "test")
#'
#' @return A list of plotly objects, one for each feature in the model
#'
#' @examples
#' \dontrun{
#' # Create comparison plots for all features using test data
#' all_plots <- plot_all_feature_predictions_comparison(
#'   pipe_regularized = my_regularized_model,
#'   pipe_unregularized = my_unregularized_model,
#'   data = my_data,
#'   target_col = "loss_ratio",
#'   weight_col = "premium",
#'   train_or_test = "test"
#' )
#' 
#' # Display the plot for a specific feature
#' all_plots[["age_group"]]
#' }
plot_all_feature_predictions_comparison <- function(pipe_regularized, pipe_unregularized, data, target_col, weight_col = NULL, lambda_index, train_or_test = "train") {
  # Validate train_or_test parameter
  if(!train_or_test %in% c("train", "test")) {
    stop("train_or_test parameter must be either 'train' or 'test'")
  }
  
  # Create the target lambda parameter name (e.g. "s5" for lambda_index = 5)
  target_lambda <- paste0("s", lambda_index)
  
  # Set train_or_test-specific variables
  preds_field_reg <- paste0("preds_", train_or_test)
  preds_field_unreg <- paste0("preds_", train_or_test)
  
  # Check if target lambda exists in both models before processing any features
  available_lambdas_reg <- colnames(pipe_regularized[[preds_field_reg]])
  available_lambdas_unreg <- colnames(pipe_unregularized[[preds_field_unreg]])
  
  if (!target_lambda %in% available_lambdas_reg) {
    stop(paste("Lambda parameter", target_lambda, "not found in regularized model predictions.", 
               "Available lambdas:", paste(available_lambdas_reg, collapse = ", ")))
  }
  
  if (!target_lambda %in% available_lambdas_unreg) {
    stop(paste("Lambda parameter", target_lambda, "not found in unregularized model predictions.", 
               "Available lambdas:", paste(available_lambdas_unreg, collapse = ", ")))
  }
  
  # Get all feature names from the regularized model
  feature_names <- names(pipe_regularized$risk_factors)
  
  # Create a list to store all plots
  plots <- list()
  
  # Create a plot for each feature using plot_feature_predictions_comparison function
  for (feature in feature_names) {
    plots[[feature]] <- plot_feature_predictions_comparison(
      pipe_regularized, 
      pipe_unregularized, 
      data, 
      feature, 
      target_col, 
      weight_col,
      lambda_index,
      train_or_test = train_or_test
    )
  }
  
  return(plots)
}



# =====================================================================================================================================
# Plot risk factors with exposure by category with interactive slider (updated for interactions)
# =====================================================================================================================================
#' Plot risk factors with exposure by category with interactive slider
#'
#' This function creates an interactive plot showing the relationship between risk factors
#' and exposure values for a specified feature (including interactions). The plot includes:
#' - Bars representing exposure values per category (right y-axis)
#' - Lines representing risk factor values per category (left y-axis)
#' - An interactive slider to switch between different risk factors
#'
#' @param pipeline_output A list containing risk factor data from the risk analysis pipeline
#' @param exposure_df A data frame containing exposure data
#' @param feature_name Character. The name of the feature to plot (can include interactions like "X1:X2")
#' @param exposure_col Character. The name of the column in exposure_df containing exposure values
#'
#' @return A plotly object for interactive visualization
#'
#' @importFrom plotly plot_ly add_trace layout
#'
#' @examples
#' \dontrun{
#' # For regular feature
#' plot_all_risk_factors_for_feature_slider(
#'   pipeline_output = risk_analysis_output,
#'   exposure_df = claims_data,
#'   feature_name = "region",
#'   exposure_col = "premium"
#' )
#'
#' # For interaction
#' plot_all_risk_factors_for_feature_slider(
#'   pipeline_output = risk_analysis_output,
#'   exposure_df = claims_data,
#'   feature_name = "region:age_group",
#'   exposure_col = "premium"
#' )
#' }
plot_all_risk_factors_for_feature_slider <- function(pipeline_output, exposure_df, feature_name, exposure_col) {
  
  # Check if the specified feature exists in risk_factors
  if (!feature_name %in% names(pipeline_output$risk_factors)) {
    stop(paste("Error: Feature", feature_name, "not found in pipeline_output$risk_factors."))
  }
  
  # Retrieve risk factor matrix for the specified feature
  risk_factor_matrix <- pipeline_output$risk_factors[[feature_name]]$risk_factor_link_dummy_encoded
  
  # Check if the matrix is valid
  if (is.null(risk_factor_matrix) || !is.matrix(risk_factor_matrix)) {
    stop(paste("Error: No valid risk factor matrix found for feature", feature_name))
  }
  
  # Calculate global y-axis limits for risk factors
  global_min <- min(risk_factor_matrix, na.rm = TRUE)
  global_max <- max(risk_factor_matrix, na.rm = TRUE)
  y_range <- global_max - global_min
  y_padding <- 0.1 * y_range
  y_min <- global_min - y_padding
  y_max <- global_max + y_padding
  
  # Get categories and risk factor columns
  categories <- as.character(rownames(risk_factor_matrix))
  risk_factor_cols <- colnames(risk_factor_matrix)
  num_factors <- length(risk_factor_cols)
  
  # Calculate total exposure for each category using the helper function
  exposure_sum <- calculate_feature_exposure(feature_name, exposure_df, exposure_col, categories)
  
  # Create a dataframe for exposure by category
  exposure_by_category <- data.frame(
    Category = names(exposure_sum),
    Exposure = as.numeric(exposure_sum)
  )
  
  # Sort exposure_by_category according to the original categories
  exposure_by_category <- exposure_by_category[match(categories, exposure_by_category$Category),]
  exposure_by_category$Exposure[is.na(exposure_by_category$Exposure)] <- 0
  
  # Initial risk factor index and current factor
  initial_factor_index <- 1
  current_factor <- risk_factor_cols[initial_factor_index]
  
  # Create the initial plot with exposure (bar) and first risk factor (line)
  p <- plot_ly() %>%
    # Add bar trace for exposure data
    add_trace(
      x = categories,
      y = exposure_by_category$Exposure,
      type = "bar",
      name = "Exposure",
      marker = list(color = "rgba(219, 64, 82, 0.7)"),
      yaxis = "y2",
      hoverinfo = "text",
      text = paste("Category:", exposure_by_category$Category,
                   "<br>Exposure:", format(exposure_by_category$Exposure, big.mark = ".", decimal.mark = ",")),
      textposition = "none"
    ) %>%
    # Add line trace for the first risk factor
    add_trace(
      x = categories,
      y = risk_factor_matrix[, initial_factor_index],
      type = "scatter",
      mode = "lines+markers",
      name = "Risk Factor",
      line = list(color = "rgb(31, 119, 180)", width = 3),
      marker = list(color = "rgb(31, 119, 180)", size = 8),
      hoverinfo = "text",
      text = paste("Category:", categories,
                   "<br>Risk Factor:", format(risk_factor_matrix[, initial_factor_index], digits = 5, nsmall = 5))
    )
  
  # Create slider steps for each risk factor
  slider_steps <- list()
  for (i in 1:num_factors) {
    slider_steps[[i]] <- list(
      label = risk_factor_cols[i],
      method = "update",
      args = list(
        list(  # Data-Updates
          y = list(exposure_by_category$Exposure, risk_factor_matrix[, i]),
          text = list(NULL,  # Exposure-Text unchanged
                      paste("Category:", categories,
                            "<br>Risk Factor:", format(risk_factor_matrix[, i], digits = 5)))
        ),
        list(  # Layout-Updates
          title = paste("Risk Factors and Exposure for", feature_name, "-", risk_factor_cols[i]),
          xaxis = list(
            type = 'category',
            tickangle = -90,
            tickmode = 'array',
            tickvals = categories,
            categoryorder = "array",
            categoryarray = categories
          )
        )
      )
    )
  }
  
  # Add layout elements including slider
  p <- p %>% layout(
    title = paste("Risk Factors and Exposure for", feature_name, "-", current_factor),
    xaxis = list(
      title = feature_name,
      tickangle = -90,
      tickfont = list(size = 12),
      type = 'category',
      tickmode = 'array',
      tickvals = categories,
      categoryorder = "array",
      categoryarray = categories
    ),
    yaxis = list(
      title = "Risk Factor",
      titlefont = list(color = "rgb(31, 119, 180)"),
      tickfont = list(color = "rgb(31, 119, 180)"),
      side = "left",
      zeroline = TRUE,
      range = c(y_min, y_max)  # Fixed range with 10% padding
    ),
    yaxis2 = list(
      title = "Exposure",
      titlefont = list(color = "rgb(219, 64, 82)"),
      tickfont = list(color = "rgb(219, 64, 82)"),
      side = "right",
      overlaying = "y",
      zeroline = FALSE
    ),
    legend = list(
      orientation = "h",
      xanchor = "center",
      x = 0.5,
      y = 1.1
    ),
    margin = list(t = 100, r = 100, l = 80, b = 80),
    hovermode = "closest",
    sliders = list(
      list(
        active = 0,
        currentvalue = list(prefix = "Current: "),
        steps = slider_steps,
        x = 0.1,
        len = 0.8,
        xanchor = "left",
        y = -0.1,
        yanchor = "top"
      )
    )
  )
  
  # Return the plot
  return(p)
}



# =====================================================================================================================================
# Create interactive risk factor plots for multiple features
# =====================================================================================================================================
#' Create interactive risk factor plots for multiple features
#'
#' This function creates interactive plots showing risk factors and exposure
#' for each feature in the input vector. Each plot includes a slider to switch
#' between different risk factors for that feature.
#'
#' @param pipeline_output A list containing risk factor data from the risk analysis pipeline
#' @param exposure_df A data frame containing exposure data
#' @param features Character vector. Names of features to plot (must exist in pipeline_output$risk_factors)
#' @param exposure_col Character. The name of the column in exposure_df containing exposure values
#' @param output_dir Optional. Directory to save HTML plots. If NULL, plots are returned in a list.
#' @param file_prefix Optional prefix for saved HTML files
#'
#' @return If output_dir is NULL, returns a list of plotly objects. Otherwise saves HTML files and returns NULL.
#'
#' @importFrom htmlwidgets saveWidget
#' @importFrom purrr map
#'
#' @examples
#' \dontrun{
#' # Return plots in a list
#' all_plots <- plot_all_features_slider(
#'   pipeline_output = risk_analysis_output,
#'   exposure_df = claims_data,
#'   features = c("region", "age_group", "vehicle_type"),
#'   exposure_col = "premium"
#' )
#'
#' # Save plots to directory
#' plot_all_features_slider(
#'   pipeline_output = risk_analysis_output,
#'   exposure_df = claims_data,
#'   features = c("region", "age_group", "vehicle_type"),
#'   exposure_col = "premium",
#'   output_dir = "risk_plots"
#' )
#' }
plot_all_features_slider <- function(pipeline_output, exposure_df, features, exposure_col, 
                                     output_dir = NULL, file_prefix = "risk_plot_") {
  
  # Validate input features
  missing_features <- setdiff(features, names(pipeline_output$risk_factors))
  if (length(missing_features) > 0) {
    warning(paste("The following features were not found and will be skipped:",
                  paste(missing_features, collapse = ", ")))
    features <- intersect(features, names(pipeline_output$risk_factors))
  }
  
  if (length(features) == 0) {
    stop("No valid features found in pipeline_output$risk_factors")
  }
  
  # Create plots for each feature
  plots <- map(features, function(feature) {
    tryCatch({
      plot_all_risk_factors_for_feature_slider(
        pipeline_output = pipeline_output,
        exposure_df = exposure_df,
        feature_name = feature,
        exposure_col = exposure_col
      )
    }, error = function(e) {
      warning(paste("Failed to create plot for feature", feature, ":", e$message))
      NULL
    })
  }) %>% setNames(features)
  
  # Remove NULL elements (failed plots)
  plots <- compact(plots)
  
  # Handle output
  if (!is.null(output_dir)) {
    # Create directory if it doesn't exist
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Save each plot as HTML
    walk(names(plots), function(feature) {
      filename <- paste0(file_prefix, feature, ".html")
      filepath <- file.path(output_dir, filename)
      htmlwidgets::saveWidget(plots[[feature]], filepath, selfcontained = TRUE)
    })
    
    message(paste(length(plots), "plots saved to", output_dir))
    invisible(NULL)
  } else {
    return(plots)
  }
}



# =====================================================================================================================================
# Save Multiple Plots as Interactive Dashboard
# =====================================================================================================================================
#' Save Multiple Plots as Interactive Dashboard
#' 
#' This function saves a list of interactive plots (e.g., plotly or ggplotly objects)
#' as an HTML dashboard with a responsive grid layout. Each plot is saved as a 
#' separate HTML widget and embedded in the dashboard using iframes.
#'
#' @param all_plots A named list of interactive plot objects (e.g., plotly or ggplotly).
#' @param output_file Path and filename for the output HTML dashboard. 
#'   Default is "risk_plots.html" in the working directory.
#' @param title Title to display at the top of the dashboard. 
#'   Default is "Risk Analysis Dashboard".
#' @param plots_per_row Number of plots to display per row in the grid layout. 
#'   Default is 2. The layout will automatically adjust for mobile devices.
#'
#' @return Invisibly returns the path to the generated HTML file. The function
#'   primarily creates an HTML dashboard file and a folder containing the 
#'   individual plot widgets.
#'
#' @details The function creates:
#' \itemize{
#'   \item A main HTML file containing the dashboard layout
#'   \item A "dashboard_files" subfolder containing individual plot HTML widgets
#' }
#' The dashboard features responsive design that adapts to different screen sizes.
#'
#' @examples
#' \dontrun{
#' # Create some sample plots
#' library(plotly)
#' p1 <- plot_ly(mtcars, x = ~mpg, y = ~wt)
#' p2 <- plot_ly(mtcars, x = ~hp, y = ~qsec)
#' 
#' # Save as dashboard
#' save_all_plots_grid(
#'   all_plots = list("MPG vs WT" = p1, "HP vs QSEC" = p2),
#'   output_file = "car_dashboard.html",
#'   title = "Car Metrics Dashboard"
#' )
#' }
#' 
#' @export
#' @importFrom htmlwidgets saveWidget
save_all_plots_grid <- function(all_plots, 
                                output_file = "risk_plots.html",
                                title = "Risk Analysis Dashboard",
                                plots_per_row = 2) {
  
  # Create a folder for the dashboard in same location as output file
  dashboard_dir <- file.path(dirname(output_file), "dashboard_files")
  dir.create(dashboard_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Save all plots as individual HTML files
  plot_files <- character(length(all_plots))
  for (i in seq_along(all_plots)) {
    plot_name <- names(all_plots)[i]
    filename <- file.path(dashboard_dir, paste0("plot_", i, ".html"))
    tryCatch({
      htmlwidgets::saveWidget(
        widget = all_plots[[i]], 
        file = filename, 
        selfcontained = TRUE
      )
      plot_files[i] <- filename
    }, error = function(e) {
      warning("Failed to save plot ", plot_name, ": ", e$message)
      plot_files[i] <- ""
    })
  }
  
  # Create HTML content with iframes
  html_content <- paste0(
    '<!DOCTYPE html>
    <html>
    <head>
      <meta charset="utf-8">
      <title>', title, '</title>
      <style>
        body {
          font-family: Arial, sans-serif;
          margin: 0;
          padding: 20px;
          background-color: #f5f5f5;
        }
        .dashboard {
          max-width: 1200px;
          margin: 0 auto;
        }
        .header {
          text-align: center;
          margin-bottom: 20px;
          padding: 10px;
          background-color: white;
          border-radius: 8px;
          box-shadow: 0 2px 5px rgba(0,0,0,0.1);
        }
        .grid {
          display: grid;
          grid-template-columns: repeat(', plots_per_row, ', 1fr);
          gap: 20px;
        }
        .plot-container {
          background: white;
          border-radius: 8px;
          box-shadow: 0 2px 5px rgba(0,0,0,0.1);
          overflow: hidden;
        }
        .plot-title {
          padding: 10px;
          text-align: center;
          font-weight: bold;
          border-bottom: 1px solid #eee;
        }
        iframe {
          width: 100%;
          height: 500px;
          border: none;
        }
        @media (max-width: 768px) {
          .grid {
            grid-template-columns: 1fr;
          }
        }
      </style>
    </head>
    <body>
      <div class="dashboard">
        <div class="header">
          <h1>', title, '</h1>
          <p>', length(all_plots), ' Plots | Generated on ', format(Sys.Date(), "%B %d, %Y"), '</p>
        </div>
        <div class="grid">'
  )
  
  # Add each plot as an iframe
  for (i in seq_along(all_plots)) {
    if (plot_files[i] != "") {
      plot_name <- names(all_plots)[i]
      rel_path <- paste0("dashboard_files/plot_", i, ".html")
      
      plot_content <- paste0(
        '<div class="plot-container">',
        '<div class="plot-title">', plot_name, '</div>',
        '<iframe src="', rel_path, '"></iframe>',
        '</div>'
      )
      
      html_content <- paste0(html_content, plot_content)
    }
  }
  
  # Close HTML
  html_content <- paste0(html_content, 
                         '    </div>
      </div>
    </body>
    </html>'
  )
  
  # Write HTML to file
  writeLines(html_content, output_file)
  
  message("Dashboard successfully saved under: ", normalizePath(output_file))
  invisible(output_file)
}



# =====================================================================================================================================
# Create risk factor dashboard with global slider for all plots
# =====================================================================================================================================
#' Create risk factor dashboard with global slider for all plots
#'
#' This function creates an interactive HTML dashboard where a single global slider
#' controls all risk factor plots simultaneously. All plots show the same risk factor
#' column at the same time.
#'
#' @param pipeline_output A list containing risk factor data from the risk analysis pipeline
#' @param exposure_df A data frame containing exposure data
#' @param features Character vector. Names of features to plot
#' @param exposure_col Character. The name of the column in exposure_df containing exposure values
#' @param output_file Path and filename for the output HTML dashboard
#' @param title Title to display at the top of the dashboard
#' @param plots_per_row Number of plots to display per row
#'
#' @return Path to the generated HTML file
#'
#' @examples
#' \dontrun{
#' create_risk_factor_dashboard(
#'   pipeline_output = risk_analysis_output,
#'   exposure_df = claims_data,
#'   features = c("region", "age_group", "vehicle_type"),
#'   exposure_col = "premium",
#'   output_file = "dashboard.html"
#' )
#' }
create_risk_factor_dashboard <- function(pipeline_output, exposure_df, features, exposure_col,
                                         output_file = "risk_factor_dashboard.html",
                                         title = "Risk Analysis Dashboard",
                                         plots_per_row = 2) {
  
  # Validate input features
  missing_features <- setdiff(features, names(pipeline_output$risk_factors))
  if (length(missing_features) > 0) {
    warning(paste("The following features were not found and will be skipped:",
                  paste(missing_features, collapse = ", ")))
    features <- intersect(features, names(pipeline_output$risk_factors))
  }
  
  if (length(features) == 0) {
    stop("No valid features found in pipeline_output$risk_factors")
  }
  
  # Get all risk factor column names (assuming they're the same across features)
  first_feature <- features[1]
  risk_factor_matrix <- pipeline_output$risk_factors[[first_feature]]$risk_factor_link_dummy_encoded
  risk_factor_cols <- colnames(risk_factor_matrix)
  
  # Prepare data for all features
  plot_data_list <- list()
  
  for (feature in features) {
    # Get risk factor matrix
    risk_factor_matrix <- pipeline_output$risk_factors[[feature]]$risk_factor_link_dummy_encoded
    
    if (is.null(risk_factor_matrix) || !is.matrix(risk_factor_matrix)) {
      warning(paste("Skipping feature", feature, "- no valid risk factor matrix"))
      next
    }
    
    # Get categories
    categories <- as.character(rownames(risk_factor_matrix))
    
    # Calculate exposure using helper function
    exposure_sum <- calculate_feature_exposure(feature, exposure_df, exposure_col, categories)
    
    # Create exposure dataframe
    exposure_by_category <- data.frame(
      Category = names(exposure_sum),
      Exposure = as.numeric(exposure_sum)
    )
    
    # Sort and fill missing categories
    exposure_by_category <- exposure_by_category[match(categories, exposure_by_category$Category),]
    exposure_by_category$Exposure[is.na(exposure_by_category$Exposure)] <- 0
    
    # Calculate y-axis range for this feature
    global_min <- min(risk_factor_matrix, na.rm = TRUE)
    global_max <- max(risk_factor_matrix, na.rm = TRUE)
    y_range <- global_max - global_min
    y_padding <- 0.1 * y_range
    y_min <- global_min - y_padding
    y_max <- global_max + y_padding
    
    # Store data for this feature
    plot_data_list[[feature]] <- list(
      categories = categories,
      risk_factor_matrix = risk_factor_matrix,
      exposure_data = exposure_by_category,
      y_range = c(y_min, y_max)
    )
  }
  
  # Create the HTML content
  html_content <- paste0(
    '<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>', title, '</title>
  <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
  <style>
    body {
      font-family: Arial, sans-serif;
      margin: 0;
      padding: 20px;
      background-color: #f5f5f5;
    }
    .dashboard {
      max-width: 1400px;
      margin: 0 auto;
      padding-right: 140px; /* Angepasst für schmaleres Kästchen */
    }
    .header {
      text-align: center;
      margin-bottom: 20px;
      padding: 10px;
      background-color: white;
      border-radius: 8px;
      box-shadow: 0 2px 5px rgba(0,0,0,0.1);
    }
    .slider-container {
      position: fixed;
      top: 50%;
      right: 20px;
      transform: translateY(-50%);
      background: white;
      padding: 15px 10px;
      border-radius: 8px;
      box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      z-index: 1000;
      width: 100px;
      height: 400px; /* Feste Höhe für das Kästchen */
      text-align: center;
      border: 2px solid #e0e0e0;
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: space-between;
    }
    .slider {
      width: 300px; /* Länge des vertikalen Sliders */
      height: 6px;
      -webkit-appearance: none;
      appearance: none;
      border-radius: 3px;
      background: #ddd;
      outline: none;
      transform: rotate(-90deg);
      transform-origin: center;
      margin: 0; /* Kein zusätzlicher Margin */
    }
    .slider::-webkit-slider-thumb {
      -webkit-appearance: none;
      appearance: none;
      width: 20px;
      height: 20px;
      border-radius: 50%;
      background: #4CAF50;
      cursor: pointer;
      box-shadow: 0 2px 4px rgba(0,0,0,0.3);
    }
    .slider::-moz-range-thumb {
      width: 20px;
      height: 20px;
      border-radius: 50%;
      background: #4CAF50;
      cursor: pointer;
      border: none;
      box-shadow: 0 2px 4px rgba(0,0,0,0.3);
    }
    .slider-label {
      font-weight: bold;
      font-size: 11px;
      color: #333;
      text-align: center;
      line-height: 1.1;
      margin: 0;
    }
    .slider-values {
      display: none;
      flex-direction: column;
      justify-content: space-between;
      font-size: 8px;
      color: #666;
      height: 60px;
      text-align: center;
      width: 80px;
    }
    .slider-values span {
      word-wrap: break-word;
      line-height: 1.1;
      overflow: hidden;
      text-overflow: ellipsis;
    }
    .current-factor {
      font-size: 9px; 
      color: #4CAF50; 
      font-weight: bold; 
      word-wrap: break-word; 
      max-width: 80px;
      line-height: 1.1;
      text-align: center;
      margin: 0;
    }
    
    /* Responsive: Slider horizontal unten bei schmalen Bildschirmen */
    @media (max-width: 1024px) {
      .slider-container {
        position: fixed;
        bottom: 20px;
        top: auto;
        right: 20px;
        left: 20px;
        transform: none;
        width: auto;
        height: auto;
        max-width: 400px;
        margin: 0 auto;
        flex-direction: row;
        justify-content: center;
        align-items: center;
        padding: 15px;
      }
      .slider {
        width: 60%;
        height: 6px;
        transform: none; /* Zurück zu horizontal auf kleinen Bildschirmen */
        margin: 0 15px;
      }
      .slider-label {
        margin-right: 15px;
        font-size: 14px;
      }
      .slider-values {
        display: none; 
        flex-direction: row;
        height: auto;
        margin-left: 15px;
        width: auto;
      }
      .slider-values span {
        margin: 0 5px;
      }
      .current-factor {
        font-size: 12px;
        max-width: none;
      }
    }
    .grid {
      display: grid;
      grid-template-columns: repeat(', plots_per_row, ', 1fr);
      gap: 20px;
    }
    .plot-container {
      background: white;
      border-radius: 8px;
      box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      overflow: hidden;
      height: 550px;
    }
    .plot-title {
      padding: 10px;
      text-align: center;
      font-weight: bold;
      border-bottom: 1px solid #eee;
    }
    .plot-div {
      height: 500px;
      width: 100%;
    }
    @media (max-width: 768px) {
      .grid {
        grid-template-columns: 1fr;
      }
      .dashboard {
        padding-right: 20px; /* Weniger Padding auf mobilen Geräten */
      }
    }
    @media (max-width: 1024px) {
      .dashboard {
        padding-right: 20px; /* Anpassung für Tablet-Größen */
      }
    }
  </style>
</head>
<body>
  <div class="dashboard">
    <div class="header">
      <h1>', title, '</h1>
      <p>', length(features), ' Features | Generated on ', format(Sys.Date(), "%B %d, %Y"), '</p>
    </div>
    
    <div class="slider-container">
      <div class="slider-label">Risk<br>Factor</div>
      <div class="slider-values">
        <span>', risk_factor_cols[length(risk_factor_cols)], '</span>
        <span>', risk_factor_cols[1], '</span>
      </div>
      <input type="range" id="global-slider" class="slider" 
             min="0" max="', length(risk_factor_cols) - 1, '" value="0" step="1">
      <div id="current-factor" class="current-factor">', risk_factor_cols[1], '</div>
    </div>
    
    <div class="grid">')
  
  # Add plot containers
  for (i in seq_along(features)) {
    feature <- features[i]
    plot_content <- paste0(
      '<div class="plot-container">',
      '<div class="plot-title">', feature, '</div>',
      '<div id="plot-', i, '" class="plot-div"></div>',
      '</div>'
    )
    html_content <- paste0(html_content, plot_content)
  }
  
  # Add JavaScript for plots and slider functionality
  html_content <- paste0(html_content, '
    </div>
  </div>

  <script>
    // Plot data
    const plotData = ', jsonlite::toJSON(plot_data_list, auto_unbox = TRUE), ';
    const riskFactorCols = ', jsonlite::toJSON(risk_factor_cols), ';
    const features = ', jsonlite::toJSON(features), ';
    
    // Initialize all plots
    function initializePlots() {
      features.forEach((feature, index) => {
        const data = plotData[feature];
        const plotId = "plot-" + (index + 1);
        
        // Create initial traces
        const exposureTrace = {
          x: data.categories,
          y: data.exposure_data.map(d => d.Exposure),
          type: "bar",
          name: "Exposure",
          marker: {color: "rgba(219, 64, 82, 0.7)"},
          yaxis: "y2",
          hovertemplate: "Category: %{x}<br>Exposure: %{y:,.0f}<extra></extra>"
        };
        
        const riskTrace = {
          x: data.categories,
          y: data.risk_factor_matrix.map(row => row[0]),
          type: "scatter",
          mode: "lines+markers",
          name: "Risk Factor",
          line: {color: "rgb(31, 119, 180)", width: 3},
          marker: {color: "rgb(31, 119, 180)", size: 8},
          hovertemplate: "Category: %{x}<br>Risk Factor: %{y:.5f}<extra></extra>"
        };
        
        const layout = {
          title: feature + " - " + riskFactorCols[0],
          xaxis: {
            title: feature,
            tickangle: -90,
            tickfont: {size: 12},
            type: "category",
            categoryorder: "array",
            categoryarray: data.categories
          },
          yaxis: {
            title: "Risk Factor",
            titlefont: {color: "rgb(31, 119, 180)"},
            tickfont: {color: "rgb(31, 119, 180)"},
            side: "left",
            zeroline: true,
            range: data.y_range
          },
          yaxis2: {
            title: "Exposure",
            titlefont: {color: "rgb(219, 64, 82)"},
            tickfont: {color: "rgb(219, 64, 82)"},
            side: "right",
            overlaying: "y",
            zeroline: false
          },
          legend: {
            orientation: "h",
            xanchor: "center",
            x: 0.5,
            y: 1.1
          },
          margin: {t: 80, r: 80, l: 80, b: 80},
          hovermode: "closest"
        };
        
        Plotly.newPlot(plotId, [exposureTrace, riskTrace], layout, {responsive: true});
      });
    }
    
    // Update all plots when slider changes
    function updateAllPlots(sliderValue) {
      const currentFactor = riskFactorCols[sliderValue];
      document.getElementById("current-factor").textContent = currentFactor;
      
      features.forEach((feature, index) => {
        const data = plotData[feature];
        const plotId = "plot-" + (index + 1);
        
        // Update risk factor data
        const newRiskData = data.risk_factor_matrix.map(row => row[sliderValue]);
        const newHoverText = data.categories.map((cat, i) => 
          `Category: ${cat}<br>Risk Factor: ${newRiskData[i].toFixed(5)}`);
        
        // Update plot
        Plotly.restyle(plotId, {
          y: [undefined, newRiskData],
          hovertemplate: [undefined, newHoverText.map(text => text + "<extra></extra>")]
        }, [0, 1]);
        
        // Update layout
        Plotly.relayout(plotId, {
          title: feature + " - " + currentFactor,
          xaxis: {
            title: feature,
            tickangle: -90,
            tickfont: {size: 12},
            type: "category",
            categoryorder: "array",
            categoryarray: data.categories
          }
        });
      });
    }
    
    // Event listener for slider
    document.getElementById("global-slider").addEventListener("input", function(e) {
      updateAllPlots(parseInt(e.target.value));
    });
    
    // Initialize plots when page loads
    document.addEventListener("DOMContentLoaded", function() {
      initializePlots();
    });
  </script>
</body>
</html>')
  
  # Write HTML to file
  writeLines(html_content, output_file)
  
  message("Dashboard successfully saved under: ", normalizePath(output_file))
  invisible(output_file)
}



# =====================================================================================================================================
# Plot Actual vs Predicted Values for Feature with Interactive Slider
# =====================================================================================================================================
#' Plot actual vs predicted values by category with interactive slider
#'
#' This function creates an interactive plot showing the relationship between actual and
#' predicted values for a specified feature across different lambda values. The plot includes:
#' - Bars representing exposure values per category (right y-axis)
#' - Dashed line representing actual values per category (left y-axis)
#' - Solid line representing predicted values per category (left y-axis)
#' - An interactive slider to switch between different lambda values
#'
#' @param pipeline_output A list containing model data from the risk analysis pipeline
#' @param data A data frame containing the features and target variable
#' @param feature_name Character. The name of the feature to plot (can include interactions like "X1:X2")
#' @param target_col Character string with the name of the target/response column
#' @param weight_col Character string with the name of the weight/exposure column
#' @param train_or_test Character string indicating whether to use training or test data ("train" or "test")
#'
#' @return A plotly object for interactive visualization
#'
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom dplyr group_by summarise mutate left_join rename
#'
#' @examples
#' \dontrun{
#' # For regular feature
#' plot_actual_vs_predicted_slider(
#'   pipeline_output = model_n,
#'   data = my_data,
#'   feature_name = "X1",
#'   target_col = "y",
#'   weight_col = "exposure",
#'   train_or_test = "train"
#' )
#'
#' # For interaction
#' plot_actual_vs_predicted_slider(
#'   pipeline_output = model_n,
#'   data = my_data,
#'   feature_name = "X1:X2",
#'   target_col = "y",
#'   weight_col = "exposure",
#'   train_or_test = "test"
#' )
#' }
plot_actual_vs_predicted_slider <- function(pipeline_output, data, feature_name, 
                                            target_col, weight_col, 
                                            train_or_test = "train") {
  
  # Validate train_or_test parameter
  if(!train_or_test %in% c("train", "test")) {
    stop("train_or_test parameter must be either 'train' or 'test'")
  }
  
  # Set train_or_test-specific variables
  index_field <- paste0(train_or_test, "_index")
  preds_field <- paste0("preds_", train_or_test)
  
  # Get predictions matrix
  predictions_matrix <- pipeline_output[[preds_field]]
  
  if (is.null(predictions_matrix) || !is.matrix(predictions_matrix)) {
    stop(paste("Error: No valid predictions matrix found for", train_or_test, "data"))
  }
  
  # Get lambda column names
  lambda_cols <- colnames(predictions_matrix)
  num_lambdas <- length(lambda_cols)
  
  # Select appropriate data based on train_or_test parameter
  selected_data <- data[pipeline_output$split_index[[index_field]], ]
  
  # =====================================================================================================================================
  # FEATURE HANDLING: Support for both single features and interactions
  # =====================================================================================================================================
  
  # Check if it's an interaction feature
  if (grepl(":", feature_name)) {
    # Split the interaction feature name
    vars <- unlist(strsplit(feature_name, ":"))
    
    # Create the new column name for the interaction
    interaction_col_name <- paste(vars, collapse = "_")
    
    # Create the interaction column if it doesn't exist
    if (!(interaction_col_name %in% names(selected_data))) {
      selected_data <- selected_data %>%
        mutate(
          !!interaction_col_name := paste(!!!syms(vars), sep = ":")
        )
    }
    
    grouping_column_name <- interaction_col_name
  } else {
    grouping_column_name <- feature_name
  }
  
  # Get unique categories and ensure proper ordering
  categories <- sort(unique(as.character(selected_data[[grouping_column_name]])))
  
  # =====================================================================================================================================
  # CALCULATE VALUES FOR ALL LAMBDAS
  # =====================================================================================================================================
  
  # Calculate actual values per feature level (only once, independent of lambda)
  actual_values <- selected_data %>%
    group_by(feature_level = .data[[grouping_column_name]]) %>%
    summarise(
      actual_rate = sum(.data[[target_col]]) / sum(.data[[weight_col]]),
      exposure = sum(.data[[weight_col]]),
      .groups = "drop"
    )
  
  # Ensure all categories are present
  actual_values <- data.frame(
    feature_level = categories,
    stringsAsFactors = FALSE
  ) %>%
    left_join(actual_values, by = "feature_level") %>%
    mutate(
      actual_rate = ifelse(is.na(actual_rate), 0, actual_rate),
      exposure = ifelse(is.na(exposure), 0, exposure)
    )
  
  # Calculate predicted values for each lambda
  pred_values_list <- list()
  y_min_global <- min(actual_values$actual_rate, na.rm = TRUE)
  y_max_global <- max(actual_values$actual_rate, na.rm = TRUE)
  
  for (i in 1:num_lambdas) {
    pred_values <- selected_data %>%
      mutate(
        prediction = predictions_matrix[, i]
      ) %>%
      group_by(feature_level = .data[[grouping_column_name]]) %>%
      summarise(
        pred_rate = sum(prediction * .data[[weight_col]]) / sum(.data[[weight_col]]),
        .groups = "drop"
      )
    
    # Ensure all categories are present
    pred_values <- data.frame(
      feature_level = categories,
      stringsAsFactors = FALSE
    ) %>%
      left_join(pred_values, by = "feature_level") %>%
      mutate(pred_rate = ifelse(is.na(pred_rate), 0, pred_rate))
    
    pred_values_list[[i]] <- pred_values
    
    # Update global y-axis limits
    y_min_global <- min(y_min_global, min(pred_values$pred_rate, na.rm = TRUE))
    y_max_global <- max(y_max_global, max(pred_values$pred_rate, na.rm = TRUE))
  }
  
  # Add padding to y-axis range
  y_range <- y_max_global - y_min_global
  y_padding <- 0.1 * y_range
  y_min <- y_min_global - y_padding
  y_max <- y_max_global + y_padding
  
  # =====================================================================================================================================
  # CREATE INITIAL PLOT
  # =====================================================================================================================================
  
  initial_lambda_index <- 1
  current_lambda <- lambda_cols[initial_lambda_index]
  
  # Create label for train/test
  train_or_test_label <- ifelse(train_or_test == "train", "Training", "Test")
  
  # Create the initial plot
  p <- plot_ly() %>%
    # Add bar trace for exposure data
    add_trace(
      x = actual_values$feature_level,
      y = actual_values$exposure,
      type = "bar",
      name = "Exposure",
      marker = list(color = "rgba(219, 64, 82, 0.7)"),
      yaxis = "y2",
      hoverinfo = "text",
      text = paste("Category:", actual_values$feature_level,
                   "<br>Exposure:", format(actual_values$exposure, big.mark = ".", decimal.mark = ",")),
      textposition = "none"
    ) %>%
    # Add dashed line trace for actual values
    add_trace(
      x = actual_values$feature_level,
      y = actual_values$actual_rate,
      type = "scatter",
      mode = "lines+markers",
      name = "Actual Values",
      line = list(color = "rgb(31, 119, 180)", width = 3, dash = "dash"),
      marker = list(color = "rgb(31, 119, 180)", size = 8),
      hoverinfo = "text",
      text = paste("Category:", actual_values$feature_level,
                   "<br>Actual:", format(actual_values$actual_rate, digits = 5, nsmall = 5))
    ) %>%
    # Add solid line trace for predicted values (initial lambda)
    add_trace(
      x = pred_values_list[[initial_lambda_index]]$feature_level,
      y = pred_values_list[[initial_lambda_index]]$pred_rate,
      type = "scatter",
      mode = "lines+markers",
      name = "Predicted Values",
      line = list(color = "rgb(44, 160, 44)", width = 3),
      marker = list(color = "rgb(44, 160, 44)", size = 8),
      hoverinfo = "text",
      text = paste("Category:", pred_values_list[[initial_lambda_index]]$feature_level,
                   "<br>Predicted:", format(pred_values_list[[initial_lambda_index]]$pred_rate, digits = 5, nsmall = 5))
    )
  
  # =====================================================================================================================================
  # CREATE SLIDER STEPS
  # =====================================================================================================================================
  
  slider_steps <- list()
  for (i in 1:num_lambdas) {
    slider_steps[[i]] <- list(
      label = lambda_cols[i],
      method = "update",
      args = list(
        list(  # Data updates
          y = list(
            actual_values$exposure,  # Exposure (unchanged)
            actual_values$actual_rate,  # Actual values (unchanged)
            pred_values_list[[i]]$pred_rate  # Predicted values (changes with lambda)
          ),
          text = list(
            paste("Category:", actual_values$feature_level,
                  "<br>Exposure:", format(actual_values$exposure, big.mark = ".", decimal.mark = ",")),
            paste("Category:", actual_values$feature_level,
                  "<br>Actual:", format(actual_values$actual_rate, digits = 5, nsmall = 5)),
            paste("Category:", pred_values_list[[i]]$feature_level,
                  "<br>Predicted:", format(pred_values_list[[i]]$pred_rate, digits = 5, nsmall = 5))
          )
        ),
        list(  # Layout updates
          title = paste("Actual vs Predicted for", feature_name, "-", train_or_test_label, 
                        "Data (", lambda_cols[i], ")"),
          xaxis = list(
            type = 'category',
            tickangle = -90,
            tickmode = 'array',
            tickvals = categories,
            categoryorder = "array",
            categoryarray = categories
          )
        )
      )
    )
  }
  
  # =====================================================================================================================================
  # ADD LAYOUT WITH SLIDER
  # =====================================================================================================================================
  
  p <- p %>% layout(
    title = paste("Actual vs Predicted for", feature_name, "-", train_or_test_label, 
                  "Data (", current_lambda, ")"),
    xaxis = list(
      title = feature_name,
      tickangle = -90,
      tickfont = list(size = 12),
      type = 'category',
      tickmode = 'array',
      tickvals = categories,
      categoryorder = "array",
      categoryarray = categories
    ),
    yaxis = list(
      title = "Rate",
      titlefont = list(color = "rgb(31, 119, 180)"),
      tickfont = list(color = "rgb(31, 119, 180)"),
      side = "left",
      zeroline = TRUE,
      range = c(y_min, y_max)
    ),
    yaxis2 = list(
      title = "Exposure",
      titlefont = list(color = "rgb(219, 64, 82)"),
      tickfont = list(color = "rgb(219, 64, 82)"),
      side = "right",
      overlaying = "y",
      zeroline = FALSE
    ),
    legend = list(
      orientation = "h",
      xanchor = "center",
      x = 0.5,
      y = 1.1
    ),
    margin = list(t = 100, r = 100, l = 80, b = 80),
    hovermode = "closest",
    sliders = list(
      list(
        active = 0,
        currentvalue = list(prefix = "Lambda: "),
        steps = slider_steps,
        x = 0.1,
        len = 0.8,
        xanchor = "left",
        y = -0.1,
        yanchor = "top"
      )
    )
  )
  
  return(p)
}

# =====================================================================================================================================
# Plot All Features - Actual vs Predicted with Slider
# =====================================================================================================================================
#' Plot actual vs predicted values for all features with interactive sliders
#'
#' This function creates interactive plots for multiple features at once, showing the
#' relationship between actual and predicted values across different lambda values.
#' Each feature gets its own plot with:
#' - Bars representing exposure values per category (right y-axis)
#' - Dashed line representing actual values per category (left y-axis)
#' - Solid line representing predicted values per category (left y-axis)
#' - An interactive slider to switch between different lambda values
#'
#' @param pipeline_output A list containing model data from the risk analysis pipeline
#' @param data A data frame containing the features and target variable
#' @param features Character vector. Names of features to plot (can include interactions like "X1:X2")
#' @param target_col Character string with the name of the target/response column
#' @param weight_col Character string with the name of the weight/exposure column
#' @param train_or_test Character string indicating whether to use training or test data ("train" or "test")
#' @return A named list of plotly objects, one for each feature
#'
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom dplyr group_by summarise mutate left_join rename
#' @importFrom purrr map compact setNames
#'
#' @examples
#' \dontrun{
#' # Plot all features
#' plots <- plot_all_features_actual_vs_predicted(
#'   pipeline_output = model_n,
#'   data = my_data,
#'   features = c("X1", "X2", "X1:X2"),
#'   target_col = "y",
#'   weight_col = "exposure",
#'   train_or_test = "train"
#' )
#'
#' # Access individual plots
#' plots$X1
#' plots# =====================================================================================================================================
# Plot All Features - Actual vs Predicted with Slider
# =====================================================================================================================================
#' Plot actual vs predicted values for all features with interactive sliders
#'
#' This function creates interactive plots for multiple features at once, showing the
#' relationship between actual and predicted values across different lambda values.
#' Each feature gets its own plot with:
#' - Bars representing exposure values per category (right y-axis)
#' - Dashed line representing actual values per category (left y-axis)
#' - Solid line representing predicted values per category (left y-axis)
#' - An interactive slider to switch between different lambda values
#'
#' @param pipeline_output A list containing model data from the risk analysis pipeline
#' @param data A data frame containing the features and target variable
#' @param features Character vector. Names of features to plot (can include interactions like "X1:X2")
#' @param target_col Character string with the name of the target/response column
#' @param weight_col Character string with the name of the weight/exposure column
#' @param train_or_test Character string indicating whether to use training or test data ("train" or "test")
plot_all_features_actual_vs_predicted <- function(pipeline_output, data, features, 
                                                  target_col, weight_col, 
                                                  train_or_test = "train") {
  
  # Validate that features exist in the data or can be constructed
  available_features <- names(pipeline_output$risk_factors)
  
  # Check which features are valid
  valid_features <- character()
  invalid_features <- character()
  
  for (feature in features) {
    # Check if feature exists in risk_factors or is an interaction that can be constructed
    if (feature %in% available_features) {
      valid_features <- c(valid_features, feature)
    } else if (grepl(":", feature)) {
      # For interactions, check if base features exist in data
      vars <- unlist(strsplit(feature, ":"))
      if (all(vars %in% names(data))) {
        valid_features <- c(valid_features, feature)
      } else {
        invalid_features <- c(invalid_features, feature)
      }
    } else {
      invalid_features <- c(invalid_features, feature)
    }
  }
  
  # Warn about invalid features
  if (length(invalid_features) > 0) {
    warning(paste("The following features were not found and will be skipped:",
                  paste(invalid_features, collapse = ", ")))
  }
  
  if (length(valid_features) == 0) {
    stop("No valid features found to plot")
  }
  
  # Create plots for each feature
  plots <- map(valid_features, function(feature) {
    tryCatch({
      message(paste("Creating plot for feature:", feature))
      
      # Create the plot
      plot_actual_vs_predicted_slider(
        pipeline_output = pipeline_output,
        data = data,
        feature_name = feature,
        target_col = target_col,
        weight_col = weight_col,
        train_or_test = train_or_test
      )
      
    }, error = function(e) {
      warning(paste("Failed to create plot for feature", feature, ":", e$message))
      return(NULL)
    })
  }) %>% setNames(valid_features)
  
  # Remove NULL elements (failed plots)
  plots <- compact(plots)
  
  # Summary message
  message(paste("\nSuccessfully created", length(plots), "plots out of", length(valid_features), "requested features"))
  
  return(plots)
}