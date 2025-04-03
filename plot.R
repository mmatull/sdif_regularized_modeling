# =====================================================================================================================================
# File: plot.R
# =====================================================================================================================================
# Description:
# This script contains functions to:
# - summarize_risk_factor_levels_per_s
# - plot_risk_factors_all
# - plot_deviance_train_test
# - plot_risk_factors_all
# - plot_risk_factors_compare_model
# - plot_feature_predictions_comparison
# - plot_all_feature_predictions_comparison
#
# Summarize Risk Factors Overview
# Plot Risk Factors and Exposure for All Features
# Plot Train and Test Deviance Over Iterations
# Plot Risk Factors and Exposure for All Features
# Compare Risk Factors Across Models with Exposure Data
# Plot Feature Predictions Comparison Between Regularized and Unregularized Models
# 
#
# Author: mmatull
# Date: 2025-03-20
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
    
    # Calculate the total exposure for each category
    exposure_sum <- tapply(
      exposure_df[[exposure_col]], 
      exposure_df[[feature]], 
      sum, 
      na.rm = TRUE
    )
    
    # Create a data frame for the exposure by category
    exposure_by_category <- data.frame(
      Category = names(exposure_sum),
      Exposure = as.numeric(exposure_sum)
    )
    
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
# Plot Train and Test Deviance Over Iterations
# =====================================================================================================================================
#'
#' This function creates a plot to compare the deviance values for both training and test datasets over iterations. 
#' It uses Plotly to generate an interactive plot with two y-axes: one for the train deviance and one for the test deviance. 
#' The plot includes markers, lines, and hover information for each iteration.
#'
#' @param deviance_train A numeric vector containing the deviance values for the training dataset.
#' @param deviance_test A numeric vector containing the deviance values for the test dataset.
#'
#' @return A Plotly plot object showing the deviance for both train and test datasets, with interactive hover information and separate y-axes.
#'
#' @examples
#' plot_deviance(pipe1$deviance_train, pipe1$deviance_test)
#'
#' @export

plot_deviance_train_test <- function(deviance_train, deviance_test) {
  
  # Determine the number of steps based on the length of the training data
  n_steps <- length(deviance_train)
  
  # Get the range of values for train and test deviance
  train_range <- range(deviance_train)
  test_range <- range(deviance_test)
  
  # Create a data frame for the plot, including both train and test deviance
  df <- data.frame(
    Step = 0:(n_steps - 1),          # Iteration steps for x-axis
    Train = deviance_train,           # Train deviance values
    Test = rev(deviance_test),        # Test deviance values (reversed order)
    Test_Step = rev(0:(n_steps - 1))  # Reversed test iteration steps for hover text
  )
  
  # Define a uniform font size for plot elements
  font_size <- 12
  
  # Create the plot using Plotly
  p <- plot_ly() %>%
    # Add trace for training data on the primary Y-axis (left side)
    add_trace(
      data = df,
      x = ~Step,                         # x-axis: Iteration step
      y = ~Train,                        # y-axis: Train deviance
      type = 'scatter',                  # Scatter plot
      mode = 'lines+markers',            # Plot lines and markers
      name = 'Train',                    # Label for the trace
      line = list(color = 'blue', width = 2),   # Line style (blue)
      marker = list(color = 'blue', size = 8),  # Marker style (blue)
      hoverinfo = 'text',                # Info shown on hover
      text = ~paste("Iteration:", Step, "<br>Train Deviance:", round(Train, 4)),  # Hover text
      yaxis = "y"                        # Primary Y-axis
    ) %>%
    # Add trace for test data on the secondary Y-axis (right side)
    add_trace(
      data = df,
      x = ~Step,                         # x-axis: Iteration step
      y = ~Test,                         # y-axis: Test deviance
      type = 'scatter',                  # Scatter plot
      mode = 'lines+markers',            # Plot lines and markers
      name = 'Test',                     # Label for the trace
      line = list(color = 'red', width = 2),    # Line style (red)
      marker = list(color = 'red', size = 8),   # Marker style (red)
      hoverinfo = 'text',                # Info shown on hover
      text = ~paste("Iteration:", Test_Step, "<br>Test Deviance:", round(Test, 4)),  # Hover text
      yaxis = "y2"                       # Secondary Y-axis
    ) %>%
    # Layout settings for both Y-axes and overall plot appearance
    layout(
      title = list(
        text = "<b>Train vs. Test Deviance</b>",  # Title of the plot
        font = list(size = font_size + 2)         # Title font size
      ),
      xaxis = list(
        title = "Iteration",                   # Label for x-axis
        zeroline = FALSE,                      # Remove zero line
        titlefont = list(size = font_size),     # Font size for x-axis title
        tickfont = list(size = font_size)       # Font size for x-axis ticks
      ),
      yaxis = list(
        title = "Train Deviance",              # Label for left Y-axis
        side = "left",                         # Place on left side
        showgrid = TRUE,                       # Show grid lines
        zeroline = FALSE,                      # Remove zero line
        titlefont = list(size = font_size),     # Font size for Y-axis title
        tickfont = list(size = font_size)       # Font size for Y-axis ticks
      ),
      yaxis2 = list(
        title = "Test Deviance",               # Label for right Y-axis
        side = "right",                        # Place on right side
        overlaying = "y",                      # Overlay on the primary Y-axis
        showgrid = FALSE,                      # No grid for the second Y-axis
        zeroline = FALSE,                      # Remove zero line
        ticklen = 4,                           # Tick length
        tickwidth = 1,                         # Tick width
        tickcolor = "#000",                    # Tick color (black)
        titlefont = list(size = font_size),     # Font size for Y-axis title
        tickfont = list(size = font_size)       # Font size for Y-axis ticks
      ),
      legend = list(
        x = 0.7,                              # Position of the legend
        y = 1,                                # Position of the legend
        font = list(size = font_size)          # Font size for legend
      ),
      margin = list(r = 75, l = 75, t = 50, b = 50),   # Margin settings
      font = list(size = font_size)            # Global font size
    )
  
  # Return the plot object
  return(p)
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
    
    # Calculate the total exposure for each category
    exposure_sum <- tapply(
      exposure_df[[exposure_col]], 
      exposure_df[[feature]], 
      sum, 
      na.rm = TRUE
    )
    
    # Create a data frame for the exposure by category
    exposure_by_category <- data.frame(
      Category = names(exposure_sum),
      Exposure = as.numeric(exposure_sum)
    )
    
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
#'
#' @return A list of Plotly plots comparing the risk factors and exposure across models.
#'
#' @examples
#' plot_risk_factors_compare_model(result1, result2, data, "exposure")
#'
#' @export
plot_risk_factors_compare_model <- function(pipe_regularized, pipe_unregularized, data, exposure_col) {
  
  
  column_index1 <- which(pipe_regularized$fitted_model_train$lambda == pipe_unregularized$relaxed_model$lambda) 
  
  if (length(column_index1) == 0) {
    stop("Error: The column index was not found. Maybe different lambda paths were used.")
  }
  
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
    all_categories <- sort(unique(c(categories1, categories2)))
    
    # Extract risk factors from pipe_regularized
    risk_values1 <- NULL
    col_name1 <- NULL
    if (!is.null(risk_factor_matrix1) && ncol(risk_factor_matrix1) >= column_index1) {
      col_name1 <- colnames(risk_factor_matrix1)[column_index1]
      risk_values1 <- risk_factor_matrix1[, column_index1]
      names(risk_values1) <- rownames(risk_factor_matrix1)
    }
    
    # Extract risk factors from pipe_unregularized (always first column, as there is only one)
    risk_values2 <- NULL
    col_name2 <- NULL
    if (!is.null(risk_factor_matrix2) && ncol(risk_factor_matrix2) > 0) {
      col_name2 <- colnames(risk_factor_matrix2)[1]  # Immer die erste (und einzige) Spalte
      risk_values2 <- risk_factor_matrix2[, 1]
      names(risk_values2) <- rownames(risk_factor_matrix2)
    }
    
    # Aggregate exposure data by category
    exposure_sum <- tapply(
      data[[exposure_col]], 
      data[[feature]], 
      sum, 
      na.rm = TRUE
    )
    
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
    plot_data <- merge(plot_data, exposure_by_category, by = "Category", all.x = TRUE)
    plot_data$Exposure[is.na(plot_data$Exposure)] <- 0
    
    # Sort categories numerically if possible
    numeric_categories <- suppressWarnings(as.numeric(as.character(plot_data$Category)))
    if (!all(is.na(numeric_categories))) {
      plot_data$NumericCategory <- numeric_categories
      plot_data <- plot_data[order(plot_data$NumericCategory), ]
    } else {
      plot_data <- plot_data[order(plot_data$Category), ]
    }
    
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
    
    if (!is.null(risk_values1)) {
      model1_name <- paste("Regularized  Model", if(!is.null(col_name1)) paste("-", col_name1) else "")
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
    }
    
    if (!is.null(risk_values2)) {
      model2_name <- paste("Unregularized Model", if(!is.null(col_name2)) paste("-", col_name1) else "")
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
    }
    
    # Set layout options for the plot
    p <- p %>%
      layout(
        title = paste("Risk Factors and Exposure for", feature),  # Plot title
        xaxis = list(
          title = feature,  # X-axis label
          tickangle = -90,  # Rotate the x-axis labels
          tickfont = list(size = 12),  # Font size for the x-axis labels
          categoryorder = "array",  # Order categories as provided
          categoryarray = plot_data$Category  # Preserve order of categories
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
    
    plots[[feature]] <- list(plot = p)
  }
  
  return(plots)
}



# =====================================================================================================================================
# Plot Feature Predictions Comparison Between Regularized and Unregularized Models
# =====================================================================================================================================
#'
#' This function creates an interactive plotly visualization comparing how regularized
#' and unregularized models predict across different levels of a specific feature.
#' It shows actual values, predicted values from both models, and exposure (sample weights)
#' for each feature level.
#'
#' @param pipe_regularized An object containing the regularized model information with fitted model results
#' @param pipe_unregularized An object containing the unregularized model information
#' @param data A data frame containing the features and target variable
#' @param feature_name Character string with the name of the feature to plot
#' @param target_col Character string with the name of the target/response column
#' @param weight_col Character string with the name of the weight/exposure column
#' @param train_or_test Character string indicating whether to use training or test data ("train" or "test")
#'
#' @return A plotly object showing the comparison between actual values, regularized model predictions,
#'         and unregularized model predictions across different levels of the specified feature
#'
#' @examples
#' \dontrun{
#' # Create comparison plot for the feature "age_group" using training data
#' plot_feature_predictions_comparison(
#'   pipe_regularized = my_regularized_model,
#'   pipe_unregularized = my_unregularized_model,
#'   data = my_data,
#'   feature_name = "age_group",
#'   target_col = "loss_ratio",
#'   weight_col = "premium",
#'   train_or_test = "train"
#' )
#' }
plot_feature_predictions_comparison <- function(pipe_regularized, pipe_unregularized, data, feature_name, target_col = NULL, weight_col = NULL, train_or_test = "train") {
  
  # Validate train_or_test parameter
  if(!train_or_test %in% c("train", "test")) {
    stop("train_or_test parameter must be either 'train' or 'test'")
  }
  
  # Set train_or_test-specific variables based on the train_or_test parameter
  index_field <- paste0(train_or_test, "_index")
  preds_field_reg <- paste0("preds_", train_or_test)
  preds_field_unreg <- paste0("preds_", train_or_test)
  
  # Get appropriate column index for regularized model
  # This is necessary because regularized models have predictions for multiple lambda values
  column_index1 <- which(pipe_regularized$fitted_model_train$lambda == pipe_unregularized$relaxed_model$lambda) 
  
  if (length(column_index1) == 0) {
    stop("Error: The column index was not found. Maybe different lambda paths were used.")
  }
  
  # Select appropriate data based on train_or_test parameter
  selected_data <- data[pipe_regularized$split[[index_field]],]
  
  # Extract feature values and ensure they're factors for proper grouping
  feature_values <- selected_data[[feature_name]]
  if (!is.factor(feature_values)) {
    feature_values <- factor(feature_values)
  }
  
  # Calculate actual values per feature level
  # This aggregates the target variable weighted by the weight column for each feature level
  actual_values <- selected_data %>%
    group_by(feature_level = feature_values) %>%
    summarise(
      actual_rate = sum(.data[[target_col]]) / sum(.data[[weight_col]]),
      exposure = sum(.data[[weight_col]]),
      .groups = "drop"
    )
  
  # Calculate predicted values from pipe_regularized (regularized model)
  # This aggregates the model predictions for each feature level
  pred1_values <- selected_data %>%
    mutate(
      prediction = pipe_regularized[[preds_field_reg]][,column_index1]
    ) %>%
    group_by(feature_level = feature_values) %>%
    summarise(
      pred_rate = sum(prediction*!!sym(weight_col))/sum(!!sym(weight_col)),
      .groups = "drop"
    )
  
  # Calculate predicted values from pipe_unregularized (unregularized model)
  # Note: For the unregularized model, we only use the first column as there's only one lambda value
  pred2_values <- selected_data %>%
    mutate(
      prediction = pipe_unregularized[[preds_field_unreg]][,1]  # Only one column for relaxed model
    ) %>%
    group_by(feature_level = feature_values) %>%
    summarise(
      pred_rate = sum(prediction*!!sym(weight_col))/sum(!!sym(weight_col)),
      .groups = "drop"
    )
  
  # Combine all data for plotting
  plot_data <- actual_values %>%
    left_join(pred1_values, by = "feature_level") %>%
    rename(pred_rate_reg = pred_rate) %>%
    left_join(pred2_values, by = "feature_level") %>%
    rename(pred_rate_unreg = pred_rate)
  
  # Convert to character for proper ordering on x-axis
  plot_data$feature_level <- as.character(plot_data$feature_level)
  
  # Try to convert to numeric for proper ordering if possible
  # This ensures numeric levels are presented in numerical order rather than lexical order
  numeric_levels <- suppressWarnings(as.numeric(plot_data$feature_level))
  if (!all(is.na(numeric_levels))) {
    plot_data <- plot_data[order(numeric_levels), ]
  } else {
    plot_data <- plot_data[order(plot_data$feature_level), ]
  }
  
  # Create train_or_test label for title (more readable)
  train_or_test_label <- ifelse(train_or_test == "train", "Training", "Test")
  
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
      name = "Regularized Model",
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
      name = "Unregularized Model",
      line = list(color = "rgb(214, 39, 40)", width = 3),  # Solid line for unregularized model
      marker = list(color = "rgb(214, 39, 40)", size = 8, symbol = "triangle-up"),  # Triangle markers for differentiation
      hoverinfo = "text",
      text = ~paste("Level:", feature_level, 
                    "<br>Unregularized Pred:", format(pred_rate_unreg, digits = 5, nsmall = 5))
    ) %>%
    # Layout configuration for plot appearance and interactivity
    layout(
      title = paste("Prediction for", feature_name, "-", train_or_test_label, "Data"),
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
plot_all_feature_predictions_comparison <- function(pipe_regularized, pipe_unregularized, data, target_col = NULL, weight_col = NULL, train_or_test = "train") {
  # Validate train_or_test parameter
  if(!train_or_test %in% c("train", "test")) {
    stop("train_or_test parameter must be either 'train' or 'test'")
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
      train_or_test = train_or_test
    )
  }
  
  return(plots)
}