# Function to install and load packages
install_and_load <- function(packages) {
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  if(length(missing_packages)) {
    install.packages(missing_packages, dependencies = TRUE)
  }
  
  lapply(packages, require, character.only = TRUE)
}

# List of required packages
required_packages <- c("MASS", 
                       "Matrix", 
                       "dplyr", 
                       "purrr",
                       "caret",
                       "glmnet", 
                       "rlang", 
                       "tweedie", 
                       "plotly")

# Install and load all packages
install_and_load(required_packages)
