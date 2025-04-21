install_and_load <- function(packages) {
  # Get all installed packages across all library paths
  installed_packages <- installed.packages()[, "Package"]
  
  # Identify missing packages
  missing_packages <- setdiff(packages, installed_packages)
  
  if (length(missing_packages) > 0) {
    # Show available library paths
    lib_paths <- .libPaths()
    
    cat("Available library paths:\n")
    for (i in seq_along(lib_paths)) {
      cat(i, ": ", lib_paths[i], "\n", sep = "")
    }
    
    # Ask user to select a path or enter a new one
    user_input <- readline(prompt = "Select a path (number) or enter a new path: ")
    
    # Determine the selected library path
    if (grepl("^[0-9]+$", user_input)) {
      selected_index <- as.integer(user_input)
      if (selected_index > 0 && selected_index <= length(lib_paths)) {
        install_path <- lib_paths[selected_index]
      } else {
        stop("Invalid selection.")
      }
    } else {
      install_path <- user_input
    }
    
    # Add the selected path to the library paths if it's not already included
    if (!(install_path %in% lib_paths)) {
      .libPaths(c(install_path, lib_paths))
    }
    
    # Save the selected library path in .Rprofile
    rprofile_path <- file.path(Sys.getenv("HOME"), ".Rprofile")
    cat(sprintf('.libPaths(c(.libPaths(), "%s"))\n', gsub("\\", "/", install_path, fixed = TRUE)), file = rprofile_path, append = TRUE)
    cat("Library path saved to .Rprofile\n")
    
    # Check which missing packages are not already in the selected library path
    missing_in_selected <- setdiff(missing_packages, installed.packages(lib.loc = install_path)[, "Package"])
    
    # Install only truly missing packages in the selected library
    if (length(missing_in_selected) > 0) {
      install.packages(missing_in_selected, dependencies = TRUE, lib = install_path)
    }
  }
  
  # Load all required packages
  invisible(lapply(packages, require, character.only = TRUE))
}

required_packages <- c("MASS", "Matrix", "dplyr", "purrr", "caret", 
                       "glmnet", "rlang", "tweedie", "plotly", "openxlsx")

install_and_load(required_packages)
