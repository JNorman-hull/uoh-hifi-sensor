library(shiny)
library(tidyverse)
library(reticulate)
library(DT)  
library(shinyjs)
library(plotly)
library(patchwork)

dir.create("./RAPID_Processed", showWarnings = FALSE, recursive = TRUE)

# Import Python functions (with no main block)
source_python("rapid_functions.py")

# Source all modules
source("modules/fileSelectionModule.R")
source("modules/processingModule.R")
source("modules/resultsModule.R")
source("modules/plotsModule.R")
source("modules/roiModule.R")

# Shared helper functions for sensor index system
get_sensor_index_file <- function(output_dir) {
  index_file <- file.path(output_dir, "uoh_sensor_index.csv")
  if (file.exists(index_file)) {
    return(index_file)
  } else {
    return(NULL)
  }
}

get_processed_sensors <- function(output_dir) {
  min_files <- list.files(path = file.path(output_dir, "csv"), 
                          pattern = "_min\\.csv$", full.names = FALSE)
  sensor_names <- gsub("_min\\.csv$", "", min_files)
  return(sensor_names)
}

get_nadir_info <- function(sensor_name, output_dir) {
  if (is.null(sensor_name) || sensor_name == "") {
    return(list(available = FALSE))
  }
  
  index_file <- get_sensor_index_file(output_dir)
  if (is.null(index_file)) {
    return(list(available = FALSE))
  }
  
  index_df <- read.csv(index_file)
  
  if (!"file" %in% names(index_df)) {
    return(list(available = FALSE))
  }
  
  sensor_row <- index_df[index_df$file == sensor_name, ]
  
  if (nrow(sensor_row) == 0) {
    return(list(available = FALSE))
  }
  
  # Handle different possible column name formats
  possible_time_cols <- c("pres_min[time]", "pres_min.time.", "pres_min.time")
  possible_value_cols <- c("pres_min[kPa]", "pres_min.kPa.", "pres_min.kPa")
  
  time_col <- NULL
  value_col <- NULL
  
  for (col in names(sensor_row)) {
    if (col %in% possible_time_cols) time_col <- col
    if (col %in% possible_value_cols) value_col <- col
  }
  
  if (!is.null(time_col) && !is.null(value_col)) {
    return(list(
      time = as.numeric(sensor_row[[time_col]]),
      value = as.numeric(sensor_row[[value_col]]),
      available = TRUE
    ))
  }
  
  return(list(available = FALSE))
}

# Create a wrapper function to get unique sensor names (without extensions)
get_sensor_names <- function(raw_data_path = "./RAW_data/RAPID") {
  # Find all IMP files
  imp_files <- list.files(path = raw_data_path, pattern = "\\.IMP$", full.names = FALSE)
  
  # Extract base names without extensions
  sensor_names <- tools::file_path_sans_ext(imp_files)
  
  return(sensor_names)
}