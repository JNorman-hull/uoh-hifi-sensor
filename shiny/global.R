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

# ========== SHARED HELPER FUNCTIONS ==========

# Sensor index system functions
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

# Safe index file operations with error handling
safe_update_sensor_index <- function(output_dir, sensor_name, updates) {
  index_file <- get_sensor_index_file(output_dir)
  if (is.null(index_file)) return(FALSE)
  
  tryCatch({
    index_df <- read.csv(index_file)
    row_idx <- which(index_df$file == sensor_name)
    
    if (length(row_idx) > 0) {
      for (col in names(updates)) {
        if (col %in% names(index_df)) {
          index_df[row_idx, col] <- updates[[col]]
        }
      }
      write.csv(index_df, index_file, row.names = FALSE)
      return(TRUE)
    }
    return(FALSE)
  }, error = function(e) {
    warning("Failed to update sensor index: ", e$message)
    return(FALSE)
  })
}

# Comprehensive sensor status checking
get_sensor_status <- function(sensor_name, output_dir) {
  index_file <- get_sensor_index_file(output_dir)
  if (is.null(index_file)) return(list(delineated = FALSE, trimmed = FALSE, exists = FALSE))
  
  tryCatch({
    index_df <- read.csv(index_file)
    sensor_row <- index_df[index_df$file == sensor_name, ]
    
    if (nrow(sensor_row) == 0) {
      return(list(delineated = FALSE, trimmed = FALSE, exists = FALSE))
    }
    
    # Check flags and verify files exist
    delineated_flag <- !is.na(sensor_row$delineated) && sensor_row$delineated == "Y"
    trimmed_flag <- !is.na(sensor_row$trimmed) && sensor_row$trimmed == "Y"
    
    if (delineated_flag) {
      delineated_file <- file.path(output_dir, "csv", "delineated", paste0(sensor_name, "_delineated.csv"))
      if (!file.exists(delineated_file)) {
        # Fix inconsistent state
        safe_update_sensor_index(output_dir, sensor_name, list(delineated = "N", trimmed = "N"))
        delineated_flag <- trimmed_flag <- FALSE
      }
    }
    
    return(list(
      delineated = delineated_flag,
      trimmed = trimmed_flag, 
      exists = TRUE
    ))
  }, error = function(e) {
    return(list(delineated = FALSE, trimmed = FALSE, exists = FALSE))
  })
}

# Standardized file reading with error handling
read_sensor_data <- function(output_dir, sensor_name, type = "min", subdir = "csv") {
  suffix <- switch(type,
                   "min" = "_min.csv",
                   "delineated" = "_delineated.csv",
                   ".csv"
  )
  
  file_path <- if (type == "delineated") {
    file.path(output_dir, subdir, "delineated", paste0(sensor_name, suffix))
  } else {
    file.path(output_dir, subdir, paste0(sensor_name, suffix))
  }
  
  if (!file.exists(file_path)) return(NULL)
  
  tryCatch({
    read.csv(file_path)
  }, error = function(e) {
    warning("Failed to read sensor data: ", e$message)
    return(NULL)
  })
}

# Sensor dropdown management
update_sensor_dropdown <- function(session, input_id, processed_sensors, current_selection = NULL) {
  choices <- processed_sensors
  if (length(choices) == 0) return()
  
  current_choice <- current_selection
  selected_value <- if (!is.null(current_choice) && current_choice %in% choices) {
    current_choice
  } else {
    choices[1]
  }
  
  updateSelectInput(session, input_id, choices = choices, selected = selected_value)
}

# Variable definitions to eliminate hardcoding
get_sensor_variables <- function() {
  list(
    names = c("pressure_kpa", "higacc_mag_g", "inacc_mag_ms", "rot_mag_degs"),
    labels = c("Pressure [kPa]", "HIG Acceleration [g]", 
               "Inertial Acceleration [m/s²]", "Rotational Magnitude [deg/s]"),
    colors = c("black", "red", "blue", "green")
  )
}

# Button state management helper
manage_button_states <- function(session, button_config) {
  for (button_id in names(button_config)) {
    if (button_config[[button_id]]) {
      shinyjs::enable(button_id)
    } else {
      shinyjs::disable(button_id)
    }
  }
}

# Enhanced nadir info function
get_nadir_info <- function(sensor_name, output_dir) {
  if (is.null(sensor_name) || sensor_name == "") {
    return(list(available = FALSE))
  }
  
  index_file <- get_sensor_index_file(output_dir)
  if (is.null(index_file)) {
    return(list(available = FALSE))
  }
  
  tryCatch({
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
  }, error = function(e) {
    warning("Error getting nadir info: ", e$message)
    return(list(available = FALSE))
  })
}

# ROI configuration loader
load_roi_configs <- function(output_dir) {
  # Define default ROI configurations
  # These could be loaded from a config file in the future
  list(
    "standard_diving" = list(
      label = "standard_diving",
      roi1_ingress = 5.0,
      roi2_prenadir = 2.0, 
      roi3_nadir = 1.0,
      roi4_postnadir = 2.0,
      roi5_outgress = 5.0
    ),
    "shallow_diving" = list(
      label = "shallow_diving", 
      roi1_ingress = 3.0,
      roi2_prenadir = 1.5,
      roi3_nadir = 0.5,
      roi4_postnadir = 1.5,
      roi5_outgress = 3.0
    ),
    "deep_diving" = list(
      label = "deep_diving",
      roi1_ingress = 8.0,
      roi2_prenadir = 3.0,
      roi3_nadir = 2.0,
      roi4_postnadir = 3.0,
      roi5_outgress = 8.0
    )
  )
}

# Create a wrapper function to get unique sensor names (without extensions)
get_sensor_names <- function(raw_data_path = "./RAW_data/RAPID") {
  # Find all IMP files
  imp_files <- list.files(path = raw_data_path, pattern = "\\.IMP$", full.names = FALSE)
  
  # Extract base names without extensions
  sensor_names <- tools::file_path_sans_ext(imp_files)
  
  return(sensor_names)
}