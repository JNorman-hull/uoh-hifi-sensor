library(shiny)
library(tidyverse)
library(reticulate)
library(DT)  
library(shinyjs)
library(plotly)
library(patchwork)
library(shinydashboard)

dir.create("./processed_sens_data", showWarnings = FALSE, recursive = TRUE)

# Import Python functions
source_python("modules/global_modules/rapid_functions.py")


#global modules
source("modules/global_modules/fileSelectionTableModule.R")
source("modules/global_modules/enhancedSensorSelectionModule.R")
source("modules/global_modules/plotModule.R")
source("modules/global_modules/summarytableModule.R")
source("modules/global_modules/statusModule.R")
source("modules/global_modules/configurationModule.R")

#sensor dashboard modules

#sensor processing modules
source("modules/sensor_processing/rawdataprocessingModule.R")
source("modules/sensor_processing/processinghelperModule.R")
source("modules/sensor_processing/processingresultsModule.R")
source("modules/sensor_processing/deploymentModule.R")

#time series modules
source("modules/time_series_analysis/plotsModule.R") #currently defunct, eventually a ggplot save environment
source("modules/time_series_analysis/roiModule.R")

#instrument modules
source("modules/instrument_analysis/pressureModule.R")
source("modules/instrument_analysis/accelerationModule.R")
source("modules/instrument_analysis/rotationModule.R")
#post-process modules

# ============================= #
# /// Shared helpers \\\ ####  
# ============================= #  

## Get sensor index ####
get_sensor_index_file <- function(output_dir, read_data = FALSE) {
  index_file <- file.path(output_dir, "index", "global_sensor_index.csv")
  if (file.exists(index_file)) {
    if (read_data) {
      tryCatch({
        return(read.csv(index_file))
      }, error = function(e) {
        warning("Failed to read sensor index: ", e$message)
        return(NULL)
      })
    } else {
      return(index_file)
    }
  } else {
    return(NULL)
  }
}

## Safe update index file ####
safe_update_sensor_index <- function(output_dir, sensor_name, updates) {
  index_df <- get_sensor_index_file(output_dir, read_data = TRUE)
  if (is.null(index_df)) return(FALSE)
  
  tryCatch({
    row_idx <- which(index_df$file == sensor_name)
    
    if (length(row_idx) > 0) {
      for (col in names(updates)) {
        if (col %in% names(index_df)) {
          index_df[row_idx, col] <- updates[[col]]
        }
      }
      # Get file path for writing
      index_file <- get_sensor_index_file(output_dir)
      write.csv(index_df, index_file, row.names = FALSE)
      return(TRUE)
    }
    return(FALSE)
  }, error = function(e) {
    warning("Failed to update sensor index: ", e$message)
    return(FALSE)
  })
}

## Get instrument index file ####
get_instrument_index_file <- function(output_dir, read_data = FALSE) {
  index_file <- file.path(output_dir, "instrument_data", "global_processed_instrument_data.csv")
  if (file.exists(index_file)) {
    if (read_data) {
      tryCatch({
        return(read.csv(index_file))
      }, error = function(e) {
        warning("Failed to read instrument index: ", e$message)
        return(NULL)
      })
    } else {
      return(index_file)
    }
  } else {
    return(NULL)
  }
}

## Safe update instrument index ####
safe_update_instrument_index <- function(output_dir, sensor_name, roi, updates) {
  index_df <- get_instrument_index_file(output_dir, read_data = TRUE)
  if (is.null(index_df)) return(FALSE)
  
  tryCatch({
    # Find the row for this sensor and ROI
    row_idx <- which(index_df$file == sensor_name & index_df$roi == roi)
    
    if (length(row_idx) > 0) {
      # Update existing row
      for (col in names(updates)) {
        if (col %in% names(index_df)) {
          index_df[row_idx, col] <- updates[[col]]
        }
      }
    } else {
      # Create new row
      new_row <- index_df[1, ]  # Copy structure
      new_row[1, ] <- NA  # Clear values
      new_row$file <- sensor_name
      new_row$roi <- roi
      
      for (col in names(updates)) {
        if (col %in% names(index_df)) {
          new_row[col] <- updates[[col]]
        }
      }
      
      index_df <- rbind(index_df, new_row)
    }
    
    # Get file path for writing
    index_file <- get_instrument_index_file(output_dir)
    write.csv(index_df, index_file, row.names = FALSE)
    return(TRUE)
  }, error = function(e) {
    warning("Failed to update instrument index: ", e$message)
    return(FALSE)
  })
}

## Get instrument column mappings ####
get_instrument_column_mapping <- function(instrument_var) {
  switch(instrument_var,
    "pres" = list(
      data_cols = c("pressure_kpa"),
      prefix = "pres",
      units = c(".kPa.")
    ),
    "acc" = list(
      data_cols = c("higacc_mag_g", "inacc_mag_ms"),
      prefix = c("acc_hig", "acc_inacc"),
      units = c(".g.", ".ms.")
    ),
    "rot" = list(
      data_cols = c("rot_mag_degs"),
      prefix = "rot",
      units = c(".degs.")
    ),
    NULL
  )
}

## Get processed sensors ####
get_processed_sensors <- function(output_dir) {
  min_files <- list.files(path = file.path(output_dir, "csv"), 
                          pattern = "_min\\.csv$", full.names = FALSE)
  sensor_names <- gsub("_min\\.csv$", "", min_files)
  return(sensor_names)
}

## File reading ####
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

## Get raw sensor names ####
# Path needs to be linked to directory_config
get_sensor_names <- function(raw_data_path = "./raw_sens_data") {
  # Find all IMP files
  imp_files <- list.files(path = raw_data_path, pattern = "\\.IMP$", full.names = FALSE)
  
  # Extract base names without extensions
  sensor_names <- tools::file_path_sans_ext(imp_files)
  
  return(sensor_names)
}

## Nadir info #####
get_nadir_info <- function(sensor_name, output_dir) {
  if (is.null(sensor_name) || sensor_name == "") {
    return(list(available = FALSE))
  }
  
  index_df <- get_sensor_index_file(output_dir, read_data = TRUE)
  if (is.null(index_df)) {
    return(list(available = FALSE))
  }
  
  tryCatch({
    if (!"file" %in% names(index_df)) {
      return(list(available = FALSE))
    }
    
    sensor_row <- index_df[index_df$file == sensor_name, ]
    
    if (nrow(sensor_row) == 0) {
      return(list(available = FALSE))
    }
    
    # Index columns are consistently named
    time_col <- "pres_min.time."
    value_col <- "pres_min.kPa."
    
    if (time_col %in% names(sensor_row) && value_col %in% names(sensor_row)) {
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

#


## Button state management ####
manage_button_states <- function(session, button_config) {
  for (button_id in names(button_config)) {
    if (button_config[[button_id]]) {
      shinyjs::enable(button_id)
    } else {
      shinyjs::disable(button_id)
    }
  }
}

# ============================= #
# /// Shared status functions \\\ ####  
# ============================= #  

## Processing status flags ####

get_sensor_status <- function(sensor_name, output_dir) {
  index_df <- get_sensor_index_file(output_dir, read_data = TRUE)
  if (is.null(index_df)) {
    return(list(
      delineated = FALSE, trimmed = FALSE, normalized = FALSE, passage_times = FALSE,
      bad_sens = FALSE, deployment_info = FALSE,
      all_pres_processed = FALSE, pres_sum_processed = FALSE, pres_rpc_processed = FALSE, pres_lrpc_processed = FALSE,
      all_acc_processed = FALSE, acc_sum_processed = FALSE, acc_hig_peaks_processed = FALSE, acc_strike_processed = FALSE, acc_collision_processed = FALSE,
      all_rot_processed = FALSE, rot_sum_processed = FALSE,
      exists = FALSE
    ))
  }
  
  tryCatch({
    sensor_row <- index_df[index_df$file == sensor_name, ]
    
    if (nrow(sensor_row) == 0) {
      return(list(
        delineated = FALSE, trimmed = FALSE, normalized = FALSE, passage_times = FALSE,
        bad_sens = FALSE, deployment_info = FALSE,
        all_pres_processed = FALSE, pres_sum_processed = FALSE, pres_rpc_processed = FALSE, pres_lrpc_processed = FALSE,
        all_acc_processed = FALSE, acc_sum_processed = FALSE, acc_hig_peaks_processed = FALSE, acc_strike_processed = FALSE, acc_collision_processed = FALSE,
        all_rot_processed = FALSE, rot_sum_processed = FALSE,
        exists = FALSE
      ))
    }
    
    # Check flags and verify files exist
    delineated_flag <- !is.na(sensor_row$delineated) && sensor_row$delineated == "Y"
    trimmed_flag <- !is.na(sensor_row$trimmed) && sensor_row$trimmed == "Y"
    normalized_flag <- !is.na(sensor_row$normalized) && sensor_row$normalized == "Y"
    passage_times_flag <- !is.na(sensor_row$passage_times) && sensor_row$passage_times == "Y"
    
    # Check bad sensor flag
    bad_sens_flag <- !is.na(sensor_row$bad_sens) && sensor_row$bad_sens == "Y"
    
    # Check deployment info flag
    deployment_info_flag <- !is.na(sensor_row$deployment_info) && sensor_row$deployment_info == "Y"
    
    # Check pressure processing flags
    all_pres_processed_flag <- !is.na(sensor_row$all_pres_processed) && sensor_row$all_pres_processed == "Y"
    pres_sum_processed_flag <- !is.na(sensor_row$pres_sum_processed) && sensor_row$pres_sum_processed == "Y"
    pres_rpc_processed_flag <- !is.na(sensor_row$pres_rpc_processed) && sensor_row$pres_rpc_processed == "Y"
    pres_lrpc_processed_flag <- !is.na(sensor_row$pres_lrpc_processed) && sensor_row$pres_lrpc_processed == "Y"
    
    # Check acceleration processing flags
    all_acc_processed_flag <- !is.na(sensor_row$all_acc_processed) && sensor_row$all_acc_processed == "Y"
    acc_sum_processed_flag <- !is.na(sensor_row$acc_sum_processed) && sensor_row$acc_sum_processed == "Y"
    acc_hig_peaks_processed_flag <- !is.na(sensor_row$acc_hig_peaks_processed) && sensor_row$acc_hig_peaks_processed == "Y"
    acc_strike_processed_flag <- !is.na(sensor_row$acc_strike_processed) && sensor_row$acc_strike_processed == "Y"
    acc_collision_processed_flag <- !is.na(sensor_row$acc_collision_processed) && sensor_row$acc_collision_processed == "Y"
    
    # Check rotation processing flags
    all_rot_processed_flag <- !is.na(sensor_row$all_rot_processed) && sensor_row$all_rot_processed == "Y"
    rot_sum_processed_flag <- !is.na(sensor_row$rot_sum_processed) && sensor_row$rot_sum_processed == "Y"
    
    if (delineated_flag) {
      delineated_file <- file.path(output_dir, "csv", "delineated", paste0(sensor_name, "_delineated.csv"))
      if (!file.exists(delineated_file)) {
        # Fix inconsistent state
        safe_update_sensor_index(output_dir, sensor_name, list(delineated = "N", trimmed = "N", normalized = "N", passage_times = "N"))
        delineated_flag <- trimmed_flag <- normalized_flag <- passage_times_flag <- FALSE
      }
    }
    
    return(list(
      delineated = delineated_flag,
      trimmed = trimmed_flag,
      normalized = normalized_flag,
      passage_times = passage_times_flag,
      bad_sens = bad_sens_flag,
      deployment_info = deployment_info_flag,
      all_pres_processed = all_pres_processed_flag,
      pres_sum_processed = pres_sum_processed_flag,
      pres_rpc_processed = pres_rpc_processed_flag,
      pres_lrpc_processed = pres_lrpc_processed_flag,
      all_acc_processed = all_acc_processed_flag,
      acc_sum_processed = acc_sum_processed_flag,
      acc_hig_peaks_processed = acc_hig_peaks_processed_flag,
      acc_strike_processed = acc_strike_processed_flag,
      acc_collision_processed = acc_collision_processed_flag,
      all_rot_processed = all_rot_processed_flag,
      rot_sum_processed = rot_sum_processed_flag,
      exists = TRUE
    ))
  }, error = function(e) {
    return(list(
      delineated = FALSE, trimmed = FALSE, normalized = FALSE, passage_times = FALSE,
      bad_sens = FALSE, deployment_info = FALSE,
      all_pres_processed = FALSE, pres_sum_processed = FALSE, pres_rpc_processed = FALSE, pres_lrpc_processed = FALSE,
      all_acc_processed = FALSE, acc_sum_processed = FALSE, acc_hig_peaks_processed = FALSE, acc_strike_processed = FALSE, acc_collision_processed = FALSE,
      all_rot_processed = FALSE, rot_sum_processed = FALSE,
      exists = FALSE
    ))
  })
}