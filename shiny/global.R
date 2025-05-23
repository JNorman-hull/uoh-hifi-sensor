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

# Create a wrapper function to get unique sensor names (without extensions)
get_sensor_names <- function(raw_data_path = "./RAW_data/RAPID") {
  # Find all IMP files
  imp_files <- list.files(path = raw_data_path, pattern = "\\.IMP$", full.names = FALSE)
  
  # Extract base names without extensions
  sensor_names <- tools::file_path_sans_ext(imp_files)
  
  return(sensor_names)
}

process_sensors_step_by_step <- function(selected_sensors, raw_data_path, output_dir, session) {
  log_messages <- character(0)
  
  # Function to update log in UI
  update_log <- function(message) {
    log_messages <<- c(log_messages, message)
    log_text <- paste(log_messages, collapse = "\n")
    session$sendCustomMessage("updateProcessLog", list(text = log_text))
    Sys.sleep(0.1)  # Small delay to allow UI to update
  }
  
  update_log("Starting processing...")
  update_log(paste("Processing", length(selected_sensors), "selected sensors"))
  
  # Initialize data structures
  summary_data <- list()
  n_files_w_time <- 0
  n_files_w_hig <- 0
  n_files_w_pres <- 0
  
  # Get current date for CSV filenames
  current_date <- format(Sys.Date(), "%d%m%y")
  
  # Process each sensor one at a time
  for (i in seq_along(selected_sensors)) {
    sensor_name <- selected_sensors[i]
    
    update_log(paste("Processing sensor", i, "/", length(selected_sensors), ":", sensor_name))
    
    # Check if files exist
    imp_file <- file.path(raw_data_path, paste0(sensor_name, ".IMP"))
    hig_file <- file.path(raw_data_path, paste0(sensor_name, ".HIG"))
    
    if (file.exists(imp_file) && file.exists(hig_file)) {
      tryCatch({
        result <- py$process_imp_hig_direct(imp_file, hig_file, output_dir)
        combined_data <- result[[1]]
        summary_info <- result[[2]]
        
        # Check for errors
        if (grepl("TIME:", summary_info$messages)) {
          n_files_w_time <- n_files_w_time + 1
        }
        if (grepl("HIG:", summary_info$messages)) {
          n_files_w_hig <- n_files_w_hig + 1
        }
        if (grepl("PRES:", summary_info$messages)) {
          n_files_w_pres <- n_files_w_pres + 1
        }
        
        summary_info$file <- sensor_name
        summary_data[[length(summary_data) + 1]] <- summary_info
        
        update_log(paste(sensor_name, "processed successfully"))
        
      }, error = function(e) {
        error_msg <- paste("Error processing", sensor_name, ":", e$message)
        update_log(error_msg)
      })
    } else {
      missing_msg <- paste("Missing IMP or HIG file for sensor:", sensor_name)
      update_log(missing_msg)
    }
  }
  
  # Create summary CSV
  if (length(summary_data) > 0) {
    # Convert to data frame
    summary_df <- do.call(rbind, lapply(summary_data, function(x) {
      as.data.frame(x, stringsAsFactors = FALSE)
    }))
    
    # Save to CSV
    summary_csv_filename <- paste0(current_date, "_batch_summary.csv")
    summary_csv_path <- file.path(output_dir, summary_csv_filename)
    write.csv(summary_df, summary_csv_path, row.names = FALSE)
    
    update_log("Batch sensor processing complete.")
    update_log(paste(length(summary_data), "total sensors processed"))
    update_log(paste(n_files_w_pres, "/", length(summary_data), "sensors contain pressure data errors"))
    update_log(paste(n_files_w_time, "/", length(summary_data), "sensors contain time series errors"))
    update_log(paste(n_files_w_hig, "/", length(summary_data), "sensors contain strike/collision event (HIG ≥ 400g)"))
    
    return(list(
      summary_data = summary_data,
      log_messages = log_messages
    ))
  } else {
    update_log("No sensors were successfully processed.")
    return(list(
      summary_data = list(),
      log_messages = log_messages
    ))
  }
}