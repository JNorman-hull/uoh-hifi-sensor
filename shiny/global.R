# Global used to load packages, data, set variable conditions, global functions

#install.packages("shiny")
#install.packages("shinyFiles")
#install.packages("shinyWidgets")

library(shiny)
library(tidyverse)
library(reticulate)
library(DT)  
library(shinyWidgets)  
library(future)
library(promises)
library(shinyjs)

# Initialize future for async operations
plan(multicore)  # Use multisession on Windows

dir.create("./RAPID_Processed", showWarnings = FALSE, recursive = TRUE)

# Import our modified Python functions (with no main block)
source_python("rapid_functions.py")

# Create a wrapper function to get unique sensor names (without extensions)
get_sensor_names <- function(raw_data_path = "./RAW_data/RAPID") {
  # Find all IMP files
  imp_files <- list.files(path = raw_data_path, pattern = "\\.IMP$", full.names = FALSE)
  
  # Extract base names without extensions
  sensor_names <- tools::file_path_sans_ext(imp_files)
  
  return(sensor_names)
}

# Function to process selected sensors
process_selected_sensors <- function(selected_sensors, 
                                     raw_data_path = "./RAW_data/RAPID", 
                                     output_dir = "./RAPID_Processed") {
  
  # Convert sensors to JSON to ensure correct transmission to Python
  sensor_json <- jsonlite::toJSON(selected_sensors)
  
  # Create a Python function to process only selected sensors
  py_run_string(paste0('
import json

def process_selected_sensors(sensor_json, raw_path="./RAW_data/RAPID", output_path="./RAPID_Processed"):
    from pathlib import Path
    import pandas as pd
    from datetime import datetime
    
    # Parse the JSON string to get the list of sensors
    sensor_list = json.loads(sensor_json)
    
    output_dir = Path(output_path)
    output_dir.mkdir(parents=True, exist_ok=True)
    
    current_date = datetime.now().strftime("%d%m%y")
    summary_data = []
    n_files_w_time = 0
    n_files_w_hig = 0
    n_files_w_pres = 0
    
    log_messages = []
    log_messages.append(f"Processing {len(sensor_list)} selected sensors")
    
    # Process only the selected sensors
    for sensor_name in sensor_list:
        imp_file = Path(raw_path) / f"{sensor_name}.IMP"
        hig_file = Path(raw_path) / f"{sensor_name}.HIG"
        
        if imp_file.exists() and hig_file.exists():
            log_messages.append(f"Processing sensor: {sensor_name}")
            
            try:
                # Process the pair directly
                combined_data, summary_info = process_imp_hig_direct(imp_file, hig_file, output_dir)
                
                # Check for errors
                if "TIME:" in summary_info["messages"]:
                    n_files_w_time += 1
                
                if "HIG:" in summary_info["messages"]:
                    n_files_w_hig += 1
                
                if "PRES:" in summary_info["messages"]:
                    n_files_w_pres += 1
                
                # Add to summary data
                summary_data.append({
                    "file": f"{sensor_name}",
                    "class": "2000_combined",
                    **summary_info
                })
                
                log_messages.append(f"{sensor_name} processed")
                
            except Exception as e:
                log_messages.append(f"Error processing {sensor_name}: {str(e)}")
        else:
            log_messages.append(f"Missing IMP or HIG file for sensor: {sensor_name}")
    
    # Create summary CSV
    if summary_data:
        summary_df = pd.DataFrame(summary_data)
        summary_csv_filename = f"{current_date}_batch_summary.csv"
        summary_csv_path = Path(output_dir) / summary_csv_filename
        summary_df.to_csv(summary_csv_path, index=False)
        
        log_messages.append("Batch sensor processing complete.")
        log_messages.append(f"{len(summary_data)} total sensors processed")
        log_messages.append(f"{n_files_w_pres}/{len(summary_data)} sensors contain pressure data errors")
        log_messages.append(f"{n_files_w_time}/{len(summary_data)} sensors contain time series errors")
        log_messages.append(f"{n_files_w_hig}/{len(summary_data)} sensors contain strike/collision event (HIG ≥ 400g)")
        
        return summary_df.to_dict("records"), log_messages
    else:
        log_messages.append("No sensors were successfully processed.")
        return [], log_messages
  '))
  
  # Call the Python function with JSON
  result <- py$process_selected_sensors(sensor_json, raw_data_path, output_dir)
  
  # Extract results
  summary_data <- result[[1]]
  log_messages <- result[[2]]
  
  return(list(
    summary_data = summary_data,
    log_messages = log_messages
  ))
}