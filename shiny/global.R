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

get_sensor_index_file <- function(output_dir) {
  index_file <- file.path(output_dir, "uoh_sensor_index.csv")
  if (file.exists(index_file)) {
    return(index_file)
  } else {
    return(NULL)
  }
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
  sensor_row <- index_df[index_df$file == sensor_name, ]
  
  if (nrow(sensor_row) == 0) {
    return(list(available = FALSE))
  }
  
  # Check for nadir data columns
  if ("pres_min.time." %in% names(sensor_row) && "pres_min.kPa." %in% names(sensor_row)) {
    return(list(
      time = as.numeric(sensor_row[["pres_min.time."]]),
      value = as.numeric(sensor_row[["pres_min.kPa."]]),
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
