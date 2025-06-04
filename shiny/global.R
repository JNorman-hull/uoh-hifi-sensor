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

#sensor dashboard modules

#sensor processing modules
source("modules/sensor_processing/resultsModule.R")
source("modules/sensor_processing/processingModule.R")
source("modules/sensor_processing/rawdataprocessingModule.R")
source("modules/sensor_processing/deploymentModule.R")

#time series modules
source("modules/time_series_analysis/plotsModule.R")
source("modules/time_series_analysis/roiModule.R")

#instrument modules
source("modules/instrument_analysis/pressureModule.R")

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

## Sensor dropdown management ####

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

## Variable definitions #####
get_sensor_variables <- function() {
  list(
    names = c("pressure_kpa", "higacc_mag_g", "inacc_mag_ms", "rot_mag_degs"),
    labels = c("Pressure [kPa]", "HIG Acceleration [g]", 
               "Inertial Acceleration [m/s²]", "Rotational Magnitude [deg/s]"),
    colors = c("black", "red", "blue", "green")
  )
}

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


## Configuration loader ####

load_config_file <- function(output_dir, config_type) {
  config_file <- file.path("config", paste0(config_type, "_config.txt"))
  
  if (!file.exists(config_file)) {
    warning("Config file not found: ", config_file)
    return(if(config_type == "roi") list() else NULL)
  }
  
  lines <- readLines(config_file)
  lines <- lines[!grepl("^\\s*#", lines)]  # Remove comments
  lines <- lines[nchar(trimws(lines)) > 0]  # Remove empties
  

  if (config_type == "roi") {
    config_list <- list()
    for (line in lines) {
      parts <- strsplit(line, ",")[[1]]
      parts <- trimws(parts)
      if (length(parts) == 8) {  # 7 values + name
        config_list[[parts[1]]] <- list(
          label = parts[1],
          roi1_sens_ingress = as.numeric(parts[2]),
          roi2_inflow_passage = as.numeric(parts[3]),
          roi3_prenadir = as.numeric(parts[4]),
          roi4_nadir = as.numeric(parts[5]),
          roi5_postnadir = as.numeric(parts[6]),
          roi6_outflow_passage = as.numeric(parts[7]),
          roi7_sens_outgress = as.numeric(parts[8])
        )
      }
    }
    return(config_list)
  }

  else if (config_type == "deployment") {
    config_list <- list()
    for (line in lines) {
      if (grepl("=", line)) {
        parts <- strsplit(line, "=")[[1]]
        key <- trimws(parts[1])
        value_string <- trimws(paste(parts[-1], collapse = "="))
        
        value_parts <- strsplit(value_string, ",")[[1]]
        value_parts <- trimws(value_parts)
        
        if (length(value_parts) >= 10) {
          config_list[[key]] <- list(
            label = key,
            site = value_parts[1],
            deployment_id = value_parts[2],
            pump_turbine = value_parts[3],
            type = value_parts[4],
            rpm = type_convert(value_parts[5]),
            head = type_convert(value_parts[6]),
            flow = type_convert(value_parts[7]),
            point_bep = type_convert(value_parts[8]),
            treatment = value_parts[9],
            run = type_convert(value_parts[10])
          )
        }
      }
    }
    return(config_list)
  }

  else if (config_type %in% c("acc", "rot")) {
    config_list <- list()
    
    for (line in lines) {
      if (grepl("=", line)) {
        # Handle threshold lines (key = value)
        parts <- strsplit(line, "=")[[1]]
        key <- trimws(parts[1])
        value <- trimws(paste(parts[-1], collapse = "="))
        config_list[[key]] <- type_convert(value)
      }
      else if (grepl(",", line)) {
        # Handle configuration lines (name, val1, val2, val3)
        parts <- strsplit(line, ",")[[1]]
        parts <- trimws(parts)
        if (length(parts) == 4 && grepl("configuration", parts[1])) {
          config_list[[parts[1]]] <- list(
            label = parts[1],
            param1 = as.numeric(parts[2]),
            param2 = as.numeric(parts[3]), 
            param3 = as.numeric(parts[4])
          )
        }
      }
    }
    return(config_list)
  }

  else if (config_type == "pres") {
    config <- list()
    for (line in lines) {
      if (grepl("=", line)) {
        parts <- strsplit(line, "=")[[1]]
        key <- trimws(parts[1])
        value <- trimws(paste(parts[-1], collapse = "="))
        config[[key]] <- type_convert(value)
      }
    }
    return(config)
  }

  else if (config_type == "directory") {
    config <- list()
    for (line in lines) {
      if (grepl("=", line)) {
        parts <- strsplit(line, "=")[[1]]
        key <- trimws(parts[1])
        value_string <- trimws(paste(parts[-1], collapse = "="))
        
        # Handle multi-part paths
        if (grepl(",", value_string)) {
          path_parts <- strsplit(value_string, ",")[[1]]
          path_parts <- trimws(path_parts)
          # Remove quotes from path parts
          path_parts <- gsub('"', '', path_parts)
          config[[key]] <- path_parts
        } else {
          # Single path value
          config[[key]] <- gsub('"', '', value_string)
        }
      }
    }
    return(config)
  }
}

## Configuration saver ####

save_config_value <- function(output_dir, config_type, key, value, append = TRUE) {
  config_file <- file.path("config", paste0(config_type, "_config.txt"))
  
  tryCatch({

    if (config_type == "roi") {
      if (length(value) != 7) stop("ROI config requires exactly 7 values")
      line <- paste(key, paste(value, collapse = ", "), sep = ", ")
      write(line, file = config_file, append = append)
    }

    else if (config_type == "deployment") {
      if (is.list(value)) {
        # If value is a list with deployment fields
        deployment_string <- paste(value$site,
          value$deployment_id, value$pump_turbine, value$type,
          value$rpm, value$head, value$flow, value$point_bep,
          value$treatment, value$run,
          sep = ", "
        )
        line <- paste(key, "=", deployment_string)
      } else if (is.vector(value) && length(value) >= 10) {
        # If value is a vector with 10+ elements
        line <- paste(key, "=", paste(value, collapse = ", "))
      } else {
        stop("Deployment config requires 10 values or a structured list")
      }
      
      if (append && file.exists(config_file)) {
        lines <- readLines(config_file)
        key_line <- grep(paste0("^\\s*", key, "\\s*="), lines)
        if (length(key_line) > 0) {
          lines[key_line[1]] <- line
        } else {
          lines <- c(lines, line)
        }
        writeLines(lines, config_file)
      } else {
        writeLines(line, config_file)
      }
    }

    else if (config_type %in% c("acc", "rot")) {
      if (grepl("configuration", key)) {
        # Configuration line format: name, val1, val2, val3
        if (is.list(value)) {
          line <- paste(key, value$param1, value$param2, value$param3, sep = ", ")
        } else if (length(value) == 3) {
          line <- paste(key, paste(value, collapse = ", "), sep = ", ")
        } else {
          stop("Configuration requires exactly 3 numeric values")
        }
      } else {
        # Threshold line format: key = value
        line <- paste(key, "=", value)
      }
      
      if (append && file.exists(config_file)) {
        lines <- readLines(config_file)
        if (grepl("configuration", key)) {
          # Look for configuration line
          key_line <- grep(paste0("^\\s*", key, "\\s*,"), lines)
        } else {
          # Look for threshold line
          key_line <- grep(paste0("^\\s*", key, "\\s*="), lines)
        }
        
        if (length(key_line) > 0) {
          lines[key_line[1]] <- line
        } else {
          lines <- c(lines, line)
        }
        writeLines(lines, config_file)
      } else {
        writeLines(line, config_file)
      }
    }

    else if (config_type == "pres") {
      line <- paste(key, "=", value)
      
      if (append && file.exists(config_file)) {
        lines <- readLines(config_file)
        key_line <- grep(paste0("^\\s*", key, "\\s*="), lines)
        if (length(key_line) > 0) {
          lines[key_line[1]] <- line
        } else {
          lines <- c(lines, line)
        }
        writeLines(lines, config_file)
      } else {
        writeLines(line, config_file)
      }
    }

    else if (config_type == "directory") {
      if (is.vector(value) && length(value) > 1) {
        # Multi-part path: join with commas and add quotes if needed
        path_string <- paste(paste0('"', value, '"'), collapse = ", ")
        line <- paste(key, "=", path_string)
      } else {
        # Single path value
        line <- paste(key, "=", value)
      }
      
      if (append && file.exists(config_file)) {
        lines <- readLines(config_file)
        key_line <- grep(paste0("^\\s*", key, "\\s*="), lines)
        if (length(key_line) > 0) {
          lines[key_line[1]] <- line
        } else {
          lines <- c(lines, line)
        }
        writeLines(lines, config_file)
      } else {
        writeLines(line, config_file)
      }
    }

    return(TRUE)
  }, error = function(e) {
    warning("Failed to save config: ", e$message)
    return(FALSE)
  })
}


type_convert <- function(x) {
  x <- trimws(x)
  num_val <- suppressWarnings(as.numeric(x))
  if (!is.na(num_val)) return(num_val)
  if (tolower(x) %in% c("true", "false", "t", "f")) return(tolower(x) %in% c("true", "t"))
  return(x)
}

# Get ROI boundaries for plots ####

## Get ROI boundaries ####
get_roi_boundaries <- function(sensor_name, output_dir, show_roi = FALSE) {
  if (!show_roi || is.null(sensor_name) || sensor_name == "") {
    return(NULL)
  }
  
  # Check if sensor is delineated and trimmed
  status <- get_sensor_status(sensor_name, output_dir)
  if (!status$delineated || !status$trimmed) {
    return(NULL)
  }
  
  # Read delineated data
  sensor_data <- read_sensor_data(output_dir, sensor_name, "delineated")
  if (is.null(sensor_data) || !"roi" %in% names(sensor_data)) {
    return(NULL)
  }
  
  # ROI levels for trimmed data
  roi_levels <- c("roi1_sens_ingress", "roi2_inflow_passage", 
                  "roi3_prenadir", "roi4_nadir", "roi5_postnadir", 
                  "roi6_outflow_passage", "roi7_sens_outgress")
  
  # Create boundaries array to match plot function expectations (10 elements)
  boundaries <- numeric(10)
  boundaries[1] <- min(sensor_data$time_s)  # Data start
  boundaries[10] <- max(sensor_data$time_s)  # Data end
  
  # Find ROI start times
  for (i in seq_along(roi_levels)) {
    roi_data <- sensor_data[sensor_data$roi == roi_levels[i], ]
    if (nrow(roi_data) > 0) {
      boundaries[i + 1] <- min(roi_data$time_s)  # boundaries[2] through boundaries[8]
    }
  }
  
  # ROI 7 end time (boundary[9])
  roi7_data <- sensor_data[sensor_data$roi == "roi7_sens_outgress", ]
  if (nrow(roi7_data) > 0) {
    boundaries[9] <- max(roi7_data$time_s)
  }
  
  return(boundaries)
}


#DATA ANALYSIS#####

#Calculate summary statistics ####

calc_summary_stats <- function(sensor_name, output_dir, isntrument = NULL){
  # Function to calculate summary statistics to be used across instrument analysis types
  
  roi_times <- get_roi_boundaries(sensor_name, output_dir)
  
  isntrument <- list(
    pres = "pressure_kpa",
    acc = list("higacc_mag_g",
                        "inacc_mag_ms"
    ),
    rot = "rot_mag_degs"
  )
  
  # for each roi defined by start and end of boundary), calculate summary statistics
  
  # Create an 'overall' summary too, which does summary on all data

  instrument_summary <- bind_rows(
      data %>%
    group_by(roi) %>%
      summarise(
        med = median(isntrument),
        min = min(isntrument),
        max = max(isntrument),
        IQR = IQR(isntrument),
        .groups = "drop"
      ), data %>%
      mutate(roi = "overall") %>%
      group_by(roi) %>%
      summarise(
        med = median(isntrument),
        min = min(isntrument),
        max = max(isntrument),
        IQR = IQR(isntrument),
        .groups = "drop"
      )
    )
    
  # Write the summary statistics to instrument_index.csv, where 'instrument' the variable name appends the column names e.g., pres_min.kPa. acc_hig_min.g., rot_min.degs.

  
  
  return(instrument_summary)
  #to be used to display the summary in a tidy DT Table in respective instrument module 
}

# ============================= #
# /// Shared plot functions \\\ ####  
# ============================= #  

## Plot configuration ####

get_default_plot_config <- function(config_name = "standard") {
  configs <- list(
    standard = list(
      left_axis = list(var = "pressure_kpa", fixed = FALSE),
      right_axis = list(var = "none", fixed = FALSE),
      show_nadir = TRUE,
      nadir_editable = TRUE,
      show_legend = TRUE,
      title_prefix = "Sensor Data"
    ),
    roi_delineation = list(
      left_axis = list(var = "pressure_kpa", fixed = FALSE),
      right_axis = list(var = "higacc_mag_g", fixed = FALSE),
      show_nadir = TRUE,
      nadir_editable = TRUE,
      show_legend = FALSE,
      title_prefix = "ROI Delineated"
    ),
    pressure_only = list(
      left_axis = list(var = "pressure_kpa", fixed = TRUE),
      right_axis = list(var = "none", fixed = TRUE),
      show_nadir = TRUE,
      nadir_editable = FALSE,
      show_legend = FALSE,
      title_prefix = "Pressure Analysis"
    )
  )
  
  return(configs[[config_name]])
}

## Create plots ####
create_sensor_plot <- function(sensor_data, sensor_name, plot_config = "standard",
                               left_var = NULL, right_var = NULL,
                               nadir_info = NULL, show_nadir = TRUE,
                               selected_nadir = NULL, roi_boundaries = NULL,
                               show_legend = TRUE, plot_source = "sensor_plot",
                               suppress_roi_lines = FALSE, time_var = "time_s") {
  
  # Get configuration
  config <- get_default_plot_config(plot_config)
  sensor_vars <- get_sensor_variables()
  
  # Override config with provided parameters
  if (!is.null(left_var)) config$left_axis$var <- left_var
  if (!is.null(right_var)) config$right_axis$var <- right_var
  config$show_nadir <- show_nadir
  config$show_legend <- show_legend
  
  # Determine left axis
  left_var <- config$left_axis$var
  left_idx <- which(sensor_vars$names == left_var)
  left_color <- sensor_vars$colors[left_idx]
  left_label <- sensor_vars$labels[left_idx]
  
  # Determine right axis
  has_right_axis <- config$right_axis$var != "none"
  right_margin <- if (has_right_axis) 80 else 30
  
  x_label <- if (time_var == "time_norm") "Normalized Time" else "Time [s]"
  
  # Create base plot
  p <- plot_ly() %>%
    layout(
      title = paste(config$title_prefix, sensor_name),
      showlegend = config$show_legend,
      margin = list(l = 80, r = right_margin, t = 50, b = 50),
      xaxis = list(
        title = x_label,  # Updated label
        showline = TRUE,
        linecolor = "black",
        linewidth = 1,
        showticklabels = TRUE,
        ticks = "outside",
        tickcolor = "black"
      ),
      yaxis = list(
        title = left_label,
        showline = TRUE,
        linecolor = "black",
        linewidth = 1,
        showticklabels = TRUE,
        ticks = "outside",
        tickcolor = "black"
      )
    )
  
  # Add left axis trace
  p <- p %>% add_trace(
    x = sensor_data[[time_var]],  # Use dynamic time variable
    y = sensor_data[[left_var]],
    name = left_label,
    type = "scatter",
    mode = "lines",
    line = list(color = left_color)
  )
  
  # Add right axis if configured
  if (has_right_axis) {
    right_var <- config$right_axis$var
    right_idx <- which(sensor_vars$names == right_var)
    right_color <- sensor_vars$colors[right_idx]
    right_label <- sensor_vars$labels[right_idx]
    
    p <- p %>% layout(
      yaxis2 = list(
        title = right_label,
        overlaying = "y",
        side = "right",
        showline = TRUE,
        linecolor = "black",
        linewidth = 1,
        showticklabels = TRUE,
        ticks = "outside",
        tickcolor = "black"
      )
    )
    
    p <- p %>% add_trace(
      x = sensor_data[[time_var]],
      y = sensor_data[[right_var]],
      name = right_label,
      yaxis = "y2",
      type = "scatter",
      mode = "lines",
      line = list(color = right_color)
    )
  }
  
  # Add nadir marker if configured and available
  if (config$show_nadir && !is.null(nadir_info) && nadir_info$available) {
    nadir_yaxis <- NULL
    if (left_var == "pressure_kpa") {
      nadir_yaxis <- "y"
    } else if (has_right_axis && config$right_axis$var == "pressure_kpa") {
      nadir_yaxis <- "y2"
    }
    
    if (!is.null(nadir_yaxis)) {
      p <- p %>% add_trace(
        x = nadir_info$time,
        y = nadir_info$value,
        name = "Pressure Nadir",
        type = "scatter",
        mode = "markers+text",
        marker = list(color = "orange", size = 10),
        text = paste("Nadir:", round(nadir_info$value, 2), "kPa"),
        textposition = "top right",
        textfont = list(color = "orange"),
        yaxis = nadir_yaxis
      )
    }
  }
  
  # Add selected nadir point if provided (for editing mode)
  if (!is.null(selected_nadir)) {
    p <- p %>% add_trace(
      x = selected_nadir$x,
      y = selected_nadir$y,
      name = "Selected Nadir",
      type = "scatter",
      mode = "markers+text",
      marker = list(color = "purple", size = 12, symbol = "diamond"),
      text = paste("New:", round(selected_nadir$y, 2), "kPa"),
      textposition = "top center",
      textfont = list(color = "purple"),
      showlegend = FALSE
    )
  }
  
  # Add ROI boundary lines if provided
  if (!is.null(roi_boundaries) && !suppress_roi_lines) {
    roi_labels <- c("", "ROI 1", "ROI 2", "ROI 3", "ROI 4", "ROI 5", "ROI 6", "ROI 7", "")
    
    for (i in 2:9) {  # Skip first and last boundaries (data start/end)
      p <- p %>% add_segments(
        x = roi_boundaries[i], xend = roi_boundaries[i],
        y = min(sensor_data[[left_var]]), 
        yend = max(sensor_data[[left_var]]),
        line = list(color = "blue", width = 2, dash = "dash"),
        showlegend = FALSE,
        hoverinfo = "text",
        text = paste(roi_labels[i])
      )
    }
  }
  

  # Set plot source for event handling
  p$x$source <- plot_source
  p <- p %>% event_register("plotly_click")
  
  return(p)
}

# End of plot functions #

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



## Define status levels ####
get_status_level_config <- function() {
  list(
    levels = list(
      "0" = list(color = "gray", label = "None", priority = 0),
      "1" = list(color = "red", label = "Error", priority = 1),
      "2" = list(color = "orange", label = "Warning", priority = 2), 
      "3" = list(color = "blue", label = "Info", priority = 3),
      "4" = list(color = "green", label = "Success", priority = 4)
    )
  )
}

## Define  status check types ####
get_status_check_definitions <- function() {
  list(
    
    bad_sensor = list(
      name = "Sensor Quality",
      checks = list(
        list(condition = function(status) status$bad_sens,
             level = 1, message = "Sensor marked as 'bad'"),
        list(condition = function(status) !status$bad_sens,
             level = 4, message = "Sensor marked as 'good'")
      )
    ),
    
    deployment_info = list(
      name = "Deployment Information",
      checks = list(
        list(condition = function(status) status$deployment_info,
             level = 4, message = "Deployment information complete"),
        list(condition = function(status) !status$deployment_info,
             level = 1, message = "Deployment information required")
      )
    ),
    
    delineation = list(
      name = "Delineation Status",
      checks = list(
        list(condition = function(status) !status$delineated, 
             level = 1, message = "Sensor requires delineation"),
        list(condition = function(status) status$delineated && !status$trimmed, 
             level = 2, message = "Sensor file delineated (not trimmed)"),
        list(condition = function(status) status$trimmed, 
             level = 4, message = "Sensor file delineated and trimmed")
      )
    ),
    
    normalization = list(
      name = "Normalization Status", 
      checks = list(
        list(condition = function(status) status$normalized,
             level = 4, message = "Time series normalized"),
        list(condition = function(status) !status$normalized,
             level = 1, message = "Time series requires normalization")
      )
    ),
    
    passage_times = list(
      name = "Passage Times",
      checks = list(
        list(condition = function(status) status$passage_times,
             level = 4, message = "Passage times calculated"),
        list(condition = function(status) !status$passage_times,
             level = 1, message = "Passage times require calculation")
      )
    ),
    
    pres_processed = list(
      name = "Pressure analysis",
      checks = list(
        list(condition = function(status) status$all_pres_processed,
             level = 4, message = "All pressure analysis complete"),
        list(condition = function(status) !status$all_pres_processed,
             level = 1, message = "Pressure processing incomplete ")
      )
    ),
    
    pres_processed_sum = list(
      name = "Pressure analysis (summary)",
      checks = list(
        list(condition = function(status) status$pres_sum_processed,
             level = 4, message = "Pressure sumary calculated"),
        list(condition = function(status) !status$pres_sum_processed,
             level = 1, message = "Pressure summary requires calculation")
      )
    ),
    
    pres_processed_rpc = list(
      name = "Pressure analysis (rpc)",
      checks = list(
        list(condition = function(status) status$pres_rpc_processed,
             level = 4, message = "Rate pressure change calculated"),
        list(condition = function(status) !status$pres_rpc_processed,
             level = 1, message = "Rate pressure change requires calculation")
      )
    ),
    
    pres_processed_lrpc = list(
      name = "Pressure analysis (lrpc)",
      checks = list(
        list(condition = function(status) status$pres_lrpc_processed,
             level = 4, message = "Log ratio pressure change calculated"),
        list(condition = function(status) !status$pres_lrpc_processed,
             level = 1, message = "Log ratio pressure change requires calculation")
      )
    ),
    
    acc_processed = list(
      name = "Acceleration analysis",
      checks = list(
        list(condition = function(status) status$all_acc_processed,
             level = 4, message = "All acceleration analysis complete"),
        list(condition = function(status) !status$all_acc_processed,
             level = 1, message = "Acceleration processing incomplete ")
      )
    ),
    
    acc_processed_sum = list(
      name = "Acceleration analysis (summary)",
      checks = list(
        list(condition = function(status) status$acc_sum_processed,
             level = 4, message = "Acceleration sumary calculated"),
        list(condition = function(status) !status$acc_sum_processed,
             level = 1, message = "Acceleration summary requires calculation")
      )
    ),

    acc_processed_peaks = list(
      name = "Acceleration peaks",
      checks = list(
        list(condition = function(status) status$acc_hig_peaks_processed,
             level = 4, message = "Acceleration peaks processed"),
        list(condition = function(status) !status$acc_hig_peaks_processed,
             level = 1, message = "Acceleration peaks requires calculation")
      )
    ),
    
    acc_processed_strikes = list(
      name = "Acceleration strikes",
      checks = list(
        list(condition = function(status) status$acc_strike_processed,
             level = 4, message = "Acceleration strikes processed"),
        list(condition = function(status) !status$acc_strike_processed,
             level = 1, message = "Acceleration strikes requires calculation")
      )
    ),
    
    acc_processed_collision = list(
      name = "Acceleration collisions",
      checks = list(
        list(condition = function(status) status$acc_collision_processed,
             level = 4, message = "Acceleration collisions processed"),
        list(condition = function(status) !status$acc_collision_processed,
             level = 1, message = "Acceleration collisions requires calculation")
      )
    ),
    
    rotation_processing = list(
      name = "Rotation Analysis",
      checks = list(
        list(condition = function(status) status$all_rot_processed,
             level = 4, message = "All rotation analysis complete"),
        list(condition = function(status) status$rot_sum_processed,
             level = 2, message = "Rotation summary calculated"),
        list(condition = function(status) !status$all_rot_processed,
             level = 1, message = "Rotation analysis requires calculation")
      )
    )
  )
}

## Evaluate status checks ####
evaluate_status_checks <- function(sensor_name, output_dir, 
                                   check_types = c("bad_sens", "deployment_info", "delineation", "normalization",
                                                   "passage_times", "pres_processed", "pres_processed_sum", "pres_processed_rpc",
                                                   "pres_processed_lrpc", "acc_processed", "acc_processed_sum", "acc_processed_peaks",
                                                   "acc_processed_strikes", "acc_processed_collision", "rotation_processing"
                                                   )) {
  if (is.null(sensor_name) || sensor_name == "") {
    return(list(
      level = 0,
      messages = "No sensor selected",
      color = "gray",
      label = "None"
    ))
  }
  
  # Get sensor status
  status <- get_sensor_status(sensor_name, output_dir)
  
  # Get check definitions and level config
  check_definitions <- get_status_check_definitions()
  level_config <- get_status_level_config()
  
  # Evaluate all requested check types
  results <- list()
  highest_level <- 0
  
  for (check_type in check_types) {
    if (check_type %in% names(check_definitions)) {
      check_def <- check_definitions[[check_type]]
      
      # Find first matching condition
      for (check in check_def$checks) {
        if (check$condition(status)) {
          results[[check_type]] <- list(
            level = check$level,
            message = check$message,
            name = check_def$name
          )
          highest_level <- max(highest_level, check$level)
          break
        }
      }
    }
  }
  
  # Get styling for highest level
  highest_config <- level_config$levels[[as.character(highest_level)]]
  
  # Combine messages
  messages <- sapply(results, function(x) x$message)
  messages <- messages[messages != ""]  # Remove empty messages
  
  return(list(
    level = highest_level,
    messages = paste(messages, collapse = "\n"),
    color = highest_config$color,
    label = highest_config$label,
    details = results
  ))
}

## Apply styling to UI #### 
apply_status_styling <- function(session, element_id, color, ns = NULL) {
  full_id <- if (!is.null(ns)) paste0("#", ns(element_id)) else paste0("#", element_id)
  
  shinyjs::runjs(paste0("
    $('", full_id, "').css({
      'color': '", color, "', 
      'font-weight': 'bold'
    });
  "))
}

## Create status display output ####

# use this in modules
# Create individual status display for a single check type
create_individual_status_display <- function(id, sensor_reactive, output_dir_reactive, output, session, check_type, invalidation_trigger = NULL) {
  ns <- session$ns
  
  # Reactive for individual status information  
  status_info <- reactive({
    # Include invalidation trigger if provided
    if (!is.null(invalidation_trigger)) {
      invalidation_trigger()
    }
    
    sensor_name <- sensor_reactive()
    output_dir <- output_dir_reactive()
    
    if (is.null(sensor_name) || sensor_name == "") {
      return(list(level = 0, messages = "", color = "gray"))
    }
    
    # Evaluate only this specific check type
    result <- evaluate_status_checks(sensor_name, output_dir, check_types = c(check_type))
    
    # Return just the message for this check type
    if (check_type %in% names(result$details)) {
      detail <- result$details[[check_type]]
      level_config <- get_status_level_config()
      color <- level_config$levels[[as.character(detail$level)]]$color
      
      return(list(
        level = detail$level,
        message = detail$message,
        color = color
      ))
    } else {
      return(list(level = 0, message = "", color = "gray"))
    }
  })
  
  # Text output
  output[[id]] <- renderText({
    status_info()$message
  })
  
  # Styling observer
  observe({
    info <- status_info()
    if (info$message != "") {  # Only apply styling if there's a message
      apply_status_styling(session, id, info$color, ns)
    }
  })
  
  return(status_info)
}

# End of status functions #


# ============================= #
# /// Shared file selection table module \\\ ####  
# ============================= #


create_sensor_table <- function(
    table_data,
    enable_selection = TRUE,
    selection_mode = 'multiple',
    highlight_config = NULL,  # list(rows = c(), color = 'orange')
    page_length = 15,
    scroll_y = "400px",
    dom_options = 'tip',
    column_widths = NULL,  # list(list(width = '60px', targets = 0))
    state_save = TRUE,
    additional_formatting = NULL,  # function to apply additional formatting
    row_names = FALSE,
    table_class = 'cell-border stripe hover'
) {
  
  # Create base datatable
  dt <- DT::datatable(
    table_data,
    selection = if(enable_selection) list(mode = selection_mode) else 'none',
    options = list(
      pageLength = page_length,
      scrollX = TRUE,
      scrollY = scroll_y,
      dom = dom_options,
      stateSave = state_save,
      columnDefs = column_widths
    ),
    rownames = row_names,
    class = table_class
    )
  
  # Apply base formatting
  dt <- dt %>% DT::formatStyle(columns = 1:ncol(table_data), fontSize = '14px')
  
  # Apply highlighting if configured
  if (!is.null(highlight_config) && length(highlight_config$rows) > 0) {
    dt <- dt %>% DT::formatStyle(
      columns = 1:ncol(table_data),
      target = 'row',
      backgroundColor = DT::styleRow(highlight_config$rows, highlight_config$color)
    )
  }
  
  # Apply any additional custom formatting
  if (!is.null(additional_formatting) && is.function(additional_formatting)) {
    dt <- additional_formatting(dt, table_data)
  }
  
  return(dt)
}

get_sensor_table_column_widths <- function() {
  list(
    list(width = '60px', targets = 0),    # No. column
    list(width = '200px', targets = 1),   # Filename column
    list(width = '80px', targets = 2:4)   # Sensor, Date, Time columns
  )
}

## Get rows to highlight based on processed sensors ####
get_highlight_rows <- function(table_data, processed_sensors, filename_column = "Filename") {
  if (length(processed_sensors) == 0) return(integer(0))
  which(table_data[[filename_column]] %in% processed_sensors)
}


