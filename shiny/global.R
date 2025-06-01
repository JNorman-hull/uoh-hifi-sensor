library(shiny)
library(tidyverse)
library(reticulate)
library(DT)  
library(shinyjs)
library(plotly)
library(patchwork)
library(shinydashboard)

dir.create("./RAPID_Processed", showWarnings = FALSE, recursive = TRUE)

# Import Python functions
source_python("rapid_functions.py")

# Source all modules
source("modules/fileSelectionModule.R")
source("modules/processingModule.R")
source("modules/resultsModule.R")
source("modules/plotsModule.R")
source("modules/roiModule.R")
source("modules/deploymentModule.R")
source("modules/pressureModule.R")

# ============================= #
# /// Shared helpers \\\ ####  
# ============================= #  

## Get sensor index ####
get_sensor_index_file <- function(output_dir) {
  index_file <- file.path(output_dir, "index", "global_sensor_index.csv")
  if (file.exists(index_file)) {
    return(index_file)
  } else {
    return(NULL)
  }
}

## Safe update index file ####
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
get_sensor_names <- function(raw_data_path = "./RAW_data/RAPID") {
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


## ROI configuration loader ####
# Not sure why this is global, maybe generalise it for a 'config loader'

load_roi_configs <- function(output_dir) {
  config_file <- file.path(output_dir, "config", "roi_config.txt")
  
  if (file.exists(config_file)) {
    config_lines <- readLines(config_file)
    config_list <- list()
    
    for (line in config_lines) {
      if (nchar(trimws(line)) > 0 && !startsWith(trimws(line), "#")) {
        # Parse: Config_name, 0.1, 1.1, 0.3, 0.2, 0.3, 1.1, 0.1
        parts <- trimws(strsplit(line, ",")[[1]])
        if (length(parts) == 8) {
          config_name <- parts[1]
          config_list[[config_name]] <- list(
            label = config_name,
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
    }
    
    return(config_list)
  } else {
    warning("ROI config file not found: ", config_file)
    return(list())
  }
}

save_custom_roi_config <- function(output_dir, roi1, roi2, roi3, roi4, roi5, roi6, roi7, custom_label) {
  config_file <- file.path(output_dir, "config", "roi_config.txt")
  
  tryCatch({
    # Use the provided custom label directly
    config_name <- trimws(custom_label)
    
    # Create new config line
    config_line <- paste(config_name, roi1, roi2, roi3, roi4, roi5, roi6, roi7, sep = ", ")
    
    # Append to file
    write(config_line, file = config_file, append = TRUE)
    
    return(list(status = TRUE, config_name = config_name))
    
  }, error = function(e) {
    warning("Failed to save custom ROI config: ", e$message)
    return(list(status = FALSE, config_name = NULL))
  })
}


# Get ROI boundaries for plots ####

## Get ROI boundaries ####
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
  index_file <- get_sensor_index_file(output_dir)
  if (is.null(index_file)) return(list(delineated = FALSE, trimmed = FALSE, normalized = FALSE, passage_times = FALSE, exists = FALSE))
  
  tryCatch({
    index_df <- read.csv(index_file)
    sensor_row <- index_df[index_df$file == sensor_name, ]
    
    if (nrow(sensor_row) == 0) {
      return(list(delineated = FALSE, trimmed = FALSE, normalized = FALSE, passage_times = FALSE, exists = FALSE))
    }
    
    # Check flags and verify files exist
    delineated_flag <- !is.na(sensor_row$delineated) && sensor_row$delineated == "Y"
    trimmed_flag <- !is.na(sensor_row$trimmed) && sensor_row$trimmed == "Y"
    normalized_flag <- !is.na(sensor_row$normalized) && sensor_row$normalized == "Y"
    passage_times_flag <- !is.na(sensor_row$passage_times) && sensor_row$passage_times == "Y"
    
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
      exists = TRUE
    ))
  }, error = function(e) {
    return(list(delineated = FALSE, trimmed = FALSE, normalized = FALSE, passage_times = FALSE, exists = FALSE))
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
    
    pressure_processing = list(
      name = "Pressure Analysis",
      checks = list(
        list(condition = function(status) status$all_pres_processed,
             level = 4, message = "All pressure analysis complete"),
        list(condition = function(status) status$pres_sum_processed,
             level = 3, message = "Basic pressure analysis complete"),
        list(condition = function(status) status$delineated && status$trimmed,
             level = 1, message = "Pressure analysis required"),
        list(condition = function(status) !status$delineated || !status$trimmed,
             level = 0, message = "")
      )
    ),
    
    acceleration_processing = list(
      name = "Acceleration Analysis", 
      checks = list(
        list(condition = function(status) status$all_acc_processed,
             level = 4, message = "All acceleration analysis complete"),
        list(condition = function(status) status$acc_sum_processed,
             level = 3, message = "Basic acceleration analysis complete"),
        list(condition = function(status) status$delineated && status$trimmed,
             level = 1, message = "Acceleration analysis required"),
        list(condition = function(status) !status$delineated || !status$trimmed,
             level = 0, message = "")
      )
    ),
    
    rotation_processing = list(
      name = "Rotation Analysis",
      checks = list(
        list(condition = function(status) status$all_rot_processed,
             level = 4, message = "All rotation analysis complete"),
        list(condition = function(status) status$rot_sum_processed,
             level = 3, message = "Basic rotation analysis complete"),
        list(condition = function(status) status$delineated && status$trimmed,
             level = 1, message = "Rotation analysis required"),
        list(condition = function(status) !status$delineated || !status$trimmed,
             level = 0, message = "")
      )
    )
  )
}

## Evaluate status checks ####
evaluate_status_checks <- function(sensor_name, output_dir, check_types = c("delineation", "normalization", "passage_times")) {
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