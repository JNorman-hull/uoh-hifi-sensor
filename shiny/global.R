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
  config_file <- file.path(output_dir, "roi_config.txt")
  
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

# Save custom ROI configuration
save_custom_roi_config <- function(output_dir, roi1, roi2, roi3, roi4, roi5, roi6, roi7) {
  config_file <- file.path(output_dir, "roi_config.txt")
  
  tryCatch({
    # Read existing configs to determine next user config number
    existing_configs <- character(0)
    if (file.exists(config_file)) {
      existing_configs <- readLines(config_file)
    }
    
    # Find highest user config number
    user_configs <- grep("^User_configuration", existing_configs, value = TRUE)
    next_num <- 1
    if (length(user_configs) > 0) {
      # Extract numbers from User_configuration# names
      nums <- as.numeric(gsub("User_configuration([0-9]+),.*", "\\1", user_configs))
      nums <- nums[!is.na(nums)]
      if (length(nums) > 0) {
        next_num <- max(nums) + 1
      }
    }
    
    # Create new config line
    config_name <- paste0("User_configuration", next_num)
    config_line <- paste(config_name, roi1, roi2, roi3, roi4, roi5, roi6, roi7, sep = ", ")
    
    # Append to file
    write(config_line, file = config_file, append = TRUE)
    
    return(list(status = TRUE, config_name = config_name))
    
  }, error = function(e) {
    warning("Failed to save custom ROI config: ", e$message)
    return(list(status = FALSE, config_name = NULL))
  })
}

# Create a wrapper function to get unique sensor names (without extensions)
get_sensor_names <- function(raw_data_path = "./RAW_data/RAPID") {
  # Find all IMP files
  imp_files <- list.files(path = raw_data_path, pattern = "\\.IMP$", full.names = FALSE)
  
  # Extract base names without extensions
  sensor_names <- tools::file_path_sans_ext(imp_files)
  
  return(sensor_names)
}

# ========== SHARED PLOTTING FUNCTIONS ==========

# Default plot configurations
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

# Shared sensor plotting function
create_sensor_plot <- function(sensor_data, sensor_name, plot_config = "standard",
                               left_var = NULL, right_var = NULL,
                               nadir_info = NULL, show_nadir = TRUE,
                               selected_nadir = NULL, roi_boundaries = NULL,
                               show_legend = TRUE, plot_source = "sensor_plot",
                               suppress_roi_lines = FALSE) {
  
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
  
  # Create base plot
  p <- plot_ly() %>%
    layout(
      title = paste(config$title_prefix, sensor_name),
      showlegend = config$show_legend,
      margin = list(l = 80, r = right_margin, t = 50, b = 50),
      xaxis = list(
        title = "Time [s]",
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
    x = sensor_data$time_s,
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
      x = sensor_data$time_s,
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