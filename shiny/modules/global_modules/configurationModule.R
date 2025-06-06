# ============================= #
# /// Configuration Management Module \\\ ####  
# ============================= #

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



configurationSidebarUI <- function(id, 
                                   config_type,
                                   label = NULL) {
  ns <- NS(id)
  
  if (is.null(label)) {
    label <- paste(tools::toTitleCase(config_type), "Configuration:")
  }
  
  tagList(
    selectInput(ns("config_choice"), label, choices = NULL, width = "100%")
  )
}

configurationServer <- function(id, 
                                output_dir,
                                config_type,
                                sensor_name = reactive(NULL),
                                auto_select_sensor_config = TRUE) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      configs = NULL,
      current_config = NULL
    )
    
    # Load configurations and update dropdown
    observe({
      values$configs <- load_config_file(output_dir(), config_type)
      
      if (length(values$configs) > 0) {
        config_names <- names(values$configs)
        choices <- setNames(config_names, gsub("_", " ", config_names))
        selected_value <- config_names[1]
        
        # Auto-select sensor's saved config if enabled
        if (auto_select_sensor_config && !is.null(sensor_name()) && sensor_name() != "") {
          index_df <- get_sensor_index_file(output_dir(), read_data = TRUE)
          if (!is.null(index_df)) {
            tryCatch({
              sensor_row <- index_df[index_df$file == sensor_name(), ]
              config_col <- paste0(config_type, "_config")
              if (nrow(sensor_row) > 0 && config_col %in% names(sensor_row) && 
                  !is.na(sensor_row[[config_col]]) && sensor_row[[config_col]] != "NA") {
                if (sensor_row[[config_col]] %in% config_names) {
                  selected_value <- sensor_row[[config_col]]
                }
              }
            }, error = function(e) {})
          }
        }
        
        updateSelectInput(session, "config_choice", 
                          choices = choices, 
                          selected = selected_value)
      } else {
        updateSelectInput(session, "config_choice", choices = c("No configurations available" = ""))
      }
    })
    
    # Update current config when selection changes
    observe({
      req(roi_config$selected_config_name(), values$configs)
      if (roi_config$selected_config_name() %in% names(values$configs)) {
        values$current_config <- values$configs[[roi_config$selected_config_name()]]
      } else {
        values$current_config <- NULL
      }
    })
    
    # Reload configs function for external use
    reload_configs <- function() {
      values$configs <- load_config_file(output_dir(), config_type)
      
      if (length(values$configs) > 0) {
        config_names <- names(values$configs)
        choices <- setNames(config_names, gsub("_", " ", config_names))
        updateSelectInput(session, "config_choice", choices = choices)
      }
    }
    
    # Return reactive values and functions
    return(list(
      current_config = reactive(values$current_config),
      selected_config_name = reactive(roi_config$selected_config_name()),
      all_configs = reactive(values$configs),
      reload_configs = reload_configs
    ))
  })
}
