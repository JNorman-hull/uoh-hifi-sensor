# Pressure Analysis Helper Functions

# Find max pressure 1s before nadir
rpc_find_max_pres <- function(sensor_data, nadir_time) {
  # Calculate 1 second before nadir (2000 rows at 2000Hz)
  search_start <- nadir_time - 1.0
  
  # Filter data to 1 second before nadir
  search_window <- sensor_data[sensor_data$time_s >= search_start & sensor_data$time_s < nadir_time, ]
  
  if (nrow(search_window) == 0) {
    return(list(pres_max_1s_nadir.kPa. = NA, pres_max_1s_nadir.time. = NA))
  }
  
  # Find maximum pressure in this window
  max_idx <- which.max(search_window$pressure_kpa)
  max_pressure <- search_window$pressure_kpa[max_idx]
  max_time <- search_window$time_s[max_idx]
  
  return(list(
    pres_max_1s_nadir.kPa. = round(max_pressure, 2),
    pres_max_1s_nadir.time. = round(max_time, 2)
  ))
}

# Function: rpc_calculate
# Calculate the rate of pressure change
rpc_calculate <- function(nadir_pressure, max_pressure_1s_before) {
  if (is.na(nadir_pressure) || is.na(max_pressure_1s_before)) {
    return(NA)
  }
  
  # Rate of pressure change = nadir - max_pressure_1s_before (pressure drop)
  rpc <- max_pressure_1s_before - nadir_pressure
  return(round(rpc, 2))
}

# Function: lrpc_get_acclim
# Identify surface and depth acclimation values from the configuration
lrpc_get_acclim <- function(pressure_config) {
  if (is.null(pressure_config)) {
    return(list(acclim_pres_surface = NA, acclim_pres_depth = NA))
  }
  
  return(list(
    acclim_pres_surface = pressure_config$acclim_pres_surface,
    acclim_pres_depth = pressure_config$acclim_pres_depth
  ))
}

# Function: lrpc_calculate
# Calculate Log Ratio Pressure change for each acclimation
lrpc_calculate <- function(nadir_pressure, acclim_surface, acclim_depth) {
  results <- list()
  
  # Calculate LRPC for surface acclimation
  if (!is.na(nadir_pressure) && !is.na(acclim_surface) && nadir_pressure > 0) {
    results$pres_lrpc_surface <- round(log(acclim_surface / nadir_pressure), 4)
  } else {
    results$pres_lrpc_surface <- NA
  }
  
  # Calculate LRPC for depth acclimation
  if (!is.na(nadir_pressure) && !is.na(acclim_depth) && nadir_pressure > 0) {
    results$pres_lrpc_depth <- round(log(acclim_depth / nadir_pressure), 4)
  } else {
    results$pres_lrpc_depth <- NA
  }
  
  return(results)
}

# Function: get_roi4_nadir_info
# Get nadir info specifically from ROI 4 in instrument index
get_roi4_nadir_info <- function(sensor_name, output_dir) {
  instrument_df <- get_instrument_index_file(output_dir, read_data = TRUE)
  
  if (is.null(instrument_df)) {
    return(list(available = FALSE, value = NA, time = NA))
  }
  
  tryCatch({
    # Get ROI 4 nadir data specifically
    roi4_row <- instrument_df[instrument_df$file == sensor_name & instrument_df$roi == "roi4_nadir", ]
    
    if (nrow(roi4_row) == 0) {
      return(list(available = FALSE, value = NA, time = NA))
    }
    
    nadir_pressure <- roi4_row$pres_min.kPa.
    nadir_time <- roi4_row$pres_min.time.
    
    if (is.na(nadir_pressure) || is.na(nadir_time)) {
      return(list(available = FALSE, value = NA, time = NA))
    }
    
    return(list(
      available = TRUE,
      value = as.numeric(nadir_pressure),
      time = as.numeric(nadir_time)
    ))
    
  }, error = function(e) {
    return(list(available = FALSE, value = NA, time = NA))
  })
}

# Function: rate_ratio_analysis
# Calculate Rate and Log ratio pressure change
rate_ratio_analysis <- function(sensor_name, output_dir, pressure_config) {
  tryCatch({
    # Read delineated data
    sensor_data <- read_sensor_data(output_dir, sensor_name, "delineated")
    
    if (is.null(sensor_data)) {
      stop("Failed to read delineated dataset")
    }
    
    # Get ROI 4 nadir info from instrument index
    roi4_nadir <- get_roi4_nadir_info(sensor_name, output_dir)
    
    if (!roi4_nadir$available) {
      stop("ROI 4 nadir information not available. Please calculate pressure summary first.")
    }
    
    # Get max pressure 1s before nadir
    max_pres_results <- rpc_find_max_pres(sensor_data, roi4_nadir$time)
    
    # Calculate RPC
    pres_rpc <- rpc_calculate(roi4_nadir$value, max_pres_results$pres_max_1s_nadir.kPa.)
    
    # Get acclimation values from config
    acclim_values <- lrpc_get_acclim(pressure_config)
    
    # Calculate LRPC
    lrpc_results <- lrpc_calculate(roi4_nadir$value, 
                                   acclim_values$acclim_pres_surface, 
                                   acclim_values$acclim_pres_depth)
    
    # Prepare updates for instrument index
    updates <- list(
      pres_max_1s_nadir.kPa. = max_pres_results$pres_max_1s_nadir.kPa.,
      pres_max_1s_nadir.time. = max_pres_results$pres_max_1s_nadir.time.,
      pres_rpc = pres_rpc,
      pres_acclim_pres_surface = acclim_values$acclim_pres_surface,
      pres_acclim_pres_depth = acclim_values$acclim_pres_depth,
      pres_lrpc_surface = lrpc_results$pres_lrpc_surface,
      pres_lrpc_depth = lrpc_results$pres_lrpc_depth
    )
    
    # Save to instrument index for overall ROI
    success <- safe_update_instrument_index(output_dir, sensor_name, "overall", updates)
    
    if (!success) {
      stop("Failed to save RPC/LRPC results to instrument index")
    }
    
    # Update sensor status flags
    sensor_updates <- list(
      pres_rpc_processed = "Y",
      pres_lrpc_processed = "Y",
      all_pres_processed = "Y",
      pres_config = pressure_config$label
    )
    
    success_sensor <- safe_update_sensor_index(output_dir, sensor_name, sensor_updates)
    
    if (!success_sensor) {
      stop("Failed to update sensor status flags")
    }
    
    return(list(success = TRUE, updates = updates))
    
  }, error = function(e) {
    return(list(success = FALSE, error = e$message))
  })
}

# Function: generate_pressure_text
# Generate the output text used in the Sensor pressure summary box
generate_pressure_text <- function(sensor_name, output_dir) {
  # Get instrument data for this sensor
  instrument_df <- get_instrument_index_file(output_dir, read_data = TRUE)
  
  if (is.null(instrument_df)) {
    return(list(
      nadir_text = "Pressure data not available",
      rpc_text = "",
      lrpc_surface_text = "",
      lrpc_depth_text = ""
    ))
  }
  
  tryCatch({
    # Get overall ROI data for this sensor
    sensor_row <- instrument_df[instrument_df$file == sensor_name & instrument_df$roi == "overall", ]
    
    if (nrow(sensor_row) == 0) {
      return(list(
        nadir_text = "Pressure analysis not completed",
        rpc_text = "",
        lrpc_surface_text = "",
        lrpc_depth_text = ""
      ))
    }
    
    # Extract values - use ROI 4 nadir if available, otherwise show empty
    roi4_nadir <- get_roi4_nadir_info(sensor_name, output_dir)
    
    nadir_pressure <- if (roi4_nadir$available) roi4_nadir$value else NA
    rpc_value <- sensor_row$pres_rpc
    lrpc_surface <- sensor_row$pres_lrpc_surface
    lrpc_depth <- sensor_row$pres_lrpc_depth
    
    # Format text outputs
    nadir_text <- if (!is.na(nadir_pressure)) {
      paste("Pressure nadir =", round(nadir_pressure, 2), "kPa")
    } else {
      "Pressure nadir: Not available"
    }
    
    rpc_text <- if (!is.na(rpc_value)) {
      paste("Rate pressure change =", round(rpc_value, 2), "kPa")
    } else {
      ""
    }
    
    lrpc_surface_text <- if (!is.na(lrpc_surface)) {
      paste("Surface acclimated LRPC =", round(lrpc_surface, 4))
    } else {
      ""
    }
    
    lrpc_depth_text <- if (!is.na(lrpc_depth)) {
      paste("Depth acclimated LRPC =", round(lrpc_depth, 4))
    } else {
      ""
    }
    
    return(list(
      nadir_text = nadir_text,
      rpc_text = rpc_text,
      lrpc_surface_text = lrpc_surface_text,
      lrpc_depth_text = lrpc_depth_text
    ))
    
  }, error = function(e) {
    return(list(
      nadir_text = "Error reading pressure data",
      rpc_text = "",
      lrpc_surface_text = "",
      lrpc_depth_text = ""
    ))
  })
}

pressureUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Introductory text at the top
    tagList(
      h3("Pressure Analysis"),
      plotModuleUI(ns("pressure_plot"), height = "600px"),
      br(),
    
      fluidRow(
        column(
          width = 12,
          div(
            style = "background-color: #f8f9fa; border: 1px solid #ccc; padding: 15px; 
                   border-radius: 5px; margin-top: 20px;",
            fluidRow(
              column(
                width = 8,
                summarytableModuleUI(ns("pressure_summary"))
              ),
              column(
                width = 4,
                 tags$h4("Sensor pressure summary", style = "margin-top: 0; color: #333;"),
                tags$p(textOutput(ns("pressure_nadir_text"))),
                tags$p(textOutput(ns("rpc_text"))),
                tags$p(textOutput(ns("lrpc_surface_text"))),
                tags$p(textOutput(ns("lrpc_depth_text")))
              )
            )
          )
        )
      ),
      br(),
      fluidRow(
            column(
        width = 6,
        div(
          style = "background-color: #f8f9fa; border: 1px solid #ccc; padding: 20px; 
                   border-radius: 5px; margin-bottom: 20px;",
          tags$h4("Barotrauama assesment", style = "margin-top: 0; color: #333;"),
          p("Hav a species index table (baro.csv) which has (species, age, size, sample size, mortality threshold). Asses our paramters against all thresholds and produce a summary report which is printed in the box, as well as saved  in ./assesment.
          Only active when all calculations have bene done.
          Uses same logic as elsewhere for managing when data is overwritten etc.")
        )
      )
    )
  ))
}

#Method 1: helpText() - styled for instructions
#helpText("This sidebar controls the pressure configuration for sensor data."),

# Method 2: p() - regular paragraph
#p("Configure pressure parameters below:"),


pressureSidebarUI <- function(id) {
  ns <- NS(id)
  
  # CSS for subtle scrollbar that appears on interaction
  scroll_css <- HTML("
    <style>
      /* Hide scrollbar by default */
      .scrollable-sidebar::-webkit-scrollbar {
        width: 8px;
        background-color: transparent;
      }
      
      /* Show scrollbar track on hover or when scrolling */
      .scrollable-sidebar:hover::-webkit-scrollbar-track,
      .scrollable-sidebar:active::-webkit-scrollbar-track,
      .scrollable-sidebar:focus::-webkit-scrollbar-track {
        background: rgba(240,240,240,0.5);
        border-radius: 4px;
      }
      
      /* Show scrollbar thumb on hover or when scrolling */
      .scrollable-sidebar:hover::-webkit-scrollbar-thumb,
      .scrollable-sidebar:active::-webkit-scrollbar-thumb,
      .scrollable-sidebar:focus::-webkit-scrollbar-thumb {
        background: rgba(180,180,180,0.5);
        border-radius: 4px;
      }
      
      /* Show scrollbar thumb when scrolling */
      .scrollable-sidebar::-webkit-scrollbar-thumb:vertical:active {
        background: rgba(150,150,150,0.7);
      }
    </style>
  ")
  
  tagList(
    scroll_css,
    div(class = "scrollable-sidebar", 
        style = "height: 90vh; overflow-y: auto; padding-right: 5px;",
        
        h4("Pressure controls"),
        
        div(style = "color: #666; font-style: italic; margin-bottom: 15px;",
            "Select a sensor to begin pressure analysis."),
        
        #status display
        statusSidebarUI(ns("status_display"),
                        show_pres_processed = TRUE,
                        show_pres_processed_sum = TRUE,
                        show_pres_processed_rpc = TRUE,
                        show_pres_processed_lrpc = TRUE),
        
        enhancedSensorSelectionUI(ns("sensor_selector"), status_filter_type = "pres_processed"),
        
        h4(""),
        
        summarytableSidebarUI(ns("pressure_summary")),
        
        actionButton(ns("rpc_lrpc_btn"), "Calculate RPC and LRPC", 
                     class = "btn-primary btn-block"),
        
        textOutput(ns("current_rpc_lrpc")),
        
        hr(),
        configurationSidebarUI(ns("pressure_config"), config_type = "pres", 
                               label = "Pressure configuration:"),
        
        h4("Pressure Parameters"),
        div(style = "margin-bottom: 10px;",
            numericInput(ns("acclim_pres_surface"), "Surface acclimation (kPa):", 
                         value = NULL, min = 0, max = 200, step = 0.1, width = "100%")),
        
        div(style = "margin-bottom: 10px;",
            numericInput(ns("acclim_pres_depth"), "Depth acclimation (kPa):", 
                         value = NULL, min = 0, max = 200, step = 0.1, width = "100%")),
        
        div(style = "margin-bottom: 10px;",
            numericInput(ns("hydrostatic_pressure"), "Hydrostatic pressure:", 
                         value = NULL, min = 0, max = 20, step = 0.01, width = "100%")),
        
        div(style = "margin-bottom: 10px;",
            numericInput(ns("nadir_threshold"), "Nadir threshold:", 
                         value = NULL, min = 0, max = 20, step = 0.01, width = "100%")),
        
        div(style = "margin-bottom: 10px;",
            numericInput(ns("rpc_threshold"), "RPC threshold:", 
                         value = NULL, min = 0, max = 20, step = 0.01, width = "100%")),
        
        div(style = "margin-bottom: 10px;",
            numericInput(ns("lrpc_threshold"), "LRPC threshold:", 
                         value = NULL, min = 0, max = 20, step = 0.01, width = "100%")),
        
        div(style = "margin-bottom: 15px;",
            textInput(ns("pressure_config_label"), "Configuration label:", 
                      value = "", width = "100%", placeholder = "e.g., Custom_depth_config")),
        
        actionButton(ns("save_pressure_config"), "Save Configuration", 
                     class = "btn-success btn-block"),
        
        textOutput(ns("pressure_config_status")),
        
        hr(), h4("Plot controls"),
        plotSidebarUI(ns("pressure_plot"), 
                      show_left_var = TRUE,   
                      show_right_var = TRUE,    
                      show_normalized = TRUE,   
                      show_nadir = TRUE,      
                      show_roi_markers = TRUE,   
                      show_legend = TRUE,
                      show_plot_width = TRUE,
                      show_plot_height = TRUE,
                      default_plot_height = 8,
                      default_plot_width = 16,
                      default_show_normalized = FALSE,
                      default_show_nadir = TRUE,
                      default_show_roi_markers = TRUE,
                      default_show_legend = FALSE,
                      default_left_var = "pressure_kpa",
                      default_right_var = "none") 
    )
  )
}

pressureServer <- function(id, raw_data_path, output_dir, processing_complete, 
                           session_state = NULL, global_sensor_state, 
                           trigger_data_update, trigger_summary_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ============================= #
    # /// Reactive values \\\ ####  
    # ============================= #   

# pressure state ####
    pressure_values <- reactiveValues(
      pressure_config = NULL,
      baseline_config = NULL,
      inputs_changed = FALSE
    )
    
# Get roi boundaries ####
    roi_boundaries <- reactive({
      get_roi_boundaries(sensor_selector$selected_sensor(), output_dir(), TRUE)
    })
    
    sensor_status <- reactive({
      req(sensor_selector$selected_sensor())
      global_sensor_state$summary_updated  # Use global
      global_sensor_state$data_updated     # Use global
      get_sensor_status(sensor_selector$selected_sensor(), output_dir())
    })
    
    # ============================= #
    # /// Data loading & processing  \\\ ####  
    # ============================= # 
    
    sensor_selector <- enhancedSensorSelectionServer("sensor_selector", output_dir, 
                                                     processing_complete, 
                                                     status_filter_type = "pres_processed",
                                                     session_state = session_state,
                                                     global_sensor_state = global_sensor_state,
                                                     trigger_summary_update = trigger_summary_update)  
    # Read selected sensor data
    selected_sensor_data <- reactive({
      req(sensor_selector$selected_sensor())
      global_sensor_state$data_updated
      global_sensor_state$summary_updated 

      
      # Check for delineated file first, fall back to minimal data
      delineated_data <- read_sensor_data(output_dir(), sensor_selector$selected_sensor(), "delineated")
      if (!is.null(delineated_data)) {
        return(delineated_data)
      }
      
      return(read_sensor_data(output_dir(), sensor_selector$selected_sensor(), "min"))
    })
    
    # Get nadir info using shared function
    nadir_info <- reactive({
      req(sensor_selector$selected_sensor())
      global_sensor_state$summary_updated  # Use global
      get_nadir_info(sensor_selector$selected_sensor(), output_dir())
    })
    
    # ============================= #
    # /// Pressure configuration \\\ ####  
    # ============================= #
    
    # Load pressure configurations
    pressure_config <- configurationServer("pressure_config",
                                           output_dir = output_dir,
                                           config_type = "pres",
                                           sensor_name = reactive(sensor_selector$selected_sensor()),
                                           auto_select_sensor_config = TRUE)
    
    # Store current config and populate inputs
    observe({
      pressure_values$pressure_config <- pressure_config$current_config()
      pressure_values$baseline_config <- pressure_values$pressure_config
      pressure_values$inputs_changed <- FALSE
      
      config <- pressure_values$pressure_config
      if (!is.null(config)) {
        updateTextInput(session, "pressure_config_label", value = config$label)
        updateNumericInput(session, "acclim_pres_surface", value = config$acclim_pres_surface)
        updateNumericInput(session, "acclim_pres_depth", value = config$acclim_pres_depth)
        updateNumericInput(session, "hydrostatic_pressure", value = config$hydrostatic_pressure)
        updateNumericInput(session, "nadir_threshold", value = config$nadir_threshold)
        updateNumericInput(session, "rpc_threshold", value = config$rpc_threshold)
        updateNumericInput(session, "lrpc_threshold", value = config$lrpc_threshold)
      }
    })
    
    # Track changes in inputs
    observe({
      if (!is.null(pressure_values$baseline_config)) {
        config <- pressure_values$baseline_config
        
        inputs_changed <- (
          input$pressure_config_label != (config$label %||% "") ||
            input$acclim_pres_surface != (config$acclim_pres_surface %||% 0) ||
            input$acclim_pres_depth != (config$acclim_pres_depth %||% 0) ||
            input$hydrostatic_pressure != (config$hydrostatic_pressure %||% 0) ||
            input$nadir_threshold != (config$nadir_threshold %||% 0) ||
            input$rpc_threshold != (config$rpc_threshold %||% 0) ||
            input$lrpc_threshold != (config$lrpc_threshold %||% 0) 
        )
        
        pressure_values$inputs_changed <- inputs_changed
      } else {
        # For no config, check if any field has content
        inputs_changed <- (
          nchar(trimws(input$pressure_config_label)) > 0 ||
            !is.null(input$acclim_pres_surface) ||
            !is.null(input$acclim_pres_depth) ||
            !is.null(input$hydrostatic_pressure) ||
            !is.null(input$nadir_threshold) ||
            !is.null(input$rpc_threshold) ||
            !is.null(input$lrpc_threshold)
        )
        
        pressure_values$inputs_changed <- inputs_changed
      }
    })
    
    # ============================= #
    # /// UI State management \\\ ####  
    # ============================= # 
    
    
    # Store current config in reactive values
    observe({
      pressure_values$pressure_config <- pressure_config$current_config()
    })
    

    # Enable/disable normalized checkbox based on sensor status  
    observe({
      req(sensor_selector$selected_sensor())
      status <- get_sensor_status(sensor_selector$selected_sensor(), output_dir())
      
      if (status$normalized) {
        shinyjs::enable(paste0("pressure_plot-show_normalized"))
      } else {
        shinyjs::disable(paste0("pressure_plot-show_normalized"))
        updateCheckboxInput(session, "pressure_plot-show_normalized", value = FALSE)
      }
    })
    
    # Enable/disable normalized checkbox based on sensor status  
    observe({
      req(sensor_selector$selected_sensor())
      global_sensor_state$summary_updated  # Add this line
      global_sensor_state$data_updated     # Add this line
      
      status <- get_sensor_status(sensor_selector$selected_sensor(), output_dir())
      
      if (status$delineated) {
        shinyjs::enable(paste0("pressure_plot-show_roi_markers"))
      } else {
        shinyjs::disable(paste0("pressure_plot-show_roi_markers"))
        updateCheckboxInput(session, "pressure_plot-show_roi_markers", value = FALSE)
      }
    })  
    
    # Button state management for RPC/LRPC
    observe({
      req(sensor_selector$selected_sensor())
      status <- sensor_status()
      
      # Check if RPC and LRPC already processed
      rpc_processed <- status$pres_rpc_processed %||% FALSE
      lrpc_processed <- status$pres_lrpc_processed %||% FALSE
      both_processed <- rpc_processed && lrpc_processed
      
      # Button enabled when summary is processed but RPC/LRPC not yet done
      can_process_rpc_lrpc <- status$pres_sum_processed && !is.null(pressure_values$pressure_config)
      
      button_states <- list(
        "rpc_lrpc_btn" = can_process_rpc_lrpc
      )
      
      manage_button_states(session, button_states)
      
      # Update button appearance and text
      if (both_processed) {
        updateActionButton(session, "rpc_lrpc_btn", 
                           label = "Recalculate RPC and LRPC")
        shinyjs::removeClass("rpc_lrpc_btn", "btn-primary")
        shinyjs::addClass("rpc_lrpc_btn", "btn-warning")
      } else {
        updateActionButton(session, "rpc_lrpc_btn", 
                           label = "Calculate RPC and LRPC")
        shinyjs::removeClass("rpc_lrpc_btn", "btn-warning") 
        shinyjs::addClass("rpc_lrpc_btn", "btn-primary")
      }
    })
 
    # ============================= #
    # /// Event handlers \\\ ####  
    # ============================= # 
    
    # Auto-uncheck nadir when normalized is checked
    observeEvent(input$`pressure_plot-show_normalized`, {
      if (input$`pressure_plot-show_normalized`) {
        updateCheckboxInput(session, "pressure_plot-show_nadir", value = FALSE)
      }
    })
    
    
    # Handle pressure info addition
    observeEvent(input$add_deploy_btn, {
      if (!is.null(sensor_selector$selected_sensor()) && sensor_selector$selected_sensor() != "") {
        showNotification(paste("Adding pressure info for:", sensor_selector$selected_sensor()), type = "message")
      } else {
        showNotification("Please select a sensor first", type = "warning")
      }
    })
    
    # Save pressure configuration
    observeEvent(input$save_pressure_config, {
      config_name <- trimws(input$pressure_config_label)
      
      if (nchar(config_name) == 0) {
        showNotification("Please enter a configuration label", type = "error")
        return()
      }
      
      # Check if config already exists
      existing_configs <- pressure_config$all_configs()
      if (!is.null(existing_configs) && config_name %in% names(existing_configs)) {
        showModal(modalDialog(
          title = "Configuration Exists",
          paste("Configuration '", config_name, "' already exists. Replace existing configuration?"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_save_pressure_config"), "Replace", class = "btn-warning")
          ),
          size = "m"
        ))
      } else {
        save_pressure_configuration()
      }
    })
    
    # Confirm save pressure configuration
    observeEvent(input$confirm_save_pressure_config, {
      removeModal()
      save_pressure_configuration()
    })
    
    # Button state management for save pressure config
    observe({
      can_save <- pressure_values$inputs_changed && 
        !is.null(input$pressure_config_label) && 
        nchar(trimws(input$pressure_config_label)) > 0
      
      if (can_save) {
        shinyjs::enable("save_pressure_config")
      } else {
        shinyjs::disable("save_pressure_config")
      }
    })
    
    # RPC and LRPC calculation button
    observeEvent(input$rpc_lrpc_btn, {
      req(sensor_selector$selected_sensor())
      
      # Check if data already exists
      status <- sensor_status()
      rpc_processed <- status$pres_rpc_processed %||% FALSE
      lrpc_processed <- status$pres_lrpc_processed %||% FALSE
      
      if (rpc_processed && lrpc_processed) {
        showModal(modalDialog(
          title = "RPC and LRPC Data Exists",
          paste("RPC and LRPC calculations already exist for", sensor_selector$selected_sensor(), 
                ". Replace existing calculations?"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_replace_rpc_lrpc"), "Replace", class = "btn-warning")
          )
        ))
      } else {
        calculate_and_save_rpc_lrpc()
      }
    })
    
    # Confirm replace RPC/LRPC
    observeEvent(input$confirm_replace_rpc_lrpc, {
      removeModal()
      calculate_and_save_rpc_lrpc()
    })
    
    # ============================= #
    # /// Helper functions \\\ ####  
    # ============================= # 
    
    # Save pressure configuration function
    save_pressure_configuration <- function() {
      config_name <- trimws(input$pressure_config_label)
      
      # Create pressure configuration values
      pressure_config_values <- c(
        input$acclim_pres_surface,
        input$acclim_pres_depth,
        input$hydrostatic_pressure,
        input$nadir_threshold,
        input$rpc_threshold,
        input$lrpc_threshold
      )
      
      # Save configuration using shared function
      success <- save_config_value(
        output_dir = output_dir(),
        config_type = "pres",
        key = config_name,
        value = pressure_config_values
      )
      
      if (success) {
        # Reload configurations and trigger global updates
        pressure_config$reload_configs()
        trigger_summary_update()
        
        # Reset change tracking
        pressure_values$inputs_changed <- FALSE
        pressure_values$baseline_config <- list(
          label = config_name,
          acclim_pres_surface = input$acclim_pres_surface,
          acclim_pres_depth = input$acclim_pres_depth,
          hydrostatic_pressure = input$hydrostatic_pressure,
          nadir_threshold = input$nadir_threshold,
          rpc_threshold = input$rpc_threshold,
          lrpc_threshold = input$lrpc_threshold
        )
        
        showNotification("Pressure configuration saved successfully!", type = "message")
      } else {
        showNotification("Failed to save pressure configuration", type = "error")
      }
    }
    
    # Calculate and save RPC/LRPC function
    calculate_and_save_rpc_lrpc <- function() {
      tryCatch({
        # Get required data
        sensor_name <- sensor_selector$selected_sensor()
        
        # Create config from current input values instead of saved config
        config <- list(
          label = input$pressure_config_label %||% "Current_values",
          acclim_pres_surface = input$acclim_pres_surface,
          acclim_pres_depth = input$acclim_pres_depth,
          hydrostatic_pressure = input$hydrostatic_pressure,
          nadir_threshold = input$nadir_threshold,
          rpc_threshold = input$rpc_threshold,
          lrpc_threshold = input$lrpc_threshold
        )
        
        # Validate inputs
        if (is.null(config$acclim_pres_surface) || is.null(config$acclim_pres_depth)) {
          showNotification("Please enter surface and depth acclimation values", type = "error")
          return()
        }
        
        # Perform calculation using helper function
        result <- rate_ratio_analysis(sensor_name, output_dir(), config)
        
        if (result$success) {
          trigger_data_update()
          trigger_summary_update()
          
          showNotification(paste("RPC and LRPC calculated and saved for", sensor_name), 
                           type = "message")
        } else {
          showNotification(paste("Error calculating RPC/LRPC:", result$error), type = "error")
        }
        
      }, error = function(e) {
        showNotification(paste("Error calculating RPC/LRPC:", e$message), type = "error")
      })
    }
    
    # ============================= #
    # /// Output render \\\ ####  
    # ============================= #   
    
    # Pressure summary text outputs
    pressure_summary_text <- reactive({
      req(sensor_selector$selected_sensor())
      global_sensor_state$summary_updated  # Invalidate when data changes
      generate_pressure_text(sensor_selector$selected_sensor(), output_dir())
    })
    
    output$pressure_nadir_text <- renderText({
      pressure_summary_text()$nadir_text
    })
    
    output$rpc_text <- renderText({
      pressure_summary_text()$rpc_text
    })
    
    output$lrpc_surface_text <- renderText({
      pressure_summary_text()$lrpc_surface_text
    })
    
    output$lrpc_depth_text <- renderText({
      pressure_summary_text()$lrpc_depth_text
    })
    
    # RPC/LRPC status output
    output$current_rpc_lrpc <- renderText({
      req(sensor_selector$selected_sensor())
      status <- sensor_status()
      
      rpc_processed <- status$pres_rpc_processed %||% FALSE
      lrpc_processed <- status$pres_lrpc_processed %||% FALSE
      
      if (rpc_processed && lrpc_processed) {
        paste("RPC and LRPC calculated for", sensor_selector$selected_sensor())
      } else {
        ""
      }
    })
    
    # Pressure config status
    output$pressure_config_status <- renderText({
      if (pressure_values$inputs_changed) {
        "Configuration modified - click Save Configuration to save changes"
      } else {
        ""
      }
    })
  
# Pressure status display ####
    status_controls <- statusModuleServer("status_display",
                                          sensor_name_reactive = reactive(sensor_selector$selected_sensor()),
                                          output_dir_reactive = reactive(output_dir()),
                                          check_types = c("pres_processed", "pres_processed_sum",
                                                          "pres_processed_rpc", "pres_processed_lrpc"),
                                          invalidation_trigger = reactive(global_sensor_state$summary_updated),
                                          individual_outputs = TRUE)
    
# Pressure summary display ####
    summary_table <- summarytableModuleServer("pressure_summary", 
                                              sensor_reactive = reactive(sensor_selector$selected_sensor()),
                                              output_dir_reactive = reactive(output_dir()),
                                              instrument_variable = "pres",
                                              global_sensor_state = global_sensor_state,
                                              trigger_data_update = trigger_data_update,
                                              trigger_summary_update = trigger_summary_update)
    
# Pressure plot ####
    plot_controls <- plotModuleServer("pressure_plot", 
                                      sensor_data = selected_sensor_data,
                                      sensor_name = reactive(sensor_selector$selected_sensor()),
                                      nadir_info = nadir_info,
                                      roi_boundaries = roi_boundaries,
                                      right_var = reactive(input$`pressure_plot-right_y_var`),
                                      left_var = reactive(input$`pressure_plot-left_y_var`),
                                      plot_width = reactive(input$`pressure_plot-plot_width`),
                                      plot_height = reactive(input$`pressure_plot-plot_height`),
                                      show_nadir = reactive(input$`pressure_plot-show_nadir`),
                                      show_legend = reactive(input$`pressure_plot-show_legend`),
                                      show_normalized = reactive(input$`pressure_plot-show_normalized`),
                                      show_roi_markers = reactive(input$`pressure_plot-show_roi_markers`),
                                      title_prefix = "Pressure Analysis",
                                      plot_source = "pressure_plot"
    )
    return(list(
      selected_sensor = reactive(sensor_selector$selected_sensor())
    ))
    
  })
}  
  