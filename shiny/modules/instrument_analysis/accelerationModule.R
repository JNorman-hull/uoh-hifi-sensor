#Acceleration peak finding functions
# Follow all existing rules for global state management (always updating global state), safe reading/writing index file and instrument file
# Manage button states, update generated text

#function: get_peak_params
# Steps: 
# Retrieve the config values loaded from the configuration loader: height, prominence, interpeak


# function: find_acceleration_peak
# For selected sensor find acceleration peaks in the 'higacc_mag_g' of the _delineated file
# Steps: use params from get_peak_params to detect acceleration peaks in the acceleration time series
# takes the parameters height (e.g., the threshold value of 95g), prominence (e.g., 25g, must rise by 25g above local baseline, scan left and right), interpeak distance (e.g.,  0.002, 4 rows (2 milliseconds). 
# After the peaks are found (peak_list), they are sorted by time series sequentially and apply a prominence filter to ensure peaks satisfy prominence criteria,
# neighboring peaks that don't meet interpeak distance removed by evaluation the distance and keeping the maximum peak
# the peak are listed  as acc_peak_1.time., acc_peak_1.g.


#function: evaluate_peak_type
# Classify each peak as collision or shear based on duration above 70% threshold
# Compute peak duration: For each event, compute the duration above 70% of the peak amplitude
#Classify collision vs shear: Compare the duration to 0.0075 s: shorter means collision, longer means shear
# we run a second filter to determine peak type for each valid peak by compute threshold =  0.7  x peak amplitude, 
# measure duration above threshold, classify collision if duration < 0.0075s. (e.g., 16 rows), Shear if duration ≥ 0.0075s.


#function: generate_peak_text
# Peak text shows number of peaks found and lists each peak value, time and type
# Use placeholder textoutputs in UI

#In summary (with example values from config):
# we 
# Identify candidate peaks by scanning higacc_mag_g and locate maxima that exceed a height (95).
# To validate peak candidate, we ensure it is prominent from local acceleration by scanning left and right and saying it must be at least 25g above local
# To avoid over-counting, we require each peak to be separated by interpeak distance of 0.002 (e.g., 2 milliseconds)
# For each peak now available, we compute the duration above 70% of the peak amplitude
# We classify duration of < 0.0075 as collision, and > 0.0075 as shear

# rough example function, do not copy, but it gives you an idea of what we're aiming for 
# find_acceleration_peaks <- function(accel, timestamps, height = 95, prominence = 25, interpeak_samples = 4, duration_threshold_samples = 15) {
#   stopifnot(length(accel) == length(timestamps))
#   
#   # Step 1: Identify candidate peaks (local maxima above `height`)
#   peaks <- which(diff(sign(diff(accel))) == -2) + 1  # local maxima
#   peaks <- peaks[accel[peaks] >= height]  # apply height filter
#   
#   # Step 2: Apply prominence filter
#   valid_peaks <- peaks[
#     sapply(peaks, function(p) {
#       left_min <- min(accel[max(1, p - interpeak_samples):p])
#       right_min <- min(accel[p:min(length(accel), p + interpeak_samples)])
#       peak_val <- accel[p]
#       (peak_val - max(left_min, right_min)) >= prominence
#     })
#   ]
#   
#   # Step 3: Enforce inter-peak distance by removing close neighbors (keep highest peak in neighborhood)
#   if (length(valid_peaks) > 1) {
#     to_keep <- logical(length(valid_peaks))
#     sorted_peaks <- valid_peaks[order(valid_peaks)]
#     i <- 1
#     while (i <= length(sorted_peaks)) {
#       p <- sorted_peaks[i]
#       window <- which((sorted_peaks > p) & (sorted_peaks <= p + interpeak_samples))
#       close_group <- c(i, window)
#       best_peak <- sorted_peaks[close_group][which.max(accel[sorted_peaks[close_group]])]
#       to_keep[which(sorted_peaks == best_peak)] <- TRUE
#       i <- max(close_group) + 1
#     }
#     valid_peaks <- sorted_peaks[to_keep]
#   }
#   
#   # Step 4: Classify each peak as collision or shear based on duration above 70% threshold
#   peak_results <- lapply(valid_peaks, function(p) {
#     peak_val <- accel[p]
#     threshold <- 0.7 * peak_val
#     
#     # walk left and right until accel drops below threshold
#     left <- p
#     while (left > 1 && accel[left] >= threshold) left <- left - 1
#     right <- p
#     while (right < length(accel) && accel[right] >= threshold) right <- right + 1
#     
#     duration_samples <- right - left
#     peak_type <- if (duration_samples < duration_threshold_samples) "collision" else "shear"
#     
#     list(
#       peak_time = timestamps[p],
#       peak = peak_val,
#       peak_type = peak_type
#     )
#   })
#   
#   do.call(rbind, lapply(peak_results, as.data.frame))
# }

accelerationUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tagList(
      h3("Acceleration Analysis"),
      plotModuleUI(ns("acceleration_plot"), height = "600px"),
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
                summarytableModuleUI(ns("acceleration_summary"))
              ),
              column(
                width = 4,
                tags$h4("Sensor acceleration summary", style = "margin-top: 0; color: #333;"),
                tags$p(textOutput(ns("acceleration_peaks_text"))),
                tags$p(textOutput(ns("acceleration_events_text"))),
                tags$p(textOutput(ns("acceleration_collisions_text")))
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
            tags$h4("Empty box", style = "margin-top: 0; color: #333;"),
            p("Not sure what's going here, yet.")
          )
        )
      )
    ))
}




accelerationSidebarUI <- function(id) {
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
        
        h4("Acceleration controls"),
        
        div(style = "color: #666; font-style: italic; margin-bottom: 15px;",
            "Select a sensor to begin acceleration analysis."),
        
        #status display
        statusSidebarUI(ns("status_display"),
                        show_acc_processed = TRUE,
                        show_acc_processed_sum = TRUE,
                        show_acc_processed_peaks = TRUE),
        
        enhancedSensorSelectionUI(ns("sensor_selector"), status_filter_type = "acc_processed"),
        
        summarytableSidebarUI(ns("acceleration_summary")),
        
        actionButton(ns("acc_peak_btn"), "Calculate acceleration peaks", 
                     class = "btn-primary btn-block"),
        
        textOutput(ns("current_peaks")),
        
        hr(),
        configurationSidebarUI(ns("acceleration_config"), config_type = "acc", 
                               label = "Acceleration configuration:"),
        
        h4("Acceleration Parameters"),
        div(style = "margin-bottom: 10px;",
            numericInput(ns("height"), "Height:", 
                         value = NULL, min = 0, max = 1000, step = 1, width = "100%")),
        
        div(style = "margin-bottom: 10px;",
            numericInput(ns("prominence"), "Prominence:", 
                         value = NULL, min = 0, max = 100, step = 0.1, width = "100%")),
        
        div(style = "margin-bottom: 10px;",
            numericInput(ns("interpeak"), "Interpeak:", 
                         value = NULL, min = 0, max = 1, step = 0.001, width = "100%")),
        
        div(style = "margin-bottom: 10px;",
            numericInput(ns("strike_threshold"), "Strike threshold:", 
                         value = NULL, min = 0, max = 1000, step = 1, width = "100%")),
        
        div(style = "margin-bottom: 10px;",
            numericInput(ns("collision_threshold"), "Collision threshold:", 
                         value = NULL, min = 0, max = 1000, step = 1, width = "100%")),
        
        div(style = "margin-bottom: 10px;",
            numericInput(ns("shear_threshold"), "Shear threshold:", 
                         value = NULL, min = 0, max = 1000, step = 1, width = "100%")),
        
        div(style = "margin-bottom: 15px;",
            textInput(ns("acceleration_config_label"), "Configuration label:", 
                      value = "", width = "100%", placeholder = "e.g., Custom_acceleration_config")),
        
        actionButton(ns("save_acceleration_config"), "Save Configuration", 
                     class = "btn-success btn-block"),
        
        textOutput(ns("acceleration_config_status")),
        
        hr(),
        
        hr(), h4("Plot controls"),
        plotSidebarUI(ns("acceleration_plot"), 
                      show_left_var = TRUE,   
                      show_right_var = TRUE,    
                      show_normalized = TRUE,   
                      show_nadir = TRUE,      
                      show_roi_markers = TRUE,
                      show_acceleration_peaks = TRUE,
                      show_legend = TRUE,
                      show_plot_width = TRUE,
                      show_plot_height = TRUE,
                      default_plot_height = 8,
                      default_plot_width = 16,
                      default_show_normalized = FALSE,
                      default_show_nadir = TRUE,
                      default_show_roi_markers = TRUE,
                      default_show_acceleration_peaks = FALSE, 
                      default_show_legend = FALSE,
                      default_left_var = "pressure_kpa",
                      default_right_var = "higacc_mag_g")
    )
  )
}

accelerationServer <- function(id, raw_data_path, output_dir, processing_complete, session_state = NULL, 
                               global_sensor_state, 
                               trigger_data_update, trigger_summary_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ============================= #
    # /// Reactive values \\\ ####  
    # ============================= #   
    
    # acceleration state ####
    acceleration_values <- reactiveValues(
      acceleration_config = NULL,
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
                                                     status_filter_type = "acc_processed",
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
    # /// UI State management \\\ ####  
    # ============================= # 
    
    
    
    # Enable/disable normalized checkbox based on sensor status  
    observe({
      req(sensor_selector$selected_sensor())
      global_sensor_state$summary_updated 
      global_sensor_state$data_updated     
      
      status <- get_sensor_status(sensor_selector$selected_sensor(), output_dir())
      
      if (status$normalized) {
        shinyjs::enable(paste0("acceleration_plot-show_normalized"))
      } else {
        shinyjs::disable(paste0("acceleration_plot-show_normalized"))
        updateCheckboxInput(session, "acceleration_plot-show_normalized", value = FALSE)
      }
    })
    
    # Enable/disable normalized checkbox based on sensor status  
    observe({
      req(sensor_selector$selected_sensor())
      global_sensor_state$summary_updated  
      global_sensor_state$data_updated     
      
      status <- get_sensor_status(sensor_selector$selected_sensor(), output_dir())
      
      if (status$delineated) {
        shinyjs::enable(paste0("acceleration_plot-show_roi_markers"))
      } else {
        shinyjs::disable(paste0("acceleration_plot-show_roi_markers"))
        updateCheckboxInput(session, "acceleration_plot-show_roi_markers", value = FALSE)
      }
    })  
    
    # Auto-uncheck nadir when normalized is checked
    observeEvent(input$`acceleration_plot-show_normalized`, {
      if (input$`acceleration_plot-show_normalized`) {
        updateCheckboxInput(session, "acceleration_plot-show_nadir", value = FALSE)
      }
    })
    
    
    # Button state management for acceleration peaks
    observe({
      req(sensor_selector$selected_sensor())
      status <- sensor_status()
      
      # Check if peaks already processed
      peaks_processed <- status$acc_hig_peaks_processed %||% FALSE
      
      # Button enabled when summary is processed
      can_process_peaks <- status$acc_sum_processed
      
      button_states <- list(
        "acc_peak_btn" = can_process_peaks
      )
      
      manage_button_states(session, button_states)
      
      # Update button appearance and text
      if (peaks_processed) {
        updateActionButton(session, "acc_peak_btn", 
                           label = "Recalculate acceleration peaks")
        shinyjs::removeClass("acc_peak_btn", "btn-primary")
        shinyjs::addClass("acc_peak_btn", "btn-warning")
      } else {
        updateActionButton(session, "acc_peak_btn", 
                           label = "Calculate acceleration peaks")
        shinyjs::removeClass("acc_peak_btn", "btn-warning") 
        shinyjs::addClass("acc_peak_btn", "btn-primary")
      }
    })
    
    observe({
      req(sensor_selector$selected_sensor())
      global_sensor_state$summary_updated  
      global_sensor_state$data_updated     
      
      status <- get_sensor_status(sensor_selector$selected_sensor(), output_dir())
      
      if (status$acc_hig_peaks_processed) {
        shinyjs::enable(paste0("acceleration_plot-show_acceleration_peaks"))
      } else {
        shinyjs::disable(paste0("acceleration_plot-show_acceleration_peaks"))
        updateCheckboxInput(session, "acceleration_plot-show_acceleration_peaks", value = FALSE)
      }
    })
    
    # ============================= #
    # /// Acceleration configuration \\\ ####  
    # ============================= #
    
    # Load acceleration configurations
    acceleration_config <- configurationServer("acceleration_config",
                                               output_dir = output_dir,
                                               config_type = "acc",
                                               sensor_name = reactive(sensor_selector$selected_sensor()),
                                               auto_select_sensor_config = TRUE)
    
    # Store current config and populate inputs
    observe({
      acceleration_values$acceleration_config <- acceleration_config$current_config()
      acceleration_values$baseline_config <- acceleration_values$acceleration_config
      acceleration_values$inputs_changed <- FALSE
      
      config <- acceleration_values$acceleration_config
      if (!is.null(config)) {
        updateTextInput(session, "acceleration_config_label", value = config$label)
        updateNumericInput(session, "height", value = config$height)
        updateNumericInput(session, "prominence", value = config$prominence)
        updateNumericInput(session, "interpeak", value = config$interpeak)
        updateNumericInput(session, "strike_threshold", value = config$strike_threshold)
        updateNumericInput(session, "collision_threshold", value = config$collision_threshold)
        updateNumericInput(session, "shear_threshold", value = config$shear_threshold)
      }
    })
    
    # Track changes in inputs
    observe({
      if (!is.null(acceleration_values$baseline_config)) {
        config <- acceleration_values$baseline_config
        
        inputs_changed <- (
          input$acceleration_config_label != (config$label %||% "") ||
            input$height != (config$height %||% 0) ||
            input$prominence != (config$prominence %||% 0) ||
            input$interpeak != (config$interpeak %||% 0) ||
            input$strike_threshold != (config$strike_threshold %||% 0) ||
            input$collision_threshold != (config$collision_threshold %||% 0) ||
            input$shear_threshold != (config$shear_threshold %||% 0)
        )
        
        acceleration_values$inputs_changed <- inputs_changed
      } else {
        # For no config, check if any field has content
        inputs_changed <- (
          nchar(trimws(input$acceleration_config_label)) > 0 ||
            !is.null(input$height) ||
            !is.null(input$prominence) ||
            !is.null(input$interpeak) ||
            !is.null(input$strike_threshold) ||
            !is.null(input$collision_threshold) ||
            !is.null(input$shear_threshold)
        )
        
        acceleration_values$inputs_changed <- inputs_changed
      }
    })
    
    # Button state management for save acceleration config
    observe({
      can_save <- acceleration_values$inputs_changed && 
        !is.null(input$acceleration_config_label) && 
        nchar(trimws(input$acceleration_config_label)) > 0
      
      if (can_save) {
        shinyjs::enable("save_acceleration_config")
      } else {
        shinyjs::disable("save_acceleration_config")
      }
    })
    # ============================= #
    # /// Event handlers \\\ ####  
    # ============================= # 
    
    
    # Acceleration peaks button
    observeEvent(input$acc_peak_btn, {
      req(sensor_selector$selected_sensor())
      
      # Check if data already exists
      status <- sensor_status()
      peaks_processed <- status$acc_hig_peaks_processed %||% FALSE
      
      if (peaks_processed) {
        showModal(modalDialog(
          title = "Acceleration Peaks Data Exists",
          paste("Acceleration peaks already exist for", sensor_selector$selected_sensor(), 
                ". Replace existing peaks?"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_replace_peaks"), "Replace", class = "btn-warning")
          )
        ))
      } else {
        calculate_and_save_peaks()
      }
    })
    
    # Confirm replace peaks
    observeEvent(input$confirm_replace_peaks, {
      removeModal()
      calculate_and_save_peaks()
    })
    
    # Handle acceleration info addition
    observeEvent(input$add_deploy_btn, {
      if (!is.null(sensor_selector$selected_sensor()) && sensor_selector$selected_sensor() != "") {
        showNotification(paste("Adding acceleration info for:", sensor_selector$selected_sensor()), type = "message")
      } else {
        showNotification("Please select a sensor first", type = "warning")
      }
    })
    # Save acceleration configuration
    observeEvent(input$save_acceleration_config, {
      config_name <- trimws(input$acceleration_config_label)
      
      if (nchar(config_name) == 0) {
        showNotification("Please enter a configuration label", type = "error")
        return()
      }
      
      # Check if config already exists
      existing_configs <- acceleration_config$all_configs()
      if (!is.null(existing_configs) && config_name %in% names(existing_configs)) {
        showModal(modalDialog(
          title = "Configuration Exists",
          paste("Configuration '", config_name, "' already exists. Replace existing configuration?"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_save_acceleration_config"), "Replace", class = "btn-warning")
          ),
          size = "m"
        ))
      } else {
        save_acceleration_configuration()
      }
    })
    
    # Confirm save acceleration configuration
    observeEvent(input$confirm_save_acceleration_config, {
      removeModal()
      save_acceleration_configuration()
    })
    # ============================= #
    # /// Helper functions \\\ ####  
    # ============================= # 
    
    
    # Get peak parameters from configuration
    get_peak_params <- function(config) {
      return(list(
        height = config$height,
        prominence = config$prominence,
        interpeak = config$interpeak,
        strike_threshold = cofgi$strike_threshold,
        collision_threshold = config$collision_threshold,
        shear_threshold = config$shear_threshold
      ))
    }
    
    # Find acceleration peaks
    find_acceleration_peaks <- function(sensor_data, params) {
      if (!"higacc_mag_g" %in% names(sensor_data)) {
        stop("higacc_mag_g column not found in sensor data")
      }
      
      accel <- sensor_data$higacc_mag_g
      timestamps <- sensor_data$time_s
      
      # Calculate sampling frequency from data
      fs <- round(1 / median(diff(timestamps), na.rm = TRUE))
      
      # Convert time-based parameters to samples
      interpeak_samples <- round(params$interpeak * fs)
      
      # Use collision_threshold to determine duration classification
      duration_threshold_samples <- params$collision_threshold
      
      # Step 1: Find local maxima above height threshold
      if (length(accel) < 3) return(data.frame())
      
      # Find local maxima
      peaks <- which(diff(sign(diff(accel))) == -2) + 1
      peaks <- peaks[accel[peaks] >= params$height]
      
      if (length(peaks) == 0) return(data.frame())
      
      # Step 2: Apply prominence filter
      valid_peaks <- peaks[sapply(peaks, function(p) {
        left_start <- max(1, p - interpeak_samples)
        right_end <- min(length(accel), p + interpeak_samples)
        
        left_min <- min(accel[left_start:p])
        right_min <- min(accel[p:right_end])
        peak_val <- accel[p]
        
        prominence <- peak_val - max(left_min, right_min)
        return(prominence >= params$prominence)
      })]
      
      if (length(valid_peaks) == 0) return(data.frame())
      
      # Step 3: Enforce inter-peak distance
      if (length(valid_peaks) > 1) {
        to_keep <- logical(length(valid_peaks))
        sorted_peaks <- valid_peaks[order(valid_peaks)]
        i <- 1
        
        while (i <= length(sorted_peaks)) {
          p <- sorted_peaks[i]
          window <- which((sorted_peaks >= p) & (sorted_peaks <= p + interpeak_samples))
          best_peak_idx <- window[which.max(accel[sorted_peaks[window]])]
          to_keep[best_peak_idx] <- TRUE
          i <- max(window) + 1
        }
        
        valid_peaks <- sorted_peaks[to_keep]
      }
      
      if (length(valid_peaks) == 0) return(data.frame())
      
      # Step 4: Classify peaks and create results
      peak_results <- map_dfr(seq_along(valid_peaks), function(i) {
        p <- valid_peaks[i]
        peak_val <- accel[p]
        peak_time <- timestamps[p]
        threshold <- 0.7 * peak_val
        
        # Find duration above 70% threshold
        left <- p
        while (left > 1 && accel[left] >= threshold) left <- left - 1
        right <- p
        while (right < length(accel) && accel[right] >= threshold) right <- right + 1
        
        duration_samples <- right - left
        peak_type <- if (duration_samples < duration_threshold_samples) "collision" else "shear"
        
        tibble(
          peak_number = i,
          peak_time = round(peak_time, 4),
          peak_value = round(peak_val, 1),
          peak_type = peak_type,
          duration_samples = duration_samples
        )
      })
      
      return(peak_results)
    }
    
    # Calculate and save acceleration peaks
    calculate_and_save_peaks <- function() {
      tryCatch({
        sensor_name <- sensor_selector$selected_sensor()
        
        # Create config from current input values
        config <- list(
          label = input$acceleration_config_label %||% "Current_values",
          height = input$height,
          prominence = input$prominence,
          interpeak = input$interpeak,
          strike_threshold = input$strike_threshold,
          collision_threshold = input$collision_threshold,
          shear_threshold = input$shear_threshold
        )
        
        # Validate inputs
        if (is.null(config$height) || is.null(config$prominence) || is.null(config$interpeak)) {
          showNotification("Please enter height, prominence, and interpeak values", type = "error")
          return()
        }
        
        if (is.null(config$collision_threshold)) {
          showNotification("Please enter collision threshold value", type = "error")
          return()
        }
        
        if (is.null(config$strike_threshold)) {
          showNotification("Please enter strike threshold value", type = "error")
          return()
        }
        
        if (is.null(config$shear_threshold)) {
          showNotification("Please enter shear threshold value", type = "error")
          return()
        }
        
        # Read delineated data
        sensor_data <- read_sensor_data(output_dir(), sensor_name, "delineated")
        if (is.null(sensor_data)) {
          showNotification("Failed to read delineated dataset", type = "error")
          return()
        }
        
        # Get peak parameters
        params <- get_peak_params(config)
        
        # Find peaks
        peak_results <- find_acceleration_peaks(sensor_data, params)
        
        # Prepare updates for instrument index
        updates <- list()
        
        if (nrow(peak_results) > 0) {
          # Add individual peak data
          for (i in seq_len(nrow(peak_results))) {
            peak <- peak_results[i, ]
            updates[[paste0("acc_peak_", i, ".time.")]] <- peak$peak_time
            updates[[paste0("acc_peak_", i, ".g.")]] <- peak$peak_value
            updates[[paste0("acc_peak_", i, "_type")]] <- peak$peak_type
          }
          
          # Add event counts - all using threshold-based counting
          updates$acc_event_95g <- sum(peak_results$peak_value >= 95)
          updates$acc_event_200g <- sum(peak_results$peak_value >= 200)
          updates$acc_event_400g <- sum(peak_results$peak_value >= 400)
          
          # Add collision count
          updates$acc_collision <- sum(peak_results$peak_type == "collision")
        } else {
          # No peaks found
          updates$acc_event_95g <- 0
          updates$acc_event_200g <- 0
          updates$acc_event_400g <- 0
          updates$acc_collision <- 0
        }
        
        # Save to instrument index for overall ROI
        success <- safe_update_instrument_index(output_dir(), sensor_name, "overall", updates)
        
        if (!success) {
          showNotification("Failed to save peak results to instrument index", type = "error")
          return()
        }
        
        # Update sensor status flags
        sensor_updates <- list(
          acc_hig_peaks_processed = "Y",
          acc_collision_processed = "Y",
          all_acc_processed = "Y",
          acc_config = config$label
        )
        
        success_sensor <- safe_update_sensor_index(output_dir(), sensor_name, sensor_updates)
        
        if (success_sensor) {
          trigger_data_update()
          trigger_summary_update()
          
          showNotification(paste("Acceleration peaks calculated and saved for", sensor_name, 
                                 "- Found", nrow(peak_results), "peaks"), type = "message")
        } else {
          showNotification("Warning: Peaks calculated but failed to update sensor status", type = "warning")
        }
        
      }, error = function(e) {
        showNotification(paste("Error calculating peaks:", e$message), type = "error")
      })
    }
    
    # Generate acceleration summary text
    generate_acceleration_text <- function(sensor_name, output_dir) {
      instrument_df <- get_instrument_index_file(output_dir, read_data = TRUE)
      
      if (is.null(instrument_df)) {
        return(list(
          peaks_text = "Acceleration data not available",
          events_text = "",
          collisions_text = ""
        ))
      }
      
      tryCatch({
        sensor_row <- instrument_df[instrument_df$file == sensor_name & instrument_df$roi == "overall", ]
        
        if (nrow(sensor_row) == 0) {
          return(list(
            peaks_text = "Acceleration analysis not completed",
            events_text = "",
            collisions_text = ""
          ))
        }
        
        # Extract values
        event_95g <- sensor_row$acc_event_95g %||% 0
        event_200g <- sensor_row$acc_event_200g %||% 0
        event_400g <- sensor_row$acc_event_400g %||% 0
        collisions <- sensor_row$acc_collision %||% 0
        
        # Format text outputs
        peaks_text <- paste("Events ≥95g =", event_95g)
        events_text <- paste("Events ≥200g =", event_200g, "| Events ≥400g =", event_400g)
        collisions_text <- paste("Collision events =", collisions)
        
        return(list(
          peaks_text = peaks_text,
          events_text = events_text,
          collisions_text = collisions_text
        ))
        
      }, error = function(e) {
        return(list(
          peaks_text = "Error reading acceleration data",
          events_text = "",
          collisions_text = ""
        ))
      })
    }
    
    
    # Save acceleration configuration function
    save_acceleration_configuration <- function() {
      config_name <- trimws(input$acceleration_config_label)
      
      # Create acceleration configuration values
      acceleration_config_values <- c(
        input$height,
        input$prominence,
        input$interpeak,
        input$strike_threshold,
        input$collision_threshold,
        input$shear_threshold
      )
      
      # Save configuration using shared function
      success <- save_config_value(
        output_dir = output_dir(),
        config_type = "acc",
        key = config_name,
        value = acceleration_config_values
      )
      
      if (success) {
        # Reload configurations and trigger global updates
        acceleration_config$reload_configs()
        trigger_summary_update()
        
        # Reset change tracking
        acceleration_values$inputs_changed <- FALSE
        acceleration_values$baseline_config <- list(
          label = config_name,
          height = input$height,
          prominence = input$prominence,
          interpeak = input$interpeak,
          strike_threshold = input$strike_threshold,
          collision_threshold = input$collision_threshold,
          shear_threshold = input$shear_threshold
        )
        
        showNotification("Acceleration configuration saved successfully!", type = "message")
      } else {
        showNotification("Failed to save acceleration configuration", type = "error")
      }
    }
    
    # ============================= #
    # /// Output render \\\ ####  
    # ============================= #    
    
    
    
    # Acceleration summary text outputs
    acceleration_summary_text <- reactive({
      req(sensor_selector$selected_sensor())
      global_sensor_state$summary_updated
      generate_acceleration_text(sensor_selector$selected_sensor(), output_dir())
    })
    
    output$acceleration_peaks_text <- renderText({
      acceleration_summary_text()$peaks_text
    })
    
    output$acceleration_events_text <- renderText({
      acceleration_summary_text()$events_text
    })
    
    output$acceleration_collisions_text <- renderText({
      acceleration_summary_text()$collisions_text
    })
    
    # Peaks status output
    output$current_peaks <- renderText({
      req(sensor_selector$selected_sensor())
      status <- sensor_status()
      
      peaks_processed <- status$acc_hig_peaks_processed %||% FALSE
      
      if (peaks_processed) {
        paste("Acceleration peaks calculated for", sensor_selector$selected_sensor())
      } else {
        ""
      }
    })
    
    # Acceleration config status
    output$acceleration_config_status <- renderText({
      if (acceleration_values$inputs_changed) {
        "Configuration modified - click Save Configuration to save changes"
      } else {
        ""
      }
    })
    
    # acceleration status display ####
    status_controls <- statusModuleServer("status_display",
                                          sensor_name_reactive = reactive(sensor_selector$selected_sensor()),
                                          output_dir_reactive = reactive(output_dir()),
                                          check_types = c("acc_processed", "acc_processed_sum", "acc_processed_peaks", "acc_processed_collision"),                                          invalidation_trigger = reactive(global_sensor_state$summary_updated),
                                          individual_outputs = TRUE)
    
    # acceleration summary display ####
    summary_table <- summarytableModuleServer("acceleration_summary", 
                                              sensor_reactive = reactive(sensor_selector$selected_sensor()),
                                              output_dir_reactive = reactive(output_dir()),
                                              instrument_variable = "acc",
                                              global_sensor_state = global_sensor_state,
                                              trigger_data_update = trigger_data_update,
                                              trigger_summary_update = trigger_summary_update)
    
    # acceleration plot ####
    plot_controls <- plotModuleServer("acceleration_plot", 
                                      sensor_data = selected_sensor_data,
                                      sensor_name = reactive(sensor_selector$selected_sensor()),
                                      nadir_info = nadir_info,
                                      output_dir = reactive(output_dir()),
                                      roi_boundaries = roi_boundaries,
                                      right_var = reactive(input$`acceleration_plot-right_y_var`),
                                      left_var = reactive(input$`acceleration_plot-left_y_var`),
                                      plot_width = reactive(input$`acceleration_plot-plot_width`),
                                      plot_height = reactive(input$`acceleration_plot-plot_height`),
                                      show_nadir = reactive(input$`acceleration_plot-show_nadir`),
                                      show_legend = reactive(input$`acceleration_plot-show_legend`),
                                      show_normalized = reactive(input$`acceleration_plot-show_normalized`),
                                      show_roi_markers = reactive(input$`acceleration_plot-show_roi_markers`),
                                      show_acceleration_peaks = reactive(input$`acceleration_plot-show_acceleration_peaks`),
                                      title_prefix = "Acceleration Analysis",
                                      plot_source = "acceleration_plot"
    )
    return(list(
      selected_sensor = reactive(sensor_selector$selected_sensor())
    ))
    
  })
}  
