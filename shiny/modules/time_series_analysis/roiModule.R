# ROI Delineation Module - Simplified

roiUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    plotModuleUI(ns("roi_plot"), height = "600px"),
    br(),
    
    fluidRow(
      column(
        width = 12,
        div(
          style = "background-color: #f8f9fa; border: 1px solid #ccc; padding: 15px; 
                   border-radius: 5px; margin-top: 20px;",
          tags$h4("Delineation and passage summary"),
          fluidRow(
            column(
              width = 8,
              DT::dataTableOutput(ns("roi_table"))
            ),
            column(
              width = 4,
              tags$p(style = "margin-top: 10px;", textOutput(ns("passage_duration_text"))),
              tags$p(textOutput(ns("ingress_nadir_text"))),
              tags$p(textOutput(ns("nadir_outgress_text")))
            )
          )
        )
      )
    ),
    hr(),
    
    h4("Delineation Instructions"),
    
    fluidRow(
      column(
        width = 12,
        tags$p("Use the table above to review regions of interest. Each entry corresponds to a delineated region. Check timestamps to ensure accurate event capture."),
        tags$ul(
          tags$li("ROI 1 Sensor ingress: Mark region sensor enters system from atmospheric pressure or other landmark feature (e.g., injection pipe)"),
          tags$li("ROI 2 Intake passage: Mark region sensor moves through intake structures and pipework leading towards the impeller."),
          tags$li("ROI 3 Pre-nadir: Mark region just before the impeller, highest risk of encountering pressure differentials and swirl flows."),
          tags$li("ROI 4 Nadir: Critical passage analysis zone with direct passage through the impeller. Hydraulic pinch point where maximum acceleration, rotation and minimum pressure likely to occur.
                  Region calculated using input time box, which is centered on the nadir point. Ensure nadir is correct first."),
          tags$li("ROI 5 Post-nadir: Mark region just after the impeller, highest risk of encountering guide vane or other forms of collision, residual turbulences and pressure recovery."),
          tags$li("ROI 6 Outflow passage: Mark region sensor moves through outflow pipework and structures leading towards sensor outgress. Velocity expected to decrease and pressure return to atmospheric pressure."),
          tags$li("ROI 7 Sensor outgress: Mark region sensor exists system from atmospheric pressure or other landmark features (e.g., stable flow indicative of tailwater)."),
          tags$li("Sensor start and end trim: Automatically calculated from start and end of data and start and end of ROI 1 and 7. Use trim tool to remove after delineation.")
        )
      )
    )
  )
}

roiSidebarUI <- function(id) {
  ns <- NS(id)
  
  sensor_vars <- get_sensor_variables()
  var_choices <- setNames(sensor_vars$names, sensor_vars$labels)
  
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
        h4("Time series controls"),
        
        div(style = "color: #666; font-style: italic; margin-bottom: 15px;",
            "Select a sensor to begin time series analysis."),
        
        statusSidebarUI(ns("status_display"),
                        show_delineation = TRUE,
                        show_normalization = TRUE,
                        show_passage_times = TRUE),
        
        enhancedSensorSelectionUI(ns("sensor_selector"), status_filter_type = "delineation"),
        
        h4("Delineate data"),
        actionButton(ns("create_delineated"), "Apply delineation configuration", class = "btn-success btn-block"),
        actionButton(ns("start_over"), "Start Over", class = "btn-danger btn-block"),
        actionButton(ns("trim_sensor"), "Trim sensor start and end", class = "btn-warning btn-block"),
        
        br(),
        
        configurationSidebarUI(ns("roi_config"), config_type = "roi", 
                               label = "Delineation configuration:"),
        
        hr(), h4("Pressure Nadir Options"),
        textOutput(ns("current_nadir_display")),
        actionButton(ns("nadir_btn"), "Modify Pressure Nadir", class = "btn-warning btn-block"),
        actionButton(ns("cancel_nadir_btn"), "Cancel", class = "btn-danger btn-block"),
        textOutput(ns("nadir_status")),
        
        hr(), h4("Time normalization"),
        actionButton(ns("normalize_time"), "Normalize time series", class = "btn-primary btn-block"),
        textOutput(ns("normalize_status")),
        
        hr(), h4("Passage time calculation"),
        actionButton(ns("passage_time"), "Calculate passage times", class = "btn-primary btn-block"),
        textOutput(ns("passage_status")),
        
        hr(),
        
        h4("Plot controls"),
        plotSidebarUI(ns("roi_plot"), 
                      show_left_var = TRUE,   
                      show_right_var = TRUE,    
                      show_normalized = TRUE,   
                      show_nadir = TRUE,      
                      show_roi_markers = TRUE,   
                      show_legend = TRUE,
                      default_show_normalized = FALSE,
                      default_show_nadir = TRUE,
                      default_show_roi_markers = TRUE,
                      default_show_legend = FALSE,
                      default_left_var = "pressure_kpa",
                      default_right_var = "higacc_mag_g") 
    )
  )
}

roiServer <- function(id, output_dir, summary_data, processing_complete = reactive(FALSE), 
                      session_state = NULL, global_sensor_state, trigger_data_update, 
                      trigger_summary_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ============================= #
    # /// Reactive values \\\ ####  
    # ============================= #   
    
    # ROI configuration state
    roi_values <- reactiveValues(
      roi_configs = NULL,
      current_config = NULL
    )
    
    # Nadir editing state
    nadir_values <- reactiveValues(
      edit_mode = FALSE,           # Whether nadir editing is active
      selected_point = NULL,       # Currently selected nadir point
      nadir_updated = 0,          # Counter to trigger nadir refresh
      baseline_click = NULL        # Baseline click for detecting new clicks
    )
    
    # ============================= #
    # /// Data loading & processing  \\\ ####  
    # ============================= # 
    
    #Sensor dropdown ####   
    sensor_selector <- enhancedSensorSelectionServer("sensor_selector",
                                                     output_dir, processing_complete,
                                                     status_filter_type = "delineation",
                                                     session_state = session_state,
                                                     global_sensor_state = global_sensor_state,
                                                     trigger_summary_update = trigger_summary_update)
    
    # Get nadir info using shared function
    nadir_info <- reactive({
      req(sensor_selector$selected_sensor())
      global_sensor_state$summary_updated  # Use global
      get_nadir_info(sensor_selector$selected_sensor(), output_dir())
    })
    
    # Get sensor status using shared function
    sensor_status <- reactive({
      req(sensor_selector$selected_sensor())
      global_sensor_state$summary_updated  # Use global
      global_sensor_state$data_updated     # Use global
      get_sensor_status(sensor_selector$selected_sensor(), output_dir())
    })
    
    # Read selected sensor data (with preference for delineated data)
    selected_sensor_data <- reactive({
      req(sensor_selector$selected_sensor())
      global_sensor_state$data_updated  # Invalidate when data changes
      
      # Check for delineated file first
      delineated_data <- read_sensor_data(output_dir(), sensor_selector$selected_sensor(), "delineated")
      if (!is.null(delineated_data)) {
        return(delineated_data)
      }
      
      # Fall back to regular minimal data
      return(read_sensor_data(output_dir(), sensor_selector$selected_sensor(), "min"))
    })
    
    ## Calculate ROI times ####
    
    # Calculate ROI times based on configuration and nadir
    roi_times <- reactive({
      req(sensor_selector$selected_sensor(), roi_values$current_config)
      global_sensor_state$summary_updated 
      roi_config$selected_config_name()
      
      nadir <- nadir_info()
      
      if (!nadir$available) return(NULL)
      
      nadir_time <- nadir$time
      config <- roi_values$current_config
      
      # Calculate ROI boundaries based on nadir time
      roi4_start <- nadir_time - (config$roi4_nadir / 2)
      roi4_end <- nadir_time + (config$roi4_nadir / 2)
      
      roi3_start <- roi4_start - config$roi3_prenadir
      roi3_end <- roi4_start
      
      roi2_start <- roi3_start - config$roi2_inflow_passage
      roi2_end <- roi3_start
      
      roi1_start <- roi2_start - config$roi1_sens_ingress
      roi1_end <- roi2_start
      
      roi5_start <- roi4_end
      roi5_end <- roi4_end + config$roi5_postnadir
      
      roi6_start <- roi5_end
      roi6_end <- roi5_end + config$roi6_outflow_passage
      
      roi7_start <- roi6_end
      roi7_end <- roi6_end + config$roi7_sens_outgress
      
      # Read sensor data to get actual start/end times
      sensor_data <- read_sensor_data(output_dir(), sensor_selector$selected_sensor(), "min")
      if (!is.null(sensor_data)) {
        data_start <- min(sensor_data$time_s)
        data_end <- max(sensor_data$time_s)
        
        roi_times_df <- data.frame(
          ROI = c("Sensor start trim", "ROI 1: Sensor ingress", "ROI 2: Inflow passage", 
                  "ROI 3: Pre-nadir", "ROI 4: Nadir", "ROI 5: Post-nadir", 
                  "ROI 6: Outflow passage", "ROI 7: Sensor outgress", "Sensor end trim"),
          `Start time` = c(paste(round(data_start, 3), "s"),
                           paste(round(roi1_start, 3), "s"),
                           paste(round(roi2_start, 3), "s"),
                           paste(round(roi3_start, 3), "s"),
                           paste(round(roi4_start, 3), "s"),
                           paste(round(roi5_start, 3), "s"),
                           paste(round(roi6_start, 3), "s"),
                           paste(round(roi7_start, 3), "s"),
                           paste(round(roi7_end, 3), "s")),
          `End Time` = c(paste(round(roi1_start, 3), "s"),
                         paste(round(roi1_end, 3), "s"),
                         paste(round(roi2_end, 3), "s"),
                         paste(round(roi3_end, 3), "s"),
                         paste(round(roi4_end, 3), "s"),
                         paste(round(roi5_end, 3), "s"),
                         paste(round(roi6_end, 3), "s"),
                         paste(round(roi7_end, 3), "s"),
                         paste(round(data_end, 3), "s")),
          Duration = c(paste(round(roi1_start - data_start, 3), "s"),
                       paste(round(config$roi1_sens_ingress, 3), "s"),
                       paste(round(config$roi2_inflow_passage, 3), "s"),
                       paste(round(config$roi3_prenadir, 3), "s"),
                       paste(round(config$roi4_nadir, 3), "s"),
                       paste(round(config$roi5_postnadir, 3), "s"),
                       paste(round(config$roi6_outflow_passage, 3), "s"),
                       paste(round(config$roi7_sens_outgress, 3), "s"),
                       paste(round(data_end - roi7_end, 3), "s")),
          check.names = FALSE
        )
        
        return(list(
          table = roi_times_df,
          boundaries = c(data_start, roi1_start, roi2_start, roi3_start, 
                         roi4_start, roi5_start, roi6_start, roi7_start, roi7_end, data_end)
        ))
      }
      
      return(NULL)
    })
    
    # ============================= #
    # /// UI State management \\\ ####  
    # ============================= # 
    
    # Load ROI configurations and update dropdown
    roi_config <- configurationServer("roi_config",
                                      output_dir = output_dir,
                                      config_type = "roi",
                                      sensor_name = reactive(sensor_selector$selected_sensor()),
                                      auto_select_sensor_config = TRUE)
    
    # Use the config
    observe({
      roi_values$current_config <- roi_config$current_config()
    })
    
    ## Normalized checkbox ####
    
    # Enable/disable normalized checkbox based on sensor status  
    observe({
      req(sensor_selector$selected_sensor())
      global_sensor_state$summary_updated  # Add this line
      global_sensor_state$data_updated     # Add this line
      
      status <- get_sensor_status(sensor_selector$selected_sensor(), output_dir())
      
      if (status$normalized) {
        shinyjs::enable(paste0("roi_plot-show_normalized"))
      } else {
        shinyjs::disable(paste0("roi_plot-show_normalized"))
        updateCheckboxInput(session, "roi_plot-show_normalized", value = FALSE)
      }
    })
    
    # Auto-uncheck nadir when normalized is checked
    observeEvent(input$`roi_plot-show_normalized`, {
      if (input$`roi_plot-show_normalized`) {
        updateCheckboxInput(session, "roi_plot-show_nadir", value = FALSE)
      }
    })
    
    ## Button state management #####
    
    observe({
      req(sensor_selector$selected_sensor())
      
      nadir <- nadir_info()
      status <- sensor_status()
      
      button_states <- list(
        "create_delineated" = nadir$available && !status$delineated,
        "start_over" = status$delineated,
        "trim_sensor" = status$delineated && !status$trimmed,
        "normalize_time" = status$delineated && status$trimmed && !status$normalized,
        "passage_time" = status$delineated && status$trimmed && !status$passage_times,
        "nadir_btn" = (!nadir_values$edit_mode || !is.null(nadir_values$selected_point)),
        "cancel_nadir_btn" = nadir_values$edit_mode
      )
      
      manage_button_states(session, button_states)
    })
    
    ## Update nadir button ####
    # Update nadir button appearance based on edit mode
    observe({
      if (nadir_values$edit_mode) {
        if (!is.null(nadir_values$selected_point)) {
          updateActionButton(session, "nadir_btn", label = "Save Pressure Nadir")
          shinyjs::removeClass("nadir_btn", "btn-warning")
          shinyjs::addClass("nadir_btn", "btn-success")
        } else {
          updateActionButton(session, "nadir_btn", label = "Select Pressure Nadir")
          shinyjs::removeClass("nadir_btn", "btn-success")
          shinyjs::addClass("nadir_btn", "btn-warning")
        }
      } else {
        # Reset to initial state when not in edit mode
        updateActionButton(session, "nadir_btn", label = "Select Pressure Nadir")
        shinyjs::removeClass("nadir_btn", "btn-success")
        shinyjs::addClass("nadir_btn", "btn-warning")
      }
    })
    
    # ============================= #
    # /// Event handlers \\\ ####  
    # ============================= # 
    
    ## Edit nadir ####
    
    # Edit nadir button
    observeEvent(input$nadir_btn, {
      if (!nadir_values$edit_mode) {
        # Start edit mode
        nadir_values$edit_mode <- TRUE
        nadir_values$selected_point <- NULL
        nadir_values$baseline_click <- event_data("plotly_click", source = "roi_nadir_plot")
      } else if (!is.null(nadir_values$selected_point)) {
        # Save nadir
        success <- safe_update_sensor_index(
          output_dir(), 
          sensor_selector$selected_sensor(),
          list(
            "pres_min.time." = nadir_values$selected_point$x,
            "pres_min.kPa." = nadir_values$selected_point$y
          )
        )
        
        if (success) {
          trigger_data_update()     # Use global trigger
          trigger_summary_update()  # Use global trigger
          nadir_values$edit_mode <- FALSE
          nadir_values$selected_point <- NULL
          showNotification("Nadir updated successfully!", type = "message")
        } else {
          showNotification("Failed to update nadir", type = "error")
        }
      }
    })
    
    # Cancel nadir editing
    observeEvent(input$cancel_nadir_btn, {
      nadir_values$edit_mode <- FALSE
      nadir_values$selected_point <- NULL
    })
    
    # Handle click events for nadir selection
    observe({
      if (nadir_values$edit_mode) {
        click_data <- event_data("plotly_click", source = "roi_nadir_plot")
        if (!is.null(click_data)) {
          if (is.null(nadir_values$baseline_click) ||
              click_data$x != nadir_values$baseline_click$x ||
              click_data$y != nadir_values$baseline_click$y) {
            nadir_values$selected_point <- list(x = click_data$x, y = click_data$y)
          }
        }
      }
    })
    
    ## Delineate and trim ####
    
    observeEvent(input$create_delineated, {
      req(sensor_selector$selected_sensor(), roi_times(), roi_values$current_config)
      
      create_delineated_dataset()
    })
    
    # Trim sensor button
    observeEvent(input$trim_sensor, {
      req(sensor_selector$selected_sensor())
      
      # Read delineated data using shared function
      sensor_data <- read_sensor_data(output_dir(), sensor_selector$selected_sensor(), "delineated")
      
      if (is.null(sensor_data)) {
        showNotification("Failed to read delineated dataset.", type = "error")
        return()
      }
      
      # Perform trimming
      trimmed_data <- sensor_data[!sensor_data$roi %in% c("trim_start", "trim_end"), ]
      
      # Save over existing delineated file
      delineated_path <- file.path(output_dir(), "csv", "delineated", paste0(sensor_selector$selected_sensor(), "_delineated.csv"))
      write.csv(trimmed_data, delineated_path, row.names = FALSE)
      
      # Update sensor index using shared function
      success <- safe_update_sensor_index(output_dir(), sensor_selector$selected_sensor(), list(trimmed = "Y"))
      
      if (success) {
        trigger_data_update()     # Use global trigger
        trigger_summary_update()  # Use global trigger
        
        showNotification("Sensor data trimmed successfully!", type = "message")
      } else {
        showNotification("Failed to update sensor index", type = "error")
      }
    })
    
    ## Start over ####
    
    observeEvent(input$start_over, {
      req(sensor_selector$selected_sensor())
      
      # Remove delineated file
      delineated_path <- file.path(output_dir(), "csv", "delineated", paste0(sensor_selector$selected_sensor(), "_delineated.csv"))
      if (file.exists(delineated_path)) {
        file.remove(delineated_path)
      }
      
      # Reset flags in sensor index using shared function
      success <- safe_update_sensor_index(
        output_dir(), 
        sensor_selector$selected_sensor(),
        list(
          delineated = "N",
          trimmed = "N",
          normalized = "N",
          roi_config = "NA",
          passage_times = "N",
          passage_duration.mm.ss. = "NA",
          ingress_nadir_duration.mm.ss. = "NA",
          nadir_outgress_duration.mm.ss. = "NA"
        )
      )
      
      if (success) {
        trigger_data_update()     # Use global trigger
        trigger_summary_update()  # Use global trigger
        
        showNotification("Reset to original sensor file", type = "message")
      } else {
        showNotification("Failed to reset sensor status", type = "error")
      }
    })
    
    ## Normalize time series ####
    
    observeEvent(input$normalize_time, {
      req(sensor_selector$selected_sensor())
      
      # Perform normalization
      tryCatch({
        # Read delineated data
        sensor_data <- read_sensor_data(output_dir(), sensor_selector$selected_sensor(), "delineated")
        
        if (is.null(sensor_data)) {
          showNotification("Failed to read delineated dataset", type = "error")
          return()
        }
        
        # Get nadir info
        nadir <- nadir_info()
        
        # Calculate normalization parameters
        start_time <- min(sensor_data$time_s)
        end_time <- max(sensor_data$time_s)
        mid_time <- nadir$time
        
        # Create normalized time column
        sensor_data <- sensor_data %>%
          mutate(time_norm = case_when(
            time_s <= start_time ~ 0,
            time_s >= end_time ~ 1,
            time_s > start_time & time_s < mid_time ~ (time_s - start_time) / (mid_time - start_time) * 0.5,
            time_s >= mid_time & time_s <= end_time ~ 0.5 + (time_s - mid_time) / (end_time - mid_time) * 0.5
          ))
        
        # Save updated delineated file
        delineated_path <- file.path(output_dir(), "csv", "delineated", paste0(sensor_selector$selected_sensor(), "_delineated.csv"))
        write.csv(sensor_data, delineated_path, row.names = FALSE)
        
        # Update sensor index
        success <- safe_update_sensor_index(output_dir(), sensor_selector$selected_sensor(), list(normalized = "Y"))
        
        if (success) {
          trigger_data_update()     # Use global trigger
          trigger_summary_update()  # Use global trigger
          
          showNotification("Time series normalized successfully!", type = "message")
        } else {
          showNotification("Warning: Normalization completed but failed to update index", type = "warning")
        }
        
      }, error = function(e) {
        showNotification(paste("Error normalizing time series:", e$message), type = "error")
      })
    })
    
    ## Calculate passage times ####
    
    observeEvent(input$passage_time, {
      req(sensor_selector$selected_sensor())
      
      # Calculate passage times
      tryCatch({
        # Read delineated data
        sensor_data <- read_sensor_data(output_dir(), sensor_selector$selected_sensor(), "delineated")
        
        if (is.null(sensor_data)) {
          showNotification("Failed to read delineated dataset", type = "error")
          return()
        }
        
        # Get nadir info
        nadir <- nadir_info()
        
        # Calculate times in seconds
        first_time <- min(sensor_data$time_s)
        last_time <- max(sensor_data$time_s)
        nadir_time <- nadir$time
        
        # Calculate durations
        passage_duration_s <- last_time - first_time
        ingress_nadir_s <- nadir_time - first_time
        nadir_outgress_s <- last_time - nadir_time
        
        # Convert to mm:ss format
        format_mm_ss <- function(seconds) {
          minutes <- floor(seconds / 60)
          secs <- round(seconds %% 60)
          sprintf("%02d:%02d", minutes, secs)
        }
        
        # Update sensor index
        success <- safe_update_sensor_index(
          output_dir(), 
          sensor_selector$selected_sensor(),
          list(
            passage_times = "Y",
            passage_duration.mm.ss. = format_mm_ss(passage_duration_s),
            ingress_nadir_duration.mm.ss. = format_mm_ss(ingress_nadir_s),
            nadir_outgress_duration.mm.ss. = format_mm_ss(nadir_outgress_s)
          )
        )
        
        if (success) {
          trigger_data_update()     # Use global trigger
          trigger_summary_update()  # Use global trigger
          
          showNotification("Passage times calculated successfully!", type = "message")
        } else {
          showNotification("Failed to update sensor index", type = "error")
        }
        
      }, error = function(e) {
        showNotification(paste("Error calculating passage times:", e$message), type = "error")
      })
    })
    
    # ============================= #
    # /// Helper functions \\\ ####  
    # ============================= # 
    
    ## Create delineated data ####
    
    # Function to create delineated dataset
    create_delineated_dataset <- function() {
      tryCatch({
        # Read original data using shared function
        sensor_data <- read_sensor_data(output_dir(), sensor_selector$selected_sensor(), "min")
        
        # Create delineated folder - always check/create fresh
        delineated_dir <- file.path(output_dir(), "csv", "delineated")
        if (!dir.exists(delineated_dir)) {
          dir.create(delineated_dir, showWarnings = FALSE, recursive = TRUE)
        }
        
        # Verify directory was created
        if (!dir.exists(delineated_dir)) {
          showNotification("Failed to create delineated directory", type = "error")
          return()
        }
        
        # Add ROI column
        times <- roi_times()
        if (is.null(times)) {
          showNotification("ROI times not available", type = "error")
          return()
        }
        
        boundaries <- times$boundaries
        
        sensor_data$roi <- cut(sensor_data$time_s, 
                               breaks = boundaries,
                               labels = c("trim_start", "roi1_sens_ingress", "roi2_inflow_passage", 
                                          "roi3_prenadir", "roi4_nadir", "roi5_postnadir", 
                                          "roi6_outflow_passage", "roi7_sens_outgress", "trim_end"),
                               include.lowest = TRUE, right = FALSE)
        
        # Save delineated file
        output_file <- file.path(delineated_dir, paste0(sensor_selector$selected_sensor(), "_delineated.csv"))
        write.csv(sensor_data, output_file, row.names = FALSE)
        
        # Verify file was created
        if (!file.exists(output_file)) {
          showNotification("Failed to create delineated file", type = "error")
          return()
        }
        
        # Update sensor index using shared function
        success <- safe_update_sensor_index(
          output_dir(),
          sensor_selector$selected_sensor(),
          list(
            delineated = "Y",
            roi_config = roi_values$current_config$label,
            trimmed = "N",
            normalized = "N",
            passage_times = "N",
            passage_duration.mm.ss. = "NA",
            ingress_nadir_duration.mm.ss. = "NA",
            nadir_outgress_duration.mm.ss. = "NA"
          )
        )
        
        if (success) {
          trigger_data_update()     # Use global trigger
          trigger_summary_update()  # Use global trigger
          
          showNotification("Delineated dataset created successfully!", type = "message")
        } else {
          showNotification("Warning: Dataset created but failed to update index", type = "warning")
        }
        
      }, error = function(e) {
        showNotification(paste("Error creating delineated dataset:", e$message), 
                         type = "error")
      })
    }
    
    # ============================= #
    # /// Output render \\\ ####  
    # ============================= #    
    
    # Display current nadir####
    
    output$current_nadir_display <- renderText({
      nadir <- nadir_info()
      if (nadir$available) {
        paste0("Time: ", round(nadir$time, 3), "s\nPressure: ", round(nadir$value, 2), " kPa")
      }
    })
    
    # Delineation, normalization, passage status ####
    status_controls <- statusModuleServer("status_display",
                                          sensor_name_reactive = reactive(sensor_selector$selected_sensor()),
                                          output_dir_reactive = reactive(output_dir()),
                                          check_types = c("delineation", "normalization", "passage_times"),
                                          invalidation_trigger = reactive(global_sensor_state$summary_updated),
                                          individual_outputs = TRUE)
    
    # Nadir editing status display ####
    
    output$nadir_status <- renderText({
      if (nadir_values$edit_mode) {
        if (!is.null(nadir_values$selected_point)) {
          paste0("Selected: ", round(nadir_values$selected_point$y, 2), " kPa at ", 
                 round(nadir_values$selected_point$x, 3), "s")
        } else {
          "Edit mode: Click on plot to select nadir"
        }
      } else {
        ""
      }
    })
    
    # Display ROI table ####
    output$roi_table <- DT::renderDataTable({
      times <- roi_times()
      if (!is.null(times)) {
        DT::datatable(
          times$table,
          options = list(
            pageLength = 7,
            scrollX = TRUE,
            dom = 't',
            ordering = FALSE,
            searching = FALSE,
            paging = FALSE,
            info = FALSE
          ),
          rownames = FALSE
        ) %>%
          DT::formatStyle(columns = 1:4, fontSize = '14px')
      }
    })
    
    # Normalize status output ####
    output$normalize_status <- renderText({
      req(sensor_selector$selected_sensor())
      status <- sensor_status()
      
      if (status$normalized) {
        "Time series normalized"
      } else {
        ""
      }
    })
    
    # Passage status output####
    output$passage_status <- renderText({
      req(sensor_selector$selected_sensor())
      status <- sensor_status()
      
      if (status$passage_times) {
        "Passage times calculated"
      } else {
        ""
      }
    })
    
    # Passage duration output #### 
    
    # Helper function to generate duration text
    generate_duration_text <- function(duration_col, prefix_text) {
      req(sensor_selector$selected_sensor())
      global_sensor_state$summary_updated
      
      index_df <- get_sensor_index_file(output_dir(), read_data = TRUE)
      if (is.null(index_df)) return(paste0(prefix_text, ": Not calculated"))
      
      tryCatch({
        sensor_row <- index_df[index_df$file == sensor_selector$selected_sensor(), ]
        
        if (nrow(sensor_row) > 0 && !is.na(sensor_row[[duration_col]]) && sensor_row[[duration_col]] != "NA") {
          time_parts <- strsplit(sensor_row[[duration_col]], ":")[[1]]
          paste0(prefix_text, ": ", as.numeric(time_parts[1]), " minutes ", as.numeric(time_parts[2]), " seconds")
        } else {
          paste0(prefix_text, ": Not calculated")
        }
      }, error = function(e) {
        paste0(prefix_text, ": Not calculated")
      })
    }
    
    output$passage_duration_text <- renderText({
      generate_duration_text("passage_duration.mm.ss.", "Overall passage duration")
    })
    
    output$ingress_nadir_text <- renderText({
      generate_duration_text("ingress_nadir_duration.mm.ss.", "Sensor ingress to nadir")
    })
    
    output$nadir_outgress_text <- renderText({
      generate_duration_text("nadir_outgress_duration.mm.ss.", "Nadir to sensor outgress")
    })
    
    # Create main plot #####
    # Setup the base plot using plot module
    plot_controls <- plotModuleServer("roi_plot", 
                                      sensor_data = selected_sensor_data,
                                      sensor_name = reactive(sensor_selector$selected_sensor()),
                                      nadir_info = nadir_info,
                                      right_var = reactive(input$`roi_plot-right_y_var`),
                                      left_var = reactive(input$`roi_plot-left_y_var`),
                                      show_nadir = reactive(input$`roi_plot-show_nadir`),
                                      show_legend = reactive(input$`roi_plot-show_legend`),
                                      show_normalized = reactive(input$`roi_plot-show_normalized`),
                                      selected_nadir = reactive({
                                        if (nadir_values$edit_mode && !is.null(nadir_values$selected_point)) {
                                          nadir_values$selected_point
                                        } else {
                                          NULL
                                        }
                                      }),
                                      roi_boundaries = reactive({
                                        times <- roi_times()
                                        if (!is.null(times)) {
                                          times$boundaries
                                        } else {
                                          NULL
                                        }
                                      }),
                                      show_roi_markers = reactive({
                                        # Show ROI markers when delineated and trimmed
                                        status <- sensor_status()
                                        status$delineated && status$trimmed
                                      }),
                                      title_prefix = "ROI Delineated",
                                      plot_source = "roi_nadir_plot"
    )
    
  })  # End of moduleServer
}     # End of roiServer