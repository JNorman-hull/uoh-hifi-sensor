# ROI Delineation Module - Simplified Approach

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
        tags$p("Use the input boxes in the sidebar to set ROI timing, then update the plot and table. Each entry corresponds to a delineated region. Check timestamps to ensure accurate event capture."),
        tags$ul(
          tags$li("ROI 1 Sensor ingress: Mark region sensor enters system from atmospheric pressure or other landmark feature (e.g., injection pipe)"),
          tags$li("ROI 2 Intake passage: Mark region sensor moves through intake structures and pipework leading towards the impeller."),
          tags$li("ROI 3 Pre-nadir: Mark region just before the impeller, highest risk of encountering pressure differentials and swirl flows."),
          tags$li("ROI 4 Nadir: Critical passage analysis zone with direct passage through the impeller. Hydraulic pinch point where maximum acceleration, rotation and minimum pressure likely to occur.
                  Region calculated using duration input, which is centered on the nadir point."),
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
        
        h4("ROI Time Settings"),
        
        # ROI timing inputs
        div(style = "margin-bottom: 10px;",
            numericInput(ns("roi1_start"), "Data start (ROI 1 Start (s)):", 
                         value = NULL, step = 0.001, width = "100%")),
        
        div(style = "margin-bottom: 10px;",
            numericInput(ns("roi2_start"), "ROI 2 Start (s):", 
                         value = NULL, step = 0.001, width = "100%")),
        
        div(style = "margin-bottom: 10px;",
            numericInput(ns("roi3_duration"), "ROI 3 Duration (s):", 
                         value = NULL, step = 0.01, width = "100%")),
        
        div(style = "margin-bottom: 10px;",
            numericInput(ns("roi4_duration"), "ROI 4 Duration (s):", 
                         value = NULL, step = 0.01, width = "100%")),
        
        div(style = "margin-bottom: 10px;",
            numericInput(ns("roi5_duration"), "ROI 5 Duration (s):", 
                         value = NULL, step = 0.01, width = "100%")),
        
        div(style = "margin-bottom: 10px;",
            numericInput(ns("roi6_end"), "ROI 6 End (s):", 
                         value = NULL, step = 0.001, width = "100%")),
        
        div(style = "margin-bottom: 15px;",
            numericInput(ns("roi7_end"), "Data end (ROI 7 End (s)):", 
                         value = NULL, step = 0.001, width = "100%")),
        
        actionButton(ns("update_roi"), "Build ROI and update plot", class = "btn-primary btn-block"),
        
        hr(),
        
        h4("Apply Delineation"),
        actionButton(ns("create_delineated"), "Apply delineation, trim and normalize", class = "btn-success btn-block"),
        actionButton(ns("start_over"), "Start Over", class = "btn-danger btn-block"),
        
        br(),
        
        h4("Pressure Nadir Options"),
        textOutput(ns("current_nadir_display")),
        actionButton(ns("nadir_btn"), "Modify Pressure Nadir", class = "btn-warning btn-block"),
        actionButton(ns("cancel_nadir_btn"), "Cancel", class = "btn-danger btn-block"),
        textOutput(ns("nadir_status")),
        
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
                      default_right_var = "none") 
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
    
    # Nadir editing state
    nadir_values <- reactiveValues(
      edit_mode = FALSE,
      selected_point = NULL,
      baseline_click = NULL
    )
    
    # ROI state
    roi_values <- reactiveValues(
      has_roi_data = FALSE,  # Track if we have ROI timing data
      # Store the actual ROI values (not reactive to inputs)
      roi1_start = NULL,
      roi2_start = NULL,
      roi3_duration = NULL,
      roi4_duration = NULL,
      roi5_duration = NULL,
      roi6_end = NULL,
      roi7_end = NULL
    )
    
    # ============================= #
    # /// Data loading & processing  \\\ ####  
    # ============================= # 
    
    # Sensor dropdown
    sensor_selector <- enhancedSensorSelectionServer("sensor_selector",
                                                     output_dir, processing_complete,
                                                     status_filter_type = "delineation",
                                                     session_state = session_state,
                                                     global_sensor_state = global_sensor_state,
                                                     trigger_summary_update = trigger_summary_update)
    
    # Get nadir info using shared function
    nadir_info <- reactive({
      req(sensor_selector$selected_sensor())
      global_sensor_state$summary_updated
      get_nadir_info(sensor_selector$selected_sensor(), output_dir())
    })
    
    # Get sensor status using shared function
    sensor_status <- reactive({
      req(sensor_selector$selected_sensor())
      global_sensor_state$summary_updated
      global_sensor_state$data_updated
      get_sensor_status(sensor_selector$selected_sensor(), output_dir())
    })
    
    # Read selected sensor data (with preference for delineated data)
    selected_sensor_data <- reactive({
      sensor_name <- sensor_selector$selected_sensor()
      req(sensor_name, nzchar(sensor_name))  # ← Ensure sensor name is valid
      
      global_sensor_state$data_updated
      
      # Check for delineated file first
      delineated_data <- read_sensor_data(output_dir(), sensor_name, "delineated")
      if (!is.null(delineated_data)) {
        return(delineated_data)
      }
      
      min_data <- read_sensor_data(output_dir(), sensor_name, "min") 
      req(min_data)  # ← Ensure we actually have data before returning
      return(min_data)
    })
    
    # ============================= #
    # /// Helper functions \\\ ####  
    # ============================= # 
    
    # Extract ROI times from delineated file
    extract_roi_times_from_delineated <- function(sensor_name) {
      delineated_data <- read_sensor_data(output_dir(), sensor_name, "delineated")
      if (is.null(delineated_data) || !"roi" %in% names(delineated_data)) {
        return(NULL)
      }
      
      roi_levels <- c("roi1_sens_ingress", "roi2_inflow_passage", "roi3_prenadir", 
                      "roi4_nadir", "roi5_postnadir", "roi6_outflow_passage", "roi7_sens_outgress")
      
      roi_times <- list()
      
      for (roi in roi_levels) {
        roi_data <- delineated_data[delineated_data$roi == roi, ]
        if (nrow(roi_data) > 0) {
          roi_times[[roi]] <- list(
            start = min(roi_data$time_s),
            end = max(roi_data$time_s),
            duration = max(roi_data$time_s) - min(roi_data$time_s)
          )
        }
      }
      
      return(roi_times)
    }
    
    # Populate input boxes from ROI data
    populate_inputs_from_roi_data <- function(roi_data) {
      if (is.null(roi_data)) {
        # Clear all inputs and stored values
        updateNumericInput(session, "roi1_start", value = NULL)
        updateNumericInput(session, "roi2_start", value = NULL)
        updateNumericInput(session, "roi3_duration", value = 0.5)
        updateNumericInput(session, "roi4_duration", value = 0.2)
        updateNumericInput(session, "roi5_duration", value = 0.5)
        updateNumericInput(session, "roi6_end", value = NULL)
        updateNumericInput(session, "roi7_end", value = NULL)
        
        # Clear stored values (defaults will be stored when user clicks "Update")
        # roi_values$roi1_start <- NULL
        # roi_values$roi2_start <- NULL
        # roi_values$roi3_duration <- NULL
        # roi_values$roi4_duration <- NULL
        # roi_values$roi5_duration <- NULL
        # roi_values$roi6_end <- NULL
        # roi_values$roi7_end <- NULL
        # roi_values$has_roi_data <- FALSE
        roi_values$has_roi_data <- FALSE
      } else {
        # Populate from extracted data with rounding
        updateNumericInput(session, "roi1_start", value = round(roi_data$roi1_sens_ingress$start, 3))
        updateNumericInput(session, "roi2_start", value = round(roi_data$roi2_inflow_passage$start, 3))
        updateNumericInput(session, "roi3_duration", value = round(roi_data$roi3_prenadir$duration, 3))
        updateNumericInput(session, "roi4_duration", value = round(roi_data$roi4_nadir$duration, 3))
        updateNumericInput(session, "roi5_duration", value = round(roi_data$roi5_postnadir$duration, 3))
        updateNumericInput(session, "roi6_end", value = round(roi_data$roi6_outflow_passage$end, 3))
        updateNumericInput(session, "roi7_end", value = round(roi_data$roi7_sens_outgress$end, 3))
        
        # Store the values (for non-reactive access)
        roi_values$roi1_start <- roi_data$roi1_sens_ingress$start
        roi_values$roi2_start <- roi_data$roi2_inflow_passage$start
        roi_values$roi3_duration <- roi_data$roi3_prenadir$duration
        roi_values$roi4_duration <- roi_data$roi4_nadir$duration
        roi_values$roi5_duration <- roi_data$roi5_postnadir$duration
        roi_values$roi6_end <- roi_data$roi6_outflow_passage$end
        roi_values$roi7_end <- roi_data$roi7_sens_outgress$end
        #roi_values$has_roi_data <- TRUE
      }
    }
    
    # Validate ROI inputs
    validate_roi_inputs <- function() {
      nadir <- nadir_info()
      if (!nadir$available) {
        return(list(valid = FALSE, message = "Nadir information not available"))
      }
      
      # Get all input values
      roi1_start <- input$roi1_start
      roi2_start <- input$roi2_start
      roi3_duration <- input$roi3_duration
      roi4_duration <- input$roi4_duration
      roi5_duration <- input$roi5_duration
      roi6_end <- input$roi6_end
      roi7_end <- input$roi7_end
      
      # Check for missing values
      if (any(is.null(c(roi1_start, roi2_start, roi3_duration, roi4_duration, roi5_duration, roi6_end, roi7_end)))) {
        return(list(valid = FALSE, message = "All ROI timing fields must be filled"))
      }
      
      # Check for positive durations
      if (any(c(roi3_duration, roi4_duration, roi5_duration) <= 0)) {
        return(list(valid = FALSE, message = "All durations must be positive"))
      }
      
      # Calculate ROI boundaries for validation
      nadir_time <- nadir$time
      roi4_start <- nadir_time - (roi4_duration / 2)
      roi4_end <- nadir_time + (roi4_duration / 2)
      roi3_start <- roi4_start - roi3_duration
      roi5_end <- roi4_end + roi5_duration
      
      # Validate logical order: roi1 < roi2 < roi3_start < roi4_start < roi4_end < roi5_end < roi6 < roi7
      if (!(roi1_start < roi2_start && roi2_start < roi3_start && roi3_start < roi4_start &&
            roi4_end < roi5_end && roi5_end < roi6_end && roi6_end < roi7_end)) {
        return(list(valid = FALSE, message = "ROI times must be in logical order"))
      }
      
      return(list(valid = TRUE))
    }
    
    # ============================= #
    # /// Initialization \\\ ####  
    # ============================= # 
    
    # Initialize ROI inputs when sensor changes
    observeEvent(sensor_selector$selected_sensor(), {
      req(sensor_selector$selected_sensor())
      
      # Try to extract ROI times from delineated file
      roi_data <- extract_roi_times_from_delineated(sensor_selector$selected_sensor())
      
      # Populate input boxes
      populate_inputs_from_roi_data(roi_data)
    })
    
    # ============================= #
    # /// ROI Times Calculation \\\ ####  
    # ============================= # 
    
    # Calculate ROI times based on stored values (NOT reactive to inputs)
    roi_times <- reactive({
      nadir <- nadir_info()
      if (!nadir$available) return(NULL)
      
      # Only calculate if roi_values$has_roi_data is TRUE
      if (!roi_values$has_roi_data) return(NULL)
      
      # Read from stored values (NOT from inputs - this breaks reactivity)
      roi1_start <- roi_values$roi1_start
      roi2_start <- roi_values$roi2_start
      roi3_duration <- roi_values$roi3_duration
      roi4_duration <- roi_values$roi4_duration
      roi5_duration <- roi_values$roi5_duration
      roi6_end <- roi_values$roi6_end
      roi7_end <- roi_values$roi7_end
      
      # Check for missing values
      if (any(is.null(c(roi1_start, roi2_start, roi3_duration, roi4_duration, roi5_duration, roi6_end, roi7_end)))) {
        return(NULL)
      }
      
      # Calculate ROI boundaries using duration-based logic
      nadir_time <- nadir$time
      
      # ROI 4 boundaries (centered on nadir)
      roi4_start <- nadir_time - (roi4_duration / 2)
      roi4_end <- nadir_time + (roi4_duration / 2)
      
      # ROI 3 boundaries (duration before ROI 4)
      roi3_end <- roi4_start
      roi3_start <- roi3_end - roi3_duration
      
      # ROI 5 boundaries (duration after ROI 4)
      roi5_start <- roi4_end
      roi5_end <- roi5_start + roi5_duration
      
      # Other ROI boundaries
      roi1_end <- roi2_start
      roi2_end <- roi3_start
      roi6_start <- roi5_end
      roi7_start <- roi6_end
      
      # Get data start/end times
      sensor_data <- read_sensor_data(output_dir(), sensor_selector$selected_sensor(), "min")
      if (is.null(sensor_data)) return(NULL)
      
      data_start <- min(sensor_data$time_s)
      data_end <- max(sensor_data$time_s)
      
      # Create table
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
                     paste(round(roi1_end - roi1_start, 3), "s"),
                     paste(round(roi2_end - roi2_start, 3), "s"),
                     paste(round(roi3_duration, 3), "s"),
                     paste(round(roi4_duration, 3), "s"),
                     paste(round(roi5_duration, 3), "s"),
                     paste(round(roi6_end - roi6_start, 3), "s"),
                     paste(round(roi7_end - roi7_start, 3), "s"),
                     paste(round(data_end - roi7_end, 3), "s")),
        check.names = FALSE
      )
      
      return(list(
        table = roi_times_df,
        boundaries = c(data_start, roi1_start, roi2_start, roi3_start, 
                       roi4_start, roi5_start, roi6_start, roi7_start, roi7_end, data_end)
      ))
    })
    
    # ============================= #
    # /// UI State management \\\ ####  
    # ============================= # 
    
    # Disable ROI boundary inputs when already delineated
    observe({
      req(sensor_selector$selected_sensor())
      global_sensor_state$summary_updated
      global_sensor_state$data_updated
      
      status <- get_sensor_status(sensor_selector$selected_sensor(), output_dir())
      
      # Disable ROI 1 start and ROI 7 end if already delineated
      if (status$delineated) {
        shinyjs::disable("roi1_start")
        shinyjs::disable("roi7_end")
      } else {
        shinyjs::enable("roi1_start")
        shinyjs::enable("roi7_end")
      }
    })
    
    # Enable/disable normalized checkbox based on sensor status  
    observe({
      req(sensor_selector$selected_sensor())
      global_sensor_state$summary_updated
      global_sensor_state$data_updated
      
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
    
    # Button state management
    observe({
      req(sensor_selector$selected_sensor())
      
      nadir <- nadir_info()
      status <- sensor_status()
      
      # Check if user has entered values in input boxes
      has_input_values <- !is.null(input$roi1_start) && !is.null(input$roi2_start) && 
        !is.null(input$roi3_duration) && !is.null(input$roi4_duration) &&
        !is.null(input$roi5_duration) && !is.null(input$roi6_end) && 
        !is.null(input$roi7_end)
      
      button_states <- list(
        "update_roi" = has_input_values,
        "create_delineated" = nadir$available && has_input_values && roi_values$has_roi_data,
        "start_over" = status$delineated,
        "normalize_time" = status$delineated && !status$normalized,
        "passage_time" = status$delineated && !status$passage_times,
        "nadir_btn" = (!nadir_values$edit_mode || !is.null(nadir_values$selected_point)),
        "cancel_nadir_btn" = nadir_values$edit_mode
      )
      
      manage_button_states(session, button_states)
    })
    
    # Update button text based on delineation status
    observe({
      req(sensor_selector$selected_sensor())
      status <- sensor_status()
      
      if (status$delineated) {
        updateActionButton(session, "create_delineated", label = "Modify current delineation")
        shinyjs::removeClass("create_delineated", "btn-success")
        shinyjs::addClass("create_delineated", "btn-warning")
      } else {
        updateActionButton(session, "create_delineated", label = "Apply delineation and trim data")
        shinyjs::removeClass("create_delineated", "btn-warning")
        shinyjs::addClass("create_delineated", "btn-success")
      }
    })
    
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
        updateActionButton(session, "nadir_btn", label = "Select Pressure Nadir")
        shinyjs::removeClass("nadir_btn", "btn-success")
        shinyjs::addClass("nadir_btn", "btn-warning")
      }
    })
    
    # ============================= #
    # /// Event handlers \\\ ####  
    # ============================= # 
    
    # Update ROI button
    observeEvent(input$update_roi, {
      validation <- validate_roi_inputs()
      if (!validation$valid) {
        showNotification(validation$message, type = "error")
        return()
      }
      
      # Copy input values to stored values (this triggers roi_times() update)
      roi_values$roi1_start <- input$roi1_start
      roi_values$roi2_start <- input$roi2_start
      roi_values$roi3_duration <- input$roi3_duration
      roi_values$roi4_duration <- input$roi4_duration
      roi_values$roi5_duration <- input$roi5_duration
      roi_values$roi6_end <- input$roi6_end
      roi_values$roi7_end <- input$roi7_end
      roi_values$has_roi_data <- TRUE
      
      showNotification("ROI plot and table updated", type = "message")
    })
    
    # Apply delineation button (now includes trimming)
    observeEvent(input$create_delineated, {
      req(sensor_selector$selected_sensor(), roi_times())
      
      validation <- validate_roi_inputs()
      if (!validation$valid) {
        showNotification(validation$message, type = "error")
        return()
      }
      
      # Check if sensor is already delineated
      status <- sensor_status()
      
      if (status$delineated) {
        # Show confirmation dialog for re-applying
        showModal(modalDialog(
          title = "Modify Delineation",
          paste("Sensor", sensor_selector$selected_sensor(), "is already delineated.", 
                "This will modify the existing delineation using the ROI 2-6 timings.", 
                "ROI 1 start and ROI 7 end boundaries will be preserved.", 
                "Continue?"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_reapply_delineation"), "Continue", class = "btn-warning")
          )
        ))
      } else {
        # Apply delineation and trim directly for new sensors
        create_delineated_dataset()
      }
    })
    
    # Confirm re-apply delineation
    observeEvent(input$confirm_reapply_delineation, {
      removeModal()
      create_delineated_dataset()
    })
    
    # Start over button
    observeEvent(input$start_over, {
      req(sensor_selector$selected_sensor())
      
      delineated_path <- file.path(output_dir(), "csv", "delineated", paste0(sensor_selector$selected_sensor(), "_delineated.csv"))
      if (file.exists(delineated_path)) {
        file.remove(delineated_path)
      }
      
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
        trigger_data_update()
        trigger_summary_update()
        showNotification("Reset to original sensor file", type = "message")
      } else {
        showNotification("Failed to reset sensor status", type = "error")
      }
    })
    
    
    
    # Edit nadir button
    observeEvent(input$nadir_btn, {
      if (!nadir_values$edit_mode) {
        nadir_values$edit_mode <- TRUE
        nadir_values$selected_point <- NULL
        nadir_values$baseline_click <- event_data("plotly_click", source = "roi_nadir_plot")
      } else if (!is.null(nadir_values$selected_point)) {
        success <- safe_update_sensor_index(
          output_dir(), 
          sensor_selector$selected_sensor(),
          list(
            "pres_min.time." = nadir_values$selected_point$x,
            "pres_min.kPa." = nadir_values$selected_point$y
          )
        )
        
        if (success) {
          trigger_data_update()
          trigger_summary_update()
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
    
    # ============================= #
    # /// Helper functions (continued) \\\ ####  
    # ============================= # 
    
    # Function to create delineated dataset
    create_delineated_dataset <- function() {
      tryCatch({
        sensor_data <- read_sensor_data(output_dir(), sensor_selector$selected_sensor(), "min")
        
        delineated_dir <- file.path(output_dir(), "csv", "delineated")
        if (!dir.exists(delineated_dir)) {
          dir.create(delineated_dir, showWarnings = FALSE, recursive = TRUE)
        }
        
        if (!dir.exists(delineated_dir)) {
          showNotification("Failed to create delineated directory", type = "error")
          return()
        }
        
        # Determine if this is a re-application
        current_status <- sensor_status()
        
        if (current_status$delineated) {
          # For re-application: get ROI 1 start and ROI 7 end from existing delineated data
          existing_roi_data <- extract_roi_times_from_delineated(sensor_selector$selected_sensor())
          if (is.null(existing_roi_data)) {
            showNotification("Cannot extract existing ROI boundaries for re-application", type = "error")
            return()
          }
          
          # Use existing boundaries for ROI 1 start and ROI 7 end, input values for others
          nadir <- nadir_info()
          nadir_time <- nadir$time
          
          # Calculate ROI boundaries using new duration logic
          roi4_start <- nadir_time - (input$roi4_duration / 2)
          roi4_end <- nadir_time + (input$roi4_duration / 2)
          roi3_start <- roi4_start - input$roi3_duration
          roi5_end <- roi4_end + input$roi5_duration
          
          boundaries <- c(
            min(sensor_data$time_s),                           # data_start
            existing_roi_data$roi1_sens_ingress$start,        # roi1_start (preserved)
            input$roi2_start,                                  # roi2_start (from input)
            roi3_start,                                        # roi3_start (calculated from duration)
            roi4_start,                                        # roi4_start (calculated)
            roi4_end,                                          # roi4_end (calculated)
            roi5_end,                                          # roi5_end (calculated from duration)
            input$roi6_end,                                    # roi6_end (from input)
            existing_roi_data$roi7_sens_outgress$end,        # roi7_end (preserved)
            max(sensor_data$time_s)                           # data_end
          )
        } else {
          # For new delineation: use all input values with duration calculations
          times <- roi_times()
          if (is.null(times)) {
            showNotification("ROI times not available", type = "error")
            return()
          }
          boundaries <- times$boundaries
        }
        
        # Apply ROI labels
        sensor_data$roi <- cut(sensor_data$time_s, 
                               breaks = boundaries,
                               labels = c("trim_start", "roi1_sens_ingress", "roi2_inflow_passage", 
                                          "roi3_prenadir", "roi4_nadir", "roi5_postnadir", 
                                          "roi6_outflow_passage", "roi7_sens_outgress", "trim_end"),
                               include.lowest = TRUE, right = FALSE)
        
        # Auto-trim: Remove trim regions immediately
        sensor_data <- sensor_data[!sensor_data$roi %in% c("trim_start", "trim_end"), ]
        
        # Auto-normalize time series (before writing file)
        tryCatch({
          nadir <- nadir_info()
          if (nadir$available) {
            start_time <- boundaries[2]  # start of roi1_sens_ingress
            end_time <- boundaries[9]    # end of roi7_sens_outgress  
            mid_time <- nadir$time
            
            sensor_data <- sensor_data %>%
              mutate(time_norm = case_when(
                time_s <= start_time ~ 0,
                time_s >= end_time ~ 1,
                time_s > start_time & time_s < mid_time ~ (time_s - start_time) / (mid_time - start_time) * 0.5,
                time_s >= mid_time & time_s <= end_time ~ 0.5 + (time_s - mid_time) / (end_time - mid_time) * 0.5
              ))
          }
        }, error = function(e) {
          warning("Time normalization failed: ", e$message)
        })
        
        # Write the trimmed, delineated, and normalized file ONCE
        output_file <- file.path(delineated_dir, paste0(sensor_selector$selected_sensor(), "_delineated.csv"))
        write.csv(sensor_data, output_file, row.names = FALSE)
        
        if (!file.exists(output_file)) {
          showNotification("Failed to create delineated file", type = "error")
          return()
        }
        
        # Auto-calculate passage times (after file written, no data modification)
        passage_updates <- list()
        tryCatch({
          nadir <- nadir_info()
          if (nadir$available) {
            first_time <- min(sensor_data$time_s)
            last_time <- max(sensor_data$time_s)
            nadir_time <- nadir$time
            
            passage_duration_s <- last_time - first_time
            ingress_nadir_s <- nadir_time - first_time
            nadir_outgress_s <- last_time - nadir_time
            
            format_mm_ss <- function(seconds) {
              minutes <- floor(seconds / 60)
              secs <- round(seconds %% 60)
              sprintf("%02d:%02d", minutes, secs)
            }
            
            passage_updates <- list(
              passage_times = "Y",
              passage_duration.mm.ss. = format_mm_ss(passage_duration_s),
              ingress_nadir_duration.mm.ss. = format_mm_ss(ingress_nadir_s),
              nadir_outgress_duration.mm.ss. = format_mm_ss(nadir_outgress_s)
            )
          }
        }, error = function(e) {
          warning("Passage time calculation failed: ", e$message)
          passage_updates <- list()
        })
        
        # Determine if this is a re-application
        current_status <- sensor_status()
        
        if (current_status$delineated) {
          # Re-applying: update roi_config and auto-calculated fields
          success <- safe_update_sensor_index(
            output_dir(),
            sensor_selector$selected_sensor(),
            c(list(roi_config = "Manual", normalized = "Y"), passage_updates)
          )
          message_text <- "Delineation re-applied with normalization and passage times!"
        } else {
          # New delineation: set all flags including auto-calculated ones
          success <- safe_update_sensor_index(
            output_dir(),
            sensor_selector$selected_sensor(),
            c(list(
              delineated = "Y",
              trimmed = "Y",
              roi_config = "Manual",
              normalized = "Y"
            ), passage_updates)
          )
          message_text <- "Delineation applied with trimming, normalization and passage times!"
        }
        
        if (success) {
          trigger_data_update()
          trigger_summary_update()
          showNotification(message_text, type = "message")
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
    
    # Display current nadir
    output$current_nadir_display <- renderText({
      nadir <- nadir_info()
      if (nadir$available) {
        paste0("Time: ", round(nadir$time, 3), "s\nPressure: ", round(nadir$value, 2), " kPa")
      }
    })
    
    # Status displays
    status_controls <- statusModuleServer("status_display",
                                          sensor_name_reactive = reactive(sensor_selector$selected_sensor()),
                                          output_dir_reactive = reactive(output_dir()),
                                          check_types = c("delineation", "normalization", "passage_times"),
                                          invalidation_trigger = reactive(global_sensor_state$summary_updated),
                                          individual_outputs = TRUE)
    
    # Nadir editing status display
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
    
    # Display ROI table
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
    
    # Status outputs
    output$normalize_status <- renderText({
      req(sensor_selector$selected_sensor())
      status <- sensor_status()
      
      if (status$normalized) {
        "Time series normalized"
      } else {
        ""
      }
    })
    
    output$passage_status <- renderText({
      req(sensor_selector$selected_sensor())
      status <- sensor_status()
      
      if (status$passage_times) {
        "Passage times calculated"
      } else {
        ""
      }
    })
    
    # Duration text outputs
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
    
    # Create main plot
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
                                        # Show ROI markers when we have ROI data
                                        !is.null(roi_times())
                                      }),
                                      title_prefix = "ROI Delineation",
                                      plot_source = "roi_nadir_plot"
    )
    
  })  # End of moduleServer
}     # End of roiServer