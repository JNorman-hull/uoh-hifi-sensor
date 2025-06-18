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
    tags$p("Adjust ROI boundaries using the sliders in the sidebar. ROI 4 (Nadir) is centered on the pressure nadir with adjustable duration. Click 'Apply Delineation' to create the delineated dataset."),
    tags$ul(
      tags$li("ROI 1 Sensor ingress: Sensor enters system"),
      tags$li("ROI 2 Intake passage: Movement through intake structures"),
      tags$li("ROI 3 Pre-nadir: Region before impeller"),
      tags$li("ROI 4 Nadir: Critical passage through impeller (duration-based, centered on nadir)"),
      tags$li("ROI 5 Post-nadir: Region after impeller"),
      tags$li("ROI 6 Outflow passage: Movement through outflow structures"),
      tags$li("ROI 7 Sensor outgress: Sensor exits system")
    )
  )
}

roiSidebarUI <- function(id) {
  ns <- NS(id)
  
  # CSS for subtle scrollbar
  scroll_css <- HTML("
    <style>
      .scrollable-sidebar::-webkit-scrollbar {
        width: 8px;
        background-color: transparent;
      }
      .scrollable-sidebar:hover::-webkit-scrollbar-track {
        background: rgba(240,240,240,0.5);
        border-radius: 4px;
      }
      .scrollable-sidebar:hover::-webkit-scrollbar-thumb {
        background: rgba(180,180,180,0.5);
        border-radius: 4px;
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
        
        
        h4("ROI Configuration"),
        configurationSidebarUI(ns("roi_config"), config_type = "roi", 
                               label = "Delineation configuration:"),
        
        textOutput(ns("config_change_status")),
        br(),
        
        # ROI 1 Start
        div(style = "margin-bottom: 5px;",
            fluidRow(
              column(8, sliderInput(ns("roi1_start"), " ROI 1 Start Time (x)", min = 0, max = 100, value = 20, step = 0.01, width = "100%")),
              column(4, numericInput(ns("roi1_start_input"), "", value = 20, step = 0.01, width = "100%"))
            )
        ),
        
        # ROI 2 Start  
        div(style = "margin-bottom: 5px;",
            fluidRow(
              column(8, sliderInput(ns("roi2_start"), " ROI 2 Start Time (x)", min = 0, max = 100, value = 20, step = 0.01, width = "100%")),
              column(4, numericInput(ns("roi2_start_input"), "", value = 20, step = 0.01, width = "100%"))
            )
        ),
        # ROI 3 Start
        div(style = "margin-bottom: 5px;",
            fluidRow(
              column(8, sliderInput(ns("roi3_start"), " ROI 3 Start Time (x)", min = 0, max = 100, value = 20, step = 0.01, width = "100%")),
              column(4, numericInput(ns("roi3_start_input"), "", value = 20, step = 0.01, width = "100%"))
            )
        ),
        
        # ROI 4 Duration (centered on nadir)
        div(style = "margin-bottom: 5px;",
            tags$label("ROI 4 Nadir Duration:", style = "font-weight: bold; margin-bottom: 5px; display: block;"),
            numericInput(ns("roi4_duration"), NULL, value = 0.4, min = 0.01, max = 2.0, step = 0.01, width = "100%")
        ),
        
        # ROI 5 End
        div(style = "margin-bottom: 5px;",
            fluidRow(
              column(8, sliderInput(ns("roi5_end"), " ROI 5 End Time (x)", min = 0, max = 100, value = 20, step = 0.01, width = "100%")),
              column(4, numericInput(ns("roi5_end_input"), "", value = 20, step = 0.01, width = "100%"))
            )
        ),
        
        # ROI 6 End
        div(style = "margin-bottom: 5px;",
            fluidRow(
              column(8, sliderInput(ns("roi6_end"), " ROI 6 End Time (x)", min = 0, max = 100, value = 20, step = 0.01, width = "100%")),
              column(4, numericInput(ns("roi6_end_input"), "", value = 20, step = 0.01, width = "100%"))
            )
        ),
        
        # ROI 7 End
        div(style = "margin-bottom: 5px;",
            fluidRow(
              column(8, sliderInput(ns("roi7_end"), " ROI 7 End Time (x)", min = 0, max = 100, value = 20, step = 0.01, width = "100%")),
              column(4, numericInput(ns("roi7_end_input"), "", value = 20, step = 0.01, width = "100%"))
            )
        ),

        h4("Actions"),
        actionButton(ns("update_plot"), "Update Plot", class = "btn-info btn-block"),
        div(style = "margin-bottom: 15px;",
            textInput(ns("config_label"), "Configuration label:", 
                      value = "", width = "100%", placeholder = "e.g., Custom_delineation")),
        actionButton(ns("save_config"), "Save Current Configuration", class = "btn-warning btn-block"),
        actionButton(ns("apply_delineation"), "Apply Delineation", class = "btn-success btn-block"),
        actionButton(ns("trim_sensor"), "Trim sensor start and end", class = "btn-warning btn-block"),
        actionButton(ns("start_over"), "Start Over", class = "btn-danger btn-block"),
        
        hr(),
        
        h4("Pressure Nadir Options"),
        textOutput(ns("current_nadir_display")),
        actionButton(ns("nadir_btn"), "Modify Pressure Nadir", class = "btn-warning btn-block"),
        actionButton(ns("cancel_nadir_btn"), "Cancel", class = "btn-danger btn-block"),
        textOutput(ns("nadir_status")),
        
        hr(),
        
        h4("Time normalization"),
        actionButton(ns("normalize_time"), "Normalize time series", class = "btn-primary btn-block"),
        textOutput(ns("normalize_status")),
        
        hr(),
        
        h4("Passage time calculation"),
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

# ROI Server - Reactive Values & Data Loading Section

roiServer <- function(id, output_dir, summary_data, processing_complete = reactive(FALSE), 
                      session_state = NULL, global_sensor_state, trigger_data_update, 
                      trigger_summary_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ============================= #
    # /// Reactive values \\\ ####  
    # ============================= #   
    
    # Simplified ROI state - no more complex custom ROI management
    roi_values <- reactiveValues(
      current_config = NULL,
      baseline_config = NULL,
      sliders_changed = FALSE,
      slider_ranges_set = FALSE,
      trim_boundaries_changed = FALSE,
      populating_sliders = FALSE,
      syncing_inputs = FALSE, 
      committed_boundaries = NULL
    )
    
    # Nadir editing state (keep this as it's still needed)
    nadir_values <- reactiveValues(
      edit_mode = FALSE,
      selected_point = NULL,
      baseline_click = NULL
    )
    
    # ============================= #
    # /// Data loading & processing  \\\ ####  
    # ============================= # 
    
    # Sensor selection
    sensor_selector <- enhancedSensorSelectionServer("sensor_selector",
                                                     output_dir, processing_complete,
                                                     status_filter_type = "delineation",
                                                     session_state = session_state,
                                                     global_sensor_state = global_sensor_state,
                                                     trigger_summary_update = trigger_summary_update)
    
    # Get nadir info using shared function
    nadir_info <- reactive({
      req(sensor_selector$selected_sensor())
      #global_sensor_state$summary_updated
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
      req(sensor_selector$selected_sensor())
      global_sensor_state$data_updated
      
      # Check for delineated file first
      delineated_data <- read_sensor_data(output_dir(), sensor_selector$selected_sensor(), "delineated")
      if (!is.null(delineated_data)) {
        return(delineated_data)
      }
      
      # Fall back to regular minimal data
      return(read_sensor_data(output_dir(), sensor_selector$selected_sensor(), "min"))
    })
    
    # Get sensor data time range for slider limits
    sensor_time_range <- reactive({
      data <- selected_sensor_data()
      if (is.null(data)) return(c(0, 60))
      
      c(min(data$time_s), max(data$time_s))
    })
    
    # ============================= #
    # /// Configuration Management \\\ ####  
    # ============================= #
    
    # Load ROI configurations (similar to pressure/acceleration)
    roi_config <- configurationServer("roi_config",
                                      output_dir = output_dir,
                                      config_type = "roi",
                                      sensor_name = reactive(sensor_selector$selected_sensor()),
                                      auto_select_sensor_config = TRUE)
    
    # Store current config
    # Store current config
    observe({
      roi_values$current_config <- roi_config$current_config()
      roi_values$sliders_changed <- FALSE
    })
    
    # Update slider ranges when sensor changes
    observe({
      req(sensor_selector$selected_sensor())
      time_range <- sensor_time_range()
      
      # Update all slider ranges
      updateSliderInput(session, "roi1_start", min = time_range[1], max = time_range[2])
      updateNumericInput(session, "roi1_start_input", min = time_range[1], max = time_range[2])
      updateSliderInput(session, "roi2_start", min = time_range[1], max = time_range[2])
      updateNumericInput(session, "roi2_start_input", min = time_range[1], max = time_range[2])
      updateSliderInput(session, "roi3_start", min = time_range[1], max = time_range[2])
      updateNumericInput(session, "roi3_start_input", min = time_range[1], max = time_range[2])
      updateSliderInput(session, "roi5_end", min = time_range[1], max = time_range[2])
      updateNumericInput(session, "roi5_end_input", min = time_range[1], max = time_range[2])
      updateSliderInput(session, "roi6_end", min = time_range[1], max = time_range[2])
      updateNumericInput(session, "roi6_end_input", min = time_range[1], max = time_range[2])
      updateSliderInput(session, "roi7_end", min = time_range[1], max = time_range[2])
      updateNumericInput(session, "roi7_end_input", min = time_range[1], max = time_range[2])
      
      roi_values$slider_ranges_set <- TRUE
    })
    
    # Populate sliders from configuration OR existing delineated data
    observe({
      req(sensor_selector$selected_sensor(), roi_values$slider_ranges_set)
      
      status <- sensor_status()
      nadir <- nadir_info()
      
      if (!nadir$available) return()
      
      if (status$delineated) {
        # If delineated data exists, extract ROI boundaries from the data
        populate_sliders_from_delineated_data()
      } else {
        # If no delineated data, calculate from configuration
        populate_sliders_from_config()
      }
    })
    
    # ============================= #
    # /// Helper Functions \\\ ####  
    # ============================= #
    
    # Populate sliders from existing delineated data
    populate_sliders_from_delineated_data <- function() {
      roi_values$populating_sliders <- TRUE 
      delineated_data <- read_sensor_data(output_dir(), sensor_selector$selected_sensor(), "delineated")
      if (is.null(delineated_data) || !"roi" %in% names(delineated_data)) return()
      
      tryCatch({
        # Extract ROI boundaries from delineated data
        roi1_start <- min(delineated_data[delineated_data$roi == "roi1_sens_ingress", "time_s"])
        roi2_start <- min(delineated_data[delineated_data$roi == "roi2_inflow_passage", "time_s"])
        roi3_start <- min(delineated_data[delineated_data$roi == "roi3_prenadir", "time_s"])
        roi4_start <- min(delineated_data[delineated_data$roi == "roi4_nadir", "time_s"])
        roi4_end <- max(delineated_data[delineated_data$roi == "roi4_nadir", "time_s"])
        roi5_end <- max(delineated_data[delineated_data$roi == "roi5_postnadir", "time_s"])
        roi6_end <- max(delineated_data[delineated_data$roi == "roi6_outflow_passage", "time_s"])
        roi7_end <- max(delineated_data[delineated_data$roi == "roi7_sens_outgress", "time_s"])
        
        # Calculate ROI 4 duration
        roi4_duration <- roi4_end - roi4_start
        
        # Update sliders without triggering change detection
        isolate({
          updateSliderInput(session, "roi1_start", value = roi1_start)
          updateNumericInput(session, "roi1_start_input", value = roi1_start)
          updateSliderInput(session, "roi2_start", value = roi2_start)
          updateNumericInput(session, "roi2_start_input", value = roi2_start)
          updateSliderInput(session, "roi3_start", value = roi3_start)
          updateNumericInput(session, "roi3_start_input", value = roi3_start)
          updateSliderInput(session, "roi5_end", value = roi5_end)
          updateNumericInput(session, "roi5_end_input", value = roi5_end)
          updateSliderInput(session, "roi6_end", value = roi6_end)
          updateNumericInput(session, "roi6_end_input", value = roi6_end)
          updateSliderInput(session, "roi7_end", value = roi7_end)
          updateNumericInput(session, "roi7_end_input", value = roi7_end)
          updateNumericInput(session, "roi4_duration", value = roi4_duration)
        })
        
        roi_values$sliders_changed <- FALSE
        
        roi_values$baseline_config <- list(
          label = "From_existing_delineation",
          roi1_sens_ingress = roi2_start - roi1_start,
          roi2_inflow_passage = roi3_start - roi2_start,
          roi3_prenadir = roi4_start - roi3_start,
          roi4_nadir = roi4_duration,
          roi5_postnadir = roi5_end - roi4_end,
          roi6_outflow_passage = roi6_end - roi5_end,
          roi7_sens_outgress = roi7_end - roi6_end
        )
        
        roi_values$committed_boundaries <- c(
          min(delineated_data$time_s),  # data_start
          roi1_start, roi2_start, roi3_start, roi4_start, 
          roi4_end, roi5_end, roi6_end, roi7_end, 
          max(delineated_data$time_s)   # data_end
        )
        
      }, error = function(e) {
        warning("Error extracting ROI boundaries from delineated data: ", e$message)
      })
    }
    
    # Populate sliders from configuration
    populate_sliders_from_config <- function() {
      roi_values$populating_sliders <- TRUE
      config <- roi_values$current_config
      nadir <- nadir_info()
      
      if (is.null(config) || !nadir$available) return()
      
      # Calculate ROI boundaries based on nadir time and config durations
      nadir_time <- nadir$time
      
      # Calculate ROI 4 boundaries
      roi4_start <- nadir_time - (config$roi4_nadir / 2)
      roi4_end <- nadir_time + (config$roi4_nadir / 2)
      
      # Calculate other ROI boundaries working backwards and forwards
      roi3_start <- roi4_start - config$roi3_prenadir
      roi2_start <- roi3_start - config$roi2_inflow_passage
      roi1_start <- roi2_start - config$roi1_sens_ingress
      
      roi5_end <- roi4_end + config$roi5_postnadir
      roi6_end <- roi5_end + config$roi6_outflow_passage
      roi7_end <- roi6_end + config$roi7_sens_outgress
      
      # Update sliders without triggering change detection
      isolate({
        updateSliderInput(session, "roi1_start", value = roi1_start)
        updateNumericInput(session, "roi1_start_input", value = roi1_start)
        updateSliderInput(session, "roi2_start", value = roi2_start)
        updateNumericInput(session, "roi2_start_input", value = roi2_start)
        updateSliderInput(session, "roi3_start", value = roi3_start)
        updateNumericInput(session, "roi3_start_input", value = roi3_start)
        updateSliderInput(session, "roi5_end", value = roi5_end)
        updateNumericInput(session, "roi5_end_input", value = roi5_end)
        updateSliderInput(session, "roi6_end", value = roi6_end)
        updateNumericInput(session, "roi6_end_input", value = roi6_end)
        updateSliderInput(session, "roi7_end", value = roi7_end)
        updateNumericInput(session, "roi7_end_input", value = roi7_end)
        updateNumericInput(session, "roi4_duration", value = config$roi4_nadir)
      })
      
      roi_values$sliders_changed <- FALSE
      roi_values$baseline_config <- config
      
      sensor_data <- read_sensor_data(output_dir(), sensor_selector$selected_sensor(), "min")
      if (!is.null(sensor_data)) {
        roi_values$committed_boundaries <- c(
          min(sensor_data$time_s),  # data_start
          roi1_start, roi2_start, roi3_start, roi4_start,
          roi4_end,    # roi5_start 
          roi5_end,    # roi6_start
          roi6_end,    # roi7_start
          roi7_end, 
          max(sensor_data$time_s)   # data_end
        )
      }
      
    }
    
    # ============================= #
    # /// Change Detection \\\ ####  
    # ============================= #
    
    # ============================= #
    # /// Bidirectional slider-input synchronization \\\ ####  
    # ============================= #
    
    # ROI 1 Start
    observeEvent(input$roi1_start, {
      if (!roi_values$populating_sliders && !roi_values$syncing_inputs) {
        roi_values$syncing_inputs <- TRUE
        updateNumericInput(session, "roi1_start_input", value = input$roi1_start)
        roi_values$syncing_inputs <- FALSE
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$roi1_start_input, {
      if (!roi_values$populating_sliders && !roi_values$syncing_inputs && 
          !is.na(input$roi1_start_input)) {
        time_range <- sensor_time_range()
        if (input$roi1_start_input >= time_range[1] && input$roi1_start_input <= time_range[2]) {
          roi_values$syncing_inputs <- TRUE
          updateSliderInput(session, "roi1_start", value = input$roi1_start_input)
          roi_values$syncing_inputs <- FALSE
        }
      }
    }, ignoreInit = TRUE)
    
    # ROI 2 Start
    observeEvent(input$roi2_start, {
      if (!roi_values$populating_sliders && !roi_values$syncing_inputs) {
        roi_values$syncing_inputs <- TRUE
        updateNumericInput(session, "roi2_start_input", value = input$roi2_start)
        roi_values$syncing_inputs <- FALSE
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$roi2_start_input, {
      if (!roi_values$populating_sliders && !roi_values$syncing_inputs && 
          !is.na(input$roi2_start_input)) {
        time_range <- sensor_time_range()
        if (input$roi2_start_input >= time_range[1] && input$roi2_start_input <= time_range[2]) {
          roi_values$syncing_inputs <- TRUE
          updateSliderInput(session, "roi2_start", value = input$roi2_start_input)
          roi_values$syncing_inputs <- FALSE
        }
      }
    }, ignoreInit = TRUE)
    
    # ROI 3 Start
    observeEvent(input$roi3_start, {
      if (!roi_values$populating_sliders && !roi_values$syncing_inputs) {
        roi_values$syncing_inputs <- TRUE
        updateNumericInput(session, "roi3_start_input", value = input$roi3_start)
        roi_values$syncing_inputs <- FALSE
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$roi3_start_input, {
      if (!roi_values$populating_sliders && !roi_values$syncing_inputs && 
          !is.na(input$roi3_start_input)) {
        time_range <- sensor_time_range()
        if (input$roi3_start_input >= time_range[1] && input$roi3_start_input <= time_range[2]) {
          roi_values$syncing_inputs <- TRUE
          updateSliderInput(session, "roi3_start", value = input$roi3_start_input)
          roi_values$syncing_inputs <- FALSE
        }
      }
    }, ignoreInit = TRUE)
    
    # ROI 5 End
    observeEvent(input$roi5_end, {
      if (!roi_values$populating_sliders && !roi_values$syncing_inputs) {
        roi_values$syncing_inputs <- TRUE
        updateNumericInput(session, "roi5_end_input", value = input$roi5_end)
        roi_values$syncing_inputs <- FALSE
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$roi5_end_input, {
      if (!roi_values$populating_sliders && !roi_values$syncing_inputs && 
          !is.na(input$roi5_end_input)) {
        time_range <- sensor_time_range()
        if (input$roi5_end_input >= time_range[1] && input$roi5_end_input <= time_range[2]) {
          roi_values$syncing_inputs <- TRUE
          updateSliderInput(session, "roi5_end", value = input$roi5_end_input)
          roi_values$syncing_inputs <- FALSE
        }
      }
    }, ignoreInit = TRUE)
    
    # ROI 6 End
    observeEvent(input$roi6_end, {
      if (!roi_values$populating_sliders && !roi_values$syncing_inputs) {
        roi_values$syncing_inputs <- TRUE
        updateNumericInput(session, "roi6_end_input", value = input$roi6_end)
        roi_values$syncing_inputs <- FALSE
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$roi6_end_input, {
      if (!roi_values$populating_sliders && !roi_values$syncing_inputs && 
          !is.na(input$roi6_end_input)) {
        time_range <- sensor_time_range()
        if (input$roi6_end_input >= time_range[1] && input$roi6_end_input <= time_range[2]) {
          roi_values$syncing_inputs <- TRUE
          updateSliderInput(session, "roi6_end", value = input$roi6_end_input)
          roi_values$syncing_inputs <- FALSE
        }
      }
    }, ignoreInit = TRUE)
    
    # ROI 7 End
    observeEvent(input$roi7_end, {
      if (!roi_values$populating_sliders && !roi_values$syncing_inputs) {
        roi_values$syncing_inputs <- TRUE
        updateNumericInput(session, "roi7_end_input", value = input$roi7_end)
        roi_values$syncing_inputs <- FALSE
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$roi7_end_input, {
      if (!roi_values$populating_sliders && !roi_values$syncing_inputs && 
          !is.na(input$roi7_end_input)) {
        time_range <- sensor_time_range()
        if (input$roi7_end_input >= time_range[1] && input$roi7_end_input <= time_range[2]) {
          roi_values$syncing_inputs <- TRUE
          updateSliderInput(session, "roi7_end", value = input$roi7_end_input)
          roi_values$syncing_inputs <- FALSE
        }
      }
    }, ignoreInit = TRUE)
    
    # Track changes in slider inputs (similar to pressure/acceleration modules)
    observeEvent(list(input$roi1_start, input$roi1_start_input, 
                      input$roi2_start, input$roi2_start_input,
                      input$roi3_start, input$roi3_start_input,
                      input$roi5_end, input$roi5_end_input,
                      input$roi6_end, input$roi6_end_input,
                      input$roi7_end, input$roi7_end_input, 
                      input$roi4_duration), {
                        cat("populating_sliders:", roi_values$populating_sliders, "\n")
                        
                        if (roi_values$syncing_inputs || roi_values$populating_sliders) return()
                        
                        if (roi_values$populating_sliders) {
                          roi_values$populating_sliders <- FALSE  # Reset flag here
                          return()
                        }
                        
                        # Only track changes after initial population and when NOT populating
                        if (!roi_values$slider_ranges_set || 
                            is.null(roi_values$baseline_config) || 
                            roi_values$populating_sliders) return()
                        
                        config <- roi_values$baseline_config
                        nadir <- nadir_info()
                        
                        if (!nadir$available) return()
                        
                        # Calculate expected values from baseline config
                        nadir_time <- nadir$time
                        roi4_start <- nadir_time - (config$roi4_nadir / 2)
                        roi4_end <- nadir_time + (config$roi4_nadir / 2)
                        
                        expected_roi1_start <- roi4_start - config$roi3_prenadir - config$roi2_inflow_passage - config$roi1_sens_ingress
                        expected_roi2_start <- roi4_start - config$roi3_prenadir - config$roi2_inflow_passage
                        expected_roi3_start <- roi4_start - config$roi3_prenadir
                        expected_roi5_end <- roi4_end + config$roi5_postnadir
                        expected_roi6_end <- expected_roi5_end + config$roi6_outflow_passage
                        expected_roi7_end <- expected_roi6_end + config$roi7_sens_outgress
                        
                        # ADD DEBUG OUTPUT HERE
                        cat("=== CHANGE DETECTION DEBUG ===\n")
                        cat("Expected roi1_start:", expected_roi1_start, "Actual:", input$roi1_start, "Diff:", abs(input$roi1_start - expected_roi1_start), "\n")
                        cat("Expected roi7_end:", expected_roi7_end, "Actual:", input$roi7_end, "Diff:", abs(input$roi7_end - expected_roi7_end), "\n")
                        
                        roi_values$trim_boundaries_changed <- (
                          abs(input$roi1_start - expected_roi1_start) > 0.05 ||
                            abs(input$roi7_end - expected_roi7_end) > 0.05
                        )
                        
                        cat("trim_boundaries_changed:", roi_values$trim_boundaries_changed, "\n")
                        cat("===============================\n")
                        
                        # Check if any slider values differ from expected
                        sliders_changed <- (
                          abs(input$roi1_start - expected_roi1_start) > 0.05 ||
                            abs(input$roi2_start - expected_roi2_start) > 0.05 ||
                            abs(input$roi3_start - expected_roi3_start) > 0.05 ||
                            abs(input$roi5_end - expected_roi5_end) > 0.05 ||
                            abs(input$roi6_end - expected_roi6_end) > 0.05 ||
                            abs(input$roi7_end - expected_roi7_end) > 0.05 ||
                            abs(input$roi4_duration - config$roi4_nadir) > 0.05
                        )
                        
                        roi_values$sliders_changed <- sliders_changed
                      }, ignoreInit = TRUE)
    
    # ============================= #
    # /// ROI Boundary Calculation \\\ ####  
    # ============================= #
    
    # Calculate current ROI boundaries from slider values
    current_roi_boundaries <- reactive({
      req(input$roi1_start, input$roi2_start, input$roi3_start, 
          input$roi5_end, input$roi6_end, input$roi7_end, input$roi4_duration)
      
      nadir <- nadir_info()
      if (!nadir$available) return(NULL)
      
      # Get sensor data to determine actual start/end times
      sensor_data <- read_sensor_data(output_dir(), sensor_selector$selected_sensor(), "min")
      if (is.null(sensor_data)) return(NULL)
      
      data_start <- min(sensor_data$time_s)
      data_end <- max(sensor_data$time_s)
      nadir_time <- nadir$time
      
      # Calculate all ROI boundaries
      roi1_start <- input$roi1_start
      roi2_start <- input$roi2_start  
      roi3_start <- input$roi3_start
      
      # ROI 4 calculated from nadir and duration
      roi4_start <- nadir_time - (input$roi4_duration / 2)
      roi4_end <- nadir_time + (input$roi4_duration / 2)
      
      # ROI 5 starts where ROI 4 ends, ends at slider value
      roi5_start <- roi4_end
      roi5_end <- input$roi5_end
      
      # ROI 6 starts where ROI 5 ends, ends at slider value  
      roi6_start <- roi5_end
      roi6_end <- input$roi6_end
      
      # ROI 7 starts where ROI 6 ends, ends at slider value
      roi7_start <- roi6_end
      roi7_end <- input$roi7_end
      
      # Return boundaries array for plotting (10 elements as expected by plot module)
      boundaries <- c(data_start, roi1_start, roi2_start, roi3_start,
                      roi4_start, roi5_start, roi6_start, roi7_start, roi7_end, data_end)
      
      return(boundaries)
    })
    
    current_roi_boundaries_debounced <- debounce(current_roi_boundaries, 2000)
    
    # Calculate ROI table data from current boundaries
    roi_table_data <- reactive({
      boundaries <- current_roi_boundaries_debounced()
      if (is.null(boundaries)) return(NULL)
      
      nadir <- nadir_info()
      if (!nadir$available) return(NULL)
      
      # Extract individual boundaries
      data_start <- boundaries[1]
      roi1_start <- boundaries[2]
      roi2_start <- boundaries[3]
      roi3_start <- boundaries[4]
      roi4_start <- boundaries[5]
      roi5_start <- boundaries[6]  # This is roi4_end
      roi6_start <- boundaries[7]  # This is roi5_end  
      roi7_start <- boundaries[8]  # This is roi6_end
      roi7_end <- boundaries[9]
      data_end <- boundaries[10]
      
      # Calculate ROI 4 end from nadir and duration
      roi4_end <- nadir$time + (input$roi4_duration / 2)
      
      # Create ROI table
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
                       paste(round(roi2_start, 3), "s"),
                       paste(round(roi3_start, 3), "s"),
                       paste(round(roi4_start, 3), "s"),
                       paste(round(roi4_end, 3), "s"),
                       paste(round(roi6_start, 3), "s"),
                       paste(round(roi7_start, 3), "s"),
                       paste(round(roi7_end, 3), "s"),
                       paste(round(data_end, 3), "s")),
        Duration = c(paste(round(roi1_start - data_start, 3), "s"),
                     paste(round(roi2_start - roi1_start, 3), "s"),
                     paste(round(roi3_start - roi2_start, 3), "s"),
                     paste(round(roi4_start - roi3_start, 3), "s"),
                     paste(round(input$roi4_duration, 3), "s"),
                     paste(round(roi6_start - roi5_start, 3), "s"),
                     paste(round(roi7_start - roi6_start, 3), "s"),
                     paste(round(roi7_end - roi7_start, 3), "s"),
                     paste(round(data_end - roi7_end, 3), "s")),
        check.names = FALSE
      )
      
      return(roi_times_df)
    })
    
    # ============================= #
    # /// UI State Management \\\ ####  
    # ============================= #
    
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
      
      button_states <- list(
        "apply_delineation" = nadir$available && (!status$delineated || roi_values$sliders_changed),
        "save_config" = roi_values$sliders_changed && !is.null(input$config_label) && nchar(trimws(input$config_label)) > 0,
        "start_over" = status$delineated,
        "trim_sensor" = status$delineated && !status$trimmed,
        "normalize_time" = status$delineated && status$trimmed && !status$normalized,
        "passage_time" = status$delineated && status$trimmed && !status$passage_times,
        "nadir_btn" = !nadir_values$edit_mode || !is.null(nadir_values$selected_point),
        "cancel_nadir_btn" = nadir_values$edit_mode,
        "update_plot" = roi_values$sliders_changed
      )
      
      manage_button_states(session, button_states)
    })
    
    # Update button text based on delineation status and slider changes
    observe({
      req(sensor_selector$selected_sensor())
      
      status <- sensor_status()
      
      if (status$delineated && roi_values$sliders_changed) {
        updateActionButton(session, "apply_delineation", label = "Modify Current Delineation")
        shinyjs::removeClass("apply_delineation", "btn-success")
        shinyjs::addClass("apply_delineation", "btn-warning")
      } else if (status$delineated && !roi_values$sliders_changed) {
        updateActionButton(session, "apply_delineation", label = "Apply Delineation")  
        shinyjs::removeClass("apply_delineation", "btn-warning")
        shinyjs::addClass("apply_delineation", "btn-success")
      } else {
        updateActionButton(session, "apply_delineation", label = "Apply Delineation")
        shinyjs::removeClass("apply_delineation", "btn-warning")
        shinyjs::addClass("apply_delineation", "btn-success")
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
    # /// Event Handlers \\\ ####  
    # ============================= #
    
    # Apply delineation button (simplified)
    observeEvent(input$apply_delineation, {
      req(sensor_selector$selected_sensor(), current_roi_boundaries())
      
      apply_delineated_dataset()
    })
    
    # Save configuration button  
    observeEvent(input$save_config, {
      config_name <- trimws(input$config_label)
      
      if (nchar(config_name) == 0) {
        showNotification("Please enter a configuration label", type = "error")
        return()
      }
      
      # Check if config already exists
      existing_configs <- roi_config$all_configs()
      if (!is.null(existing_configs) && config_name %in% names(existing_configs)) {
        showModal(modalDialog(
          title = "Configuration Exists",
          paste("Configuration '", config_name, "' already exists. Replace existing configuration?"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_save_config"), "Replace", class = "btn-warning")
          ),
          size = "m"
        ))
      } else {
        save_roi_configuration()
      }
    })
    
    # Confirm save configuration
    observeEvent(input$confirm_save_config, {
      removeModal()
      save_roi_configuration()
    })
    
    # Nadir editing handlers (keep existing functionality)
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
    
    observeEvent(input$update_plot, {
      roi_values$committed_boundaries <- current_roi_boundaries()
      roi_values$sliders_changed <- FALSE  # Reset the flag since we've "committed" the changes
    })
    
    # Start over button (simplified)
    observeEvent(input$start_over, {
      req(sensor_selector$selected_sensor())
      
      # Remove delineated file
      delineated_path <- file.path(output_dir(), "csv", "delineated", paste0(sensor_selector$selected_sensor(), "_delineated.csv"))
      if (file.exists(delineated_path)) {
        file.remove(delineated_path)
      }
      
      # Reset flags in sensor index
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
    
    # Trim sensor button (keep existing)
    observeEvent(input$trim_sensor, {
      req(sensor_selector$selected_sensor())
      
      sensor_data <- read_sensor_data(output_dir(), sensor_selector$selected_sensor(), "delineated")
      
      if (is.null(sensor_data)) {
        showNotification("Failed to read delineated dataset.", type = "error")
        return()
      }
      
      trimmed_data <- sensor_data[!sensor_data$roi %in% c("trim_start", "trim_end"), ]
      
      delineated_path <- file.path(output_dir(), "csv", "delineated", paste0(sensor_selector$selected_sensor(), "_delineated.csv"))
      write.csv(trimmed_data, delineated_path, row.names = FALSE)
      
      success <- safe_update_sensor_index(output_dir(), sensor_selector$selected_sensor(), list(trimmed = "Y"))
      
      if (success) {
        trigger_data_update()
        trigger_summary_update()
        showNotification("Sensor data trimmed successfully!", type = "message")
      } else {
        showNotification("Failed to update sensor index", type = "error")
      }
    })
    
    # Normalize time series (keep existing)
    observeEvent(input$normalize_time, {
      req(sensor_selector$selected_sensor())
      
      tryCatch({
        sensor_data <- read_sensor_data(output_dir(), sensor_selector$selected_sensor(), "delineated")
        
        if (is.null(sensor_data)) {
          showNotification("Failed to read delineated dataset", type = "error")
          return()
        }
        
        nadir <- nadir_info()
        start_time <- min(sensor_data$time_s)
        end_time <- max(sensor_data$time_s)
        mid_time <- nadir$time
        
        sensor_data <- sensor_data %>%
          mutate(time_norm = case_when(
            time_s <= start_time ~ 0,
            time_s >= end_time ~ 1,
            time_s > start_time & time_s < mid_time ~ (time_s - start_time) / (mid_time - start_time) * 0.5,
            time_s >= mid_time & time_s <= end_time ~ 0.5 + (time_s - mid_time) / (end_time - mid_time) * 0.5
          ))
        
        delineated_path <- file.path(output_dir(), "csv", "delineated", paste0(sensor_selector$selected_sensor(), "_delineated.csv"))
        write.csv(sensor_data, delineated_path, row.names = FALSE)
        
        success <- safe_update_sensor_index(output_dir(), sensor_selector$selected_sensor(), list(normalized = "Y"))
        
        if (success) {
          trigger_data_update()
          trigger_summary_update()
          showNotification("Time series normalized successfully!", type = "message")
        } else {
          showNotification("Warning: Normalization completed but failed to update index", type = "warning")
        }
        
      }, error = function(e) {
        showNotification(paste("Error normalizing time series:", e$message), type = "error")
      })
    })
    
    # Calculate passage times (keep existing)
    observeEvent(input$passage_time, {
      req(sensor_selector$selected_sensor())
      
      tryCatch({
        sensor_data <- read_sensor_data(output_dir(), sensor_selector$selected_sensor(), "delineated")
        
        if (is.null(sensor_data)) {
          showNotification("Failed to read delineated dataset", type = "error")
          return()
        }
        
        nadir <- nadir_info()
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
          trigger_data_update()
          trigger_summary_update()
          showNotification("Passage times calculated successfully!", type = "message")
        } else {
          showNotification("Failed to update sensor index", type = "error")
        }
        
      }, error = function(e) {
        showNotification(paste("Error calculating passage times:", e$message), type = "error")
      })
    })
    
    # ============================= #
    # /// Helper Functions \\\ ####  
    # ============================= #
    
    # Apply delineated dataset (simplified)
    apply_delineated_dataset <- function() {
      tryCatch({
        boundaries <- current_roi_boundaries()
        status <- sensor_status()
        nadir <- nadir_info()
        
        cat("trim_boundaries_changed:", roi_values$trim_boundaries_changed, "\n")
        cat("status$trimmed:", status$trimmed, "\n")
        cat("Will reset to original?", (roi_values$trim_boundaries_changed || !status$trimmed), "\n")
        
        if (is.null(boundaries)) {
          showNotification("ROI boundaries not available", type = "error")
          return()
        }
        
        # Simple logic: if trim boundaries changed OR not yet trimmed, reset to original
        if (status$delineated && roi_values$trim_boundaries_changed) {
          cat("RESETTING TO ORIGINAL FILE (trim boundaries changed)\n")
          # Reset to original file because trim boundaries changed on existing delineation
          sensor_data <- read_sensor_data(output_dir(), sensor_selector$selected_sensor(), "min")
          
          # Apply full delineation with trim regions
          sensor_data$roi <- cut(sensor_data$time_s, 
                                 breaks = boundaries,
                                 labels = c("trim_start", "roi1_sens_ingress", "roi2_inflow_passage", 
                                            "roi3_prenadir", "roi4_nadir", "roi5_postnadir", 
                                            "roi6_outflow_passage", "roi7_sens_outgress", "trim_end"),
                                 include.lowest = TRUE, right = FALSE)
          
          reset_flags <- list(trimmed = "N", normalized = "N", passage_times = "N")
          
        } else if (!status$delineated) {
          cat("FIRST TIME DELINEATION\n")
          # First time delineation - work with original file (already loaded)
          sensor_data <- selected_sensor_data()
          
          # Apply full delineation with trim regions
          sensor_data$roi <- cut(sensor_data$time_s, 
                                 breaks = boundaries,
                                 labels = c("trim_start", "roi1_sens_ingress", "roi2_inflow_passage", 
                                            "roi3_prenadir", "roi4_nadir", "roi5_postnadir", 
                                            "roi6_outflow_passage", "roi7_sens_outgress", "trim_end"),
                                 include.lowest = TRUE, right = FALSE)
          
          reset_flags <- list(trimmed = "N", normalized = "N", passage_times = "N")
          
        } else {
          cat("MODIFYING EXISTING TRIMMED FILE\n")
          # Just modify internal ROIs on existing trimmed file
          sensor_data <- selected_sensor_data()
          
          # FIXED: Create proper boundaries for trimmed data
          data_start <- min(sensor_data$time_s)
          data_end <- max(sensor_data$time_s)
          nadir_time <- nadir$time
          
          # Recalculate internal boundaries within the trimmed data range
          roi4_start <- nadir_time - (input$roi4_duration / 2)
          roi4_end <- nadir_time + (input$roi4_duration / 2)
          
          # Create boundaries array for trimmed data (no trim regions)
          internal_boundaries <- c(
            data_start,           # Actual trimmed start
            input$roi2_start,     # roi1 -> roi2
            input$roi3_start,     # roi2 -> roi3  
            roi4_start,           # roi3 -> roi4
            roi4_end,             # roi4 -> roi5 (roi5_start)
            input$roi5_end,       # roi5 -> roi6 (roi6_start)
            input$roi6_end,       # roi6 -> roi7 (roi7_start)
            data_end              # Actual trimmed end
          )
          
          # Check boundaries are in order
          if (any(diff(internal_boundaries) <= 0)) {
            showNotification("Error: ROI boundaries must be in ascending order", type = "error")
            return()
          }
          
          # Re-assign ROI labels (7 regions for 8 boundaries)
          sensor_data$roi <- cut(sensor_data$time_s, 
                                 breaks = internal_boundaries,
                                 labels = c("roi1_sens_ingress", "roi2_inflow_passage", 
                                            "roi3_prenadir", "roi4_nadir", "roi5_postnadir", 
                                            "roi6_outflow_passage", "roi7_sens_outgress"),
                                 include.lowest = TRUE, right = FALSE)
          
          # Reset normalized and passage times (but keep trimmed = Y)
          reset_flags <- list(normalized = "N", passage_times = "N")
        }
        
        # Save file
        delineated_dir <- file.path(output_dir(), "csv", "delineated")
        dir.create(delineated_dir, showWarnings = FALSE, recursive = TRUE)
        output_file <- file.path(delineated_dir, paste0(sensor_selector$selected_sensor(), "_delineated.csv"))
        write.csv(sensor_data, output_file, row.names = FALSE)
        
        # Update sensor index
        config_name <- if (roi_values$sliders_changed) "Custom" else roi_config$selected_config_name()
        
        updates <- c(list(delineated = "Y", roi_config = config_name), reset_flags)
        success <- safe_update_sensor_index(output_dir(), sensor_selector$selected_sensor(), updates)
        
        if (success) {
          trigger_data_update()
          trigger_summary_update()
          roi_values$sliders_changed <- FALSE
          showNotification("ROI boundaries updated successfully!", type = "message")
        }
        
      }, error = function(e) {
        showNotification(paste("Error applying delineation:", e$message), type = "error")
      })
    }
    
    # Save ROI configuration
    save_roi_configuration <- function() {
      config_name <- trimws(input$config_label)
      nadir <- nadir_info()
      
      if (!nadir$available) {
        showNotification("Nadir information not available", type = "error")
        return()
      }
      
      # Convert current slider values back to durations
      nadir_time <- nadir$time
      roi4_start <- nadir_time - (input$roi4_duration / 2)
      roi4_end <- nadir_time + (input$roi4_duration / 2)
      
      roi1_duration <- input$roi2_start - input$roi1_start
      roi2_duration <- input$roi3_start - input$roi2_start
      roi3_duration <- roi4_start - input$roi3_start
      roi4_duration <- input$roi4_duration
      roi5_duration <- input$roi5_end - roi4_end
      roi6_duration <- input$roi6_end - input$roi5_end
      roi7_duration <- input$roi7_end - input$roi6_end
      
      # Save configuration
      success <- save_config_value(
        output_dir = output_dir(),
        config_type = "roi",
        key = config_name,
        value = c(roi1_duration, roi2_duration, roi3_duration, roi4_duration, 
                  roi5_duration, roi6_duration, roi7_duration)
      )
      
      if (success) {
        roi_config$reload_configs()
        trigger_summary_update()
        
        # Reset change tracking and update baseline to current values
        roi_values$sliders_changed <- FALSE
        roi_values$baseline_config <- list(
          label = config_name,
          roi1_sens_ingress = roi1_duration,
          roi2_inflow_passage = roi2_duration,
          roi3_prenadir = roi3_duration,
          roi4_nadir = roi4_duration,
          roi5_postnadir = roi5_duration,
          roi6_outflow_passage = roi6_duration,
          roi7_sens_outgress = roi7_duration
        )
        updateTextInput(session, "config_label", value = "")
        
        # Apply the saved configuration to the current sensor
        apply_delineated_dataset()
        
        showNotification("ROI configuration saved and applied successfully!", type = "message")
      } else {
        showNotification("Failed to save ROI configuration", type = "error")
      }
    }
    
    # ============================= #
    # /// Output Rendering \\\ ####  
    # ============================= #
    
    # Configuration change status
    output$config_change_status <- renderText({
      req(sensor_selector$selected_sensor())
      
      if (roi_values$sliders_changed) {
        status <- sensor_status()
        
        if (status$delineated) {
          "ROI markers modified. Click 'Modify Current Delineation' to update the sensor."
        } else {
          "ROI markers modified. Click 'Apply Delineation' to apply configuration to sensor."
        }
      } else {
        ""
      }
    })
    
    # Status display
    status_controls <- statusModuleServer("status_display",
                                          sensor_name_reactive = reactive(sensor_selector$selected_sensor()),
                                          output_dir_reactive = reactive(output_dir()),
                                          check_types = c("delineation", "normalization", "passage_times"),
                                          invalidation_trigger = reactive(global_sensor_state$summary_updated),
                                          individual_outputs = TRUE)
    
    # Display current nadir
    output$current_nadir_display <- renderText({
      nadir <- nadir_info()
      if (nadir$available) {
        paste0("Time: ", round(nadir$time, 3), "s\nPressure: ", round(nadir$value, 2), " kPa")
      }
    })
    
    # Nadir editing status
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
    
    # Status outputs (keep existing)
    output$normalize_status <- renderText({
      req(sensor_selector$selected_sensor())
      status <- sensor_status()
      if (status$normalized) "Time series normalized" else ""
    })
    
    output$passage_status <- renderText({
      req(sensor_selector$selected_sensor())
      status <- sensor_status()
      if (status$passage_times) "Passage times calculated" else ""
    })
    
    # Duration text outputs (keep existing helper function)
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
    
    # Display ROI table
    output$roi_table <- DT::renderDataTable({
      table_data <- roi_table_data()
      if (!is.null(table_data)) {
        DT::datatable(
          table_data,
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
    
    # Main plot
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
                                        # If we have committed boundaries from button click, use those
                                        if (!is.null(roi_values$committed_boundaries)) {
                                          return(roi_values$committed_boundaries)
                                        }
                                        
                                        # Otherwise use existing data boundaries (for initial load)
                                        status <- sensor_status()
                                        if (status$delineated && status$trimmed) {
                                          return(get_roi_boundaries(sensor_selector$selected_sensor(), output_dir(), TRUE))
                                        }
                                        
                                        # Fallback to calculated boundaries for first load
                                        return(isolate(current_roi_boundaries()))
                                      }),
                                      show_roi_markers = reactive(TRUE),  # Always show ROI markers
                                      title_prefix = "ROI Delineation",
                                      plot_source = "roi_nadir_plot"
    )
    
  })  # End of moduleServer
}