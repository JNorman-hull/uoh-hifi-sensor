# ROI Delineation Module

# ROI Delineation Module

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
        tags$p("Use the sliders in the sidebar to adjust ROI boundaries. Each slider controls the start time of its respective ROI. The boundaries are sequential and constrained."),
        tags$ul(
          tags$li("ROI 1 Sensor ingress: Mark region sensor enters system from atmospheric pressure or other landmark feature (e.g., injection pipe)"),
          tags$li("ROI 2 Intake passage: Mark region sensor moves through intake structures and pipework leading towards the impeller."),
          tags$li("ROI 3 Pre-nadir: Mark region just before the impeller, highest risk of encountering pressure differentials and swirl flows."),
          tags$li("ROI 4 Nadir: Critical passage analysis zone with direct passage through the impeller. Hydraulic pinch point where maximum acceleration, rotation and minimum pressure likely to occur. Duration is controlled by the nadir duration input."),
          tags$li("ROI 5 Post-nadir: Mark region just after the impeller, highest risk of encountering guide vane or other forms of collision, residual turbulences and pressure recovery."),
          tags$li("ROI 6 Outflow passage: Mark region sensor moves through outflow pipework and structures leading towards sensor outgress. Velocity expected to decrease and pressure return to atmospheric pressure."),
          tags$li("ROI 7 Sensor outgress: Mark region sensor exits system from atmospheric pressure or other landmark features (e.g., stable flow indicative of tailwater)."),
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
        
        configurationSidebarUI(ns("roi_config"), config_type = "roi", 
                               label = "Delineation configuration:"),
        
        div(style = "display: flex; align-items: center; justify-content: start; margin-bottom: 15px;",
            tags$label("ROI 4 Nadir Duration (s):", `for` = ns("roi4_nadir_duration"), 
                       style = "margin-right: 8px;"),
            numericInput(ns("roi4_nadir_duration"), NULL, value = 0.2, min = 0.1, max = 2.0, step = 0.1,
                         width = "80px")
        ),
        
        h4("ROI Boundary Adjustment"),
        div(style = "color: #666; font-style: italic; margin-bottom: 10px;",
            "Adjust ROI boundaries using sliders below. Boundaries are sequential."),
        
        # ROI boundary sliders
        div(style = "margin-bottom: 10px;",
            sliderInput(ns("roi1_start"), "ROI 1 Start (s):", 
                        min = 0, max = 100, value = 20, step = 0.1, width = "100%")),
        
        div(style = "margin-bottom: 10px;",
            sliderInput(ns("roi2_start"), "ROI 2 Start (s):", 
                        min = 0, max = 100, value = 25, step = 0.1, width = "100%")),
        
        div(style = "margin-bottom: 10px;",
            sliderInput(ns("roi3_start"), "ROI 3 Start (s):", 
                        min = 0, max = 100, value = 28, step = 0.1, width = "100%")),
        
        div(style = "margin-bottom: 10px;",
            sliderInput(ns("roi4_start"), "ROI 4 Start (s):", 
                        min = 0, max = 100, value = 29, step = 0.1, width = "100%")),
        
        div(style = "margin-bottom: 10px;",
            sliderInput(ns("roi5_start"), "ROI 5 Start (s):", 
                        min = 0, max = 100, value = 31, step = 0.1, width = "100%")),
        
        div(style = "margin-bottom: 10px;",
            sliderInput(ns("roi6_start"), "ROI 6 Start (s):", 
                        min = 0, max = 100, value = 35, step = 0.1, width = "100%")),
        
        div(style = "margin-bottom: 10px;",
            sliderInput(ns("roi7_start"), "ROI 7 Start (s):", 
                        min = 0, max = 100, value = 40, step = 0.1, width = "100%")),
        
        div(style = "margin-bottom: 15px;",
            sliderInput(ns("roi7_end"), "ROI 7 End (s):", 
                        min = 0, max = 100, value = 45, step = 0.1, width = "100%")),
        
        div(style = "display: flex; align-items: center; justify-content: start; margin-bottom: 15px;",
            tags$label("Label:", `for` = ns("roi_config_label"), 
                       style = "margin-right: 8px;"),
            textInput(ns("roi_config_label"), NULL, value = "", 
                      width = "200px", placeholder = "e.g., Peter_PS_Sep24")
        ),
        
        actionButton(ns("apply_roi_boundaries"), "Apply ROI Boundaries", 
                     class = "btn btn-success", style = "width: 100%; margin-bottom: 15px;"),
        
        actionButton(ns("save_roi_config"), "Save Current Configuration", 
                     class = "btn btn-warning", style = "width: 100%; margin-bottom: 15px;"),
        
        actionButton(ns("start_over"), "Start Over", class = "btn-danger btn-block"),
        actionButton(ns("trim_sensor"), "Trim sensor start and end", class = "btn-warning btn-block"),
        
        br(),
        
        checkboxInput(ns("round_roi"), "Round ROI to nearest 0.1s", value = FALSE),
        textOutput(ns("round_status")),
        checkboxInput(ns("match_pre_post"), "Match pre- and post-nadir ROI", value = FALSE),
        textOutput(ns("match_status")),
        
        textOutput(ns("dynamic_instruction")),
        
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
    
    # ROI slider state
    roi_slider_values <- reactiveValues(
      updating_sliders = FALSE,  # Prevent recursive updates
      data_start = NULL,         # Sensor data start time
      data_end = NULL,           # Sensor data end time
      nadir_time = NULL,         # Current nadir time
      boundaries_set = FALSE     # Track if sliders have been populated
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
    
    # ============================= #
    # /// ROI Slider Helper Functions \\\ ####
    # ============================= #
    
    # Extract ROI boundaries from existing delineated data
    extract_roi_boundaries_from_data <- function(sensor_data) {
      if (is.null(sensor_data) || !"roi" %in% names(sensor_data)) {
        return(NULL)
      }
      
      # Find where each ROI starts and ends
      roi_levels <- c("roi1_sens_ingress", "roi2_inflow_passage", 
                      "roi3_prenadir", "roi4_nadir", "roi5_postnadir", 
                      "roi6_outflow_passage", "roi7_sens_outgress")
      
      boundaries <- list()
      
      # Data start and end
      boundaries$data_start <- min(sensor_data$time_s)
      boundaries$data_end <- max(sensor_data$time_s)
      
      # Find ROI boundaries
      for (i in seq_along(roi_levels)) {
        roi_data <- sensor_data[sensor_data$roi == roi_levels[i], ]
        if (nrow(roi_data) > 0) {
          if (i == 1) {
            boundaries$roi1_start <- min(roi_data$time_s)
          } else {
            boundaries[[paste0("roi", i, "_start")]] <- min(roi_data$time_s)
          }
          
          if (i == length(roi_levels)) {
            boundaries$roi7_end <- max(roi_data$time_s)
          }
        }
      }
      
      # Calculate ROI 4 duration
      if (!is.null(boundaries$roi4_start) && !is.null(boundaries$roi5_start)) {
        boundaries$roi4_duration <- boundaries$roi5_start - boundaries$roi4_start
      }
      
      return(boundaries)
    }
    
    # Calculate ROI boundaries from configuration
    calculate_roi_boundaries_from_config <- function(config, nadir_time, data_start, data_end) {
      if (is.null(config) || !nadir_time$available) {
        return(NULL)
      }
      
      nadir <- nadir_time$time
      
      # Calculate ROI 4 boundaries (nadir-centered)
      roi4_start <- nadir - (config$roi4_nadir / 2)
      roi4_end <- nadir + (config$roi4_nadir / 2)
      
      # Calculate other boundaries working backwards and forwards
      roi3_start <- roi4_start - config$roi3_prenadir
      roi2_start <- roi3_start - config$roi2_inflow_passage
      roi1_start <- roi2_start - config$roi1_sens_ingress
      
      roi5_start <- roi4_end
      roi6_start <- roi5_start + config$roi5_postnadir
      roi7_start <- roi6_start + config$roi6_outflow_passage
      roi7_end <- roi7_start + config$roi7_sens_outgress
      
      return(list(
        data_start = data_start,
        data_end = data_end,
        roi1_start = roi1_start,
        roi2_start = roi2_start,
        roi3_start = roi3_start,
        roi4_start = roi4_start,
        roi5_start = roi4_end,  # ROI 5 starts where ROI 4 ends
        roi6_start = roi6_start,
        roi7_start = roi7_start,
        roi7_end = roi7_end,
        roi4_duration = config$roi4_nadir
      ))
    }
    
    # Update slider constraints based on current values
    update_slider_constraints <- function(session, boundaries) {
      if (is.null(boundaries)) return()
      
      # Update slider ranges with proper constraints
      updateSliderInput(session, "roi1_start", 
                        min = boundaries$data_start, 
                        max = boundaries$roi2_start - 0.1,
                        value = boundaries$roi1_start)
      
      updateSliderInput(session, "roi2_start", 
                        min = boundaries$roi1_start + 0.1, 
                        max = boundaries$roi3_start - 0.1,
                        value = boundaries$roi2_start)
      
      updateSliderInput(session, "roi3_start", 
                        min = boundaries$roi2_start + 0.1, 
                        max = boundaries$roi4_start - 0.1,
                        value = boundaries$roi3_start)
      
      updateSliderInput(session, "roi4_start", 
                        min = boundaries$roi3_start + 0.1, 
                        max = boundaries$roi5_start - 0.1,
                        value = boundaries$roi4_start)
      
      updateSliderInput(session, "roi5_start", 
                        min = boundaries$roi4_start + boundaries$roi4_duration + 0.1, 
                        max = boundaries$roi6_start - 0.1,
                        value = boundaries$roi5_start)
      
      updateSliderInput(session, "roi6_start", 
                        min = boundaries$roi5_start + 0.1, 
                        max = boundaries$roi7_start - 0.1,
                        value = boundaries$roi6_start)
      
      updateSliderInput(session, "roi7_start", 
                        min = boundaries$roi6_start + 0.1, 
                        max = boundaries$roi7_end - 0.1,
                        value = boundaries$roi7_start)
      
      updateSliderInput(session, "roi7_end", 
                        min = boundaries$roi7_start + 0.1, 
                        max = boundaries$data_end,
                        value = boundaries$roi7_end)
      
      # Update nadir duration
      updateNumericInput(session, "roi4_nadir_duration", value = boundaries$roi4_duration)
    }
    
    # Get current slider values as boundaries
    get_current_slider_boundaries <- function(input) {
      list(
        roi1_start = input$roi1_start,
        roi2_start = input$roi2_start,
        roi3_start = input$roi3_start,
        roi4_start = input$roi4_start,
        roi5_start = input$roi5_start,
        roi6_start = input$roi6_start,
        roi7_start = input$roi7_start,
        roi7_end = input$roi7_end,
        roi4_duration = input$roi4_nadir_duration
      )
    }
    
    # ============================= #
    # /// Slider Population and Management \\\ ####
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
    
    # Populate sliders when sensor or configuration changes
    observe({
      req(sensor_selector$selected_sensor())
      
      if (roi_slider_values$updating_sliders) return()
      
      # Get sensor data to determine time range
      sensor_data <- selected_sensor_data()
      if (is.null(sensor_data)) return()
      
      roi_slider_values$data_start <- min(sensor_data$time_s)
      roi_slider_values$data_end <- max(sensor_data$time_s)
      
      # Check if sensor has existing delineated data
      status <- sensor_status()
      boundaries <- NULL
      
      if (status$delineated) {
        # Extract boundaries from existing delineated data
        boundaries <- extract_roi_boundaries_from_data(sensor_data)
        if (!is.null(boundaries)) {
          roi_slider_values$updating_sliders <- TRUE
          update_slider_constraints(session, boundaries)
          roi_slider_values$updating_sliders <- FALSE
          roi_slider_values$boundaries_set <- TRUE
          return()
        }
      }
      
      # If no delineated data, use configuration + nadir
      nadir <- nadir_info()
      config <- roi_values$current_config
      
      if (!is.null(config) && nadir$available) {
        boundaries <- calculate_roi_boundaries_from_config(
          config, nadir, roi_slider_values$data_start, roi_slider_values$data_end
        )
        
        if (!is.null(boundaries)) {
          roi_slider_values$updating_sliders <- TRUE
          update_slider_constraints(session, boundaries)
          roi_slider_values$updating_sliders <- FALSE
          roi_slider_values$boundaries_set <- TRUE
        }
      }
    })
    
    # Update ROI 5 start when ROI 4 start or duration changes
    observe({
      if (roi_slider_values$updating_sliders || !roi_slider_values$boundaries_set) return()
      
      req(input$roi4_start, input$roi4_nadir_duration)
      
      new_roi5_start <- input$roi4_start + input$roi4_nadir_duration
      
      roi_slider_values$updating_sliders <- TRUE
      updateSliderInput(session, "roi5_start", value = new_roi5_start)
      
      # Update constraints for downstream sliders
      if (!is.null(input$roi6_start)) {
        updateSliderInput(session, "roi5_start", max = input$roi6_start - 0.1)
      }
      roi_slider_values$updating_sliders <- FALSE
    })
    
    # Dynamic constraint updates when sliders change
    observe({
      if (roi_slider_values$updating_sliders || !roi_slider_values$boundaries_set) return()
      
      req(input$roi1_start, input$roi2_start)
      
      roi_slider_values$updating_sliders <- TRUE
      updateSliderInput(session, "roi1_start", max = input$roi2_start - 0.1)
      updateSliderInput(session, "roi2_start", min = input$roi1_start + 0.1)
      roi_slider_values$updating_sliders <- FALSE
    })
    
    # Add similar observers for other adjacent slider pairs
    observe({
      if (roi_slider_values$updating_sliders || !roi_slider_values$boundaries_set) return()
      
      req(input$roi2_start, input$roi3_start)
      
      roi_slider_values$updating_sliders <- TRUE
      updateSliderInput(session, "roi2_start", max = input$roi3_start - 0.1)
      updateSliderInput(session, "roi3_start", min = input$roi2_start + 0.1)
      roi_slider_values$updating_sliders <- FALSE
    })
    
    observe({
      if (roi_slider_values$updating_sliders || !roi_slider_values$boundaries_set) return()
      
      req(input$roi3_start, input$roi4_start)
      
      roi_slider_values$updating_sliders <- TRUE
      updateSliderInput(session, "roi3_start", max = input$roi4_start - 0.1)
      updateSliderInput(session, "roi4_start", min = input$roi3_start + 0.1)
      roi_slider_values$updating_sliders <- FALSE
    })
    
    observe({
      if (roi_slider_values$updating_sliders || !roi_slider_values$boundaries_set) return()
      
      req(input$roi5_start, input$roi6_start)
      
      roi_slider_values$updating_sliders <- TRUE
      updateSliderInput(session, "roi5_start", max = input$roi6_start - 0.1)
      updateSliderInput(session, "roi6_start", min = input$roi5_start + 0.1)
      roi_slider_values$updating_sliders <- FALSE
    })
    
    observe({
      if (roi_slider_values$updating_sliders || !roi_slider_values$boundaries_set) return()
      
      req(input$roi6_start, input$roi7_start)
      
      roi_slider_values$updating_sliders <- TRUE
      updateSliderInput(session, "roi6_start", max = input$roi7_start - 0.1)
      updateSliderInput(session, "roi7_start", min = input$roi6_start + 0.1)
      roi_slider_values$updating_sliders <- FALSE
    })
    
    observe({
      if (roi_slider_values$updating_sliders || !roi_slider_values$boundaries_set) return()
      
      req(input$roi7_start, input$roi7_end)
      
      roi_slider_values$updating_sliders <- TRUE
      updateSliderInput(session, "roi7_start", max = input$roi7_end - 0.1)
      updateSliderInput(session, "roi7_end", min = input$roi7_start + 0.1)
      roi_slider_values$updating_sliders <- FALSE
    })
    
    # Calculate ROI boundaries from current slider values for plotting
    roi_boundaries_from_sliders <- reactive({
      if (!roi_slider_values$boundaries_set) return(NULL)
      
      req(input$roi1_start, input$roi2_start, input$roi3_start, input$roi4_start,
          input$roi5_start, input$roi6_start, input$roi7_start, input$roi7_end)
      
      # Return boundaries in the format expected by plotting
      c(roi_slider_values$data_start,
        input$roi1_start, input$roi2_start, input$roi3_start, 
        input$roi4_start, input$roi5_start, input$roi6_start, 
        input$roi7_start, input$roi7_end,
        roi_slider_values$data_end)
    })
    
    # Update roi_times reactive to use slider values
    roi_times <- reactive({
      boundaries <- roi_boundaries_from_sliders()
      if (is.null(boundaries)) return(NULL)
      
      # Create the table as before, but using slider values
      roi_times_df <- data.frame(
        ROI = c("Sensor start trim", "ROI 1: Sensor ingress", "ROI 2: Inflow passage", 
                "ROI 3: Pre-nadir", "ROI 4: Nadir", "ROI 5: Post-nadir", 
                "ROI 6: Outflow passage", "ROI 7: Sensor outgress", "Sensor end trim"),
        `Start time` = c(paste(round(boundaries[1], 3), "s"),
                         paste(round(boundaries[2], 3), "s"),
                         paste(round(boundaries[3], 3), "s"),
                         paste(round(boundaries[4], 3), "s"),
                         paste(round(boundaries[5], 3), "s"),
                         paste(round(boundaries[6], 3), "s"),
                         paste(round(boundaries[7], 3), "s"),
                         paste(round(boundaries[8], 3), "s"),
                         paste(round(boundaries[9], 3), "s")),
        `End Time` = c(paste(round(boundaries[2], 3), "s"),
                       paste(round(boundaries[3], 3), "s"),
                       paste(round(boundaries[4], 3), "s"),
                       paste(round(boundaries[5], 3), "s"),
                       paste(round(boundaries[6], 3), "s"),
                       paste(round(boundaries[7], 3), "s"),
                       paste(round(boundaries[8], 3), "s"),
                       paste(round(boundaries[9], 3), "s"),
                       paste(round(boundaries[10], 3), "s")),
        Duration = c(paste(round(boundaries[2] - boundaries[1], 3), "s"),
                     paste(round(boundaries[3] - boundaries[2], 3), "s"),
                     paste(round(boundaries[4] - boundaries[3], 3), "s"),
                     paste(round(boundaries[5] - boundaries[4], 3), "s"),
                     paste(round(boundaries[6] - boundaries[5], 3), "s"),
                     paste(round(boundaries[7] - boundaries[6], 3), "s"),
                     paste(round(boundaries[8] - boundaries[7], 3), "s"),
                     paste(round(boundaries[9] - boundaries[8], 3), "s"),
                     paste(round(boundaries[10] - boundaries[9], 3), "s")),
        check.names = FALSE
      )
      
      return(list(
        table = roi_times_df,
        boundaries = boundaries
      ))
    })
    
    # ============================= #
    # /// UI State management \\\ ####  
    # ============================= # 
    
    # Reset checkboxes when sensor changes
    observeEvent(sensor_selector$selected_sensor(), {
      # Reset standardization checkboxes when switching sensors
      updateCheckboxInput(session, "round_roi", value = FALSE)
      updateCheckboxInput(session, "match_pre_post", value = FALSE)
    })
    
    ## Normalized checkbox ####
    
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
    
    ## Button state management #####
    
    observe({
      req(sensor_selector$selected_sensor())
      
      nadir <- nadir_info()
      status <- sensor_status()
      
      button_states <- list(
        "apply_roi_boundaries" = roi_slider_values$boundaries_set,
        "save_roi_config" = roi_slider_values$boundaries_set && 
          !is.null(input$roi_config_label) && 
          nchar(trimws(input$roi_config_label)) > 0,
        "start_over" = status$delineated,
        "trim_sensor" = status$delineated && !status$trimmed,
        "normalize_time" = status$delineated && status$trimmed && !status$normalized,
        "passage_time" = status$delineated && status$trimmed && !status$passage_times,
        "nadir_btn" = !nadir_values$edit_mode || !is.null(nadir_values$selected_point),
        "cancel_nadir_btn" = nadir_values$edit_mode,
        "round_roi" = roi_slider_values$boundaries_set,
        "match_pre_post" = roi_slider_values$boundaries_set && isTRUE(input$round_roi)
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
    
    ## Apply ROI boundaries ####
    observeEvent(input$apply_roi_boundaries, {
      req(sensor_selector$selected_sensor())
      
      # Check if ROI boundaries exist
      if (!roi_slider_values$boundaries_set) {
        showNotification("ROI boundaries not set. Please load a configuration first.", type = "warning")
        return()
      }
      
      # Check if delineated file already exists
      status <- sensor_status()
      if (status$delineated) {
        showModal(modalDialog(
          title = "Delineated Data Exists",
          "Delineated data already exists. Replace existing delineation?",
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_replace_delineation"), "Replace", class = "btn-warning")
          )
        ))
      } else {
        apply_slider_boundaries()
      }
    })
    
    # Confirm replace delineation
    observeEvent(input$confirm_replace_delineation, {
      removeModal()
      apply_slider_boundaries()
    })
    
    ## Save ROI configuration ####
    observeEvent(input$save_roi_config, {
      config_name <- trimws(input$roi_config_label)
      
      if (nchar(config_name) == 0) {
        showNotification("Please enter a configuration label", type = "error")
        return()
      }
      
      if (!roi_slider_values$boundaries_set) {
        showNotification("No ROI boundaries set to save", type = "warning")
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
            actionButton(ns("confirm_save_roi_config"), "Replace", class = "btn-warning")
          ),
          size = "m"
        ))
      } else {
        save_slider_configuration()
      }
    })
    
    # Confirm save configuration
    observeEvent(input$confirm_save_roi_config, {
      removeModal()
      save_slider_configuration()
    })
    
    ## Standardization options ####
    observeEvent(input$round_roi, {
      if (input$round_roi && roi_slider_values$boundaries_set) {
        # Round all slider values to nearest 0.1s
        roi_slider_values$updating_sliders <- TRUE
        
        updateSliderInput(session, "roi1_start", value = round(input$roi1_start * 10) / 10)
        updateSliderInput(session, "roi2_start", value = round(input$roi2_start * 10) / 10)
        updateSliderInput(session, "roi3_start", value = round(input$roi3_start * 10) / 10)
        updateSliderInput(session, "roi4_start", value = round(input$roi4_start * 10) / 10)
        updateSliderInput(session, "roi5_start", value = round(input$roi5_start * 10) / 10)
        updateSliderInput(session, "roi6_start", value = round(input$roi6_start * 10) / 10)
        updateSliderInput(session, "roi7_start", value = round(input$roi7_start * 10) / 10)
        updateSliderInput(session, "roi7_end", value = round(input$roi7_end * 10) / 10)
        
        roi_slider_values$updating_sliders <- FALSE
        showNotification("ROI times rounded to nearest 0.1s", type = "message", duration = 3)
      }
    })
    
    observeEvent(input$match_pre_post, {
      if (input$match_pre_post && roi_slider_values$boundaries_set) {
        # Check if rounding was done first
        if (!isTRUE(input$round_roi)) {
          updateCheckboxInput(session, "match_pre_post", value = FALSE)
          showNotification("Please round ROI times first before matching durations", type = "warning", duration = 4)
          return()
        }
        
        req(input$roi3_start, input$roi4_start, input$roi5_start, input$roi6_start, input$roi4_nadir_duration)
        
        # Calculate current ROI 3 and 5 durations
        roi4_start_time <- input$roi4_start
        roi4_end_time <- input$roi4_start + input$roi4_nadir_duration
        
        roi3_duration <- roi4_start_time - input$roi3_start
        roi5_duration <- input$roi6_start - roi4_end_time
        
        # Use the average duration for both
        avg_duration <- (roi3_duration + roi5_duration) / 2
        
        roi_slider_values$updating_sliders <- TRUE
        
        # Recalculate ROI 3 start and ROI 6 start
        updateSliderInput(session, "roi3_start", value = roi4_start_time - avg_duration)
        updateSliderInput(session, "roi6_start", value = roi4_end_time + avg_duration)
        
        roi_slider_values$updating_sliders <- FALSE
        
        showNotification(paste0("Pre/post-nadir ROI matched to ", round(avg_duration, 3), "s duration"), 
                         type = "message", duration = 3)
      }
    })
    
    # Reset match checkbox if round is unchecked
    observeEvent(input$round_roi, {
      if (!isTRUE(input$round_roi)) {
        updateCheckboxInput(session, "match_pre_post", value = FALSE)
      }
    })
    
    ## Trim and other existing functionality ####
    
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
        trigger_data_update()     
        trigger_summary_update()  
        
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
        trigger_data_update()     
        trigger_summary_update()  
        
        updateCheckboxInput(session, "round_roi", value = FALSE)
        updateCheckboxInput(session, "match_pre_post", value = FALSE)
        
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
    # /// Helper functions \\\ ####  
    # ============================= # 
    
    ## Apply slider boundaries to create delineated dataset ####
    apply_slider_boundaries <- function() {
      tryCatch({
        # Read original data using shared function
        sensor_data <- read_sensor_data(output_dir(), sensor_selector$selected_sensor(), "min")
        
        # Create delineated folder
        delineated_dir <- file.path(output_dir(), "csv", "delineated")
        if (!dir.exists(delineated_dir)) {
          dir.create(delineated_dir, showWarnings = FALSE, recursive = TRUE)
        }
        
        if (!dir.exists(delineated_dir)) {
          showNotification("Failed to create delineated directory", type = "error")
          return()
        }
        
        # Get boundaries from sliders
        boundaries <- c(roi_slider_values$data_start,
                        input$roi1_start, input$roi2_start, input$roi3_start, 
                        input$roi4_start, input$roi5_start, input$roi6_start, 
                        input$roi7_start, input$roi7_end,
                        roi_slider_values$data_end)
        
        # Add ROI column using boundaries
        sensor_data$roi <- cut(sensor_data$time_s, 
                               breaks = boundaries,
                               labels = c("trim_start", "roi1_sens_ingress", "roi2_inflow_passage", 
                                          "roi3_prenadir", "roi4_nadir", "roi5_postnadir", 
                                          "roi6_outflow_passage", "roi7_sens_outgress", "trim_end"),
                               include.lowest = TRUE, right = FALSE)
        
        # Save delineated file
        output_file <- file.path(delineated_dir, paste0(sensor_selector$selected_sensor(), "_delineated.csv"))
        write.csv(sensor_data, output_file, row.names = FALSE)
        
        if (!file.exists(output_file)) {
          showNotification("Failed to create delineated file", type = "error")
          return()
        }
        
        # Determine config label to save
        config_label <- if (!is.null(input$roi_config_label) && nchar(trimws(input$roi_config_label)) > 0) {
          trimws(input$roi_config_label)
        } else {
          "custom"
        }
        
        # Update sensor index using shared function
        success <- safe_update_sensor_index(
          output_dir(),
          sensor_selector$selected_sensor(),
          list(
            delineated = "Y",
            roi_config = config_label,
            trimmed = "N",
            normalized = "N",
            passage_times = "N",
            passage_duration.mm.ss. = "NA",
            ingress_nadir_duration.mm.ss. = "NA",
            nadir_outgress_duration.mm.ss. = "NA"
          )
        )
        
        if (success) {
          trigger_data_update()     
          trigger_summary_update()  
          showNotification("Delineated dataset created successfully!", type = "message")
        } else {
          showNotification("Warning: Dataset created but failed to update index", type = "warning")
        }
        
      }, error = function(e) {
        showNotification(paste("Error creating delineated dataset:", e$message), 
                         type = "error")
      })
    }
    
    ## Save slider configuration ####
    save_slider_configuration <- function() {
      tryCatch({
        config_name <- trimws(input$roi_config_label)
        
        # Convert slider positions back to durations
        roi1_duration <- input$roi2_start - input$roi1_start
        roi2_duration <- input$roi3_start - input$roi2_start
        roi3_duration <- input$roi4_start - input$roi3_start
        roi4_duration <- input$roi4_nadir_duration
        roi5_duration <- input$roi6_start - input$roi5_start
        roi6_duration <- input$roi7_start - input$roi6_start
        roi7_duration <- input$roi7_end - input$roi7_start
        
        # Save configuration using shared function
        success <- save_config_value(
          output_dir = output_dir(),
          config_type = "roi",
          key = config_name,
          value = c(roi1_duration, roi2_duration, roi3_duration, roi4_duration, 
                    roi5_duration, roi6_duration, roi7_duration)
        )
        
        if (success) {
          # Reload configurations
          roi_config$reload_configs()
          trigger_summary_update()
          
          showNotification(paste("ROI configuration '", config_name, "' saved successfully!"), type = "message")
        } else {
          showNotification("Failed to save ROI configuration", type = "error")
        }
        
      }, error = function(e) {
        showNotification(paste("Error saving configuration:", e$message), type = "error")
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
    
    # Dynamic ROI instruction ####
    output$dynamic_instruction <- renderText({
      if (!roi_slider_values$boundaries_set) {
        "Load a configuration or select a delineated sensor to adjust ROI boundaries"
      } else {
        "Use sliders above to adjust ROI boundaries, then apply delineation"
      }
    })
    
    ##Round status ####
    output$round_status <- renderText({
      if (input$round_roi && roi_slider_values$boundaries_set) {
        "ROI times rounded to 0.1s precision"
      } else {
        ""
      }
    })
    ##Match pre/post ####
    output$match_status <- renderText({
      if (input$match_pre_post && roi_slider_values$boundaries_set) {
        "Pre/post-nadir ROI durations matched"  
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
                                      roi_boundaries = roi_boundaries_from_sliders,  # Use new reactive
                                      show_roi_markers = reactive({
                                        # Show ROI markers when boundaries are set and sensor is delineated+trimmed
                                        status <- sensor_status()
                                        roi_slider_values$boundaries_set && status$delineated && status$trimmed
                                      }),
                                      title_prefix = "ROI Delineation",
                                      plot_source = "roi_nadir_plot"
    )
    
  })  # End of moduleServer
}     # End of roiServer