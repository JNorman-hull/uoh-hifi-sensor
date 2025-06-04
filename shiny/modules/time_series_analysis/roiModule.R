# ROI Delineation Module

roiUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    plotModuleUI(ns("roi_plot"), height = "600px"),
    br(),
    
    fluidRow(
      column(
        width = 4,
        
        div(
          style = "background-color: #f8f9fa; border: 1px solid #ccc; padding: 15px; 
                 border-radius: 5px; margin-bottom: 20px;",
          
          tags$h4("ROI Controls", style = "margin-top: 0; text-align: center;"),
          
          selectInput(ns("config_choice"), "Configuration:", choices = NULL, width = "100%"),
          
          div(style = "display: flex; align-items: center; justify-content: start; margin-bottom: 15px;",
              tags$label("ROI 4 Nadir Duration (s):", `for` = ns("roi4_nadir_duration"), 
                         style = "margin-right: 8px;"),
              numericInput(ns("roi4_nadir_duration"), NULL, value = 0.2, min = 0.1, max = 2.0, step = 0.1,
                           width = "80px")
          ),
          
          div(style = "display: flex; align-items: center; justify-content: start; margin-bottom: 15px;",
                tags$label("Configuration Label:", `for` = ns("roi_config_label"), 
                           style = "margin-right: 8px;"),
                textInput(ns("roi_config_label"), NULL, value = "", 
                          width = "200px", placeholder = "e.g., Peter_PS_Sep24")
          ),
          
          actionButton(ns("create_custom_roi"), "Create Custom Delineation", 
                       class = "btn btn-sm btn-warning", style = "width: 100%; margin-bottom: 15px;"),
          
          actionButton(ns("mark_roi_dynamic"), "Mark ROI 1 Start", 
                       class = "btn btn-sm btn-primary", style = "width: 100%; margin-bottom: 10px;"),
          
          actionButton(ns("cancel_custom_roi"), "Cancel Custom", 
                       class = "btn btn-sm btn-danger", style = "width: 100%; margin-bottom: 10px;"),
          
          textOutput(ns("dynamic_instruction")),
          
          hr(),
          
          tags$h4("Standardize ROI", style = "margin-top: 0; text-align: center;"),
          
          checkboxInput(ns("round_roi"), "Round ROI to nearest 0.1s", value = FALSE),
          textOutput(ns("round_status")),
          checkboxInput(ns("match_pre_post"), "Match pre- and post-nadir ROI", value = FALSE),
          textOutput(ns("match_status"))
        )
      ),
      
      column(
        width = 8,
        tags$h4("Passage Information"),
        DT::dataTableOutput(ns("roi_table")),
        
        div(
          style = "background-color: #f8f9fa; border: 1px solid #ccc; padding: 15px; 
                 border-radius: 5px; margin-top: 20px;",
          
          fluidRow(
            column(
              width = 4,
              tags$h4("Passage Times", style = "margin-top: 0; text-align: center;"),
              actionButton(ns("passage_time"), "Calculate passage times", 
                           class = "btn-primary", style = "width: 100%;"),
              textOutput(ns("passage_status"))
            ),
            column(
              width = 8,
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
          tags$li("ROI 1 Sensor ingress: Mark region sesnsor enters system from atmospheric pressure or other landmark feature (e.g., injection pipe)"),
          tags$li("ROI 2 Intake passage: Mark region sesnsor moves through intake structures and pipework leading towards the impeller."),
          tags$li("ROI 3 Pre-nadir: Mark region just before the impeller, highest risk of encoruntering pressure differentials and swirl flows."),
          tags$li("ROI 4 Nadir: Critical passage analysis zone with direct passage through the impeller. Hydraulic pinch point where maximum acceleration, rotation and minimum pressure likley to occur.
                  Region calculated using input time box, which is centered on the nadir point. Ensure nadir is correct first."),
          tags$li("ROI 5 Post-nadir: Mark region just after the impeller, highest risk of encoruntering guide vane or other forms of collision, residual turbulences and pressure recovery."),
          tags$li("ROI 6 Outflow passage: Mark region sensor moves through outflow pipework and structures leading towards sensor outgress. Velcoity expected to decrease and pressure return to atmopsheric pressure."),
          tags$li("ROI 7 Sensor outgress: Mark region sensor exists system from atmospheric pressure or other landmark features (e.g., stable flow indictiave of tailwater)."),
          tags$li("Sensor start and end trim: Automatically calculated from start and end of data and start and end of ROI 1 and 7. Use trim tool to remove after delineatition.")
        )
      )
      
    )
  )
}


roiSidebarUI <- function(id) {
  ns <- NS(id)
  
  sensor_vars <- get_sensor_variables()
  var_choices <- setNames(sensor_vars$names, sensor_vars$labels)
  
  tagList(
    h4("Time series controls"),
    
    div(style = "color: #666; font-style: italic; margin-bottom: 15px;",
        "Select a sensor to begin time series analysis."),
    
    div(style = "margin-bottom: 15px;", 
        textOutput(ns("delineation_status")),
        textOutput(ns("normalization_status")), 
        textOutput(ns("passage_times_status"))
    ),
    
    enhancedSensorSelectionUI(ns("sensor_selector"), status_filter_type = "delineation"),
    
    hr(), h4("Plot controls"),
    plotSidebarUI(ns("pressure_plot"), 
                  show_left_var = TRUE,   
                  show_right_var = TRUE,    
                  show_normalized = TRUE,   
                  show_nadir = TRUE,      
                  show_roi_markers = FALSE,   
                  show_legend = TRUE,
                  default_show_normalized = FALSE,
                  default_show_nadir = TRUE,
                  default_show_roi_markers = FALSE,
                  default_show_legend = FALSE,
                  default_left_var = "pressure_kpa",
                  default_right_var = "higacc_mag_g"),     
    
    hr(), h4("Delineate data"),
    actionButton(ns("create_delineated"), "Create delineated dataset", class = "btn-primary btn-block"),
    actionButton(ns("start_over"), "Start Over", class = "btn-warning btn-block"),
    actionButton(ns("trim_sensor"), "Trim sensor start and end", class = "btn-danger btn-block"),
    
    hr(), h4("Pressure Nadir Options"),
    verbatimTextOutput(ns("current_nadir_display")),
    actionButton(ns("nadir_btn"), "Modify Pressure Nadir", class = "btn-warning btn-block"),
    actionButton(ns("cancel_nadir_btn"), "Cancel", class = "btn-danger btn-block"),
    textOutput(ns("nadir_status")),
    
    hr(), h4("Time normalization"),
    actionButton(ns("normalize_time"), "Normalize time series", class = "btn-primary btn-block"),
    textOutput(ns("normalize_status"))
  )
}

roiServer <- function(id, output_dir, summary_data, processing_complete = reactive(FALSE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
# ============================= #
# /// Reactive values \\\ ####  
# ============================= #   
    

    # ROI configuration state
    roi_values <- reactiveValues(
      roi_configs = NULL,          # All available ROI configurations
      current_config = NULL,       # Currently selected configuration
      summary_updated = 0,         # Counter to trigger summary data refresh
      data_updated = 0            # Counter to trigger data refresh
    )
    
    # Nadir editing state
    nadir_values <- reactiveValues(
      edit_mode = FALSE,           # Whether nadir editing is active
      selected_point = NULL,       # Currently selected nadir point
      nadir_updated = 0,          # Counter to trigger nadir refresh
      baseline_click = NULL        # Baseline click for detecting new clicks
    )
    
    # Custom ROI creation state
    custom_roi_values <- reactiveValues(
      custom_edit_mode = FALSE,    # Whether custom ROI mode is active
      current_roi_step = 0,        # Current step in ROI marking process (0-6)
      selected_boundaries = list(), # Selected ROI boundaries
      baseline_click = NULL,       # Baseline click for detecting new clicks
      pending_point = NULL,         # Point marked but not yet confirmed
      standardization_applied = FALSE,  # Track if any standardization was used
      baseline_boundaries = list()      # Store original boundaries for reset
    )

 
# ============================= #
# /// Data loading & processing  \\\ ####  
# ============================= # 

#Sensor dropdown ####   
    sensor_selector <- enhancedSensorSelectionServer("sensor_selector", output_dir, processing_complete, status_filter_type = "delineation")
    
# Get nadir info using shared function
    nadir_info <- reactive({
      req(sensor_selector$selected_sensor())
      roi_values$summary_updated
      nadir_values$nadir_updated
      get_nadir_info(sensor_selector$selected_sensor(), output_dir())
    })
    
# Get sensor status using shared function
    sensor_status <- reactive({
      req(sensor_selector$selected_sensor())
      roi_values$summary_updated
      roi_values$data_updated
      
      get_sensor_status(sensor_selector$selected_sensor(), output_dir())
    })
    
# Read selected sensor data (with preference for delineated data)
    selected_sensor_data <- reactive({
      req(sensor_selector$selected_sensor())
      roi_values$data_updated  # Invalidate when data changes
      
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
      input$config_choice
      
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
    observe({
      roi_values$roi_configs <- load_config_file(output_dir(), "roi")
      
      if (length(roi_values$roi_configs) > 0) {
        config_names <- names(roi_values$roi_configs)
        choices <- setNames(config_names, gsub("_", " ", config_names))
        selected_value <- config_names[1]
        
        # Check if sensor has a saved configuration
        if (!is.null(sensor_selector$selected_sensor()) && sensor_selector$selected_sensor() != "") {
          index_df <- get_sensor_index_file(output_dir(), read_data = TRUE)
          if (!is.null(index_df)) {
            tryCatch({
              sensor_row <- index_df[index_df$file == sensor_selector$selected_sensor(), ]
              if (nrow(sensor_row) > 0 && !is.na(sensor_row$roi_config) && sensor_row$roi_config != "NA") {
                if (sensor_row$roi_config %in% config_names) {
                  selected_value <- sensor_row$roi_config
                }
              }
            },
            error = function(e) {
            })
          }
        }
        updateSelectInput(session, "config_choice", 
                          choices = choices, 
                          selected = selected_value)
      }
    })
    
# Update current config when dropdown selection changes
    observe({
      req(input$config_choice, roi_values$roi_configs)
      roi_values$current_config <- roi_values$roi_configs[[input$config_choice]]
    })
    
# Reset checkboxes when sensor changes
    observeEvent(sensor_selector$selected_sensor(), {
      # Reset standardization checkboxes when switching sensors
      updateCheckboxInput(session, "round_roi", value = FALSE)
      updateCheckboxInput(session, "match_pre_post", value = FALSE)
      
      # Reset custom ROI state when switching sensors
      custom_roi_values$custom_edit_mode <- FALSE
      custom_roi_values$current_roi_step <- 0
      custom_roi_values$selected_boundaries <- list()
      custom_roi_values$standardization_applied <- FALSE
      custom_roi_values$baseline_boundaries <- list()
    })
 
## Normalized checkbox ####
    
## Enable/disable normalized view checkbox
    observe({
      req(sensor_selector$selected_sensor())
      status <- sensor_status()
      
      if (status$normalized) {
        shinyjs::enable("show_normalized")
      } else {
        shinyjs::disable("show_normalized")
        updateCheckboxInput(session, "show_normalized", value = FALSE)
      }
    })
    
## Button state management #####
    
    observe({
      req(sensor_selector$selected_sensor())
      
      nadir <- nadir_info()
      status <- sensor_status()
      
      button_states <- list(
        "create_delineated" = nadir$available && !status$delineated && !custom_roi_values$custom_edit_mode,
        "start_over" = status$delineated && !custom_roi_values$custom_edit_mode,
        "trim_sensor" = status$delineated && !status$trimmed && !custom_roi_values$custom_edit_mode,
        "normalize_time" = status$delineated && status$trimmed && !status$normalized && !custom_roi_values$custom_edit_mode,
        "passage_time" = status$delineated && status$trimmed && !status$passage_times && !custom_roi_values$custom_edit_mode,
        "nadir_btn" = (!custom_roi_values$custom_edit_mode) && (!nadir_values$edit_mode || !is.null(nadir_values$selected_point)),
        "cancel_nadir_btn" = nadir_values$edit_mode,
        "create_custom_roi" = !custom_roi_values$custom_edit_mode || custom_roi_values$current_roi_step == 6,
        "cancel_custom_roi" = custom_roi_values$custom_edit_mode,
        "roi_config_label" = custom_roi_values$custom_edit_mode,
        "mark_roi_dynamic" = custom_roi_values$custom_edit_mode,
        "round_roi" = custom_roi_values$custom_edit_mode && custom_roi_values$current_roi_step == 6,
        "match_pre_post" = custom_roi_values$custom_edit_mode && custom_roi_values$current_roi_step == 6 && isTRUE(input$round_roi)
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
    
## Update custom ROI button ####    
# Update custom ROI buttons based on edit mode
    observe({
      if (custom_roi_values$custom_edit_mode) {
        step_names <- c("Mark ROI 1 Start", "Mark ROI 2 Start", "Mark ROI 3 Start", 
                        "Mark ROI 5 End", "Mark ROI 6 End", "Mark ROI 7 End")
        
        if (custom_roi_values$current_roi_step < 6) {
          button_text <- step_names[custom_roi_values$current_roi_step + 1]
          updateActionButton(session, "mark_roi_dynamic", label = button_text)
        }
        
        # Update main button appearance when all boundaries marked
        if (custom_roi_values$current_roi_step == 6) {
          updateActionButton(session, "create_custom_roi", label = "Save Custom Delineation")
          shinyjs::removeClass("create_custom_roi", "btn-info")
          shinyjs::addClass("create_custom_roi", "btn-success")
        }
      } else {
        # Reset to initial state when not in edit mode
        updateActionButton(session, "mark_roi_dynamic", label = "Mark ROI 1 Start")
        updateActionButton(session, "create_custom_roi", label = "Create Custom Delineation")
        shinyjs::removeClass("create_custom_roi", "btn-success")
        shinyjs::addClass("create_custom_roi", "btn-info")
      }
    })
    
    # Export custom edit mode for conditional UI
    output$custom_edit_mode <- reactive({
      custom_roi_values$custom_edit_mode
    })
    outputOptions(output, "custom_edit_mode", suspendWhenHidden = FALSE)

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
          nadir_values$nadir_updated <- nadir_values$nadir_updated + 1
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
    
    # Confirm replacement
    observeEvent(input$confirm_replace, {
      req(roi_values$current_config)
      removeModal()
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
        # Trigger data refresh
        roi_values$data_updated <- roi_values$data_updated + 1
        roi_values$summary_updated <- roi_values$summary_updated + 1
        
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
        # Trigger updates
        roi_values$summary_updated <- roi_values$summary_updated + 1
        roi_values$data_updated <- roi_values$data_updated + 1
        
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
          # Trigger data refresh
          roi_values$data_updated <- roi_values$data_updated + 1
          roi_values$summary_updated <- roi_values$summary_updated + 1
          
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
          # Trigger data refresh
          roi_values$summary_updated <- roi_values$summary_updated + 1
          
          showNotification("Passage times calculated successfully!", type = "message")
        } else {
          showNotification("Failed to update sensor index", type = "error")
        }
        
      }, error = function(e) {
        showNotification(paste("Error calculating passage times:", e$message), type = "error")
      })
    })
    
## Create custom ROI incl. modal####
    
    # Create custom ROI button
    observeEvent(input$create_custom_roi, {
      if (!custom_roi_values$custom_edit_mode) {
        # Start custom mode
        custom_roi_values$custom_edit_mode <- TRUE
        custom_roi_values$current_roi_step <- 0
        custom_roi_values$selected_boundaries <- list()
        custom_roi_values$standardization_applied <- FALSE
        custom_roi_values$baseline_boundaries <- list()
        custom_roi_values$custom_nadir_duration <- input$roi4_nadir_duration
        custom_roi_values$baseline_click <- event_data("plotly_click", source = "roi_nadir_plot")
        updateActionButton(session, "create_custom_roi", label = "Save Custom Delineation")
        updateCheckboxInput(session, "round_roi", value = FALSE)
        updateCheckboxInput(session, "match_pre_post", value = FALSE)
      } else if (custom_roi_values$current_roi_step == 6) {
        # Check if label is provided
        if (is.null(input$roi_config_label) || nchar(trimws(input$roi_config_label)) == 0) {
          showNotification("Please enter a configuration label before saving", type = "error", duration = 4)
          return()
        }
        
        # Check standardization
        if (!custom_roi_values$standardization_applied) {
          showModal(modalDialog(
            title = "ROI Not Standardized",
            "ROI boundaries have not been standardized. Continue without standardization?",
            footer = tagList(
              modalButton("No, go back"),
              actionButton(ns("confirm_save_without_standardization"), "Yes, continue", class = "btn-warning")
            ),
            size = "m"
          ))
        } else {
          save_custom_configuration()
        }
      }
    })
    
    # If user clicks continue, then remove the message and continue to save
    
    observeEvent(input$confirm_save_without_standardization, {
      removeModal()
      save_custom_configuration() 
    })
    
### Round ROI 0.1s ####
    observeEvent(input$round_roi, {
      if (input$round_roi && length(custom_roi_values$selected_boundaries) == 6) {
        # Store original boundaries if first standardization
        if (!custom_roi_values$standardization_applied) {
          custom_roi_values$baseline_boundaries <- custom_roi_values$selected_boundaries
        }
        
        # Round all boundaries to nearest 0.1s
        for (boundary_name in names(custom_roi_values$selected_boundaries)) {
          rounded_time <- round(custom_roi_values$selected_boundaries[[boundary_name]] * 10) / 10
          custom_roi_values$selected_boundaries[[boundary_name]] <- rounded_time
        }
        
        custom_roi_values$standardization_applied <- TRUE
        showNotification("ROI times rounded to nearest 0.1s", type = "message", duration = 3)
      }
    })
    
### Match pre/post nadir ROI ####  
    observeEvent(input$match_pre_post, {
      if (input$match_pre_post && length(custom_roi_values$selected_boundaries) == 6) {
        # Check if rounding was done first
        if (!isTRUE(input$round_roi)) {
          updateCheckboxInput(session, "match_pre_post", value = FALSE)
          showNotification("Please round ROI times first before matching durations", type = "warning", duration = 4)
          return()
        }
        
        nadir <- nadir_info()
        nadir_duration <- input$roi4_nadir_duration
        
        if (!custom_roi_values$standardization_applied) {
          custom_roi_values$baseline_boundaries <- custom_roi_values$selected_boundaries
        }
        
        # Calculate current ROI 3 and 5 durations
        roi4_start <- nadir$time - (nadir_duration / 2)
        roi4_end <- nadir$time + (nadir_duration / 2)
        
        roi3_duration <- roi4_start - custom_roi_values$selected_boundaries$roi3_start
        roi5_duration <- custom_roi_values$selected_boundaries$roi5_end - roi4_end
        
        # Use the average duration for both
        avg_duration <- (roi3_duration + roi5_duration) / 2
        
        # Recalculate ROI 3 start and ROI 5 end
        custom_roi_values$selected_boundaries$roi3_start <- roi4_start - avg_duration
        custom_roi_values$selected_boundaries$roi5_end <- roi4_end + avg_duration
        
        custom_roi_values$standardization_applied <- TRUE
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
# Reset roi values if cancel #
    observeEvent(input$cancel_custom_roi, {
      custom_roi_values$custom_edit_mode <- FALSE
      custom_roi_values$current_roi_step <- 0
      updateTextInput(session, "roi_config_label", value = "")
      custom_roi_values$selected_boundaries <- list()
      custom_roi_values$pending_point <- NULL
      custom_roi_values$standardization_applied <- FALSE
      custom_roi_values$baseline_boundaries <- list()
      updateTextInput(session, "roi_config_label", value = "")
      # Reset checkboxes
      updateCheckboxInput(session, "round_roi", value = FALSE)
      updateCheckboxInput(session, "match_pre_post", value = FALSE)
      
      updateActionButton(session, "create_custom_roi", label = "Create Custom Delineation")
    })
    
    # Dynamic ROI marking button
    observeEvent(input$mark_roi_dynamic, {
      if (is.null(custom_roi_values$pending_point)) {
        showNotification("Mark ROI on plot first", type = "warning", duration = 3)
        return()
      }
      
      step_names <- c("roi1_start", "roi2_start", "roi3_start", "roi5_end", "roi6_end", "roi7_end")
      boundary_name <- step_names[custom_roi_values$current_roi_step + 1]
      
      custom_roi_values$selected_boundaries[[boundary_name]] <- custom_roi_values$pending_point$x
      custom_roi_values$current_roi_step <- custom_roi_values$current_roi_step + 1
      custom_roi_values$pending_point <- NULL
      
      # Reset baseline click for next selection
      custom_roi_values$baseline_click <- event_data("plotly_click", source = "roi_nadir_plot")
    })
    
    # Handle click events for custom ROI selection
    observe({
      if (custom_roi_values$custom_edit_mode && custom_roi_values$current_roi_step < 6) {
        click_data <- event_data("plotly_click", source = "roi_nadir_plot")
        if (!is.null(click_data)) {
          # Only respond if this click is different from baseline
          if (is.null(custom_roi_values$baseline_click) ||
              click_data$x != custom_roi_values$baseline_click$x) {
            custom_roi_values$pending_point <- list(x = click_data$x)
          }
        }
      }
    })
    

# ============================= #
# /// Helper functions \\\ ####  
# ============================= # 

## Save custom ROI ####
    
# Save custom ROI configuration function
    save_custom_configuration <- function() {
      req(custom_roi_values$current_roi_step == 6)
      req(length(custom_roi_values$selected_boundaries) == 6)
      
      nadir <- nadir_info()
      req(nadir$available)
      
      # Get nadir duration from input
      nadir_duration <- input$roi4_nadir_duration
      
      # Calculate ROI 4 boundaries
      roi4_start <- nadir$time - (nadir_duration / 2)
      roi4_end <- nadir$time + (nadir_duration / 2)
      
      # Extract selected boundaries
      roi1_start <- custom_roi_values$selected_boundaries$roi1_start
      roi2_start <- custom_roi_values$selected_boundaries$roi2_start
      roi3_start <- custom_roi_values$selected_boundaries$roi3_start
      roi5_end <- custom_roi_values$selected_boundaries$roi5_end
      roi6_end <- custom_roi_values$selected_boundaries$roi6_end
      roi7_end <- custom_roi_values$selected_boundaries$roi7_end
      
      # Calculate durations
      roi1_duration <- roi2_start - roi1_start
      roi2_duration <- roi3_start - roi2_start
      roi3_duration <- roi4_start - roi3_start
      roi4_duration <- nadir_duration
      roi5_duration <- roi5_end - roi4_end
      roi6_duration <- roi6_end - roi5_end
      roi7_duration <- roi7_end - roi6_end
      
      # Save configuration
      config_name <- trimws(input$roi_config_label)
      
      success <- save_config_value(
        output_dir = output_dir(),
        config_type = "roi",
        key = config_name,
        value = c(roi1_duration, roi2_duration, roi3_duration, roi4_duration, 
                  roi5_duration, roi6_duration, roi7_duration)
      )
      
      if (success) {
        # Reset custom mode
        custom_roi_values$custom_edit_mode <- FALSE
        custom_roi_values$current_roi_step <- 0
        custom_roi_values$selected_boundaries <- list()
        updateActionButton(session, "create_custom_roi", label = "Create Custom Delineation")
        shinyjs::removeClass("create_custom_roi", "btn-success")
        shinyjs::addClass("create_custom_roi", "btn-info")
        
        # Reload configs and set current config to the newly saved one
        roi_values$roi_configs <- load_config_file(output_dir(), "roi")
        roi_values$current_config <- roi_values$roi_configs[[config_name]]

        create_delineated_dataset()
        
        showNotification("Custom ROI configuration saved and applied successfully!", type = "message")
      } else {
        showNotification("Failed to save custom ROI configuration", type = "error")
      }
    }
    
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
          # Trigger cache refresh by incrementing counter
          roi_values$summary_updated <- roi_values$summary_updated + 1
          roi_values$data_updated <- roi_values$data_updated + 1
          
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
    delineation_status <- create_individual_status_display(
      "delineation_status", 
      reactive(sensor_selector$selected_sensor()), 
      reactive(output_dir()),
      output, session, "delineation",
      invalidation_trigger = reactive(roi_values$summary_updated)  # Triggers refresh
    )
    
    normalization_status <- create_individual_status_display(
      "normalization_status",
      reactive(sensor_selector$selected_sensor()), 
      reactive(output_dir()),
      output, session, "normalization",
      invalidation_trigger = reactive(roi_values$summary_updated)
    )
    
    passage_times_status <- create_individual_status_display(
      "passage_times_status",
      reactive(sensor_selector$selected_sensor()), 
      reactive(output_dir()),
      output, session, "passage_times", 
      invalidation_trigger = reactive(roi_values$summary_updated)
    )
    
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
      if (custom_roi_values$custom_edit_mode) {
        if (!is.null(custom_roi_values$pending_point)) {
          paste0("Time: ", round(custom_roi_values$pending_point$x, 3), "s - Click button to confirm")
        } else if (custom_roi_values$current_roi_step < 6) {
          step_instructions <- c("Click plot to mark ROI 1 start", "Click plot to mark ROI 2 start", 
                                 "Click plot to mark ROI 3 start", "Click plot to mark ROI 5 end",
                                 "Click plot to mark ROI 6 end", "Click plot to mark ROI 7 end")
          step_instructions[custom_roi_values$current_roi_step + 1]
        } else {
          "All boundaries selected - click Save Custom Delineation"
        }
      }
    })

##Round status ####
    output$round_status <- renderText({
      if (input$round_roi && custom_roi_values$standardization_applied) {
        "ROI times rounded to 0.1s precision"
      } else {
        ""
      }
    })
##Match pre/post ####
    output$match_status <- renderText({
      if (input$match_pre_post && custom_roi_values$standardization_applied) {
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
          rownames = FALSE,
          class = 'cell-border stripe compact'
        ) %>%
          DT::formatStyle(columns = 1:4, fontSize = '11px')
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
      roi_values$summary_updated
      
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
    # Get plot controls from the module
    # Setup the base plot using plot module
    plot_controls <- plotModuleServer("roi_plot", 
                                      sensor_data = selected_sensor_data,
                                      sensor_name = reactive(sensor_selector$selected_sensor()),
                                      nadir_info = nadir_info,
                                      selected_nadir = reactive({
                                        if (nadir_values$edit_mode && !is.null(nadir_values$selected_point)) {
                                          nadir_values$selected_point
                                        } else {
                                          NULL
                                        }
                                      }),
                                      roi_boundaries = reactive({
                                        times <- roi_times()
                                        # Show standard ROI boundaries only when NOT in custom edit mode
                                        if (!custom_roi_values$custom_edit_mode && !is.null(times)) {
                                          times$boundaries
                                        } else {
                                          NULL
                                        }
                                      }),
                                      left_var = reactive(input$`roi_plot-left_y_var`),
                                      right_var = reactive(input$`roi_plot-right_y_var`),
                                      show_nadir = reactive(input$`roi_plot-show_nadir`),
                                      show_legend = reactive(input$`roi_plot-show_legend`),
                                      show_normalized = reactive(input$`roi_plot-show_normalized`),
                                      show_roi_markers = reactive(FALSE), # Always controlled by delineation state
                                      custom_edit_mode = reactive(custom_roi_values$custom_edit_mode),
                                      title_prefix = "ROI Delineated",
                                      plot_source = "roi_nadir_plot"
    )
    
    # Add custom ROI editing lines when in edit mode - KEEP THIS FUNCTIONALITY
    observeEvent({
      list(
        custom_roi_values$custom_edit_mode,
        custom_roi_values$selected_boundaries,
        custom_roi_values$pending_point,
        input$roi4_nadir_duration,
        selected_sensor_data()
      )
    }, {
      if (custom_roi_values$custom_edit_mode) {
        
        sensor_data <- selected_sensor_data()
        nadir <- nadir_info()
        req(sensor_data, nadir$available)
        
        # Get current left axis variable from the plot module
        left_var <- if (!is.null(plot_controls$left_var())) plot_controls$left_var() else "pressure_kpa"
        
        if (left_var %in% names(sensor_data)) {
          y_min <- min(sensor_data[[left_var]], na.rm = TRUE)
          y_max <- max(sensor_data[[left_var]], na.rm = TRUE)
          
          # Prepare custom lines data
          custom_lines <- list()
          
          # ROI 4 nadir duration lines (orange solid lines)
          nadir_duration <- input$roi4_nadir_duration %||% 0.4
          roi4_start <- nadir$time - (nadir_duration / 2)
          roi4_end <- nadir$time + (nadir_duration / 2)
          
          custom_lines <- append(custom_lines, list(
            list(
              x = c(roi4_start, roi4_start), y = c(y_min, y_max),
              mode = "lines", line = list(color = "orange", width = 2, dash = "solid"),
              hovertext = "ROI 4 Start (Nadir)", hoverinfo = "text", showlegend = FALSE,
              name = "roi4_start"
            ),
            list(
              x = c(roi4_end, roi4_end), y = c(y_min, y_max),
              mode = "lines", line = list(color = "orange", width = 2, dash = "solid"),
              hovertext = "ROI 4 End (Nadir)", hoverinfo = "text", showlegend = FALSE,
              name = "roi4_end"
            )
          ))
          
          # Custom boundary lines (purple dashed lines for confirmed selections)
          for (boundary_name in names(custom_roi_values$selected_boundaries)) {
            boundary_time <- custom_roi_values$selected_boundaries[[boundary_name]]
            boundary_label <- gsub("_", " ", toupper(boundary_name))
            
            custom_lines <- append(custom_lines, list(
              list(
                x = c(boundary_time, boundary_time), y = c(y_min, y_max),
                mode = "lines", line = list(color = "purple", width = 2, dash = "dash"),
                hovertext = boundary_label, hoverinfo = "text", showlegend = FALSE,
                name = paste0("boundary_", boundary_name)
              )
            ))
          }
          
          # Pending point line (green dotted line for current selection)
          if (!is.null(custom_roi_values$pending_point)) {
            custom_lines <- append(custom_lines, list(
              list(
                x = c(custom_roi_values$pending_point$x, custom_roi_values$pending_point$x), 
                y = c(y_min, y_max),
                mode = "lines", line = list(color = "green", width = 2, dash = "dot"),
                hovertext = "Pending Selection", hoverinfo = "text", showlegend = FALSE,
                name = "pending_point"
              )
            ))
          }
          
          # Add the custom lines to the plot using plotlyProxy
          plotlyProxy("roi_plot", session) %>%
            plotlyProxyInvoke("deleteTraces", 
                              # Remove existing custom traces first
                              which(names(plotlyProxy("roi_plot", session)$x$data) %in% 
                                      c("roi4_start", "roi4_end", 
                                        paste0("boundary_", names(custom_roi_values$selected_boundaries)), 
                                        "pending_point"))) %>%
            plotlyProxyInvoke("addTraces", custom_lines)
        }
      } else {
        # Remove custom lines when not in edit mode
        plotlyProxy("roi_plot", session) %>%
          plotlyProxyInvoke("deleteTraces", 
                            # Remove all custom trace types
                            which(names(plotlyProxy("roi_plot", session)$x$data) %in% 
                                    c("roi4_start", "roi4_end", 
                                      paste0("boundary_", names(custom_roi_values$selected_boundaries)), 
                                      "pending_point")))
      }
    }, ignoreInit = TRUE)
    
  })  # End of moduleServer
}     # End of roiServer
    