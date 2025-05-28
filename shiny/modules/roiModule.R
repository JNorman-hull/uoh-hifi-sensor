# ROI Delineation Module

roiUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    plotlyOutput(ns("roi_plot"), height = "600px"),
    br(),
    
    fluidRow(
      column(
        width = 4,
        
        # ROI Controls box (now left-aligned by being in a column)
        div(
          style = "background-color: #f8f9fa; border: 1px solid #ccc; padding: 15px; 
                 border-radius: 5px; margin-bottom: 20px;",
          
          tags$h4("ROI Controls", style = "margin-top: 0; text-align: center;"),
          
          selectInput(ns("config_choice"), "Configuration:", choices = NULL, width = "100%"),
          
          actionButton(ns("create_custom_roi"), "Create Custom Delineation", 
                       class = "btn btn-sm btn-warning", style = "width: 100%; margin-bottom: 15px;"),
          
          # Inline numericInput
          div(style = "display: flex; align-items: center; justify-content: start; margin-bottom: 15px;",
              tags$label("ROI 4 Nadir Duration (s):", `for` = ns("roi4_nadir_duration"), 
                         style = "margin-right: 8px;"),
              numericInput(ns("roi4_nadir_duration"), NULL, value = 0.2, min = 0.1, max = 2.0, step = 0.1,
                           width = "80px")
          ),
          
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
      
      # ROI Table next to controls
      column(
        width = 8,
        tags$h4("ROI Information"),
        DT::dataTableOutput(ns("roi_table"))
      )
    ),  # Close the first fluidRow
    
    # Horizontal rule after the row
    hr(),
    
    h4("ROI Instructions"),
    
    # Instructional text in a new fluidRow
    fluidRow(
      column(
        width = 12,
        tags$p("Use the table above to review detected ROIs. Each entry corresponds to a delineated region based on sensor thresholds or manual marking. Check timestamps to ensure accurate event capture."),
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
    h4("ROI Delineation Options"),
    selectInput(ns("plot_sensor"), "Select Sensor:", choices = NULL),
    
    div(style = "margin-bottom: 15px;", textOutput(ns("delineation_status"))),
    
    hr(), h4("Plot Axis Options"),
    selectInput(ns("left_y_var"), "Left Y-Axis:", choices = var_choices, selected = "pressure_kpa"),
    selectInput(ns("right_y_var"), "Right Y-Axis:", choices = c("None" = "none", var_choices), selected = "higacc_mag_g"),
    
    hr(), h4("Pressure Nadir Options"),
    checkboxInput(ns("show_nadir"), "Show Pressure Nadir", value = TRUE),
    verbatimTextOutput(ns("current_nadir_display")),
    actionButton(ns("nadir_btn"), "Modify Pressure Nadir", class = "btn-warning btn-block"),
    actionButton(ns("cancel_nadir_btn"), "Cancel", class = "btn-danger btn-block"),
    textOutput(ns("nadir_status")),
    
    hr(), h4("Delineate data"),
    actionButton(ns("create_delineated"), "Create delineated dataset", class = "btn-primary btn-block"),
    actionButton(ns("start_over"), "Start Over", class = "btn-warning btn-block"),
    actionButton(ns("trim_sensor"), "Trim sensor start and end", class = "btn-danger btn-block"),
    
    hr(), h4("Passage times"),
    actionButton(ns("passage_time"), "Calculate passage times", class = "btn-primary btn-block"),
    textOutput(ns("passage_status")),
    
    hr(), h4("Time normalization"),
    actionButton(ns("normalize_time"), "Normalize time series", class = "btn-primary btn-block"),
    textOutput(ns("normalize_status"))
  )
}

roiServer <- function(id, output_dir, summary_data, processing_complete = reactive(FALSE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Values for ROI processing
    roi_values <- reactiveValues(
      roi_configs = NULL,
      current_config = NULL,
      summary_updated = 0,
      data_updated = 0
    )
    
    # Nadir editing values (moved from plots module)
    nadir_values <- reactiveValues(
      edit_mode = FALSE,
      selected_point = NULL,
      nadir_updated = 0,
      baseline_click = NULL
    )
    
    # Custom ROI editing values - simplified
    custom_roi_values <- reactiveValues(
      custom_edit_mode = FALSE,
      current_roi_step = 0,  # 0-6, tracking which boundary to select
      selected_boundaries = list(),
      custom_nadir_duration = 0.4,
      baseline_click = NULL,
      pending_point = NULL
    )
    
    # Get processed sensors using shared function
    processed_sensors <- reactive({
      processing_complete()
      get_processed_sensors(output_dir())
    })
    
    # Update sensor dropdown using shared function
    observe({
      update_sensor_dropdown(session, "plot_sensor", processed_sensors(), input$plot_sensor)
    })
    
    # Load ROI configurations using shared function
    observe({
      roi_values$roi_configs <- load_roi_configs(output_dir())
      
      if (length(roi_values$roi_configs) > 0) {
        config_names <- names(roi_values$roi_configs)
        choices <- setNames(config_names, gsub("_", " ", config_names))
        
        # Determine which config to select based on sensor's current roi_config
        selected_value <- config_names[1]  # Default fallback
        
        if (!is.null(input$plot_sensor) && input$plot_sensor != "") {
          index_file <- get_sensor_index_file(output_dir())
          if (!is.null(index_file)) {
            tryCatch({
              index_df <- read.csv(index_file)
              sensor_row <- index_df[index_df$file == input$plot_sensor, ]
              if (nrow(sensor_row) > 0 && !is.na(sensor_row$roi_config) && sensor_row$roi_config != "NA") {
                if (sensor_row$roi_config %in% config_names) {
                  selected_value <- sensor_row$roi_config
                }
              }
            }, error = function(e) {
              # Silently fall back to default
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
    
    # Get nadir info using shared function with reactivity
    nadir_info <- reactive({
      req(input$plot_sensor)
      roi_values$summary_updated  # Add reactivity to catch nadir updates
      nadir_values$nadir_updated  # Force refresh when nadir is updated
      get_nadir_info(input$plot_sensor, output_dir())
    })
    
    # Display current nadir (moved from plots module)
    output$current_nadir_display <- renderText({
      nadir <- nadir_info()
      if (nadir$available) {
        paste0("Time: ", round(nadir$time, 3), "s\nPressure: ", round(nadir$value, 2), " kPa")
      } else {
        "No nadir data available"
      }
    })
    
    # Output for conditional panel
    output$custom_edit_mode <- reactive({
      custom_roi_values$custom_edit_mode
    })
    outputOptions(output, "custom_edit_mode", suspendWhenHidden = FALSE)
    
    # Get sensor status using shared function
    sensor_status <- reactive({
      req(input$plot_sensor)
      roi_values$summary_updated  # Reactivity to delineation changes
      roi_values$data_updated     # Reactivity to trimming changes
      
      get_sensor_status(input$plot_sensor, output_dir())
    })
    
    # Delineation status display
    output$delineation_status <- renderText({
      req(input$plot_sensor)
      
      nadir <- nadir_info()
      status <- sensor_status()
      
      if (!nadir$available) {
        "No nadir data available"
      } else if (!status$delineated) {
        "Sensor requires delineation"
      } else if (status$trimmed) {
        "Sensor file delineated and trimmed"
      } else {
        "Sensor file delineated (not trimmed)"
      }
    })
    
    # CSS styling for status text
    observe({
      req(input$plot_sensor)
      
      nadir <- nadir_info()
      status <- sensor_status()
      
      status_color <- if (!nadir$available) {
        "orange"
      } else if (!status$delineated) {
        "red"
      } else if (status$trimmed) {
        "green"
      } else {
        "blue"
      }
      
      shinyjs::runjs(paste0("
        $('#", ns("delineation_status"), "').css({
          'color': '", status_color, "', 
          'font-weight': 'bold'
        });
      "))
    })
    
    # Button state management using shared function (including nadir editing buttons)
    observe({
      req(input$plot_sensor)
      
      nadir <- nadir_info()
      status <- sensor_status()
      
      button_states <- list(
        "create_delineated" = nadir$available && !status$delineated && !custom_roi_values$custom_edit_mode,
        "start_over" = status$delineated && !custom_roi_values$custom_edit_mode,
        "trim_sensor" = status$delineated && !status$trimmed && !custom_roi_values$custom_edit_mode,
        "nadir_btn" = (!custom_roi_values$custom_edit_mode) && (!nadir_values$edit_mode || !is.null(nadir_values$selected_point)),
        "cancel_nadir_btn" = nadir_values$edit_mode,
        "create_custom_roi" = nadir$available && (!custom_roi_values$custom_edit_mode || custom_roi_values$current_roi_step == 6),
        "cancel_custom_roi" = custom_roi_values$custom_edit_mode,
        "mark_roi_dynamic" = custom_roi_values$custom_edit_mode
      )
      
      manage_button_states(session, button_states)
    })
    
    # Nadir editing functionality (moved from plots module)
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
          input$plot_sensor,
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
    
    # Keep the cancel button handler but remove the old save handler:
    observeEvent(input$cancel_nadir_btn, {
      nadir_values$edit_mode <- FALSE
      nadir_values$selected_point <- NULL
    })
    
    # Handle click events for nadir selection
    observe({
      if (nadir_values$edit_mode) {
        click_data <- event_data("plotly_click", source = "roi_nadir_plot")
        if (!is.null(click_data)) {
          # Only respond if this click is different from the baseline we stored
          if (is.null(nadir_values$baseline_click) ||
              click_data$x != nadir_values$baseline_click$x ||
              click_data$y != nadir_values$baseline_click$y) {
            nadir_values$selected_point <- list(x = click_data$x, y = click_data$y)
          }
        }
      }
    })
    
    # Status display for nadir editing
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
    
    # Custom ROI functionality
    # Create custom ROI button
    observeEvent(input$create_custom_roi, {
      if (!custom_roi_values$custom_edit_mode) {
        # Start custom mode
        custom_roi_values$custom_edit_mode <- TRUE
        custom_roi_values$current_roi_step <- 0
        custom_roi_values$selected_boundaries <- list()
        custom_roi_values$custom_nadir_duration <- input$roi4_nadir_duration %||% 0.4
        custom_roi_values$baseline_click <- event_data("plotly_click", source = "roi_nadir_plot")
        updateActionButton(session, "create_custom_roi", label = "Save Custom Delineation")
      } else if (custom_roi_values$current_roi_step == 6) {
        # Save configuration
        save_custom_configuration()
      }
    })
    
    # Cancel custom ROI
    observeEvent(input$cancel_custom_roi, {
      custom_roi_values$custom_edit_mode <- FALSE
      custom_roi_values$current_roi_step <- 0
      custom_roi_values$selected_boundaries <- list()
      custom_roi_values$pending_point <- NULL
      updateActionButton(session, "create_custom_roi", label = "Create Custom Delineation")
    })
    
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
          # Change to green using shinyjs
          shinyjs::removeClass("create_custom_roi", "btn-info")
          shinyjs::addClass("create_custom_roi", "btn-success")
        }
      } else {
        # Reset to initial state when not in edit mode
        updateActionButton(session, "mark_roi_dynamic", label = "Mark ROI 1 Start")
        updateActionButton(session, "create_custom_roi", label = "Create Custom Delineation")
        # Reset to blue using shinyjs
        shinyjs::removeClass("create_custom_roi", "btn-success")
        shinyjs::addClass("create_custom_roi", "btn-info")
      }
    })
    
    # Single dynamic ROI marking button
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
    
    # Add instruction text output
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
      success <- save_custom_roi_config(
        output_dir(),
        roi1_duration, roi2_duration, roi3_duration, roi4_duration,
        roi5_duration, roi6_duration, roi7_duration
      )
      
      if (success$status) {
        # Reset custom mode
        custom_roi_values$custom_edit_mode <- FALSE
        custom_roi_values$current_roi_step <- 0
        custom_roi_values$selected_boundaries <- list()
        updateActionButton(session, "create_custom_roi", label = "Create Custom Delineation")
        shinyjs::removeClass("create_custom_roi", "btn-success")
        shinyjs::addClass("create_custom_roi", "btn-info")
        
        # Reload configurations
        roi_values$roi_configs <- load_roi_configs(output_dir())
        
        # IMPORTANT: Set the new config as current BEFORE delineating
        roi_values$current_config <- roi_values$roi_configs[[success$config_name]]
        
        # Automatically delineate the dataset with the new config
        create_delineated_dataset()
        
        showNotification("Custom ROI configuration saved and applied successfully!", type = "message")
      } else {
        showNotification("Failed to save custom ROI configuration", type = "error")
      }

    }
    
    # Calculate ROI times
    roi_times <- reactive({
      req(input$plot_sensor, roi_values$current_config)
      input$config_choice  # Make reactive to config changes
      
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
      sensor_data <- read_sensor_data(output_dir(), input$plot_sensor, "min")
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
          rownames = FALSE,
          class = 'cell-border stripe compact'
        ) %>%
          DT::formatStyle(columns = 1:4, fontSize = '11px')
      }
    })
    
    # Show notification when nadir is not available for selected sensor (only once per sensor)
    last_nadir_warning <- reactiveVal("")
    
    observe({
      req(input$plot_sensor)
      nadir <- nadir_info()
      
      if (!nadir$available && last_nadir_warning() != input$plot_sensor) {
        showNotification(
          paste("No nadir data available for", input$plot_sensor, ". Sensor must be listed in summary file."),
          type = "warning",
          duration = 8
        )
        last_nadir_warning(input$plot_sensor)
      }
    })
    
    # Create delineated dataset
    observeEvent(input$create_delineated, {
      req(input$plot_sensor, roi_times(), roi_values$current_config)
      
      # Check if already delineated using shared function
      status <- get_sensor_status(input$plot_sensor, output_dir())
      if (status$delineated) {
        showModal(modalDialog(
          title = "Confirm Replacement",
          "Sensor data already delineated. Continue and replace delineated file?",
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_replace"), "Replace", class = "btn-warning")
          )
        ))
        return()
      }
      
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
      req(input$plot_sensor)
      
      # Check status using shared function
      status <- get_sensor_status(input$plot_sensor, output_dir())
      
      if (!status$delineated) {
        showNotification("No delineated dataset found. Please delineate dataset first.", type = "warning")
        return()
      }
      
      if (status$trimmed) {
        showNotification("Sensor data already trimmed. Delineate dataset again if you want to re-trim.", type = "warning")
        return()
      }
      
      # Read delineated data using shared function
      sensor_data <- read_sensor_data(output_dir(), input$plot_sensor, "delineated")
      
      if (is.null(sensor_data)) {
        showNotification("Failed to read delineated dataset.", type = "error")
        return()
      }
      
      # Check if trim levels exist
      if (!"roi" %in% names(sensor_data) || 
          !any(sensor_data$roi %in% c("trim_start", "trim_end"))) {
        showNotification("Sensor data already trimmed. Delineate dataset again if you want to re-trim.", type = "warning")
        return()
      }
      
      # Perform trimming
      trimmed_data <- sensor_data[!sensor_data$roi %in% c("trim_start", "trim_end"), ]
      
      # Save over existing delineated file
      delineated_path <- file.path(output_dir(), "csv", "delineated", paste0(input$plot_sensor, "_delineated.csv"))
      write.csv(trimmed_data, delineated_path, row.names = FALSE)
      
      # Update sensor index using shared function
      success <- safe_update_sensor_index(output_dir(), input$plot_sensor, list(trimmed = "Y"))
      
      if (success) {
        # Trigger data refresh
        roi_values$data_updated <- roi_values$data_updated + 1
        roi_values$summary_updated <- roi_values$summary_updated + 1
        
        showNotification("Sensor data trimmed successfully!", type = "message")
      } else {
        showNotification("Failed to update sensor index", type = "error")
      }
    })
    
    # Start over button
    observeEvent(input$start_over, {
      req(input$plot_sensor)
      
      # Remove delineated file
      delineated_path <- file.path(output_dir(), "csv", "delineated", paste0(input$plot_sensor, "_delineated.csv"))
      if (file.exists(delineated_path)) {
        file.remove(delineated_path)
      }
      
      # Reset flags in sensor index using shared function
      success <- safe_update_sensor_index(
        output_dir(), 
        input$plot_sensor,
        list(
          delineated = "N",
          trimmed = "N",
          roi_config = "NA"
        )
      )
      
      if (success) {
        # Trigger updates
        roi_values$summary_updated <- roi_values$summary_updated + 1
        roi_values$data_updated <- roi_values$data_updated + 1
        
        showNotification("Reset to original sensor file", type = "message")
      } else {
        showNotification("Failed to reset sensor status", type = "error")
      }
    })
    
    # Function to create delineated dataset
    create_delineated_dataset <- function() {
      tryCatch({
        # Read original data using shared function
        sensor_data <- read_sensor_data(output_dir(), input$plot_sensor, "min")
        
        if (is.null(sensor_data)) {
          showNotification("Source file not found", type = "error")
          return()
        }
        
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
        output_file <- file.path(delineated_dir, paste0(input$plot_sensor, "_delineated.csv"))
        write.csv(sensor_data, output_file, row.names = FALSE)
        
        # Verify file was created
        if (!file.exists(output_file)) {
          showNotification("Failed to create delineated file", type = "error")
          return()
        }
        
        # Update sensor index using shared function
        success <- safe_update_sensor_index(
          output_dir(),
          input$plot_sensor,
          list(
            delineated = "Y",
            roi_config = roi_values$current_config$label,
            trimmed = "N"  # Reset trimmed status when re-delineating
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
    
    # Read selected sensor data (with preference for delineated data)
    selected_sensor_data <- reactive({
      req(input$plot_sensor)
      roi_values$data_updated  # Invalidate when data changes
      
      # Check for delineated file first
      delineated_data <- read_sensor_data(output_dir(), input$plot_sensor, "delineated")
      if (!is.null(delineated_data)) {
        return(delineated_data)
      }
      
      # Fall back to regular minimal data
      return(read_sensor_data(output_dir(), input$plot_sensor, "min"))
    })
    
    # Create ROI plot using shared plotting function
    output$roi_plot <- renderPlotly({
      sensor_data <- selected_sensor_data()
      req(sensor_data)
      
      nadir <- nadir_info()
      req(nadir$available)
      
      times <- roi_times()
      roi_boundaries <- if (!is.null(times)) times$boundaries else NULL
      
      selected_nadir <- if (nadir_values$edit_mode && !is.null(nadir_values$selected_point)) {
        nadir_values$selected_point
      } else {
        NULL
      }
      
      suppress_roi_lines <- isTRUE(custom_roi_values$custom_edit_mode)
      
      # Create base plot
      p <- create_sensor_plot(
        sensor_data = sensor_data,
        sensor_name = input$plot_sensor,
        plot_config = "roi_delineation",
        left_var = input$left_y_var,
        right_var = input$right_y_var,
        nadir_info = nadir,
        show_nadir = input$show_nadir,
        selected_nadir = selected_nadir,
        roi_boundaries = roi_boundaries,
        show_legend = FALSE,
        plot_source = "roi_nadir_plot",
        suppress_roi_lines = suppress_roi_lines
      )
      
      # Add custom ROI lines in edit mode
      if (custom_roi_values$custom_edit_mode &&
          !is.null(input$left_y_var) &&
          input$left_y_var %in% names(sensor_data)) {
        
        y_min <- min(sensor_data[[input$left_y_var]], na.rm = TRUE)
        y_max <- max(sensor_data[[input$left_y_var]], na.rm = TRUE)
        
        # Add ROI 4 nadir duration lines (based on user input)
        nadir_duration <- input$roi4_nadir_duration %||% 0.4
        roi4_start <- nadir$time - (nadir_duration / 2)
        roi4_end <- nadir$time + (nadir_duration / 2)
        
        # ROI 4 start line
        p <- p %>% add_segments(
          x = roi4_start, xend = roi4_start,
          y = y_min, yend = y_max,
          line = list(color = "orange", width = 2, dash = "solid"),
          text = "ROI 4 Start (Nadir)",
          hoverinfo = "text",
          showlegend = FALSE
        )
        
        # ROI 4 end line  
        p <- p %>% add_segments(
          x = roi4_end, xend = roi4_end,
          y = y_min, yend = y_max,
          line = list(color = "orange", width = 2, dash = "solid"),
          text = "ROI 4 End (Nadir)",
          hoverinfo = "text",
          showlegend = FALSE
        )
        
        # Add custom boundary lines (purple)
        for (boundary_name in names(custom_roi_values$selected_boundaries)) {
          boundary_time <- custom_roi_values$selected_boundaries[[boundary_name]]
          boundary_label <- gsub("_", " ", toupper(boundary_name))
          
          p <- p %>% add_segments(
            x = boundary_time, xend = boundary_time,
            y = y_min, yend = y_max,
            line = list(color = "purple", width = 2, dash = "dash"),
            text = boundary_label,
            hoverinfo = "text",
            showlegend = FALSE
          )
        }
        
        # Add pending point line (green)
        if (!is.null(custom_roi_values$pending_point)) {
          x_val <- custom_roi_values$pending_point$x
          p <- p %>% add_segments(
            x = x_val, xend = x_val,
            y = y_min, yend = y_max,
            line = list(color = "green", width = 2, dash = "dot"),
            text = "Pending Selection",
            hoverinfo = "text",
            showlegend = FALSE
          )
        }
      }  # End of if (custom_edit_mode)
      
      return(p)
    })
  })
}