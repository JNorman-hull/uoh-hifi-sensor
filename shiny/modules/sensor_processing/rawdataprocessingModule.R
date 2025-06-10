# File Selection Module - Using shared components

rawdataprocessingUI <- function(id) {
  ns <- NS(id)
  
  fileSelectionTableUI(
    ns("sensor_table"),
    title = "RAW Rapid data index",
    help_text = "Index of RAW RAPID data files. Green = sensor processed. Orange = sensor requires processing."
    
  )
}

rawdataprocessingsidebarUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Raw data prcoessing controls"),

    div(style = "color: #666; font-style: italic; margin-bottom: 15px;",
        "Select sensor(s) to process binary RAPID data."),
    div(style = "color: #666; font-style: bold; margin-bottom: 15px;",
        "Raw RAPID sensor data (.imp and .hig) should be added to ./raw_sens_data."),
    
    hr(),
    
    processinghelperUI(ns("processing_helper")),
    
    hr(),
    
    # Use shared controls
    fileSelectionControlsUI(
      ns("sensor_table"),
      show_select_all = TRUE,
      show_clear_all = TRUE,
      show_summary = TRUE
    ),
    
    br(),
    
    actionButton(ns("process_btn"), "Process Selected Sensors", 
                 class = "btn-primary btn-block")
  )
}

rawdataprocessingServer <- function(id, raw_data_path, output_dir, processing_complete = reactive(FALSE),
                                    global_sensor_state, trigger_data_update, trigger_summary_update, trigger_processing_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Get processed sensors
    processed_sensors <- reactive({
      if (is.null(output_dir)) return(character(0))
      
      # Add global state invalidation
      global_sensor_state$summary_updated
      global_sensor_state$processing_updated
      
      tryCatch({
        processing_complete()
      }, error = function(e) {})
      
      index_df <- get_sensor_index_file(output_dir(), read_data = TRUE)
      if (is.null(index_df)) {
        return(character(0))
      }
      
      tryCatch({
        return(index_df$file)
      }, error = function(e) {
        return(character(0))
      })
    })
    
    # Prepare sensor data
    sensor_data <- reactive({
      req(raw_data_path())
      global_sensor_state$processing_updated
      
      sensor_names <- get_sensor_names(raw_data_path())
      
      if (length(sensor_names) == 0) return(NULL)
      
      sensor_info <- map(sensor_names, function(name) {
        tryCatch({
          py$parse_filename_info(name)
        }, error = function(e) {
          list(
            sensor = if(nchar(name) >= 3) substr(name, 1, 3) else name,
            date_deploy = "Unknown",
            time_deploy = "Unknown"
          )
        })
      })
      
      tibble(
        No. = seq_along(sensor_names),
        Filename = sensor_names,
        Sensor = map_chr(sensor_info, ~ .x$sensor %||% "Unknown"),
        Date = map_chr(sensor_info, ~ .x$date_deploy %||% "Unknown"),
        Time = map_chr(sensor_info, ~ .x$time_deploy %||% "Unknown"),
        Status = ifelse(sensor_names %in% processed, "Processed", "Requires Processing")
      )
    })
    
    
    
    # Use the shared table module
    table_results <- fileSelectionTableServer(
      "sensor_table",
      sensor_data_reactive = sensor_data,
      highlight_sensors_reactive = processed_sensors(), 
      enable_selection = TRUE,
      selection_mode = 'multiple'
    )
    
  
    
    observeEvent(input$process_btn, {
      processing_helper$process_sensors()
    })
    
    observe({
      selected_sensors <- table_results$selected_items()
      is_processing <- processing_helper$is_processing()
      
      # Enable button only when sensors are selected AND not processing
      can_process <- length(selected_sensors) > 0 && !is_processing
      
      if (can_process) {
        shinyjs::enable("process_btn")
      } else {
        shinyjs::disable("process_btn")
      }
    })
    
    observeEvent(input$process_btn, {
      shinyjs::disable("process_btn")  # Disable immediately on click
      processing_helper$process_sensors()
    })
    
    selected_sensors <- table_results$selected_items
    
    # Then call processinghelperServer
    processing_helper <- processinghelperServer("processing_helper", 
                                                selected_sensors, 
                                                raw_data_path, 
                                                output_dir,
                                                global_sensor_state,
                                                trigger_data_update,
                                                trigger_summary_update,
                                                trigger_processing_update)
    
    return(list(
      selected_sensors = table_results$selected_items,
      sensor_names = reactive({
        data <- sensor_data()
        if (is.null(data)) character(0) else data$Filename
      }),
      process_trigger = reactive(input$process_btn),
      # Return processing helper outputs
      processing_complete = processing_helper$processing_complete,
      newly_processed_sensors = processing_helper$newly_processed_sensors,
      summary_data = processing_helper$summary_data
    ))
  })
}