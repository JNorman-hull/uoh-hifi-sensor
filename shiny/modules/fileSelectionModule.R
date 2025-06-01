# File Selection Module - Using shared components

fileSelectionUI <- function(id) {
  ns <- NS(id)
  
  fileSelectionTableUI(
    ns("sensor_table"),
    title = "RAW Rapid data index",
    help_text = "Index of RAW RAPID data files. IMP and HIG must be present. 
                 Sensors already in global index are highlighted in orange."
  )
}

fileSidebarUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Raw data prcoessing controls"),

    div(style = "color: #666; font-style: italic; margin-bottom: 15px;",
        "Select sensor(s) to process binary RAPID data."),
    
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

fileSelectionServer <- function(id, raw_data_path, output_dir, processing_complete = reactive(FALSE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # File location outputs
    output$raw_data_location <- renderText({
      paste("Raw Data Path:", raw_data_path())
    })
    
    output$output_location <- renderText({
      paste("Output Path:", output_dir())
    })
    
    # Prepare sensor data
    sensor_data <- reactive({
      req(raw_data_path())
      
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
        Sensor = map_chr(sensor_info, "sensor"),
        Date = map_chr(sensor_info, "date_deploy"),
        Time = map_chr(sensor_info, "time_deploy")
      )
    })
    
    # Get processed sensors
    processed_sensors <- reactive({
      if (is.null(output_dir)) return(character(0))
      
      tryCatch({
        processing_complete()
      }, error = function(e) {})
      
      index_file <- get_sensor_index_file(output_dir())
      if (is.null(index_file) || !file.exists(index_file)) {
        return(character(0))
      }
      
      tryCatch({
        index_df <- read.csv(index_file)
        return(index_df$file)
      }, error = function(e) {
        return(character(0))
      })
    })
    
    # Use the shared table module
    table_results <- fileSelectionTableServer(
      "sensor_table",
      sensor_data_reactive = sensor_data,
      highlight_sensors_reactive = processed_sensors,
      enable_selection = TRUE,
      selection_mode = 'multiple'
    )
    
    # Return values
    return(list(
      selected_sensors = table_results$selected_items,
      sensor_names = reactive({
        data <- sensor_data()
        if (is.null(data)) character(0) else data$Filename
      }),
      process_trigger = reactive(input$process_btn)
    ))
  })
}