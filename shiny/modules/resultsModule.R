resultsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Recently Processed Sensors"),
    DT::dataTableOutput(ns("results_table"))
  )
}

resultsServer <- function(id, newly_processed_sensors, processing_complete) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Create sensor data for newly processed sensors only
    results_data <- reactive({
      req(processing_complete())
      processed <- newly_processed_sensors()
      
      if (length(processed) == 0) return(NULL)
      
      # Get sensor info for processed sensors
      sensor_info <- map(processed, function(name) {
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
        No. = seq_along(processed),
        Filename = processed,
        Sensor = map_chr(sensor_info, "sensor"),
        Date = map_chr(sensor_info, "date_deploy"),
        Time = map_chr(sensor_info, "time_deploy")
      )
    })
    
    # Display results table with green highlighting
    output$results_table <- DT::renderDataTable({
      data <- results_data()
      
      if (is.null(data) || nrow(data) == 0) {
        return(DT::datatable(
          data.frame(Message = "No sensors processed in this session yet"), 
          options = list(dom = 't', ordering = FALSE),
          rownames = FALSE
        ))
      }
      
      DT::datatable(
        data,
        selection = 'none',
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          scrollY = "400px",
          dom = 'tip',
          columnDefs = list(
            list(width = '60px', targets = 0),
            list(width = '200px', targets = 1),
            list(width = '80px', targets = 2:4)
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe'
      ) %>%
        DT::formatStyle(columns = 1:5, fontSize = '14px') %>%
        DT::formatStyle(
          columns = 1:5,
          target = 'row',
          backgroundColor = 'lightgreen'
        )
    })
  })
}