
processingUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Processing Log"),
    verbatimTextOutput(ns("process_log"))
  )
}

processingServer <- function(id, selected_sensors, raw_data_path, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      log_messages = character(0),
      summary_data = NULL,
      is_processing = FALSE,
      processing_complete = FALSE
    )
    
    # Display processing log
    output$process_log <- renderText({
      paste(values$log_messages, collapse = "\n")
    })
    
    # Helper function to update messages
    updateLogMessage <- function(message) {
      values$log_messages <- c(values$log_messages, message)
      session$sendCustomMessage("updateProcessLog", list(text = paste(values$log_messages, collapse = "\n")))
    }
    
    # Process sensors function
    process_sensors <- function() {
      sensors <- selected_sensors()
      
      # Check if any sensors are selected
      if (length(sensors) == 0) {
        updateLogMessage("No sensors selected. Please select at least one sensor to process.")
        return()
      }
      
      # Prevent multiple processing jobs
      if (values$is_processing) {
        updateLogMessage("Processing already in progress. Please wait...")
        return()
      }
      
      # Reset state
      values$is_processing <- TRUE
      values$processing_complete <- FALSE
      values$summary_data <- NULL
      values$log_messages <- character(0)
      
      # Process in step-by-step manner
      result <- process_sensors_step_by_step(
        sensors, 
        raw_data_path(), 
        output_dir(),
        session
      )
      
      # Update after completion
      values$summary_data <- result$summary_data
      values$log_messages <- result$log_messages
      values$processing_complete <- TRUE
      values$is_processing <- FALSE
      
      # Send completion notification
      session$sendCustomMessage("processingComplete", list(success = TRUE))
    }
    
    return(list(
      summary_data = reactive(values$summary_data),
      processing_complete = reactive(values$processing_complete),
      is_processing = reactive(values$is_processing),
      log_messages = reactive(values$log_messages),
      process_sensors = process_sensors
    ))
  })
}