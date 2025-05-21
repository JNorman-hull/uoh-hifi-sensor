server <- function(input, output, session) {
  
  # Define reactive values
  values <- reactiveValues(
    sensor_names = NULL,
    processing_complete = FALSE,
    log_messages = character(0),
    summary_data = NULL,
    is_processing = FALSE
  )
  
  # Raw data location
  raw_data_path <- reactive({
    "./RAW_data/RAPID"
  })
  
  # Output directory
  output_dir <- reactive({
    "./RAPID_Processed"
  })
  
  # Display file locations
  output$raw_data_location <- renderText({
    paste("Raw Data Path:", raw_data_path())
  })
  
  output$output_location <- renderText({
    paste("Output Path:", output_dir())
  })
  
  # Get available sensors on app start
  observe({
    values$sensor_names <- get_sensor_names(raw_data_path())
  })
  
  # Generate sensor checkboxes dynamically
  output$sensor_checkboxes <- renderUI({
    if (is.null(values$sensor_names) || length(values$sensor_names) == 0) {
      return(p("No sensor files found in the specified directory."))
    }
    
    checkbox_list <- lapply(values$sensor_names, function(sensor) {
      # Sanitize sensor name for input ID using R's built-in function
      input_id <- paste0("sensor_", make.names(sensor))
      
      checkboxInput(
        inputId = input_id,
        label = sensor,
        value = input$select_all
      )
    })
    
    tagList(checkbox_list)
  })
  
  # Update individual checkboxes when Select All is changed
  observeEvent(input$select_all, {
    if (!is.null(values$sensor_names)) {
      for (sensor in values$sensor_names) {
        input_id <- paste0("sensor_", make.names(sensor))
        updateCheckboxInput(session, input_id, value = input$select_all)
      }
    }
  })
  
  # Display processing log
  output$process_log <- renderText({
    paste(values$log_messages, collapse = "\n")
  })
  
  # Helper function to consistently update messages
  updateLogMessage <- function(message) {
    values$log_messages <- c(values$log_messages, message)
    session$sendCustomMessage("updateProcessLog", list(text = paste(values$log_messages, collapse = "\n")))
  }
  
  # Process button click handler
  observeEvent(input$process_btn, {
    # Prevent multiple processing jobs
    if (values$is_processing) {
      updateLogMessage("Processing already in progress. Please wait...")
      return()
    }
    
    # Get selected sensors
    selected_sensors <- c()
    
    for (sensor in values$sensor_names) {
      input_id <- paste0("sensor_", make.names(sensor))
      if (!is.null(input[[input_id]]) && isTRUE(input[[input_id]])) {
        selected_sensors <- c(selected_sensors, sensor)
      }
    }
    
    # Check if any sensors are selected
    if (length(selected_sensors) == 0) {
      updateLogMessage("No sensors selected. Please select at least one sensor to process.")
      return()
    }
    
    # Reset state
    values$is_processing <- TRUE
    values$processing_complete <- FALSE
    values$summary_data <- NULL
    values$log_messages <- character(0)
    
    # Disable the process button during processing
    shinyjs::disable("process_btn")
    
    # Process in step-by-step manner
    result <- process_sensors_step_by_step(
      selected_sensors, 
      raw_data_path(), 
      output_dir(),
      session
    )
    
    # Update after completion
    values$summary_data <- result$summary_data
    values$log_messages <- result$log_messages
    values$processing_complete <- TRUE
    values$is_processing <- FALSE
    
    # Re-enable the button
    shinyjs::enable("process_btn")
  })
  
  # Display results table
  output$results_table <- DT::renderDataTable({
    req(values$processing_complete)
    req(length(values$summary_data) > 0)
    
    # Convert the list of dictionaries to an R data frame
    summary_df <- do.call(rbind, lapply(values$summary_data, function(x) {
      df <- as.data.frame(x, stringsAsFactors = FALSE)
      
      # Reorder to put 'file' first
      col_order <- c("file", setdiff(names(df), "file"))
      df[, col_order]
    }))
    
    # If conversion worked and we have data, display it
    if (is.data.frame(summary_df) && nrow(summary_df) > 0) {
      DT::datatable(
        summary_df,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        )
      )
    } else {
      # If conversion failed, show a message
      return(NULL)
    }
  })
}