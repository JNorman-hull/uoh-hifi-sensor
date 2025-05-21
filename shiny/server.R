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
    
    # Force UI to update by switching to log tab
    updateTabsetPanel(session, "mainTabset", selected = "processing_log")
    
    # Use a small delay to ensure UI updates before processing starts
    shinyjs::delay(100, {
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
      
      # Switch to results tab when processing is complete
      updateTabsetPanel(session, "mainTabset", selected = "results_summary")
      
      # Re-enable the button
      shinyjs::enable("process_btn")
    })
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
  
  #=================================================================
  # NEW CODE FOR INTERACTIVE PLOTLY PLOTS
  #=================================================================
  
  # Create a reactive expression to get the list of processed sensors
  processed_sensors <- reactive({
    # Look for _min.csv files in the output directory
    min_files <- list.files(path = file.path(output_dir(), "csv"), 
                            pattern = "_min\\.csv$", full.names = FALSE)
    
    # Extract base names (remove _min.csv)
    sensor_names <- gsub("_min\\.csv$", "", min_files)
    
    return(sensor_names)
  })
  
  # Update the sensor dropdown when processing is complete
  observe({
    # Use either processed sensors or sensors from summary data
    choices <- processed_sensors()
    
    # If we have summary data, use those sensor names instead
    if (!is.null(values$summary_data) && length(values$summary_data) > 0) {
      sensor_names <- sapply(values$summary_data, function(x) x$file)
      if (length(sensor_names) > 0) {
        choices <- sensor_names
      }
    }
    
    # If we have choices, update the dropdown
    if (length(choices) > 0) {
      updateSelectInput(session, "plot_sensor", choices = choices)
    }
  })
  
  # Create a reactive to read the selected sensor data
  selected_sensor_data <- reactive({
    req(input$plot_sensor)
    
    # File path to the minimal CSV
    file_path <- file.path(output_dir(), "csv", paste0(input$plot_sensor, "_min.csv"))
    
    # Check if file exists
    if (!file.exists(file_path)) {
      return(NULL)
    }
    
    # Read the CSV
    data <- read.csv(file_path)
    return(data)
  })
  
  # Get nadir information for the selected sensor
  nadir_info <- reactive({
    req(input$plot_sensor)
    req(values$summary_data)
    
    # Find the summary data for the selected sensor
    selected_summary <- NULL
    for (summary in values$summary_data) {
      if (summary$file == input$plot_sensor) {
        selected_summary <- summary
        break
      }
    }
    
    if (is.null(selected_summary)) {
      return(NULL)
    }
    
    return(list(
      time = as.numeric(selected_summary$`pres_min[time]`),
      value = as.numeric(selected_summary$`pres_min[kPa]`)
    ))
  })
  
  # Create the plot
  output$sensor_plot <- renderPlotly({
    # Require sensor data
    sensor_data <- selected_sensor_data()
    req(sensor_data)
    
    # Mapping from variable names to display names, colors
    var_names <- c("pressure_kpa", "higacc_mag_g", "inacc_mag_ms", "rot_mag_degs")
    var_labels <- c("Pressure [kPa]", "HIG Acceleration [g]", 
                    "Inertial Acceleration [m/s²]", "Rotational Magnitude [deg/s]")
    colors <- c("black", "red", "blue", "green")
    
    # Get color for left axis (match variable index)
    left_var <- input$left_y_var
    left_color <- colors[which(var_names == left_var)]
    left_label <- var_labels[which(var_names == left_var)]
    
    # Initialize the plot
    p <- plot_ly() %>%
      layout(
        title = paste("Sensor Data:", input$plot_sensor),
        xaxis = list(
          title = "Time [s]",
          showline = TRUE,
          linecolor = "black",
          linewidth = 1,
          showticklabels = TRUE,
          ticks = "outside",
          tickcolor = "black"
        ),
        yaxis = list(
          title = left_label,
          showline = TRUE,
          linecolor = "black",
          linewidth = 1,
          showticklabels = TRUE,
          ticks = "outside",
          tickcolor = "black"
        )
      )
    
    # Add the left Y-axis trace
    p <- p %>% add_trace(
      x = sensor_data$time_s,
      y = sensor_data[[left_var]],
      name = left_label,
      type = "scatter",
      mode = "lines",
      line = list(color = left_color)
    )
    
    # If right y-axis is selected, add it
    if (input$right_y_var != "") {
      right_var <- input$right_y_var
      right_color <- colors[which(var_names == right_var)]
      right_label <- var_labels[which(var_names == right_var)]
      
      # Add second y-axis specification
      p <- p %>% layout(
        yaxis2 = list(
          title = right_label,
          overlaying = "y",
          side = "right",
          showline = TRUE,
          linecolor = "black",
          linewidth = 1,
          showticklabels = TRUE,
          ticks = "outside",
          tickcolor = "black"
        )
      )
      
      # Add the right Y-axis trace
      p <- p %>% add_trace(
        x = sensor_data$time_s,
        y = sensor_data[[right_var]],
        name = right_label,
        yaxis = "y2",
        type = "scatter",
        mode = "lines",
        line = list(color = right_color)
      )
    }
    
    # If show nadir is selected and we have the information, add the nadir point
    if (input$show_nadir && !is.null(nadir_info())) {
      nadir <- nadir_info()
      
      # Determine which axis to use for the nadir point (pressure should be displayed on one of the axes)
      nadir_yaxis <- NULL
      if (left_var == "pressure_kpa") {
        nadir_yaxis <- "y"
      } else if (input$right_y_var == "pressure_kpa") {
        nadir_yaxis <- "y2"
      }
      
      # Only add the nadir if we're displaying pressure on either axis
      if (!is.null(nadir_yaxis)) {
        p <- p %>% add_trace(
          x = nadir$time,
          y = nadir$value,
          name = "Pressure Nadir",
          type = "scatter",
          mode = "markers+text",
          marker = list(color = "orange", size = 10),
          text = paste("Nadir:", round(nadir$value, 2), "kPa"),
          textposition = "top right",
          textfont = list(color = "orange"),
          yaxis = nadir_yaxis
        )
      }
    }
    
    return(p)
  })
  
  #  EXPAND CODE HERE
  
  

}