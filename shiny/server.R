server <- function(input, output, session) {
  
  # Raw data location and output directory
  raw_data_path <- reactive({
    "./RAW_data/RAPID"
  })
  
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
  
  # Initialize modules
  file_selection <- fileSelectionServer("file_selection", raw_data_path)
  processing <- processingServer("processing", file_selection$selected_sensors, raw_data_path, output_dir)
  resultsServer("results", processing$summary_data, processing$processing_complete)
  plotsServer("plots", output_dir, processing$summary_data, processing$processing_complete)
  roiServer("roi", output_dir, processing$summary_data, processing$processing_complete)
  
  # Process button click handler
  observeEvent(input$process_btn, {
    # Check if sensors are selected
    if (length(file_selection$selected_sensors()) == 0) {
      showNotification("No sensors selected. Please select at least one sensor to process.", 
                       type = "warning")
      return()
    }
    
    # Prevent multiple processing jobs
    if (processing$is_processing()) {
      showNotification("Processing already in progress. Please wait...", 
                       type = "warning")
      return()
    }
    
    # Call processing - let it handle everything
    processing$process_sensors()
  })
  
  # Handle UI changes when processing state changes
  observe({
    if (processing$is_processing()) {
      shinyjs::disable("process_btn")
      
      # Force UI to update by switching to log tab with delay
      shinyjs::delay(100, {
        updateTabsetPanel(session, "mainTabset", selected = "processing_log")
      })
    } else {
      shinyjs::enable("process_btn")
    }
  })
  
  # Handle completion
  observe({
    if (processing$processing_complete()) {
      updateTabsetPanel(session, "mainTabset", selected = "results_summary")
    }
  })
}