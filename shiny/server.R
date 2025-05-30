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
  file_selection <- fileSelectionServer("file_selection", raw_data_path, output_dir, processing$processing_complete)
  processing <- processingServer("processing", file_selection$selected_sensors, raw_data_path, output_dir)
  resultsServer("results", processing$newly_processed_sensors, processing$processing_complete)
  plotsServer("plots", output_dir, processing$summary_data, processing$processing_complete)
  roiServer("roi", output_dir, processing$summary_data, processing$processing_complete)
  
  observe({
    # Get selected sensors count
    selected_count <- length(file_selection$selected_sensors())
    
    # Disable button if processing OR no sensors selected
    if (processing$is_processing() || selected_count == 0) {
      shinyjs::disable("process_btn")
    } else {
      shinyjs::enable("process_btn")
    }
  })
  
  # Process button click handler
  observeEvent(input$process_btn, {
    processing$process_sensors()
  })
  
  output$dynamic_sidebar <- renderUI({
    # Debug: print the current tab values
    cat("Main tab:", input$mainTabset, "\n")
    cat("Processing tab:", input$processingTabset, "\n")
    cat("Visualization tab:", input$visualizationTabset, "\n")
    
    switch(input$mainTabset,
           
           "time_series_analysis" = {
             switch(input$visualizationTabset,
                    "interactive_plots" = plotsSidebarUI("plots"),
                    "roi_delineation" = roiSidebarUI("roi")
             )
           },
           
           "sensor_processing" = {
             switch(input$processingTabset,
                    "process_raw_data" = fileSidebarUI("file_selection"),
                    "add_deployment_info" = deploymentSidebarUI("deployment_info")
             )
           }
    )
  })
  
}