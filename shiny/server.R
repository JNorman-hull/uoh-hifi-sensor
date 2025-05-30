# server.R - Better approach
server <- function(input, output, session) {
  
  raw_data_path <- reactive("./RAW_data/RAPID")
  output_dir <- reactive("./RAPID_Processed")
  
  # Create a default processing_complete for initial load
  default_processing_complete <- reactive(FALSE)
  
  # Initialize file_selection first with default
  file_selection <- fileSelectionServer("file_selection", raw_data_path, output_dir, default_processing_complete)
  
  # Then processing with selected sensors
  processing <- processingServer("processing", file_selection$selected_sensors, raw_data_path, output_dir)
  
  # Initialize other modules with real processing_complete
  resultsServer("results", processing$newly_processed_sensors, processing$processing_complete)
  plotsServer("plots", output_dir, processing$summary_data, processing$processing_complete)
  roiServer("roi", output_dir, processing$summary_data, processing$processing_complete)
  deploymentServer("deployment_info", raw_data_path, output_dir, processing$processing_complete)
  pressureServer("pressure", raw_data_path, output_dir, processing$processing_complete)
  
  # Handle process button click here since it spans modules
  observeEvent(input$`file_selection-process_btn`, {
    processing$process_sensors()
  })
  
  output$processing_sidebar <- renderUI({
    switch(input$processingTabset,
           "process_raw_data" = fileSidebarUI("file_selection"),
           "add_deployment_info" = deploymentSidebarUI("deployment_info")
    )
  })
  
  # Sidebar for time series analysis tabs  
  output$time_sidebar <- renderUI({
    switch(input$visualizationTabset,
           "interactive_plots" = plotsSidebarUI("plots"),
           "roi_delineation" = roiSidebarUI("roi")
    )
  })
}