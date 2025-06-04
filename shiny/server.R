# server.R - Better approach
server <- function(input, output, session) {
  
  raw_data_path <- reactive("./raw_sens_data")
  output_dir <- reactive("./processed_sens_data")
  
  # Create a default processing_complete for initial load
  default_processing_complete <- reactive(FALSE)
  
  # Initialize file_selection first with default
  file_selection <- rawdataprocessingServer("raw_data", raw_data_path, output_dir, default_processing_complete)
  
  # Then processing with selected sensors
  processing <- processinghelperServer("processing_helper", file_selection$selected_sensors, raw_data_path, output_dir)
  
  # Initialize other modules with real processing_complete
  processingresultsServer("processing_results", processing$newly_processed_sensors, processing$processing_complete)
  plotsServer("plots", output_dir, processing$summary_data, processing$processing_complete)
  roiServer("roi", output_dir, processing$summary_data, processing$processing_complete)
  deploymentServer("deployment_info", raw_data_path, output_dir, processing$processing_complete)
  pressureServer("pressure", raw_data_path, output_dir, processing$processing_complete)
  
  # Handle process button click here since it spans modules
  observeEvent(input$`file_selection-process_btn`, {
    processing$process_sensors()
  })
  
}
