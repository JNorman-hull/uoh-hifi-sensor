# server.R - Better approach
server <- function(input, output, session) {
  
  raw_data_path <- reactive("C:/Users/joshn/OneDrive - hull.ac.uk/REDEEM 2.0/HS_JOSH_NORMAN/RAPID_processing/uoh-hifi-sensor/shiny/raw_sens_data")
  output_dir <- reactive("./processed_sens_data")
  
  session_state <- reactiveValues(
    selected_sensor = NULL,
    deployment_filter = "all",
    treatment_filter = "all", 
    run_filter = "all",
    quality_filter = "all",
    status_filter = "all",
    last_active_tab = NULL
  )
  
  global_sensor_state <- reactiveValues(
    data_updated = 0,           # For any sensor data changes
    summary_updated = 0,        # For summary/status changes
    processing_updated = 0,     # For processing completion
    magic_trigger_pressure = FALSE,
    magic_trigger_acceleration = FALSE,
    magic_processing_sensor = NULL,
    batch_processing = FALSE,
    magic_trigger_summary = FALSE
  )
  
  
  # Global trigger functions
  trigger_data_update <- function() {
    global_sensor_state$data_updated <- global_sensor_state$data_updated + 1
  }
  
  trigger_summary_update <- function() {
    global_sensor_state$summary_updated <- global_sensor_state$summary_updated + 1
  }
  
  trigger_processing_update <- function() {
    global_sensor_state$processing_updated <- global_sensor_state$processing_updated + 1
  }
  
  # Create a default processing_complete for initial load
  default_processing_complete <- reactive(FALSE)
  
  raw_processing <- rawdataprocessingServer("raw_data", raw_data_path, output_dir, default_processing_complete,
                                            global_sensor_state, trigger_data_update, trigger_summary_update, trigger_processing_update)
  
  # Initialize other modules using outputs from raw_processing
  processingresultsServer("processing_results", 
                          raw_processing$newly_processed_sensors, 
                          raw_processing$processing_complete)
  
  # Initialize other modules with real processing_complete
  plotsServer("plots", output_dir, raw_processing$summary_data, raw_processing$processing_complete)
  
  roiServer("roi", output_dir, raw_processing$summary_data, raw_processing$processing_complete, 
            session_state, global_sensor_state, trigger_data_update, trigger_summary_update)
  
  deploymentServer("deployment_info", raw_data_path, output_dir, raw_processing$processing_complete, 
                   session_state, global_sensor_state, trigger_data_update, trigger_summary_update)
  
  pressureServer("pressure", raw_data_path, output_dir, raw_processing$processing_complete, 
                 session_state, global_sensor_state, trigger_data_update, trigger_summary_update)
  
  accelerationServer("acceleration", raw_data_path, output_dir, raw_processing$processing_complete, 
                     session_state, global_sensor_state, trigger_data_update, trigger_summary_update)
  
  rotationServer("rotation", raw_data_path, output_dir, raw_processing$processing_complete, 
                 session_state, global_sensor_state, trigger_data_update, trigger_summary_update)
  
  
  observe({
    shinyjs::hide("sidebar_pressure")
    shinyjs::hide("sidebar_acceleration")
    shinyjs::hide("sidebar_rotation")
    
    if (input$InstrumentTabset == "pres_analysis") {
      shinyjs::show("sidebar_pressure")
    } else if (input$InstrumentTabset == "acc_analysis") {
      shinyjs::show("sidebar_acceleration")
    } else if (input$InstrumentTabset == "rot_analysis") {
      shinyjs::show("sidebar_rotation")
    }
    
    
    shinyjs::hide("sidebar_roi")
    shinyjs::hide("sidebar_plots")
    
    if (input$visualizationTabset == "roi_delineation") {
      shinyjs::show("sidebar_roi")
    } else if (input$visualizationTabset == "interactive_plots") {
      shinyjs::show("sidebar_plots")
    }
    
    shinyjs::hide("sidebar_raw")
    shinyjs::hide("sidebar_deployment")
    
    if (input$processingTabset == "process_raw_data") {
      shinyjs::show("sidebar_raw")
    } else if (input$processingTabset == "add_deployment_info") {
      shinyjs::show("sidebar_deployment")
    }
  })
  
}
