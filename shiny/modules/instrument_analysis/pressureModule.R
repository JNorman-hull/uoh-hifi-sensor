pressureUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Introductory text at the top
    tagList(
      h3("Pressure Analysis"),
      plotModuleUI(ns("pressure_plot"), height = "600px"),
      br(),
    
    # Two smaller boxes side by side
    fluidRow(
      column(
        width = 6,
        div(
          style = "background-color: #f8f9fa; border: 1px solid #ccc; padding: 20px; 
                   border-radius: 5px; margin-bottom: 20px; margin-right: 10px;",
          tags$h4("RPC and LRPC Calculator", style = "margin-top: 0; color: #333;"),
          p("Calculate rate and ratio pressure change here. Add the pressure config file, check what value the surface and depth is set to. Have a slidebar to set the depth if known.
            Have two buttons - one for calculate rate pressure change (find max pressure 1s < nadir time, measure change). The output which is saved is printed under the button  'Max pressure (nadir - 1s) = max_pres_1s' It's only shown if the value does not = NA. Rate pressure change = x Kpa/s-1, again only shows value if not NA.
            One for calcxulate Log ratio pressure change (log of acclimation prersssure/nadir pressure). This is caculated using two acclimation pressures - one for surface, one for depth.
            Start over button which gives modal message for replacing previous pressure analysis if any of the check values = Y. When pressed it resets the pres sttus variables and sets any calculations back to NA")
        )
      ),
      column(
        width = 6,
        div(
          style = "background-color: #f8f9fa; border: 1px solid #ccc; padding: 20px; 
                   border-radius: 5px; margin-bottom: 20px;",
          tags$h4("Characterize pressure profile", style = "margin-top: 0; color: #333;"),
          p("Calculate pressure summary data button. Follow similiar logic developed in roi module. Summarize by roi and produce a table of summary data.
            Build helper functions for perfomring the filtering and summarising tasks")
        )
      )
    ),
    
    fluidRow(
      column(
        width = 6,
        div(
          style = "background-color: #f8f9fa; border: 1px solid #ccc; padding: 20px; 
                   border-radius: 5px; margin-bottom: 20px; margin-right: 10px;",
          tags$h4("Sensor pressure summary", style = "margin-top: 0; color: #333;"),
          p("Provide all the summary information for the currently selected sensor here. RPC, LRPC (surface, depth), pressure nadir, .")
        )
      ),
      column(
        width = 6,
        div(
          style = "background-color: #f8f9fa; border: 1px solid #ccc; padding: 20px; 
                   border-radius: 5px; margin-bottom: 20px;",
          tags$h4("Barotrauama assesment", style = "margin-top: 0; color: #333;"),
          p("Hav a species index table (baro.csv) which has (species, age, size, sample size, mortality threshold). Asses our paramters against all thresholds and produce a summary report which is printed in the box, as well as saved  in ./assesment.
          Only active when all calculations have bene done.
          Uses same logic as elsewhere for managing when data is overwritten etc.")
        )
      )
    )
  ))
}

pressureSidebarUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Pressure controls"),
    
    #Method 1: helpText() - styled for instructions
    #helpText("This sidebar controls the pressure configuration for sensor data."),
    
    # Method 2: p() - regular paragraph
    #p("Configure pressure parameters below:"),
    
    # Method 3: Custom styled text
    div(style = "color: #666; font-style: italic; margin-bottom: 15px;",
        "Select a sensor to begin pressure analysis."),
    
    # Delineation status display
    div(style = "margin-bottom: 15px;", 
        textOutput(ns("pressure_status"))),
    
    enhancedSensorSelectionUI(ns("sensor_selector"), status_filter_type = "pres_processed"),

    hr(), h4("Plot controls"),
    plotSidebarUI(ns("pressure_plot"), 
                  show_left_var = FALSE,   
                  show_right_var = FALSE,    
                  show_normalized = TRUE,   
                  show_nadir = TRUE,      
                  show_roi_markers = TRUE,   
                  show_legend = TRUE,
                  default_show_normalized = FALSE,
                  default_show_nadir = FALSE,
                  default_show_roi_markers = FALSE,
                  default_show_legend = FALSE),    
    
    hr(),
    
    actionButton(ns("add_deploy_btn"), "Add pressure Information", 
                 class = "btn-primary btn-block")
  )
}

pressureServer <- function(id, raw_data_path, output_dir, processing_complete) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ============================= #
    # /// Reactive values \\\ ####  
    # ============================= #   

# pressure state ####
    pressure_values <- reactiveValues(
      data_updated = 0            # Counter to trigger data refresh
    )
    
# Get roi boundaries ####
    roi_boundaries <- reactive({
      get_roi_boundaries(sensor_selector$selected_sensor(), output_dir(), TRUE)
    })
    
    # ============================= #
    # /// Data loading & processing  \\\ ####  
    # ============================= # 
    
    sensor_selector <- enhancedSensorSelectionServer("sensor_selector", output_dir, processing_complete, status_filter_type = "pres_processed")
    
    # Read selected sensor data
    selected_sensor_data <- reactive({
      req(sensor_selector$selected_sensor())
      pressure_values$data_updated  

      
      # Check for delineated file first, fall back to minimal data
      delineated_data <- read_sensor_data(output_dir(), sensor_selector$selected_sensor(), "delineated")
      if (!is.null(delineated_data)) {
        return(delineated_data)
      }
      
      return(read_sensor_data(output_dir(), sensor_selector$selected_sensor(), "min"))
    })
    
    # Get nadir info using shared function
    nadir_info <- reactive({
      req(sensor_selector$selected_sensor())
      pressure_values$data_updated 
      get_nadir_info(sensor_selector$selected_sensor(), output_dir())
    })
    
    # ============================= #
    # /// UI State management \\\ ####  
    # ============================= # 
    

# Enable/disable ROI checkbox ####
    observe({
      req(sensor_selector$selected_sensor())
      status <- get_sensor_status(sensor_selector$selected_sensor(), output_dir())
      
      if (status$delineated && status$trimmed) {
        shinyjs::enable("show_roi_markers")
      } else {
        shinyjs::disable("show_roi_markers")
        updateCheckboxInput(session, "show_roi_markers", value = FALSE)
      }
    })
    
    # ============================= #
    # /// Event handlers \\\ ####  
    # ============================= # 
    
    # Handle pressure info addition
    observeEvent(input$add_deploy_btn, {
      if (!is.null(sensor_selector$selected_sensor()) && sensor_selector$selected_sensor() != "") {
        showNotification(paste("Adding pressure info for:", sensor_selector$selected_sensor()), type = "message")
      } else {
        showNotification("Please select a sensor first", type = "warning")
      }
    })
    
    # ============================= #
    # /// Helper functions \\\ ####  
    # ============================= # 
    
    # Add helper functions here as needed
    
    # ============================= #
    # /// Output render \\\ ####  
    # ============================= #    
    
    # pressure status display using shared function
    pressure_status <- create_individual_status_display(
      "pressure_status", 
      reactive(sensor_selector$selected_sensor()), 
      reactive(output_dir()),
      output, session, "pres_processed",
      invalidation_trigger = reactive(pressure_values$data_updated)
    )
    

    #Don't need custom reactive to change default behaviors, as defaults are desired here
    plot_controls <- plotModuleServer("pressure_plot", 
                                      sensor_data = selected_sensor_data,
                                      sensor_name = reactive(sensor_selector$selected_sensor()),
                                      nadir_info = nadir_info,
                                      roi_boundaries = roi_boundaries,
                                      #don't need to provide left and right var as default = pressure
                                      show_nadir = reactive(input$`pressure_plot-show_nadir`),
                                      show_legend = reactive(input$`pressure_plot-show_legend`),
                                      show_normalized = reactive(input$`pressure_plot-show_normalized`),
                                      show_roi_markers = reactive(input$`pressure_plot-show_roi_markers`),
                                      title_prefix = "Pressure Analysis",
                                      plot_source = "pressure_plot"
    )
    return(list(
      selected_sensor = reactive(sensor_selector$selected_sensor())
    ))
    
  })
}  
  