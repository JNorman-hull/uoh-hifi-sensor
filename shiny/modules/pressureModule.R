pressureUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Introductory text at the top
    tagList(
      h3("Pressure Analysis"),
      plotlyOutput(ns("pressure_plot"), height = "600px"),
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
    helpText("This sidebar controls the pressure configuration for sensor data."),
    
    # Method 2: p() - regular paragraph
    p("Configure pressure parameters below:"),
    
    # Method 3: Custom styled text
    div(style = "color: #666; font-style: italic; margin-bottom: 15px;",
        "Select a sensor to begin pressure configuration."),
    
    # Delineation status display
    div(style = "margin-bottom: 15px;", 
        textOutput(ns("pressure_status"))),
    
    selectInput(ns("sensor_dropdown"), NULL, choices = NULL, width = "100%"),
    
    # Text input
    div(
      tags$label("LABEL:", style = "font-weight: bold; margin-bottom: 5px; display: block;"),
      textInput(ns("input_text"), NULL, value = "INPUT TEXT HERE", width = "100%")
    ),
    
    # Checkboxes
    div(style = "margin: 15px 0;",
        checkboxInput(ns("checkbox1"), "Checkbox", value = FALSE),
        checkboxInput(ns("checkbox2"), "Checkbox", value = FALSE),
        checkboxInput(ns("show_roi_markers"), "Show ROI markers", value = FALSE)
    ),
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
    get_roi_boundaries(input$sensor_dropdown, output_dir(), input$show_roi_markers)
  }) 
    
    # ============================= #
    # /// Data loading & processing  \\\ ####  
    # ============================= # 
    
    # Get processed sensors using shared function
    processed_sensors <- reactive({
      processing_complete()
      get_processed_sensors(output_dir())
    })
    
    # Read selected sensor data
    selected_sensor_data <- reactive({
      req(input$sensor_dropdown)
      pressure_values$data_updated  

      
      # Check for delineated file first, fall back to minimal data
      delineated_data <- read_sensor_data(output_dir(), input$sensor_dropdown, "delineated")
      if (!is.null(delineated_data)) {
        return(delineated_data)
      }
      
      return(read_sensor_data(output_dir(), input$sensor_dropdown, "min"))
    })
    
    # Get nadir info using shared function
    nadir_info <- reactive({
      req(input$sensor_dropdown)
      pressure_values$data_updated 
      get_nadir_info(input$sensor_dropdown, output_dir())
    })
    
    # ============================= #
    # /// UI State management \\\ ####  
    # ============================= # 
    
# Update sensor dropdown ####
    observe({
      update_sensor_dropdown(session, "sensor_dropdown", processed_sensors(), input$sensor_dropdown)
    })
# Enable/disable ROI checkbox ####
    observe({
      req(input$sensor_dropdown)
      status <- get_sensor_status(input$sensor_dropdown, output_dir())
      
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
      if (!is.null(input$sensor_dropdown) && input$sensor_dropdown != "") {
        showNotification(paste("Adding pressure info for:", input$sensor_dropdown), type = "message")
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
      reactive(input$sensor_dropdown), 
      reactive(output_dir()),
      output, session, "pres_processed",
      invalidation_trigger = reactive(pressure_values$data_updated)
    )
    
    output$pressure_plot <- renderPlotly({
      sensor_data <- selected_sensor_data()
      req(sensor_data)
      
      nadir <- nadir_info()
      
      if (!"pressure_kpa" %in% names(sensor_data)) {
        showNotification("Pressure data not available for this sensor", type = "warning")
        return(NULL)
      }
      
      tryCatch({
        p <- create_sensor_plot(
          sensor_data = sensor_data,
          sensor_name = input$sensor_dropdown,
          plot_config = "pressure_only",
          nadir_info = nadir,
          show_nadir = TRUE,
          show_legend = FALSE,
          plot_source = "pressure_plot",
          roi_boundaries = roi_boundaries()
        )
        return(p)
      }, error = function(e) {
        cat("Plot error:", e$message, "\n")
        showNotification(paste("Plot error:", e$message), type = "error")
        return(NULL)
      })
    })
    
    return(list(
      selected_sensor = reactive(input$sensor_dropdown)
    ))
    
  })
}  
  