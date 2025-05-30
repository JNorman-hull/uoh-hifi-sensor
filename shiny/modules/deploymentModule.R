deploymentUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Introductory text at the top
    h3("Deployment Information Management"),
    helpText("This panel allows you to view and edit deployment information for processed sensors."),
    
    # Main large box spanning full width
    fluidRow(
      column(
        width = 12,
        div(
          style = "background-color: #f8f9fa; border: 1px solid #ccc; padding: 20px; 
                   border-radius: 5px; margin-bottom: 20px;",
          tags$h4("Deployment Overview", style = "margin-top: 0; color: #333;"),
          p("View and manage deployment metadata for your sensor data here."),
          hr(),
          p("Additional content will be displayed based on selected sensor.")
        )
      )
    ),
    
    # Two smaller boxes side by side
    fluidRow(
      column(
        width = 6,
        div(
          style = "background-color: #f8f9fa; border: 1px solid #ccc; padding: 20px; 
                   border-radius: 5px; margin-bottom: 20px; margin-right: 10px;",
          tags$h4("Box header", style = "margin-top: 0; color: #333;"),
          p("Left panel content area.")
        )
      ),
      column(
        width = 6,
        div(
          style = "background-color: #f8f9fa; border: 1px solid #ccc; padding: 20px; 
                   border-radius: 5px; margin-bottom: 20px;",
          tags$h4("Box header", style = "margin-top: 0; color: #333;"),
          p("Right panel content area.")
        )
      )
    )
  )
}

deploymentSidebarUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Deployment controls"),
    
    #Method 1: helpText() - styled for instructions
    helpText("This sidebar controls the deployment configuration for sensor data."),
    
    # Method 2: p() - regular paragraph
    p("Configure deployment parameters below:"),
    
    # Method 3: Custom styled text
    div(style = "color: #666; font-style: italic; margin-bottom: 15px;",
        "Select a sensor to begin deployment configuration."),
    
    # Delineation status display
    div(style = "margin-bottom: 15px;", 
        textOutput(ns("delineation_status"))),
    
    selectInput(ns("sensor_dropdown"), NULL, choices = NULL, width = "100%"),
    
    # Text input
    div(
      tags$label("LABEL:", style = "font-weight: bold; margin-bottom: 5px; display: block;"),
      textInput(ns("input_text"), NULL, value = "INPUT TEXT HERE", width = "100%")
    ),
    
    # Checkboxes
    div(style = "margin: 15px 0;",
        checkboxInput(ns("checkbox1"), "Checkbox", value = FALSE),
        checkboxInput(ns("checkbox2"), "Checkbox", value = FALSE)
    ),
    
    hr(),
    
    actionButton(ns("add_deploy_btn"), "Add deployment Information", 
                 class = "btn-primary btn-block")
  )
}




deploymentServer <- function(id, raw_data_path, output_dir, processing_complete) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ============================= #
    # /// Reactive values \\\ ####  
    # ============================= #   
    
    # Deployment state
    deployment_values <- reactiveValues(
      data_updated = 0            # Counter to trigger data refresh
    )
    
    # ============================= #
    # /// Data loading & processing  \\\ ####  
    # ============================= # 
    
    # Get processed sensors using shared function
    processed_sensors <- reactive({
      processing_complete()
      get_processed_sensors(output_dir())
    })
    
    # ============================= #
    # /// UI State management \\\ ####  
    # ============================= # 
    
    # Update sensor dropdown using shared function
    observe({
      update_sensor_dropdown(session, "sensor_dropdown", processed_sensors(), input$sensor_dropdown)
    })
    
    # ============================= #
    # /// Event handlers \\\ ####  
    # ============================= # 
    
    # Handle deployment info addition
    observeEvent(input$add_deploy_btn, {
      if (!is.null(input$sensor_dropdown) && input$sensor_dropdown != "") {
        showNotification(paste("Adding deployment info for:", input$sensor_dropdown), type = "message")
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
    
    # Delineation status display using shared function
    delineation_status <- create_individual_status_display(
      "delineation_status", 
      reactive(input$sensor_dropdown), 
      reactive(output_dir()),
      output, session, "delineation",
      invalidation_trigger = reactive(deployment_values$data_updated)
    )
    
    # Return any reactive values other modules might need
    return(list(
      selected_sensor = reactive(input$sensor_dropdown)
    ))
    
  })  # End of moduleServer
}     # End of deploymentServer
