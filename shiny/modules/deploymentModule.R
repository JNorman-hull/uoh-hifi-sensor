deploymentUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Replace the main box with the selection table
    fileSelectionTableUI(
      ns("deployment_table"),
      title = "Processed sensor index",
      help_text = "All processed sensors are shown here. Select sensors to add or edit deployment information. Sensors with deployment information are coloured green. Sensors without are coloured orange.",
      show_title = TRUE
    ),
    
    # Keep the two smaller boxes below
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
          
          # Boxes will be replaced by 
          
        )
      )
    )
  )
}

deploymentSidebarUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Deployment Information Controls"),
    
    #helpText("This sidebar controls the deployment configuration for sensor data."),
    
    #p("Configure deployment parameters below:"),
    
    div(style = "color: #666; font-style: italic; margin-bottom: 15px;",
        "Select sensor(s) to view and manage deployment configuration."),
    
    # Delineation status display
    div(style = "margin-bottom: 15px;", 
        textOutput(ns("delineation_status"))),
    # This no longer shows, as removed selected sensor dropdown. 
    # Will update this to reflect sensors in file selection
    # "Sensor requires deployment information", "One or more sensor requires deployment information", "All selected sensors have deployment information"
    
    hr(),

    selectInput(ns("deployment_config"), "Deployment Configuration:", 
                choices = NULL, width = "100%"),
    
    hr(),
    
    fileSelectionControlsUI(
      ns("deployment_table"),
      show_select_all = TRUE,
      show_clear_all = TRUE,
      show_summary = TRUE
    ),
    
    br(),
    
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
      data_updated = 0,            # Counter to trigger data refresh
      deployment_configs = NULL,   # All available deployment configurations  
      current_config = NULL        # Currently selected configuration
    )
    
    # ============================= #
    # /// Data loading & processing  \\\ ####  
    # ============================= # 
    
    # Get processed sensors using shared function
    processed_sensors <- reactive({
      processing_complete()
      get_processed_sensors(output_dir())
    })
    
    # Prepare table data for processed sensors only
    processed_sensor_data <- reactive({
      processing_complete()  # Trigger when processing completes
      deployment_values$data_updated  # Trigger on updates
      
      sensors <- processed_sensors()
      
      # Return NULL if no processed sensors
      if (length(sensors) == 0) {
        return(NULL)
      }
      
      # Get sensor info for each processed sensor
      sensor_info <- map(sensors, function(name) {
        tryCatch({
          py$parse_filename_info(name)
        }, error = function(e) {
          list(
            sensor = if(nchar(name) >= 3) substr(name, 1, 3) else name,
            date_deploy = "Unknown",
            time_deploy = "Unknown"
          )
        })
      })
      
      # Create the data frame
      tibble(
        No. = seq_along(sensors),
        Filename = sensors,
        Sensor = map_chr(sensor_info, "sensor"),
        Date = map_chr(sensor_info, "date_deploy"),
        Time = map_chr(sensor_info, "time_deploy"),
        
      )
    })
    
    

    # ============================= #
    # /// UI State management \\\ ####  
    # ============================= # 
    
# Custom table format ####
#formatting function to handle empty state
    custom_table_formatting <- function(dt, table_data) {
      if (is.null(table_data) || nrow(table_data) == 0) {
        # Return a datatable with the message
        return(DT::datatable(
          data.frame(Message = "No sensors processed. Process sensors first to add deployment information"),
          options = list(dom = 't', ordering = FALSE),
          rownames = FALSE,
          selection = 'none'
        ))
      }
      return(dt)
    }
    
    
    
    # ============================= #
    # /// Event handlers \\\ ####  
    # ============================= # 
    
    # Handle deployment info addition
    observeEvent(input$add_deploy_btn, {
      if (!is.null(input$selected_sensor) && input$selected_sensor != "") {
        showNotification(paste("Adding deployment info for:", input$selected_sensor), type = "message")
        # Add actual deployment logic here
      } else {
        showNotification("Please select a sensor first", type = "warning")
      }
    })
    
    # Load deployment configurations and update dropdown
    observe({
      deployment_values$deployment_configs <- load_config_file(output_dir(), "deployment")
      
      if (length(deployment_values$deployment_configs) > 0) {
        config_names <- names(deployment_values$deployment_configs)
        choices <- setNames(config_names, gsub("_", " ", config_names))
        selected_value <- config_names[1]
        
        updateSelectInput(session, "deployment_config", 
                          choices = choices, 
                          selected = selected_value)
      }
    })
    
    # Update current config when dropdown selection changes
    observe({
      req(input$deployment_config, deployment_values$deployment_configs)
      deployment_values$current_config <- deployment_values$deployment_configs[[input$deployment_config]]
    })
    
    # ============================= #
    # /// Output render \\\ ####  
    # ============================= #    
    
    # Delineation status display using shared function
    delineation_status <- create_individual_status_display(
      "delineation_status", 
      reactive(input$selected_sensor), 
      reactive(output_dir()),
      output, session, "delineation",
      invalidation_trigger = reactive(deployment_values$data_updated)
    )
    
    # Use the shared table module
    table_results <- fileSelectionTableServer(
      "deployment_table",
      sensor_data_reactive = reactive({
        data <- processed_sensor_data()
        if (is.null(data)) {
          # Return empty data frame with message
          data.frame(Message = "No sensors processed. Process sensors first to add deployment information")
        } else {
          data
        }
      }),
      highlight_sensors_reactive = reactive(NULL),  # No highlighting needed
      enable_selection = TRUE,
      selection_mode = 'single',  # Single selection for deployment
      custom_formatting = custom_table_formatting
    )
    
    
    # Return any reactive values other modules might need
    return(list(
      selected_sensor = reactive(input$selected_sensor),
      selected_sensors_from_table = table_results$selected_items
    ))
    
  })  # End of moduleServer
}     # End of deploymentServer