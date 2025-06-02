deploymentUI <- function(id) {
  ns <- NS(id)
  
  # Define all input fields in a list
  input_fields <- list(
    list(id = "deployment_config_label", label = "Configuration Label:", placeholder = "e.g., PumpWizard_configuration"),
    list(id = "site_config_label", label = "Site:", placeholder = "e.g., Johnson_Lane"),
    list(id = "deployment_id_config_label", label = "Deployment ID:", placeholder = "e.g., JL_2025_FFP"),
    list(id = "pump_turbine_config_label", label = "Pump/turbine model:", placeholder = "e.g., Pentair_XRW"),
    list(id = "type_config_label", label = "Pump/turbine type:", placeholder = "e.g., Axial_flow"),
    list(id = "rpm_config_label", label = "Rotation Per Minute (RPM):", placeholder = "e.g., 500"),
    list(id = "head_config_label", label = "Head:", placeholder = "e.g., 3"),
    list(id = "flow_config_label", label = "Flow:", placeholder = "e.g., 1.32"),
    list(id = "point_bep_config_label", label = "Point of Best Efficiency Point (BEP):", placeholder = "e.g., 100"),
    list(id = "treatment_config_label", label = "Treatment:", placeholder = "e.g., Scenario_1"),
    list(id = "run_config_label", label = "Run:", placeholder = "e.g., 1")
  )
  
  tagList(
    fileSelectionTableUI(
      ns("deployment_table"),
      title = "Processed sensor index",
      help_text = "All processed sensors are shown here. Select sensors to add or edit deployment information. Sensors with deployment information are coloured green. Sensors without are coloured orange.",
      show_title = TRUE
    ),
    
    fluidRow(
      column(
        width = 6,
        div(
          style = "background-color: #f8f9fa; border: 1px solid #ccc; padding: 20px; 
                   border-radius: 5px; margin-bottom: 20px; margin-right: 10px;",
          tags$h4("Configure deployment information", style = "margin-top: 0; color: #333;"),
          selectInput(ns("deployment_config"), "Configuration:", choices = NULL, width = "100%"),
          hr(),
          
          # Generate inputs from the list
          lapply(input_fields, function(field) {
            div(
              style = "display: flex; align-items: center; justify-content: start; margin-bottom: 15px;",
              tags$label(field$label, `for` = ns(field$id), style = "margin-right: 8px;"),
              textInput(ns(field$id), NULL, value = "", width = "200px", placeholder = field$placeholder)
            )
          })
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
    h4("Deployment Information Controls"),
    
    #helpText("This sidebar controls the deployment configuration for sensor data."),
    
    #p("Configure deployment parameters below:"),
    
    div(style = "color: #666; font-style: italic; margin-bottom: 15px;",
        "Select sensor(s) to view and manage deployment configuration."),
    
    # Delineation status display
    div(style = "margin-bottom: 15px;", 
        textOutput(ns("deployment_info"))),
    # This no longer shows, as removed selected sensor dropdown. 
    # Will update this to reflect sensors in file selection
    # "Sensor requires deployment information", "One or more sensor requires deployment information", "All selected sensors have deployment information"

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
    
# Deployment state ####
    deployment_values <- reactiveValues(
      data_updated = 0,            # Counter to trigger data refresh
      deployment_configs = NULL,   # All available deployment configurations  
      current_config = NULL        # Currently selected configuration
    )
    
    # ============================= #
    # /// Data loading & processing  \\\ ####  
    # ============================= # 
    
# Get processed sensors ####
    processed_sensors <- reactive({
      processing_complete()
      get_processed_sensors(output_dir())
    })
    
# Prepare table data ####
    processed_sensor_data <- reactive({
      processing_complete()  # Trigger when processing completes
      deployment_values$data_updated  # Trigger on updates
      
      # Read directly from sensor index
      index_df <- get_sensor_index_file(output_dir(), read_data = TRUE)
      
      # Return NULL if no index file
      if (is.null(index_df)) {
        return(NULL)
      }
      
      # Select desired columns and add row numbers
      selected_columns <- c("file", "sensor", "date_deploy", "bad_sens", "site", 
                            "deployment_id", "pump_turbine", "type", "rpm", "head", 
                            "flow", "point_bep", "treatment", "run")

      # Create the data frame with row numbers
      result_df <- index_df[, selected_columns, drop = FALSE]
      result_df <- cbind(`No.` = seq_len(nrow(result_df)), result_df)
      
      return(result_df)
    })
    
    # ============================= #
    # /// UI State management \\\ ####  
    # ============================= # 
    
# Custom table format ####
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
    
# Load deployment configurations ####
    observe({
      deployment_values$deployment_configs <- load_config_file(output_dir(), "deployment")
      
      if (length(deployment_values$deployment_configs) > 0) {
        config_names <- names(deployment_values$deployment_configs)
        choices <- setNames(config_names, gsub("_", " ", config_names))
        
        # Use current selection if it exists and is valid, otherwise use first
        current_selection <- input$deployment_config
        selected_value <- if (!is.null(current_selection) && current_selection %in% config_names) {
          current_selection
        } else {
          config_names[1]
        }
        
        updateSelectInput(session, "deployment_config", 
                          choices = choices, 
                          selected = selected_value)
      }
    })
    
# Update deployment config  ####
    observe({
      req(input$deployment_config, deployment_values$deployment_configs)
      deployment_values$current_config <- deployment_values$deployment_configs[[input$deployment_config]]
    })    
    
    # ============================= #
    # /// Event handlers \\\ ####  
    # ============================= # 
    
#  Deployment info button ####
    observeEvent(input$add_deploy_btn, {
      if (!is.null(input$selected_sensor) && input$selected_sensor != "") {
        showNotification(paste("Adding deployment info for:", input$selected_sensor), type = "message")
        # Add actual deployment logic here
      } else {
        showNotification("Please select a sensor first", type = "warning")
      }
    })

    # ============================= #
    # /// Output render \\\ ####  
    # ============================= #    

# Deployment status display ####
    deployment_status <- create_individual_status_display(
      "deployment_info", 
      reactive(input$selected_sensor), 
      reactive(output_dir()),
      output, session, "deployment_info",
      invalidation_trigger = reactive(deployment_values$data_updated)
    )
    
# Table display ####
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
      selection_mode = 'multiple',  # Changed from 'single' to 'multiple'
      custom_formatting = custom_table_formatting
    )
    
# Return reactive####
#not sure if needed
    return(list(
      selected_sensor = reactive(input$selected_sensor),
      selected_sensors_from_table = table_results$selected_items
    ))
    
  })  # End of moduleServer
}     # End of deploymentServer