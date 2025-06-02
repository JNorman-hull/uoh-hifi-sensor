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
          tags$h4("Configure deployment information", style = "margin-top: 0; color: #333;"),
          selectInput(ns("deployment_config"), "Configuration:", choices = NULL, width = "100%"),
          
          hr(),
          
          div(style = "display: flex; align-items: center; justify-content: start; margin-bottom: 15px;",
              tags$label("Configuration Label:", `for` = ns("deployment_config_label"), 
                         style = "margin-right: 8px;"),
              textInput(ns("deployment_config_label"), NULL, value = "", 
                        width = "200px", placeholder = "e.g., PumpWizard_configuration")
          ),
          
          div(style = "display: flex; align-items: center; justify-content: start; margin-bottom: 15px;",
              tags$label("Site:", `for` = ns("site_config_label"), 
                         style = "margin-right: 8px;"),
              textInput(ns("site_config_label"), NULL, value = "", 
                        width = "200px", placeholder = "e.g., Johnson_Lane")
          ),
          
          div(style = "display: flex; align-items: center; justify-content: start; margin-bottom: 15px;",
              tags$label("Deployment ID:", `for` = ns("deployment_id_config_label"), 
                         style = "margin-right: 8px;"),
              textInput(ns("deployment_id_config_label"), NULL, value = "", 
                        width = "200px", placeholder = "e.g., JL_2025_FFP")
          ),
          
          div(style = "display: flex; align-items: center; justify-content: start; margin-bottom: 15px;",
              tags$label("Pump/turbine model:", `for` = ns("pump_turbine_config_label"), 
                         style = "margin-right: 8px;"),
              textInput(ns("pump_turbine_config_label"), NULL, value = "", 
                        width = "200px", placeholder = "e.g., Pentair_XRW")
          ),
          
          div(style = "display: flex; align-items: center; justify-content: start; margin-bottom: 15px;",
              tags$label("Pump/turbine type:", `for` = ns("type_config_label"), 
                         style = "margin-right: 8px;"),
              textInput(ns("type_config_label"), NULL, value = "", 
                        width = "200px", placeholder = "e.g., Axial_flow")
          ),
          
          div(style = "display: flex; align-items: center; justify-content: start; margin-bottom: 15px;",
              tags$label("Rotation Per Minute (RPM):", `for` = ns("rpm_config_label"), 
                         style = "margin-right: 8px;"),
              textInput(ns("rpm_config_label"), NULL, value = "", 
                        width = "200px", placeholder = "e.g., 500")
          ),
          
          div(style = "display: flex; align-items: center; justify-content: start; margin-bottom: 15px;",
              tags$label("Head:", `for` = ns("head_config_label"), 
                         style = "margin-right: 8px;"),
              textInput(ns("head_config_label"), NULL, value = "", 
                        width = "200px", placeholder = "e.g., 3")
          ),
          
          div(style = "display: flex; align-items: center; justify-content: start; margin-bottom: 15px;",
              tags$label("Flow:", `for` = ns("flow_config_label"), 
                         style = "margin-right: 8px;"),
              textInput(ns("flow_config_label"), NULL, value = "", 
                        width = "200px", placeholder = "e.g., 1.32")
          ),
          
          div(style = "display: flex; align-items: center; justify-content: start; margin-bottom: 15px;",
              tags$label("Point of Best Efficiency Point (BEP):", `for` = ns("point_bep_config_label"), 
                         style = "margin-right: 8px;"),
              textInput(ns("point_bep_config_label"), NULL, value = "", 
                        width = "200px", placeholder = "e.g., 100")
          ),
          
          div(style = "display: flex; align-items: center; justify-content: start; margin-bottom: 15px;",
              tags$label("Treatment:", `for` = ns("treatment_config_label"), 
                         style = "margin-right: 8px;"),
              textInput(ns("treatment_config_label"), NULL, value = "", 
                        width = "200px", placeholder = "e.g., Scenario_1")
          ),
          
          div(style = "display: flex; align-items: center; justify-content: start; margin-bottom: 15px;",
              tags$label("Run:", `for` = ns("run_config_label"), 
                         style = "margin-right: 8px;"),
              textInput(ns("run_config_label"), NULL, value = "", 
                        width = "200px", placeholder = "e.g., 1")
          )
        )
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