deploymentUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fileSelectionTableUI(
      ns("deployment_table"),
      title = "Processed sensor index",
      help_text = "All processed sensors are shown here. Select sensors to add or edit deployment information. Green = deployment information present. Orange = requires deployment information.",
      show_title = TRUE
    )
  )
}

deploymentSidebarUI <- function(id) {
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
  
  # Generate inputs from the list
  input_pairs <- split(input_fields, ceiling(seq_along(input_fields) / 2))
  
  tagList(
    h4("Deployment Information Controls"),
    
    div(style = "color: #666; font-style: italic; margin-bottom: 15px;",
        "Select sensor(s) to view and manage deployment configuration."),
    
    # Deployment status display
    div(style = "margin-bottom: 15px;", 
        textOutput(ns("deployment_status"))),
    
    textOutput(ns("config_change_status")),
    br(),
    textOutput(ns("deployment_summary")),
    
    hr(),
    
    selectInput(ns("deployment_config"), "Configuration:", choices = NULL, width = "100%"),
        hr(),
        
        # Create fluidRows with two columns each
        lapply(input_pairs, function(pair) {
          fluidRow(
            lapply(pair, function(field) {
              column(
                width = 6,
                div(
                  style = "margin-bottom: 15px;",
                  textInput(ns(field$id), label = field$label, value = "", 
                            width = "100%", placeholder = field$placeholder)
                )
              )
            })
          )
        }),
        
    br(),
    
    actionButton(ns("save_config_btn"), "Save Configuration", 
                     class = "btn-success btn-block"),
    
    fileSelectionControlsUI(
      ns("deployment_table"),
      show_select_all = TRUE,
      show_clear_all = TRUE,
      show_summary = TRUE
    ),
    
    br(),
    
    actionButton(ns("add_deploy_btn"), "Add Deployment Information", 
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
      current_config = NULL,       # Currently selected configuration
      inputs_changed = FALSE,      # Track if inputs differ from config
      baseline_config = NULL,       # Store original config for comparison
    )
    
    # ============================= #
    # /// Data loading & processing  \\\ ####  
    # ============================= # 
    
    # Get processed sensors
    processed_sensors <- reactive({
      processing_complete()
      get_processed_sensors(output_dir())
    })
    
    # Prepare table data with proper column structure
    processed_sensor_data <- reactive({
      processing_complete()  # Trigger when processing completes
      deployment_values$data_updated  # Trigger on updates
      
      # Read directly from sensor index
      index_df <- get_sensor_index_file(output_dir(), read_data = TRUE)
      
      # Return NULL if no index file
      if (is.null(index_df)) {
        return(NULL)
      }
      
      # Select columns to match expected structure for file selection table
      result_df <- index_df %>%
        select(file, sensor, date_deploy, bad_sens, deployment_info) %>%
        mutate(
          No. = row_number(),
          Filename = file,
          Sensor = sensor,
          Date = date_deploy,
          Status = ifelse(deployment_info == "Y", "Complete", "Required")
        ) %>%
        select(No., Filename, Sensor, Date, Status)
      
      return(result_df)
    })
    
    # Get sensors with deployment info for highlighting
    sensors_with_deployment <- reactive({
      deployment_values$data_updated  # Invalidate when data changes
      
      index_df <- get_sensor_index_file(output_dir(), read_data = TRUE)
      if (is.null(index_df)) return(character(0))
      
      tryCatch({
        # Return sensors where deployment_info == "Y"
        return(index_df[index_df$deployment_info == "Y", "file", drop = TRUE])
      }, error = function(e) {
        return(character(0))
      })
    })
    
    # Get sensors without deployment info for highlighting  
    sensors_without_deployment <- reactive({
      deployment_values$data_updated  # Invalidate when data changes
      
      index_df <- get_sensor_index_file(output_dir(), read_data = TRUE)
      if (is.null(index_df)) return(character(0))
      
      tryCatch({
        # Return sensors where deployment_info != "Y"
        return(index_df[index_df$deployment_info != "Y", "file", drop = TRUE])
      }, error = function(e) {
        return(character(0))
      })
    })
    
    # ============================= #
    # /// UI State management \\\ ####  
    # ============================= # 
    
# Load deployment configurations and update dropdown ####
    observe({
      deployment_values$deployment_configs <- load_config_file(output_dir(), "deployment")
      
      choices <- c("Blank" = "Blank")
      
      if (length(deployment_values$deployment_configs) > 0) {
        config_names <- names(deployment_values$deployment_configs)
        config_choices <- setNames(config_names, gsub("_", " ", config_names))
        choices <- c(choices, config_choices)
      }
      
      updateSelectInput(session, "deployment_config", choices = choices)
    })
    
# Update current config when dropdown selection changes ####
    observe({
      req(input$deployment_config)  # Only require the input, not the configs
      
      if (input$deployment_config == "Blank") {
        deployment_values$current_config <- NULL
        deployment_values$baseline_config <- NULL
      } else {
        req(deployment_values$deployment_configs)
        deployment_values$current_config <- deployment_values$deployment_configs[[input$deployment_config]]
        deployment_values$baseline_config <- deployment_values$current_config
      }
      
      deployment_values$inputs_changed <- FALSE
    })
    
    # Populate input fields from selected configuration
    observe({
      if (input$deployment_config == "Blank" || is.null(deployment_values$current_config)) {
        # Populate with empty values for Blank option
        isolate({
          updateTextInput(session, "deployment_config_label", value = "")
          updateTextInput(session, "site_config_label", value = "")
          updateTextInput(session, "deployment_id_config_label", value = "")
          updateTextInput(session, "pump_turbine_config_label", value = "")
          updateTextInput(session, "type_config_label", value = "")
          updateTextInput(session, "rpm_config_label", value = "")
          updateTextInput(session, "head_config_label", value = "")
          updateTextInput(session, "flow_config_label", value = "")
          updateTextInput(session, "point_bep_config_label", value = "")
          updateTextInput(session, "treatment_config_label", value = "")
          updateTextInput(session, "run_config_label", value = "")
        })
      } else {
        # Populate with actual configuration values
        req(deployment_values$current_config)
        config <- deployment_values$current_config
        
        isolate({
          updateTextInput(session, "deployment_config_label", value = input$deployment_config)
          updateTextInput(session, "site_config_label", value = config$site)
          updateTextInput(session, "deployment_id_config_label", value = config$deployment_id)
          updateTextInput(session, "pump_turbine_config_label", value = config$pump_turbine)
          updateTextInput(session, "type_config_label", value = config$type)
          updateTextInput(session, "rpm_config_label", value = as.character(config$rpm))
          updateTextInput(session, "head_config_label", value = as.character(config$head))
          updateTextInput(session, "flow_config_label", value = as.character(config$flow))
          updateTextInput(session, "point_bep_config_label", value = as.character(config$point_bep))
          updateTextInput(session, "treatment_config_label", value = config$treatment)
          updateTextInput(session, "run_config_label", value = as.character(config$run))
        })
      }
    })
    
# Track changes in input fields ####
    observe({
      if (!is.null(deployment_values$baseline_config)) {
        # Check changes against a real baseline config
        config <- deployment_values$baseline_config
        
        inputs_changed <- (
          input$site_config_label != config$site ||
            input$deployment_id_config_label != config$deployment_id ||
            input$pump_turbine_config_label != config$pump_turbine ||
            input$type_config_label != config$type ||
            input$rpm_config_label != as.character(config$rpm) ||
            input$head_config_label != as.character(config$head) ||
            input$flow_config_label != as.character(config$flow) ||
            input$point_bep_config_label != as.character(config$point_bep) ||
            input$treatment_config_label != config$treatment ||
            input$run_config_label != as.character(config$run)
        )
        
        deployment_values$inputs_changed <- inputs_changed
      } else {
        # For blank config, check if any field has content
        inputs_changed <- (
          nchar(trimws(input$site_config_label)) > 0 ||
            nchar(trimws(input$deployment_id_config_label)) > 0 ||
            nchar(trimws(input$pump_turbine_config_label)) > 0 ||
            nchar(trimws(input$type_config_label)) > 0 ||
            nchar(trimws(input$rpm_config_label)) > 0 ||
            nchar(trimws(input$head_config_label)) > 0 ||
            nchar(trimws(input$flow_config_label)) > 0 ||
            nchar(trimws(input$point_bep_config_label)) > 0 ||
            nchar(trimws(input$treatment_config_label)) > 0 ||
            nchar(trimws(input$run_config_label)) > 0
        )
        
        deployment_values$inputs_changed <- inputs_changed
      }
    })
    
# Button state management ####
    observe({
      # Get selected sensors from table
      selected_sensors <- table_results$selected_items()
      
      button_states <- list(
        "save_config_btn" = deployment_values$inputs_changed && !is.null(input$deployment_config_label) && nchar(trimws(input$deployment_config_label)) > 0,
        "add_deploy_btn" = length(selected_sensors) > 0
      )
      
      manage_button_states(session, button_states)
    })

# Auto-select configuration ####
    observe({
      selected_sensors <- table_results$selected_items()
      
      if (length(selected_sensors) == 0) {
        # No sensors selected - show Blank
        updateSelectInput(session, "deployment_config", selected = "Blank")
        return()
      }
      
      # Get configs for selected sensors
      index_df <- get_sensor_index_file(output_dir(), read_data = TRUE)
      if (is.null(index_df)) {
        updateSelectInput(session, "deployment_config", selected = "Blank")
        return()
      }
      
      tryCatch({
        sensor_rows <- index_df[index_df$file %in% selected_sensors, ]
        configs <- unique(sensor_rows$deployment_config)
        configs <- configs[!is.na(configs) & configs != "NA" & configs != ""]
        
        if (length(configs) == 1) {
          # All sensors have same config - show it
          updateSelectInput(session, "deployment_config", selected = configs[1])
        } else {
          # Multiple configs or no configs - show Blank
          updateSelectInput(session, "deployment_config", selected = "Blank")
        }
      }, error = function(e) {
        updateSelectInput(session, "deployment_config", selected = "Blank")
      })
    })

    # ============================= #
    # /// Event handlers \\\ ####  
    # ============================= # 

# Save configuration button ####
    observeEvent(input$save_config_btn, {
      req(input$deployment_config_label)
      
      config_name <- trimws(input$deployment_config_label)
      if (nchar(config_name) == 0) {
        showNotification("Please enter a configuration label", type = "error")
        return()
      }
      
      # Check if config already exists
      if (!is.null(deployment_values$deployment_configs) && config_name %in% names(deployment_values$deployment_configs)) {
        showModal(modalDialog(
          title = "Configuration Exists",
          paste("Configuration '", config_name, "' already exists. Replace existing configuration?"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_save_config"), "Replace", class = "btn-warning")
          ),
          size = "m"
        ))
      } else {
        save_deployment_configuration()
      }
    })
    
# Confirm save configuration ####
    observeEvent(input$confirm_save_config, {
      removeModal()
      save_deployment_configuration()
    })
    
# Add deployment information button ####
    observeEvent(input$add_deploy_btn, {
      selected_sensors <- table_results$selected_items()
      
      if (length(selected_sensors) == 0) {
        showNotification("Please select sensors first", type = "warning")
        return()
      }
      
      # Check if any selected sensors already have deployment info
      index_df <- get_sensor_index_file(output_dir(), read_data = TRUE)
      if (!is.null(index_df)) {
        existing_sensors <- character(0)
        tryCatch({
          sensor_rows <- index_df[index_df$file %in% selected_sensors, ]
          existing_sensors <- sensor_rows[sensor_rows$deployment_info == "Y", "file", drop = TRUE]
        }, error = function(e) {
          warning("Error checking existing deployment info: ", e$message)
        })
        
        if (length(existing_sensors) > 0) {
          showModal(modalDialog(
            title = "Sensors Already Have Deployment Information",
            paste("The following sensors already have deployment information:", 
                  paste(existing_sensors, collapse = ", "), 
                  ". Continue and replace existing deployment information?"),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("confirm_replace_deployment"), "Replace", class = "btn-warning")
            )
          ))
          return()
        }
      }
      
      # If no existing deployment info, proceed directly
      apply_deployment_information()
    })
    
# Confirm replace deployment information ####
    observeEvent(input$confirm_replace_deployment, {
      removeModal()
      apply_deployment_information()
    })
    
    # ============================= #
    # /// Helper functions \\\ ####  
    # ============================= # 
    
# Save deployment configuration function ####
    save_deployment_configuration <- function() {
      config_name <- trimws(input$deployment_config_label)
      
      # Create deployment configuration from inputs
      deployment_config <- list(
        site = trimws(input$site_config_label),
        deployment_id = trimws(input$deployment_id_config_label),
        pump_turbine = trimws(input$pump_turbine_config_label),
        type = trimws(input$type_config_label),
        rpm = type_convert(input$rpm_config_label),
        head = type_convert(input$head_config_label),
        flow = type_convert(input$flow_config_label),
        point_bep = type_convert(input$point_bep_config_label),
        treatment = trimws(input$treatment_config_label),
        run = type_convert(input$run_config_label)
      )
      
      # Save configuration using shared function
      success <- save_config_value(
        output_dir = output_dir(),
        config_type = "deployment",
        key = config_name,
        value = deployment_config
      )
      
      if (success) {
        # Reload configurations
        deployment_values$deployment_configs <- load_config_file(output_dir(), "deployment")
        
        # Update dropdown to show new configuration
        config_names <- names(deployment_values$deployment_configs)
        choices <- setNames(config_names, gsub("_", " ", config_names))
        updateSelectInput(session, "deployment_config", 
                          choices = choices, 
                          selected = config_name)
        
        # Reset change tracking
        deployment_values$inputs_changed <- FALSE
        deployment_values$baseline_config <- deployment_config
        
        showNotification("Deployment configuration saved successfully!", type = "message")
      } else {
        showNotification("Failed to save deployment configuration", type = "error")
      }
    }
    
# Apply deployment information to selected sensors ####
    apply_deployment_information <- function() {
      selected_sensors <- table_results$selected_items()
      
      tryCatch({
        # Read values directly from input fields
        config_name <- if (input$deployment_config == "Blank") "Custom" else input$deployment_config
        
        success_count <- 0
        
        # Apply deployment info to each selected sensor
        for (sensor_name in selected_sensors) {
          updates <- list(
            deployment_info = "Y",
            deployment_config = config_name,  # Save which config was used
            site = trimws(input$site_config_label),
            deployment_id = trimws(input$deployment_id_config_label),
            pump_turbine = trimws(input$pump_turbine_config_label),
            type = trimws(input$type_config_label),
            rpm = type_convert(input$rpm_config_label),
            head = type_convert(input$head_config_label),
            flow = type_convert(input$flow_config_label),
            point_bep = type_convert(input$point_bep_config_label),
            treatment = trimws(input$treatment_config_label),
            run = type_convert(input$run_config_label)
          )
          
          success <- safe_update_sensor_index(output_dir(), sensor_name, updates)
          if (success) {
            success_count <- success_count + 1
          }
        }
        
        if (success_count > 0) {
          # Trigger data refresh
          deployment_values$data_updated <- deployment_values$data_updated + 1
          
          # Show success message
          if (success_count == length(selected_sensors)) {
            showNotification(paste("Deployment information added to", success_count, "sensors"), type = "message")
          } else {
            showNotification(paste("Deployment information added to", success_count, "of", length(selected_sensors), "sensors"), type = "warning")
          }
        } else {
          showNotification("Failed to add deployment information", type = "error")
        }
        
      }, error = function(e) {
        showNotification(paste("Error applying deployment information:", e$message), type = "error")
      })
    }
    
    # ============================= #
    # /// Output render \\\ ####  
    # ============================= #    
# Deployment status display using shared function ####
    deployment_status <- create_individual_status_display(
      "deployment_status", 
      reactive({
        selected_sensors <- table_results$selected_items()
        if (length(selected_sensors) == 0) return("")
        
        # For multiple sensors, show summary status
        if (length(selected_sensors) == 1) {
          return(selected_sensors[1])
        } else {
          # Return a summary indicator for multiple sensors
          return("multiple_sensors")
        }
      }), 
      reactive(output_dir()),
      output, session, "deployment_info",
      invalidation_trigger = reactive(deployment_values$data_updated)
    )
    
# Config change status ####
    output$config_change_status <- renderText({
      if (deployment_values$inputs_changed) {
        "Configuration has been modified - click Save Configuration to save changes"
      } else {
        ""
      }
    })
    
# Deployment summary ####
    output$deployment_summary <- renderText({
      selected_sensors <- table_results$selected_items()
      if (length(selected_sensors) == 0) {
        return("No sensors selected")
      }
      
      # Get deployment status for selected sensors
      index_df <- get_sensor_index_file(output_dir(), read_data = TRUE)
      if (is.null(index_df)) return("No sensor data available")
      
      tryCatch({
        selected_rows <- index_df[index_df$file %in% selected_sensors, ]
        with_deployment <- sum(selected_rows$deployment_info == "Y", na.rm = TRUE)
        without_deployment <- length(selected_sensors) - with_deployment
        
        paste0("Selected: ", length(selected_sensors), " sensors | ",
               "With deployment info: ", with_deployment, " | ",
               "Without deployment info: ", without_deployment)
      }, error = function(e) {
        return("Error reading deployment status")
      })
    })
    
# Custom table formatting with dual highlighting ####
    custom_table_formatting <- function(dt, table_data) {
      if (is.null(table_data) || nrow(table_data) == 0) {
        return(DT::datatable(
          data.frame(Message = "No sensors processed. Process sensors first to add deployment information"),
          options = list(dom = 't', ordering = FALSE),
          rownames = FALSE,
          selection = 'none'
        ))
      }
      
      # Get sensors with and without deployment info
      sensors_with <- sensors_with_deployment()
      sensors_without <- sensors_without_deployment()
      
      # Get row indices for highlighting
      green_rows <- which(table_data$Filename %in% sensors_with)
      orange_rows <- which(table_data$Filename %in% sensors_without)
      
      # Apply green highlighting for sensors with deployment info
      if (length(green_rows) > 0) {
        dt <- dt %>% DT::formatStyle(
          columns = 1:ncol(table_data),
          target = 'row',
          backgroundColor = DT::styleRow(green_rows, 'lightgreen')
        )
      }
      
      # Apply orange highlighting for sensors without deployment info
      if (length(orange_rows) > 0) {
        dt <- dt %>% DT::formatStyle(
          columns = 1:ncol(table_data),
          target = 'row',
          backgroundColor = DT::styleRow(orange_rows, 'orange')
        )
      }
      
      return(dt)
    }
    
# Table display with custom highlighting ####
    table_results <- fileSelectionTableServer(
      "deployment_table",
      sensor_data_reactive = reactive({
        data <- processed_sensor_data()
        if (is.null(data)) {
          data.frame(Message = "No sensors processed. Process sensors first to add deployment information")
        } else {
          data
        }
      }),
      highlight_sensors_reactive = reactive(NULL),  # Custom highlighting handled in formatting
      enable_selection = TRUE,
      selection_mode = 'multiple',
      custom_formatting = custom_table_formatting
    )
    
    # Return reactive values
    return(list(
      selected_sensors_from_table = table_results$selected_items
    ))
    
  })  # End of moduleServer
}     # End of deploymentServer