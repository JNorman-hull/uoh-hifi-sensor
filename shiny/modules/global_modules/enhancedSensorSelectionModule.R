
# ============================= #
# /// Enhanced Sensor Selection Module \\\ ####  
# ============================= #

# Sensor dropdown management ####

update_sensor_dropdown <- function(session, input_id, processed_sensors, current_selection = NULL) {
  choices <- processed_sensors
  if (length(choices) == 0) return()
  
  current_choice <- current_selection
  selected_value <- if (!is.null(current_choice) && current_choice %in% choices) {
    current_choice
  } else {
    choices[1]
  }
  
  updateSelectInput(session, input_id, choices = choices, selected = selected_value)
}

## Enhanced Sensor Selection UI ####
enhancedSensorSelectionUI <- function(id, label = "Select Sensor:", show_filters = TRUE, status_filter_type = NULL) {
  ns <- NS(id)
  
  tagList(
    
    selectInput(ns("sensor_selection"), label, choices = NULL, width = "100%"),
    
    div(style = "margin-top: 5px; color: #666; font-size: 12px; font-style: italic;",
        textOutput(ns("sensor_count"))),
    
    if (show_filters) {
      tagList(
        
        div(style = "margin-bottom: 10px;",
            checkboxInput(ns("mark_sensor_bad"), "Mark sensor as bad", value = FALSE)),
        
        div(style = "margin-bottom: 15px; color: #666; font-size: 12px;",
            textOutput(ns("sensor_status_text"))),
        
        div(style = "margin-bottom: 10px;",
            selectInput(ns("quality_filter"), "Data quality:", 
                        choices = c("Show all" = "all", "Good" = "good", "Bad" = "bad"), 
                        selected = "all", width = "100%")),
        
        if (!is.null(status_filter_type)) {
          div(style = "margin-bottom: 10px;",
              selectInput(ns("status_filter"), paste0(tools::toTitleCase(gsub("_", " ", status_filter_type)), ":"), 
                          choices = c("Show all" = "all", "Yes" = "yes", "No" = "no"), 
                          selected = "all", width = "100%"))
        },
        
        div(style = "margin-bottom: 10px;",
            selectInput(ns("deployment_filter"), "Deployment ID:", 
                        choices = c("Show all" = "all"), selected = "all", width = "100%")),
        
        div(style = "margin-bottom: 10px;",
            selectInput(ns("treatment_filter"), "Treatment:", 
                        choices = c("Show all" = "all"), selected = "all", width = "100%")),
        
        div(style = "margin-bottom: 10px;",
            selectInput(ns("run_filter"), "Run:", 
                        choices = c("Show all" = "all"), selected = "all", width = "100%")),
        hr()
      )
    }
  )
}

## Enhanced Sensor Selection Server ####
enhancedSensorSelectionServer <- function(id, output_dir, processing_complete = reactive(TRUE), status_filter_type = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values for tracking updates
    values <- reactiveValues(
      index_updated = 0,  # Counter to force index reload when sensor status changes
      current_sensor = NULL,  # Track current sensor to detect changes
      expected_checkbox_value = NULL  # Track what the checkbox should be
    )
    
    # Get sensor index data with invalidation trigger
    sensor_index_data <- reactive({
      processing_complete()  # Invalidate when processing completes
      values$index_updated   # Invalidate when sensor status changes
      get_sensor_index_file(output_dir(), read_data = TRUE)
    })
    
    # Get current sensor status using existing shared function
    sensor_status <- reactive({
      req(input$sensor_selection)
      values$index_updated  # Invalidate when status changes
      get_sensor_status(input$sensor_selection, output_dir())
    })
    
    # Get available deployment IDs
    available_deployment_ids <- reactive({
      index_df <- sensor_index_data()
      if (is.null(index_df)) return(character(0))
      
      tryCatch({
        unique_ids <- unique(index_df$deployment_id)
        unique_ids <- unique_ids[!is.na(unique_ids) & unique_ids != "NA" & unique_ids != ""]
        return(sort(unique_ids))
      }, error = function(e) {
        return(character(0))
      })
    })
    
    # Update deployment filter dropdown
    observe({
      ids <- available_deployment_ids()
      choices <- c("Show all" = "all")
      if (length(ids) > 0) {
        choices <- c(choices, setNames(ids, ids))
      }
      updateSelectInput(session, "deployment_filter", choices = choices)
    })
    
    # Get available treatments based on deployment filter
    available_treatments <- reactive({
      index_df <- sensor_index_data()
      if (is.null(index_df)) return(character(0))
      
      tryCatch({
        filtered_df <- if (input$deployment_filter == "all") {
          index_df
        } else {
          index_df[index_df$deployment_id == input$deployment_filter & !is.na(index_df$deployment_id), ]
        }
        
        unique_treatments <- unique(filtered_df$treatment)
        unique_treatments <- unique_treatments[!is.na(unique_treatments) & unique_treatments != "NA" & unique_treatments != ""]
        return(sort(unique_treatments))
      }, error = function(e) {
        return(character(0))
      })
    })
    
    # Update treatment filter dropdown
    observe({
      treatments <- available_treatments()
      choices <- c("Show all" = "all")
      if (length(treatments) > 0) {
        choices <- c(choices, setNames(treatments, treatments))
      }
      updateSelectInput(session, "treatment_filter", choices = choices)
      
      # Enable/disable based on deployment selection
      if (!is.null(input$deployment_filter) && length(input$deployment_filter) > 0) {
        if (input$deployment_filter == "all") {
          shinyjs::disable("treatment_filter")
          updateSelectInput(session, "treatment_filter", selected = "all")
        } else {
          shinyjs::enable("treatment_filter")
        }
      }
    })
    
    # Get available runs based on filters
    available_runs <- reactive({
      index_df <- sensor_index_data()
      if (is.null(index_df)) return(character(0))
      
      tryCatch({
        filtered_df <- index_df
        
        if (input$deployment_filter != "all") {
          filtered_df <- filtered_df[filtered_df$deployment_id == input$deployment_filter & !is.na(filtered_df$deployment_id), ]
        }
        
        if (input$treatment_filter != "all") {
          filtered_df <- filtered_df[filtered_df$treatment == input$treatment_filter & !is.na(filtered_df$treatment), ]
        }
        
        unique_runs <- unique(filtered_df$run)
        unique_runs <- unique_runs[!is.na(unique_runs) & unique_runs != "NA" & unique_runs != ""]
        return(sort(as.numeric(unique_runs)))
      }, error = function(e) {
        return(character(0))
      })
    })
    
    # Update run filter dropdown
    observe({
      runs <- available_runs()
      choices <- c("Show all" = "all")
      if (length(runs) > 0) {
        choices <- c(choices, setNames(as.character(runs), as.character(runs)))
      }
      updateSelectInput(session, "run_filter", choices = choices)
      
      # Enable/disable based on treatment selection
      if (!is.null(input$treatment_filter) && length(input$treatment_filter) > 0) {
        if (input$treatment_filter == "all") {
          shinyjs::disable("run_filter")
          updateSelectInput(session, "run_filter", selected = "all")
        } else {
          shinyjs::enable("run_filter")
        }
      }
    })
    
    # Check if bad sensors exist in current filter (for quality filter activation)
    has_bad_sensors_in_filter <- reactive({
      index_df <- sensor_index_data()
      if (is.null(index_df)) return(FALSE)
      
      tryCatch({
        filtered_df <- index_df
        
        # Apply all filters except quality filters
        if (input$deployment_filter != "all") {
          filtered_df <- filtered_df[filtered_df$deployment_id == input$deployment_filter & !is.na(filtered_df$deployment_id), ]
        }
        
        if (input$treatment_filter != "all") {
          filtered_df <- filtered_df[filtered_df$treatment == input$treatment_filter & !is.na(filtered_df$treatment), ]
        }
        
        if (input$run_filter != "all") {
          filtered_df <- filtered_df[filtered_df$run == input$run_filter & !is.na(filtered_df$run), ]
        }
        
        # Check if any sensors are marked as bad
        return(any(filtered_df$bad_sens == "Y", na.rm = TRUE))
      }, error = function(e) {
        return(FALSE)
      })
    })
    
    # Enable/disable quality filter based on bad sensors presence
    observe({
      has_bad <- has_bad_sensors_in_filter()
      if (!is.null(has_bad) && length(has_bad) > 0) {
        if (has_bad) {
          shinyjs::enable("quality_filter")
        } else {
          shinyjs::disable("quality_filter")
          if (!is.null(input$quality_filter)) {
            updateSelectInput(session, "quality_filter", selected = "all")
          }
        }
      }
    })
    
    # Check if any sensors have processing status (for status filter activation)
    has_processing_status <- reactive({
      if (is.null(status_filter_type)) return(FALSE)
      
      index_df <- sensor_index_data()
      if (is.null(index_df)) return(FALSE)
      
      tryCatch({
        filtered_df <- index_df
        
        # Apply all filters except status filter
        if (input$deployment_filter != "all") {
          filtered_df <- filtered_df[filtered_df$deployment_id == input$deployment_filter & !is.na(filtered_df$deployment_id), ]
        }
        
        if (input$treatment_filter != "all") {
          filtered_df <- filtered_df[filtered_df$treatment == input$treatment_filter & !is.na(filtered_df$treatment), ]
        }
        
        if (input$run_filter != "all") {
          filtered_df <- filtered_df[filtered_df$run == input$run_filter & !is.na(filtered_df$run), ]
        }
        
        if (input$quality_filter == "good") {
          filtered_df <- filtered_df[filtered_df$bad_sens != "Y" | is.na(filtered_df$bad_sens), ]
        } else if (input$quality_filter == "bad") {
          filtered_df <- filtered_df[filtered_df$bad_sens == "Y" & !is.na(filtered_df$bad_sens), ]
        }
        
        # Check if any sensors have been processed for this status type
        for (i in seq_len(nrow(filtered_df))) {
          sensor_name <- filtered_df$file[i]
          status_result <- evaluate_status_checks(sensor_name, output_dir(), check_types = c(status_filter_type))
          if (status_result$level == 4) {  # Level 4 = processed/complete
            return(TRUE)
          }
        }
        
        return(FALSE)
      }, error = function(e) {
        return(FALSE)
      })
    })
    
    # Enable/disable status filter based on processing status
    observe({
      if (!is.null(status_filter_type)) {
        has_processing <- has_processing_status()
        if (!is.null(has_processing) && length(has_processing) > 0) {
          if (has_processing) {
            shinyjs::enable("status_filter")
          } else {
            shinyjs::disable("status_filter")
            if (!is.null(input$status_filter)) {
              updateSelectInput(session, "status_filter", selected = "all")
            }
          }
        }
      }
    })
    # Get filtered sensors
    filtered_sensors <- reactive({
      index_df <- sensor_index_data()
      if (is.null(index_df)) return(character(0))
      
      tryCatch({
        filtered_df <- index_df
        
        # Filter by deployment ID
        if (input$deployment_filter != "all") {
          filtered_df <- filtered_df[filtered_df$deployment_id == input$deployment_filter & !is.na(filtered_df$deployment_id), ]
        }
        
        # Filter by treatment
        if (input$treatment_filter != "all") {
          filtered_df <- filtered_df[filtered_df$treatment == input$treatment_filter & !is.na(filtered_df$treatment), ]
        }
        
        # Filter by run
        if (input$run_filter != "all") {
          filtered_df <- filtered_df[filtered_df$run == input$run_filter & !is.na(filtered_df$run), ]
        }
        
        # Filter by data quality
        if (input$quality_filter == "good") {
          filtered_df <- filtered_df[filtered_df$bad_sens != "Y" | is.na(filtered_df$bad_sens), ]
        } else if (input$quality_filter == "bad") {
          filtered_df <- filtered_df[filtered_df$bad_sens == "Y" & !is.na(filtered_df$bad_sens), ]
        }
        # "show all" includes both good and bad sensors
        
        # Filter by processing status
        if (!is.null(status_filter_type) && !is.null(input$status_filter) && input$status_filter != "all") {
          processed_sensors <- character(0)
          unprocessed_sensors <- character(0)
          
          for (i in seq_len(nrow(filtered_df))) {
            sensor_name <- filtered_df$file[i]
            status_result <- evaluate_status_checks(sensor_name, output_dir(), check_types = c(status_filter_type))
            
            if (status_result$level == 4) {  # Level 4 = processed/complete
              processed_sensors <- c(processed_sensors, sensor_name)
            } else {
              unprocessed_sensors <- c(unprocessed_sensors, sensor_name)
            }
          }
          
          if (input$status_filter == "yes") {
            filtered_df <- filtered_df[filtered_df$file %in% processed_sensors, ]
          } else if (input$status_filter == "no") {
            filtered_df <- filtered_df[filtered_df$file %in% unprocessed_sensors, ]
          }
        }
        
        return(sort(filtered_df$file))
      }, error = function(e) {
        return(character(0))
      })
    })
    
    # Update sensor dropdown
    observe({
      sensors <- filtered_sensors()
      if (length(sensors) == 0) {
        updateSelectInput(session, "sensor_selection", choices = c("No sensors available" = ""))
      } else {
        current_selection <- input$sensor_selection
        selected_value <- if (!is.null(current_selection) && current_selection %in% sensors) {
          current_selection
        } else {
          sensors[1]
        }
        updateSelectInput(session, "sensor_selection", choices = sensors, selected = selected_value)
      }
    })
    
    # Update mark_sensor_bad checkbox when sensor changes
    observe({
      req(input$sensor_selection)
      status <- sensor_status()
      
      # Update expected value and current sensor tracking
      values$current_sensor <- input$sensor_selection
      values$expected_checkbox_value <- status$bad_sens
      
      # Update checkbox to match sensor's actual status
      updateCheckboxInput(session, "mark_sensor_bad", value = status$bad_sens)
    })
    
    # Handle marking sensor as bad/good (only when user deliberately changes it)
    observeEvent(input$mark_sensor_bad, {
      req(input$sensor_selection)
      
      # Only trigger if the checkbox value differs from what we expect for this sensor
      # (i.e., user deliberately changed it, not just switching sensors)
      status <- sensor_status()
      if (input$mark_sensor_bad == status$bad_sens) {
        # Checkbox matches current sensor status - this is just a sensor switch, not a user change
        return()
      }
      
      new_status <- if (input$mark_sensor_bad) "Y" else "N"
      
      success <- safe_update_sensor_index(
        output_dir(), 
        input$sensor_selection,
        list(bad_sens = new_status)
      )
      
      if (success) {
        # Force index reload
        values$index_updated <- values$index_updated + 1
        
        # Update expected value
        values$expected_checkbox_value <- input$mark_sensor_bad
        
        status_text <- if (input$mark_sensor_bad) "bad" else "good"
        showNotification(
          paste("Sensor", input$sensor_selection, "marked as", status_text), 
          type = "message"
        )
      } else {
        showNotification("Failed to update sensor status", type = "error")
        # Revert checkbox if update failed
        updateCheckboxInput(session, "mark_sensor_bad", value = !input$mark_sensor_bad)
      }
    }, ignoreInit = TRUE)
    
    # Render sensor count
    output$sensor_count <- renderText({
      sensors <- filtered_sensors()
      count <- length(sensors)
      
      if (count == 0) {
        "No sensors available with current filters"
      } else if (count == 1) {
        "1 sensor available"
      } else {
        paste(count, "sensors available")
      }
    })
    
    # Render sensor status text
    output$sensor_status_text <- renderText({
      req(input$sensor_selection)
      status <- sensor_status()
      
      if (status$bad_sens) {
        paste("Sensor", input$sensor_selection, "marked as bad")
      } else {
        paste("Sensor", input$sensor_selection, "marked as good")
      }
    })
    
    # Return reactive values
    return(list(
      selected_sensor = reactive({
        if (is.null(input$sensor_selection) || input$sensor_selection == "") {
          return(NULL)
        }
        return(input$sensor_selection)
      }),
      filtered_sensors = filtered_sensors,
      deployment_filter = reactive(input$deployment_filter),
      treatment_filter = reactive(input$treatment_filter),
      run_filter = reactive(input$run_filter),
      quality_filter = reactive(input$quality_filter),
      status_filter = if (!is.null(status_filter_type)) reactive(input$status_filter) else NULL
    ))
  })
}