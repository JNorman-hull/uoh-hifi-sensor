processingUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Processing messages"),
    helpText('Processing report printed here'),
    verbatimTextOutput(ns("process_log"))
  )
}

# Helper function to create consistent log updater
create_log_updater <- function(session, reactive_values) {
  function(message) {
    reactive_values$log_messages <- c(reactive_values$log_messages, message)
    session$sendCustomMessage("updateProcessLog", 
                              list(text = paste(reactive_values$log_messages, collapse = "\n")))
    Sys.sleep(0.1)  # Small delay to allow UI to update
  }
}

processingServer <- function(id, selected_sensors, raw_data_path, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      log_messages = character(0),
      summary_data = NULL,
      is_processing = FALSE,
      processing_complete = FALSE,
      sensors_before_processing = character(0),
      sensors_actually_processed = character(0)
    )
    
    # Create the log updater
    update_log <- create_log_updater(session, values)
    
    observe({
      # Only set initial message if log is empty
      if (length(values$log_messages) == 0) {
        isolate({
          values$log_messages <- 'Select sensors and click "Process Selected Sensors" to begin processing'
        })
      }
    })
    
    # Display processing log
    output$process_log <- renderText({
      if (length(values$log_messages) == 0) {
        return('Loading...')  # Fallback message
      }
      paste(values$log_messages, collapse = "\n")
    })
    # Get summary data from sensor index file using shared function
    summary_data_from_index <- reactive({
      values$processing_complete  # Invalidate when processing completes
      
      index_file <- get_sensor_index_file(output_dir())
      if (!is.null(index_file)) {
        tryCatch({
          index_df <- read.csv(index_file)
          # Convert to list format for compatibility with existing code
          summary_list <- list()
          for (i in seq_len(nrow(index_df))) {
            summary_list[[i]] <- as.list(index_df[i, ])
          }
          return(summary_list)
        }, error = function(e) {
          warning("Error reading sensor index: ", e$message)
          return(list())
        })
      } else {
        return(list())
      }
    })
    
    # Get newly processed sensors by comparing before/after
    newly_processed_sensors <- reactive({
      req(values$processing_complete)
      return(values$sensors_actually_processed)
    })
    
    # Processing function with consistent logging
    process_sensors_step_by_step <- function(selected_sensors, raw_data_path, output_dir) {
      update_log(paste("Processing", length(selected_sensors), "selected sensors"))
      
      # Initialize counters for summary statistics
      counters <- list(
        n_files_w_time = 0,
        n_files_w_hig = 0,
        n_files_w_pres = 0,
        n_processed = 0,
        n_failed = 0
      )
      
      # Process each sensor one at a time
      for (i in seq_along(selected_sensors)) {
        sensor_name <- selected_sensors[i]
        update_log(paste("Processing sensor", i, "/", length(selected_sensors), ":", sensor_name))
        
        # Check if files exist
        imp_file <- file.path(raw_data_path, paste0(sensor_name, ".IMP"))
        hig_file <- file.path(raw_data_path, paste0(sensor_name, ".HIG"))
        
        if (file.exists(imp_file) && file.exists(hig_file)) {
          tryCatch({
            result <- py$process_imp_hig_direct(imp_file, hig_file, output_dir)
            combined_data <- result[[1]]
            summary_info <- result[[2]]
            
            # Count error types for summary
            if (grepl("TIME:", summary_info$messages)) {
              counters$n_files_w_time <- counters$n_files_w_time + 1
            }
            if (grepl("HIG:", summary_info$messages)) {
              counters$n_files_w_hig <- counters$n_files_w_hig + 1
            }
            if (grepl("PRES:", summary_info$messages)) {
              counters$n_files_w_pres <- counters$n_files_w_pres + 1
            }
            
            counters$n_processed <- counters$n_processed + 1
            update_log(paste(sensor_name, "processed successfully"))
            
          }, error = function(e) {
            counters$n_failed <<- counters$n_failed + 1
            error_msg <- paste("Error processing", sensor_name, ":", e$message)
            update_log(error_msg)
          })
        } else {
          counters$n_failed <- counters$n_failed + 1
          missing_msg <- paste("Missing IMP or HIG file for sensor:", sensor_name)
          update_log(missing_msg)
        }
      }
      
      # Final summary
      update_log("Sensor processing complete.")
      update_log(paste(counters$n_processed, "total sensors processed successfully"))
      if (counters$n_failed > 0) {
        update_log(paste(counters$n_failed, "sensors failed to process"))
      }
      if (counters$n_processed > 0) {
        update_log(paste(counters$n_files_w_pres, "/", counters$n_processed, "sensors contain pressure data errors"))
        update_log(paste(counters$n_files_w_time, "/", counters$n_processed, "sensors contain time series errors"))
        update_log(paste(counters$n_files_w_hig, "/", counters$n_processed, "sensors contain strike/collision event (HIG ≥ 400g)"))
      }
      
      return(list(counters = counters))
    }
    
    # Move the actual processing logic to a separate function
    start_processing <- function() {
      values$sensors_actually_processed <- selected_sensors()  
      
      # Snapshot sensors before processing
      index_file <- get_sensor_index_file(output_dir())
      if (!is.null(index_file) && file.exists(index_file)) {
        tryCatch({
          index_df <- read.csv(index_file)
          values$sensors_before_processing <- index_df$file
        }, error = function(e) {
          values$sensors_before_processing <- character(0)
        })
      } else {
        values$sensors_before_processing <- character(0)
      }
      
      # Set processing state
      values$is_processing <- TRUE
      values$processing_complete <- FALSE
      update_log("Starting processing...")
      
      # Process in step-by-step manner
      result <- process_sensors_step_by_step(
        selected_sensors(), 
        raw_data_path(), 
        output_dir()
      )
      
      # Update completion state
      values$processing_complete <- TRUE
      values$is_processing <- FALSE
      
      # Send completion notification
      session$sendCustomMessage("processingComplete", list(success = TRUE))
    }
    
    # Process sensors function
    process_sensors <- function() {
      sensors <- selected_sensors()
      
      # Check for existing sensors BEFORE setting processing state
      existing_sensors <- character(0)
      index_file <- get_sensor_index_file(output_dir())
      if (!is.null(index_file)) {
        tryCatch({
          index_df <- read.csv(index_file)
          existing_sensors <- intersect(sensors, index_df$file)
        }, error = function(e) {
          warning("Error checking existing sensors: ", e$message)
        })
      }
      
      if (length(existing_sensors) > 0) {
        showModal(modalDialog(
          title = "Sensors Already Exist",
          paste("The following sensors already exist in the index:", 
                paste(existing_sensors, collapse = ", "), 
                ". Continue and replace existing data?"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_replace_sensors"), "Replace", class = "btn-warning")
          )
        ))
        return()
      }
      
      # If no existing sensors, proceed directly
      start_processing()
    }
    
    # Add confirm replacement handler
    observeEvent(input$confirm_replace_sensors, {
      removeModal()
      start_processing()
    })
    
    return(list(
      summary_data = summary_data_from_index,
      processing_complete = reactive(values$processing_complete),
      is_processing = reactive(values$is_processing),
      log_messages = reactive(values$log_messages),
      newly_processed_sensors = newly_processed_sensors,
      process_sensors = process_sensors
    ))
  })
}