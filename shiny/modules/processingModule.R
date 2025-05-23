
processingUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Processing Log"),
    verbatimTextOutput(ns("process_log"))
  )
}

processingServer <- function(id, selected_sensors, raw_data_path, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      log_messages = character(0),
      summary_data = NULL,
      is_processing = FALSE,
      processing_complete = FALSE
    )
    
    # Display processing log
    output$process_log <- renderText({
      paste(values$log_messages, collapse = "\n")
    })
    
    # Helper function to update messages
    updateLogMessage <- function(message) {
      values$log_messages <- c(values$log_messages, message)
      session$sendCustomMessage("updateProcessLog", list(text = paste(values$log_messages, collapse = "\n")))
    }
    
    # Process sensors function
    process_sensors <- function() {
      sensors <- selected_sensors()
      
      # Check if any sensors are selected
      if (length(sensors) == 0) {
        updateLogMessage("No sensors selected. Please select at least one sensor to process.")
        return()
      }
      
      # Prevent multiple processing jobs
      if (values$is_processing) {
        updateLogMessage("Processing already in progress. Please wait...")
        return()
      }
      
      # Reset state
      values$is_processing <- TRUE
      values$processing_complete <- FALSE
      values$summary_data <- NULL
      values$log_messages <- character(0)
      
      # Process in step-by-step manner
      result <- process_sensors_step_by_step(
        sensors, 
        raw_data_path(), 
        output_dir(),
        session
      )
      
      # Update after completion
      values$summary_data <- result$summary_data
      values$log_messages <- result$log_messages
      values$processing_complete <- TRUE
      values$is_processing <- FALSE
      
      # Send completion notification
      session$sendCustomMessage("processingComplete", list(success = TRUE))
    }
    
    return(list(
      summary_data = reactive(values$summary_data),
      processing_complete = reactive(values$processing_complete),
      is_processing = reactive(values$is_processing),
      log_messages = reactive(values$log_messages),
      process_sensors = process_sensors
    ))
  })
}


process_sensors_step_by_step <- function(selected_sensors, raw_data_path, output_dir, session) {
  log_messages <- character(0)
  
  # Function to update log in UI
  update_log <- function(message) {
    log_messages <<- c(log_messages, message)
    log_text <- paste(log_messages, collapse = "\n")
    session$sendCustomMessage("updateProcessLog", list(text = log_text))
    Sys.sleep(0.1)  # Small delay to allow UI to update
  }
  
  update_log("Starting processing...")
  update_log(paste("Processing", length(selected_sensors), "selected sensors"))
  
  # Initialize data structures
  summary_data <- list()
  n_files_w_time <- 0
  n_files_w_hig <- 0
  n_files_w_pres <- 0
  
  # Get current date for CSV filenames
  current_date <- format(Sys.Date(), "%d%m%y")
  
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
        
        # Check for errors
        if (grepl("TIME:", summary_info$messages)) {
          n_files_w_time <- n_files_w_time + 1
        }
        if (grepl("HIG:", summary_info$messages)) {
          n_files_w_hig <- n_files_w_hig + 1
        }
        if (grepl("PRES:", summary_info$messages)) {
          n_files_w_pres <- n_files_w_pres + 1
        }
        
        summary_info$file <- sensor_name
        summary_data[[length(summary_data) + 1]] <- summary_info
        
        update_log(paste(sensor_name, "processed successfully"))
        
      }, error = function(e) {
        error_msg <- paste("Error processing", sensor_name, ":", e$message)
        update_log(error_msg)
      })
    } else {
      missing_msg <- paste("Missing IMP or HIG file for sensor:", sensor_name)
      update_log(missing_msg)
    }
  }
  
  # Create summary CSV
  if (length(summary_data) > 0) {
    # Convert to data frame
    summary_df <- do.call(rbind, lapply(summary_data, function(x) {
      as.data.frame(x, stringsAsFactors = FALSE)
    }))
    
    # Save to CSV
    summary_csv_filename <- paste0(current_date, "_batch_summary.csv")
    summary_csv_path <- file.path(output_dir, summary_csv_filename)
    write.csv(summary_df, summary_csv_path, row.names = FALSE)
    
    update_log("Batch sensor processing complete.")
    update_log(paste(length(summary_data), "total sensors processed"))
    update_log(paste(n_files_w_pres, "/", length(summary_data), "sensors contain pressure data errors"))
    update_log(paste(n_files_w_time, "/", length(summary_data), "sensors contain time series errors"))
    update_log(paste(n_files_w_hig, "/", length(summary_data), "sensors contain strike/collision event (HIG ≥ 400g)"))
    
    return(list(
      summary_data = summary_data,
      log_messages = log_messages
    ))
  } else {
    update_log("No sensors were successfully processed.")
    return(list(
      summary_data = list(),
      log_messages = log_messages
    ))
  }
}