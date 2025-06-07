summarytableModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Descriptive statistics and central tendencies"),
    
    # Summary table
    DT::dataTableOutput(ns("summary_table"))
  )
}

summarytableSidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
  h4("Summary measurments"),
  
  # Action button for processing summary information
  actionButton(ns("process_summary"), "Process summary information", 
               class = "btn-success btn-block"),
  
  # Status text output
  div(style = "margin-top: 10px; margin-bottom: 15px;",
      textOutput(ns("summary_status")))
  )
}


summarytableModuleServer <- function(id, sensor_reactive, output_dir_reactive, instrument_variable = NULL, 
                                     global_sensor_state, trigger_data_update, trigger_summary_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ============================= #
    # /// Reactive values \\\ ####  
    # ============================= #   
    
     # ============================= #
    # /// Data loading & processing  \\\ ####  
    # ============================= # 
    
    # Get sensor status
    sensor_status <- reactive({
      req(sensor_reactive())
      global_sensor_state$summary_updated
      global_sensor_state$data_updated
      get_sensor_status(sensor_reactive(), output_dir_reactive())
    })
    
    # Read existing summary data from instrument index
    existing_summary_data <- reactive({
      req(sensor_reactive())
      global_sensor_state$summary_updated 
      
      instrument_df <- get_instrument_index_file(output_dir_reactive(), read_data = TRUE)
      if (is.null(instrument_df)) return(NULL)
      
      tryCatch({
        sensor_rows <- instrument_df[instrument_df$file == sensor_reactive(), ]
        if (nrow(sensor_rows) > 0) {
          return(sensor_rows)
        }
        return(NULL)
      }, error = function(e) {
        return(NULL)
      })
    })
    
    # ============================= #
    # /// UI State management \\\ ####  
    # ============================= # 
    
    observe({
      req(sensor_reactive(), instrument_variable)
      status <- sensor_status()
      
      # Check if summary already processed
      status_col <- paste0(instrument_variable, "_sum_processed")
      already_processed <- status[[status_col]] %||% FALSE
      
      # Button state: enabled when delineated and trimmed
      can_process <- status$delineated && status$trimmed
      
      button_states <- list(
        "process_summary" = can_process
      )
      
      manage_button_states(session, button_states)
      
      # Update button appearance and text
      if (already_processed) {
        updateActionButton(session, "process_summary", 
                           label = "Recalculate summary information")
        shinyjs::removeClass("process_summary", "btn-success")
        shinyjs::addClass("process_summary", "btn-warning")
      } else {
        updateActionButton(session, "process_summary", 
                           label = "Process summary information")
        shinyjs::removeClass("process_summary", "btn-warning") 
        shinyjs::addClass("process_summary", "btn-success")
      }
    })
    
    # ============================= #
    # /// Event handlers \\\ ####  
    # ============================= # 
    
    observeEvent(input$process_summary, {
      req(sensor_reactive(), instrument_variable)
      
      # Check if data already exists
      status <- sensor_status()
      status_col <- paste0(instrument_variable, "_sum_processed")
      already_processed <- status[[status_col]] %||% FALSE
      
      if (already_processed) {
        showModal(modalDialog(
          title = "Summary Data Exists",
          paste("Summary data already exists for", sensor_reactive(), 
                ". Replace existing summary data?"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_replace_summary"), "Replace", class = "btn-warning")
          )
        ))
      } else {
        calculate_and_save_summary()
      }
    })
    
    observeEvent(input$confirm_replace_summary, {
      removeModal()
      calculate_and_save_summary()
    })
    
    # ============================= #
    # /// Helper functions \\\ ####  
    # ============================= #
    
    calculate_and_save_summary <- function() {
      tryCatch({
        # Read delineated data
        sensor_data <- read_sensor_data(output_dir_reactive(), sensor_reactive(), "delineated")
        
        if (is.null(sensor_data)) {
          showNotification("Failed to read delineated dataset", type = "error")
          return()
        }
        
        # Get instrument column mapping
        mapping <- get_instrument_column_mapping(instrument_variable)
        if (is.null(mapping)) {
          showNotification("Invalid instrument variable", type = "error")
          return()
        }
        
        # Get actual ROI levels from the data, plus overall
        roi_levels <- c("overall")
        if ("roi" %in% names(sensor_data)) {
          actual_rois <- unique(sensor_data$roi)
          # Remove trim regions and add to roi_levels
          actual_rois <- actual_rois[!actual_rois %in% c("trim_start", "trim_end")]
          roi_levels <- c(roi_levels, actual_rois)
        }
        
        # Process each ROI
        for (roi in roi_levels) {
          # Filter data for this ROI
          if (roi == "overall") {
            roi_data <- sensor_data
          } else {
            roi_data <- sensor_data[sensor_data$roi == roi, ]
          }
          
          if (nrow(roi_data) == 0) next
          
          # Calculate summary for each instrument column
          updates <- list()
          
          for (i in seq_along(mapping$data_cols)) {
            col <- mapping$data_cols[i]
            prefix <- mapping$prefix[i]
            unit <- mapping$units[i]
            
            if (!col %in% names(roi_data)) next
            
            # Calculate statistics
            min_idx <- which.min(roi_data[[col]])
            max_idx <- which.max(roi_data[[col]])
            
            min_val <- roi_data[[col]][min_idx]
            min_time <- roi_data$time_s[min_idx]
            max_val <- roi_data[[col]][max_idx]
            max_time <- roi_data$time_s[max_idx]
            median_val <- median(roi_data[[col]], na.rm = TRUE)
            iqr_val <- IQR(roi_data[[col]], na.rm = TRUE)
            
            # Create column names matching instrument index structure
            updates[[paste0(prefix, "_min", unit)]] <- round(min_val, 2)
            updates[[paste0(prefix, "_min.time.")]] <- round(min_time, 2)
            updates[[paste0(prefix, "_max", unit)]] <- round(max_val, 2)
            updates[[paste0(prefix, "_max.time.")]] <- round(max_time, 2)
            updates[[paste0(prefix, "_median", unit)]] <- round(median_val, 2)
            updates[[paste0(prefix, "_iqr", unit)]] <- round(iqr_val, 2)
          }
          
          # Save to instrument index using the actual ROI name
          success <- safe_update_instrument_index(
            output_dir_reactive(), 
            sensor_reactive(),
            roi,  # This will now be "overall", "roi1_sens_ingress", "roi4_nadir", etc.
            updates
          )
          
          if (!success) {
            showNotification(paste("Failed to save summary for", roi), type = "warning")
          }
        }
        
        # Update status flag in sensor index
        status_col <- paste0(instrument_variable, "_sum_processed")
        success <- safe_update_sensor_index(
          output_dir_reactive(), 
          sensor_reactive(),
          setNames(list("Y"), status_col)
        )
        
        if (success) {
          trigger_data_update()     # Use global trigger
          trigger_summary_update()  # Use global trigger
          showNotification(paste("Summary information calculated and saved for", sensor_reactive()), 
                           type = "message")
        } else {
          showNotification("Summary calculated but failed to update sensor status", type = "warning")
        }
        
      }, error = function(e) {
        showNotification(paste("Error calculating summary:", e$message), type = "error")
      })
    }
    
    # ============================= #
    # /// Output render \\\ ####  
    # ============================= #    
    
    output$summary_status <- renderText({
      req(sensor_reactive(), instrument_variable)
      status <- sensor_status()
      status_col <- paste0(instrument_variable, "_sum_processed")
      
      if (status[[status_col]] %||% FALSE) {
        paste("Summary information calculated for", sensor_reactive())
      } else {
        ""
      }
    })
    
    output$summary_table <- DT::renderDataTable({
      req(sensor_reactive(), instrument_variable)
      
      summary_data <- existing_summary_data()
      
      if (is.null(summary_data)) {
        return(DT::datatable(
          data.frame(Message = paste("No summary data available for", instrument_variable)),
          options = list(dom = 't', ordering = FALSE),
          rownames = FALSE,
          selection = 'none'
        ))
      }
      
      # Get column mapping
      mapping <- get_instrument_column_mapping(instrument_variable)
      if (is.null(mapping)) return(NULL)
      
      # Build display table
      roi_display_names <- c(
        "overall" = "Overall",
        "roi1_sens_ingress" = "ROI 1: Sensor ingress",
        "roi2_inflow_passage" = "ROI 2: Inflow passage", 
        "roi3_prenadir" = "ROI 3: Pre-nadir",
        "roi4_nadir" = "ROI 4: Nadir",
        "roi5_postnadir" = "ROI 5: Post-nadir",
        "roi6_outflow_passage" = "ROI 6: Outflow passage",
        "roi7_sens_outgress" = "ROI 7: Sensor outgress"
      )
      
      # Get all relevant columns but exclude .time. columns
      relevant_cols <- c("roi")
      for (prefix in mapping$prefix) {
        matching_cols <- names(summary_data)[grepl(paste0("^", prefix, "_"), names(summary_data))]
        # Exclude time columns
        matching_cols <- matching_cols[!grepl("\\.time\\.", matching_cols)]
        relevant_cols <- c(relevant_cols, matching_cols)
      }
      
      # Select and arrange data
      display_data <- summary_data %>%
        select(all_of(relevant_cols)) %>%
        arrange(factor(roi, levels = c("overall", "roi1_sens_ingress", "roi2_inflow_passage", 
                                       "roi3_prenadir", "roi4_nadir", "roi5_postnadir", 
                                       "roi6_outflow_passage", "roi7_sens_outgress")))
      
      # Replace ROI names with display names
      display_data$roi <- roi_display_names[display_data$roi]
      
      # Create readable column names for display
      if (instrument_variable == "acc") {
        # Show both HIG and Inertial acceleration (without time columns)
        display_data <- display_data %>%
          select(
            ROI = roi,
            `HIG Min (g)` = acc_hig_min.g.,
            `HIG Max (g)` = acc_hig_max.g.,
            `HIG Median (g)` = acc_hig_median.g.,
            `HIG IQR (g)` = acc_hig_iqr.g.,
            `Inertial Min (m/s²)` = acc_inacc_min.ms.,
            `Inertial Max (m/s²)` = acc_inacc_max.ms.,
            `Inertial Median (m/s²)` = acc_inacc_median.ms.,
            `Inertial IQR (m/s²)` = acc_inacc_iqr.ms.
          )
      } else if (instrument_variable == "pres") {
        # Pressure columns (without time columns)
        display_data <- display_data %>%
          select(
            ROI = roi,
            `Min (kPa)` = pres_min.kPa.,
            `Max (kPa)` = pres_max.kPa.,
            `Median (kPa)` = pres_median.kPa.,
            `IQR (kPa)` = pres_iqr.kPa.
          )
      } else if (instrument_variable == "rot") {
        # Rotation columns (without time columns)
        display_data <- display_data %>%
          select(
            ROI = roi,
            `Min (deg/s)` = rot_min.degs.,
            `Max (deg/s)` = rot_max.degs.,
            `Median (deg/s)` = rot_median.degs.,
            `IQR (deg/s)` = rot_iqr.degs.
          )
      }
      
      DT::datatable(
        display_data,
        options = list(
          pageLength = 8,
          scrollX = TRUE,
          dom = 't',
          ordering = FALSE,
          searching = FALSE,
          paging = FALSE,
          info = FALSE
        ),
        rownames = FALSE,
        selection = 'none'
      ) %>%
        DT::formatStyle(columns = 1:ncol(display_data), fontSize = '12px') %>%
        DT::formatRound(columns = 2:ncol(display_data), digits = 2) %>%
        DT::formatStyle(
          "ROI",
          target = "row",
          backgroundColor = DT::styleEqual("ROI 4: Nadir", "lightgreen")
        )
    })
    
    return(list(
      summary_data = existing_summary_data
    ))
  })
}