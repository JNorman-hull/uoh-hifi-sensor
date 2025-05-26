# ROI Delineation Module

roiUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("ROI Delineated Time Series"),
    plotlyOutput(ns("roi_plot"), height = "600px")
  )
}

roiSidebarUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("ROI Delineation Options"),
    selectInput(ns("plot_sensor"), "Select Sensor:", choices = NULL),
    
    div(style = "margin-bottom: 15px;",
        textOutput(ns("delineation_status"))
    ),
    
    # Use ns() to properly namespace the output ID in the condition
    conditionalPanel(
      condition = paste0("output['", ns("nadir_available"), "'] == true"),
      
      hr(),
      h4("ROI Configuration"),
      selectInput(ns("config_choice"), "Configuration:", choices = NULL),
      
      div(style = "max-height: 180px; overflow-y: auto; margin-bottom: 15px;",
          DT::dataTableOutput(ns("roi_table"))
      ),
      
      actionButton(ns("create_delineated"), "Create delineated dataset", 
                   class = "btn-primary btn-block"),
      actionButton(ns("start_over"), "Start Over", 
                   class = "btn-warning btn-block"),
      actionButton(ns("trim_sensor"), "Trim sensor start and end", 
                   class = "btn-danger btn-block")
    )
  )
}

roiServer <- function(id, output_dir, summary_data, processing_complete = reactive(FALSE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Values for ROI processing
    roi_values <- reactiveValues(
      roi_configs = NULL,
      current_config = NULL,
      summary_updated = 0,
      data_updated = 0
    )
    
    # Get processed sensors using shared function
    processed_sensors <- reactive({
      processing_complete()
      get_processed_sensors(output_dir())
    })
    
    # Update sensor dropdown using shared function
    observe({
      update_sensor_dropdown(session, "plot_sensor", processed_sensors(), input$plot_sensor)
    })
    
    # Load ROI configurations using shared function
    observe({
      roi_values$roi_configs <- load_roi_configs(output_dir())
      
      if (length(roi_values$roi_configs) > 0) {
        config_names <- names(roi_values$roi_configs)
        # Create named vector: display names without underscores, values with underscores
        choices <- setNames(config_names, gsub("_", " ", config_names))
        current_choice <- input$config_choice
        
        selected_value <- if (!is.null(current_choice) && current_choice %in% config_names) {
          current_choice
        } else {
          config_names[1]
        }
        
        updateSelectInput(session, "config_choice", 
                          choices = choices, 
                          selected = selected_value)
      }
    })
    
    # Update current config when dropdown selection changes
    observe({
      req(input$config_choice, roi_values$roi_configs)
      roi_values$current_config <- roi_values$roi_configs[[input$config_choice]]
    })
    
    # Get nadir info using shared function with reactivity
    nadir_info <- reactive({
      req(input$plot_sensor)
      roi_values$summary_updated  # Add reactivity to catch nadir updates
      get_nadir_info(input$plot_sensor, output_dir())
    })
    
    # Output for conditional panel
    output[[ns("nadir_available")]] <- reactive({
      nadir_info()$available
    })
    outputOptions(output, ns("nadir_available"), suspendWhenHidden = FALSE)
    
    # Get sensor status using shared function
    sensor_status <- reactive({
      req(input$plot_sensor)
      roi_values$summary_updated  # Reactivity to delineation changes
      roi_values$data_updated     # Reactivity to trimming changes
      
      get_sensor_status(input$plot_sensor, output_dir())
    })
    
    # Delineation status display
    output$delineation_status <- renderText({
      req(input$plot_sensor)
      
      nadir <- nadir_info()
      status <- sensor_status()
      
      if (!nadir$available) {
        "No nadir data available"
      } else if (!status$delineated) {
        "Sensor requires delineation"
      } else if (status$trimmed) {
        "Sensor file delineated and trimmed"
      } else {
        "Sensor file delineated (not trimmed)"
      }
    })
    
    # CSS styling for status text
    observe({
      req(input$plot_sensor)
      
      nadir <- nadir_info()
      status <- sensor_status()
      
      status_color <- if (!nadir$available) {
        "orange"
      } else if (!status$delineated) {
        "red"
      } else if (status$trimmed) {
        "green"
      } else {
        "blue"
      }
      
      shinyjs::runjs(paste0("
        $('#", ns("delineation_status"), "').css({
          'color': '", status_color, "', 
          'font-weight': 'bold'
        });
      "))
    })
    
    # Manage button states using shared function
    observe({
      req(input$plot_sensor)
      
      nadir <- nadir_info()
      status <- sensor_status()
      
      button_states <- list(
        "create_delineated" = nadir$available && !status$delineated,
        "start_over" = status$delineated,
        "trim_sensor" = status$delineated && !status$trimmed
      )
      
      manage_button_states(session, button_states)
    })
    
    # Calculate ROI times
    roi_times <- reactive({
      req(input$plot_sensor, roi_values$current_config)
      input$config_choice  # Make reactive to config changes
      
      nadir <- nadir_info()
      
      if (!nadir$available) return(NULL)
      
      nadir_time <- nadir$time
      config <- roi_values$current_config
      
      # Calculate ROI boundaries based on nadir time
      roi3_start <- nadir_time - (config$roi3_nadir / 2)
      roi3_end <- nadir_time + (config$roi3_nadir / 2)
      
      roi2_start <- roi3_start - config$roi2_prenadir
      roi2_end <- roi3_start
      
      roi1_start <- roi2_start - config$roi1_ingress
      roi1_end <- roi2_start
      
      roi4_start <- roi3_end
      roi4_end <- roi3_end + config$roi4_postnadir
      
      roi5_start <- roi4_end
      roi5_end <- roi4_end + config$roi5_outgress
      
      # Read sensor data to get actual start/end times
      sensor_data <- read_sensor_data(output_dir(), input$plot_sensor, "min")
      if (!is.null(sensor_data)) {
        data_start <- min(sensor_data$time_s)
        data_end <- max(sensor_data$time_s)
        
        roi_times_df <- data.frame(
          ROI = c("Sensor start trim", "ROI 1: Ingress", "ROI 2: Pre-nadir", 
                  "ROI 3: Nadir", "ROI 4: Post-nadir", "ROI 5: Outgress", "Sensor end trim"),
          `Start time` = c(paste(round(data_start, 3), "s"),
                           paste(round(roi1_start, 3), "s"),
                           paste(round(roi2_start, 3), "s"),
                           paste(round(roi3_start, 3), "s"),
                           paste(round(roi4_start, 3), "s"),
                           paste(round(roi5_start, 3), "s"),
                           paste(round(roi5_end, 3), "s")),
          `End Time` = c(paste(round(roi1_start, 3), "s"),
                         paste(round(roi1_end, 3), "s"),
                         paste(round(roi2_end, 3), "s"),
                         paste(round(roi3_end, 3), "s"),
                         paste(round(roi4_end, 3), "s"),
                         paste(round(roi5_end, 3), "s"),
                         paste(round(data_end, 3), "s")),
          Duration = c(paste(round(roi1_start - data_start, 3), "s"),
                       paste(round(config$roi1_ingress, 3), "s"),
                       paste(round(config$roi2_prenadir, 3), "s"),
                       paste(round(config$roi3_nadir, 3), "s"),
                       paste(round(config$roi4_postnadir, 3), "s"),
                       paste(round(config$roi5_outgress, 3), "s"),
                       paste(round(data_end - roi5_end, 3), "s")),
          check.names = FALSE
        )
        
        return(list(
          table = roi_times_df,
          boundaries = c(data_start, roi1_start, roi2_start, roi3_start, 
                         roi4_start, roi5_start, roi5_end, data_end)
        ))
      }
      
      return(NULL)
    })
    
    # Display ROI table
    output$roi_table <- DT::renderDataTable({
      times <- roi_times()
      if (!is.null(times)) {
        DT::datatable(
          times$table,
          options = list(
            pageLength = 7,
            scrollX = TRUE,
            dom = 't',
            ordering = FALSE,
            searching = FALSE,
            paging = FALSE,
            info = FALSE
          ),
          rownames = FALSE,
          class = 'cell-border stripe compact'
        ) %>%
          DT::formatStyle(columns = 1:4, fontSize = '11px')
      }
    })
    
    # Show notification when nadir is not available for selected sensor (only once per sensor)
    last_nadir_warning <- reactiveVal("")
    
    observe({
      req(input$plot_sensor)
      nadir <- nadir_info()
      
      if (!nadir$available && last_nadir_warning() != input$plot_sensor) {
        showNotification(
          paste("No nadir data available for", input$plot_sensor, ". Sensor must be listed in summary file."),
          type = "warning",
          duration = 8
        )
        last_nadir_warning(input$plot_sensor)
      }
    })
    
    # Create delineated dataset
    observeEvent(input$create_delineated, {
      req(input$plot_sensor, roi_times(), roi_values$current_config)
      
      # Check if already delineated using shared function
      status <- get_sensor_status(input$plot_sensor, output_dir())
      if (status$delineated) {
        showModal(modalDialog(
          title = "Confirm Replacement",
          "Sensor data already delineated. Continue and replace delineated file?",
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_replace"), "Replace", class = "btn-warning")
          )
        ))
        return()
      }
      
      create_delineated_dataset()
    })
    
    # Confirm replacement
    observeEvent(input$confirm_replace, {
      req(roi_values$current_config)
      removeModal()
      create_delineated_dataset()
    })
    
    # Trim sensor button
    observeEvent(input$trim_sensor, {
      req(input$plot_sensor)
      
      # Check status using shared function
      status <- get_sensor_status(input$plot_sensor, output_dir())
      
      if (!status$delineated) {
        showNotification("No delineated dataset found. Please delineate dataset first.", type = "warning")
        return()
      }
      
      if (status$trimmed) {
        showNotification("Sensor data already trimmed. Delineate dataset again if you want to re-trim.", type = "warning")
        return()
      }
      
      # Read delineated data using shared function
      sensor_data <- read_sensor_data(output_dir(), input$plot_sensor, "delineated")
      
      if (is.null(sensor_data)) {
        showNotification("Failed to read delineated dataset.", type = "error")
        return()
      }
      
      # Check if trim levels exist
      if (!"roi" %in% names(sensor_data) || 
          !any(sensor_data$roi %in% c("trim_start", "trim_end"))) {
        showNotification("Sensor data already trimmed. Delineate dataset again if you want to re-trim.", type = "warning")
        return()
      }
      
      # Perform trimming
      trimmed_data <- sensor_data[!sensor_data$roi %in% c("trim_start", "trim_end"), ]
      
      # Save over existing delineated file
      delineated_path <- file.path(output_dir(), "csv", "delineated", paste0(input$plot_sensor, "_delineated.csv"))
      write.csv(trimmed_data, delineated_path, row.names = FALSE)
      
      # Update sensor index using shared function
      success <- safe_update_sensor_index(output_dir(), input$plot_sensor, list(trimmed = "Y"))
      
      if (success) {
        # Trigger data refresh
        roi_values$data_updated <- roi_values$data_updated + 1
        roi_values$summary_updated <- roi_values$summary_updated + 1
        
        showNotification("Sensor data trimmed successfully!", type = "message")
      } else {
        showNotification("Failed to update sensor index", type = "error")
      }
    })
    
    # Start over button
    observeEvent(input$start_over, {
      req(input$plot_sensor)
      
      # Remove delineated file
      delineated_path <- file.path(output_dir(), "csv", "delineated", paste0(input$plot_sensor, "_delineated.csv"))
      if (file.exists(delineated_path)) {
        file.remove(delineated_path)
      }
      
      # Reset flags in sensor index using shared function
      success <- safe_update_sensor_index(
        output_dir(), 
        input$plot_sensor,
        list(
          delineated = "N",
          trimmed = "N",
          roi_config = "NA"
        )
      )
      
      if (success) {
        # Trigger updates
        roi_values$summary_updated <- roi_values$summary_updated + 1
        roi_values$data_updated <- roi_values$data_updated + 1
        
        showNotification("Reset to original sensor file", type = "message")
      } else {
        showNotification("Failed to reset sensor status", type = "error")
      }
    })
    
    # Function to create delineated dataset
    create_delineated_dataset <- function() {
      tryCatch({
        # Read original data using shared function
        sensor_data <- read_sensor_data(output_dir(), input$plot_sensor, "min")
        
        if (is.null(sensor_data)) {
          showNotification("Source file not found", type = "error")
          return()
        }
        
        # Create delineated folder - always check/create fresh
        delineated_dir <- file.path(output_dir(), "csv", "delineated")
        if (!dir.exists(delineated_dir)) {
          dir.create(delineated_dir, showWarnings = FALSE, recursive = TRUE)
        }
        
        # Verify directory was created
        if (!dir.exists(delineated_dir)) {
          showNotification("Failed to create delineated directory", type = "error")
          return()
        }
        
        # Add ROI column
        times <- roi_times()
        if (is.null(times)) {
          showNotification("ROI times not available", type = "error")
          return()
        }
        
        boundaries <- times$boundaries
        
        sensor_data$roi <- cut(sensor_data$time_s, 
                               breaks = boundaries,
                               labels = c("trim_start", "roi1_ingress", "roi2_prenadir", 
                                          "roi3_nadir", "roi4_postnadir", "roi5_outgress", "trim_end"),
                               include.lowest = TRUE, right = FALSE)
        
        # Save delineated file
        output_file <- file.path(delineated_dir, paste0(input$plot_sensor, "_delineated.csv"))
        write.csv(sensor_data, output_file, row.names = FALSE)
        
        # Verify file was created
        if (!file.exists(output_file)) {
          showNotification("Failed to create delineated file", type = "error")
          return()
        }
        
        # Update sensor index using shared function
        success <- safe_update_sensor_index(
          output_dir(),
          input$plot_sensor,
          list(
            delineated = "Y",
            roi_config = roi_values$current_config$label,
            trimmed = "N"  # Reset trimmed status when re-delineating
          )
        )
        
        if (success) {
          # Trigger cache refresh by incrementing counter
          roi_values$summary_updated <- roi_values$summary_updated + 1
          roi_values$data_updated <- roi_values$data_updated + 1
          
          showNotification("Delineated dataset created successfully!", type = "message")
        } else {
          showNotification("Warning: Dataset created but failed to update index", type = "warning")
        }
        
      }, error = function(e) {
        showNotification(paste("Error creating delineated dataset:", e$message), 
                         type = "error")
      })
    }
    
    # Read selected sensor data (with preference for delineated data)
    selected_sensor_data <- reactive({
      req(input$plot_sensor)
      roi_values$data_updated  # Invalidate when data changes
      
      # Check for delineated file first
      delineated_data <- read_sensor_data(output_dir(), input$plot_sensor, "delineated")
      if (!is.null(delineated_data)) {
        return(delineated_data)
      }
      
      # Fall back to regular minimal data
      return(read_sensor_data(output_dir(), input$plot_sensor, "min"))
    })
    
    # Create ROI plot
    output$roi_plot <- renderPlotly({
      sensor_data <- selected_sensor_data()
      req(sensor_data)
      
      nadir <- nadir_info()
      req(nadir$available)
      
      times <- roi_times()
      
      # Fixed configuration: pressure (left), HIG acceleration (right)
      p <- plot_ly() %>%
        layout(
          title = paste("ROI Delineated:", input$plot_sensor),
          showlegend = FALSE,
          margin = list(l = 80, r = 80, t = 50, b = 50),
          xaxis = list(
            title = "Time [s]",
            showline = TRUE,
            linecolor = "black",
            linewidth = 1
          ),
          yaxis = list(
            title = "Pressure [kPa]",
            showline = TRUE,
            linecolor = "black",
            linewidth = 1
          ),
          yaxis2 = list(
            title = "HIG Acceleration [g]",
            overlaying = "y",
            side = "right",
            showline = TRUE,
            linecolor = "black",
            linewidth = 1
          )
        )
      
      # Add pressure trace
      p <- p %>% add_trace(
        x = sensor_data$time_s,
        y = sensor_data$pressure_kpa,
        name = "Pressure [kPa]",
        type = "scatter",
        mode = "lines",
        line = list(color = "black")
      )
      
      # Add HIG acceleration trace
      p <- p %>% add_trace(
        x = sensor_data$time_s,
        y = sensor_data$higacc_mag_g,
        name = "HIG Acceleration [g]",
        yaxis = "y2",
        type = "scatter",
        mode = "lines",
        line = list(color = "red")
      )
      
      # Add pressure nadir
      p <- p %>% add_trace(
        x = nadir$time,
        y = nadir$value,
        name = "Pressure Nadir",
        type = "scatter",
        mode = "markers+text",
        marker = list(color = "orange", size = 10),
        text = paste("Nadir:", round(nadir$value, 2), "kPa"),
        textposition = "top right",
        textfont = list(color = "orange")
      )
      
      # Add ROI boundary lines if we have ROI times
      if (!is.null(times)) {
        roi_labels <- c("", "ROI 1", "ROI 2", "ROI 3", "ROI 4", "ROI 5", "")
        
        for (i in 2:7) {  # Skip first and last boundaries (data start/end)
          p <- p %>% add_segments(
            x = times$boundaries[i], xend = times$boundaries[i],
            y = min(sensor_data$pressure_kpa), 
            yend = max(sensor_data$pressure_kpa),
            line = list(color = "blue", width = 2, dash = "dash"),
            showlegend = FALSE,
            hoverinfo = "text",
            text = paste(roi_labels[i])
          )
        }
      }
      
      return(p)
    })
  })
}