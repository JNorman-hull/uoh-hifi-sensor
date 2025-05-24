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
    
    # Cached summary data - reads file when processing completes OR when delineation happens
    summary_data_cache <- reactive({
      processing_complete()  # Invalidate when processing completes
      roi_values$summary_updated  # Invalidate when delineation creates/updates summary
      
      summary_file <- get_latest_summary_file(output_dir())
      if (!is.null(summary_file) && file.exists(summary_file)) {
        read.csv(summary_file)
      } else {
        NULL
      }
    })
    
    # Get processed sensors using shared function
    processed_sensors <- reactive({
      processing_complete()
      get_processed_sensors(output_dir())
    })
    
    # Update sensor dropdown
    observe({
      choices <- processed_sensors()
      current_choice <- input$plot_sensor
      
      if (length(choices) > 0) {
        selected_value <- if (!is.null(current_choice) && current_choice %in% choices) {
          current_choice
        } else {
          choices[1]
        }
        
        updateSelectInput(session, "plot_sensor", 
                          choices = choices, 
                          selected = selected_value)
      }
    })
    
    # Load ROI configurations using shared function
    observe({
      roi_values$roi_configs <- load_roi_configs(output_dir())
      
      if (length(roi_values$roi_configs) > 0) {
        choices <- names(roi_values$roi_configs)
        current_choice <- input$config_choice
        
        selected_value <- if (!is.null(current_choice) && current_choice %in% choices) {
          current_choice
        } else {
          choices[1]
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
    
    # Get nadir info using shared function
    nadir_info <- reactive({
      req(input$plot_sensor)
      get_nadir_info(input$plot_sensor, output_dir())
    })
    
    # Output for conditional panel
    output[[ns("nadir_available")]] <- reactive({
      nadir_info()$available
    })
    outputOptions(output, ns("nadir_available"), suspendWhenHidden = FALSE)
    
    # Calculate ROI times
    roi_times <- reactive({
      req(input$plot_sensor, roi_values$current_config)
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
      file_path <- file.path(output_dir(), "csv", paste0(input$plot_sensor, "_min.csv"))
      if (file.exists(file_path)) {
        sensor_data <- read.csv(file_path)
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
    
    observeEvent(input$trim_sensor, {
      req(input$plot_sensor)
      
      # Check if delineated file exists
      delineated_path <- file.path(output_dir(), "csv", "delineated", paste0(input$plot_sensor, "_delineated.csv"))
      
      if (!file.exists(delineated_path)) {
        showNotification("No delineated dataset found. Please delineate dataset first.", type = "warning")
        return()
      }
      
      # Read delineated data
      sensor_data <- read.csv(delineated_path)
      
      # Check if trim levels exist
      if (!"roi" %in% names(sensor_data) || 
          !any(sensor_data$roi %in% c("trim_start", "trim_end"))) {
        showNotification("Sensor data already trimmed. Delineate dataset again if you want to re-trim.", type = "warning")
        return()
      }
      
      # Perform trimming
      trimmed_data <- sensor_data[!sensor_data$roi %in% c("trim_start", "trim_end"), ]
      
      # Save over existing delineated file
      write.csv(trimmed_data, delineated_path, row.names = FALSE)
      
      # Trigger data refresh
      roi_values$data_updated <- roi_values$data_updated + 1
      
      showNotification("Sensor data trimmed successfully!", type = "message")
    })
    
    
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
    
    # Check if already delineated - check both summary file AND actual file existence
    is_already_delineated <- reactive({
      req(input$plot_sensor)
      
      # Check if delineated file actually exists in filesystem
      delineated_dir <- file.path(output_dir(), "csv", "delineated")
      delineated_file <- file.path(delineated_dir, paste0(input$plot_sensor, "_delineated.csv"))
      file_exists <- file.exists(delineated_file)
      
      # Also check summary file flag
      summary_flag <- FALSE
      summary_df <- summary_data_cache()
      if (!is.null(summary_df) && "delineated" %in% names(summary_df)) {
        sensor_row <- summary_df[summary_df$file == input$plot_sensor, ]
        if (nrow(sensor_row) > 0) {
          summary_flag <- (sensor_row$delineated == "Y")
        }
      }
      
      # Return TRUE only if both file exists AND summary says it's delineated
      return(file_exists && summary_flag)
    })
    
    # Create delineated dataset
    observeEvent(input$create_delineated, {
      req(input$plot_sensor, roi_times(), roi_values$current_config)
      
      # Check if already delineated
      if (is_already_delineated()) {
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
    
    # Function to create delineated dataset
    create_delineated_dataset <- function() {
      tryCatch({
        # Read original data
        file_path <- file.path(output_dir(), "csv", paste0(input$plot_sensor, "_min.csv"))
        
        if (!file.exists(file_path)) {
          showNotification(paste("Source file not found:", basename(file_path)), type = "error")
          return()
        }
        
        sensor_data <- read.csv(file_path)
        
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
        
        # Update summary file using shared function
        summary_file <- get_latest_summary_file(output_dir())
        if (!is.null(summary_file) && file.exists(summary_file)) {
          summary_df <- read.csv(summary_file)
          
          # Add delineated and roi_config columns if they don't exist
          if (!"delineated" %in% names(summary_df)) {
            summary_df$delineated <- "N"
          }
          if (!"roi_config" %in% names(summary_df)) {
            summary_df$roi_config <- ""
          }
          
          # Update this sensor
          sensor_idx <- which(summary_df$file == input$plot_sensor)
          if (length(sensor_idx) > 0) {
            summary_df$delineated[sensor_idx] <- "Y"
            summary_df$roi_config[sensor_idx] <- roi_values$current_config$label
            write.csv(summary_df, summary_file, row.names = FALSE)
            
            # Trigger cache refresh by incrementing counter
            roi_values$summary_updated <- roi_values$summary_updated + 1
          }
        }
        
        showNotification("Delineated dataset created successfully!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error creating delineated dataset:", e$message), 
                         type = "error")
      })
      
      roi_values$data_updated <- roi_values$data_updated + 1
    }
    
    # Read selected sensor data
    selected_sensor_data <- reactive({
      req(input$plot_sensor)
      roi_values$data_updated 
      
      file_path <- file.path(output_dir(), "csv", paste0(input$plot_sensor, "_min.csv"))
      delineated_path <- file.path(output_dir(), "csv", "delineated", paste0(input$plot_sensor, "_delineated.csv"))
      
      if (file.exists(delineated_path)) {
        return(read.csv(delineated_path))
      } else if (file.exists(file_path)) {
        return(read.csv(file_path))
      }
      
      return(NULL)
    })
    
    # Create ROI plot
    output$roi_plot <- renderPlotly({
      sensor_data <- selected_sensor_data()
      req(sensor_data)
      nadir <- nadir_info()
      req(nadir$available)
      if (!nadir$available) {
        return(plotly_empty() %>% layout(title = "No nadir data available for this sensor"))
      }
      
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
      
      # Add ROI boundary lines
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
            text = paste("Start of", roi_labels[i])
          )
        }
      }
      
      return(p)
    })
  })
load_roi_configs <- function(output_dir) {
  config_file <- file.path(output_dir, "roi_config.txt")
  
  if (file.exists(config_file)) {
    config_lines <- readLines(config_file)
    config_list <- list()
    
    for (line in config_lines) {
      if (nchar(trimws(line)) > 0 && !startsWith(trimws(line), "#")) {
        # Parse: Config_name, 1.1, 0.3, 0.2, 0.3, 1.1
        parts <- trimws(strsplit(line, ",")[[1]])
        if (length(parts) == 6) {
          config_name <- parts[1]
          config_list[[config_name]] <- list(
            label = config_name,
            roi1_ingress = as.numeric(parts[2]),
            roi2_prenadir = as.numeric(parts[3]),
            roi3_nadir = as.numeric(parts[4]),
            roi4_postnadir = as.numeric(parts[5]),
            roi5_outgress = as.numeric(parts[6])
          )
        }
      }
    }
    
    return(config_list)
  } else {
    # Create default config file with multiple configurations
    default_configs <- c(
      "Default_configuration, 1.1, 0.3, 0.2, 0.3, 1.1",
      "Quick_passage, 0.8, 0.2, 0.1, 0.2, 0.8",
      "Extended_analysis, 1.5, 0.5, 0.3, 0.5, 1.5"
    )
    writeLines(default_configs, config_file, sep = "\n")    
    # Return the default configs
    return(list(
      "Default_configuration" = list(
        label = "Default_configuration",
        roi1_ingress = 1.1, roi2_prenadir = 0.3, roi3_nadir = 0.2,
        roi4_postnadir = 0.3, roi5_outgress = 1.1
      ),
      "Quick_passage" = list(
        label = "Quick_passage",
        roi1_ingress = 0.8, roi2_prenadir = 0.2, roi3_nadir = 0.1,
        roi4_postnadir = 0.2, roi5_outgress = 0.8
      ),
      "Extended_analysis" = list(
        label = "Extended_analysis",
        roi1_ingress = 1.5, roi2_prenadir = 0.5, roi3_nadir = 0.3,
        roi4_postnadir = 0.5, roi5_outgress = 1.5
      )
    ))
  }
}
}