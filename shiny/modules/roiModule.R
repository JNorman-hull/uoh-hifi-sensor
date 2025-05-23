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
    
    conditionalPanel(
      condition = paste0("output.", ns("nadir_available"), " == true"),
      
      hr(),
      h4("ROI Configuration"),
      DT::dataTableOutput(ns("roi_table"), height = "200px"),
      
      hr(),
      actionButton(ns("create_delineated"), "Create delineated dataset", 
                   class = "btn-primary btn-block")
    ),
    
    br(),
    textOutput(ns("status_message"))
  )
}

roiServer <- function(id, output_dir, summary_data, processing_complete = reactive(FALSE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Values for ROI processing
    roi_values <- reactiveValues(
      roi_config = NULL,
      roi_times = NULL,
      delineated_created = FALSE
    )
    
    # Get processed sensors
    processed_sensors <- reactive({
      processing_complete()
      min_files <- list.files(path = file.path(output_dir(), "csv"), 
                              pattern = "_min\\.csv$", full.names = FALSE)
      sensor_names <- gsub("_min\\.csv$", "", min_files)
      return(sensor_names)
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
    
    # Load ROI configuration
    observe({
      config_file <- file.path(output_dir(), "roi_config.txt")
      
      if (file.exists(config_file)) {
        config_data <- readLines(config_file)
        if (length(config_data) > 0) {
          # Parse: Default_configuration, 1.1, 0.3, 0.2, 0.3, 1.1
          parts <- trimws(strsplit(config_data[1], ",")[[1]])
          if (length(parts) == 6) {
            roi_values$roi_config <- list(
              label = parts[1],
              roi1_ingress = as.numeric(parts[2]),
              roi2_prenadir = as.numeric(parts[3]),
              roi3_nadir = as.numeric(parts[4]),
              roi4_postnadir = as.numeric(parts[5]),
              roi5_outgress = as.numeric(parts[6])
            )
          }
        }
      } else {
        # Create default config
        default_config <- "Default_configuration, 1.1, 0.3, 0.2, 0.3, 1.1"
        writeLines(default_config, config_file)
        
        roi_values$roi_config <- list(
          label = "Default_configuration",
          roi1_ingress = 1.1,
          roi2_prenadir = 0.3,
          roi3_nadir = 0.2,
          roi4_postnadir = 0.3,
          roi5_outgress = 1.1
        )
      }
    })
    
    # Check nadir availability
    nadir_info <- reactive({
      req(input$plot_sensor)
      
      summary_files <- list.files(path = output_dir(), pattern = "batch_summary\\.csv$", full.names = TRUE)
      
      if (length(summary_files) > 0) {
        file_info <- file.info(summary_files)
        summary_files <- summary_files[order(file_info$mtime, decreasing = TRUE)]
        
        summary_file <- summary_files[1]
        if (file.exists(summary_file)) {
          summary_df <- read.csv(summary_file)
          
          if ("file" %in% names(summary_df)) {
            sensor_row <- summary_df[summary_df$file == input$plot_sensor, ]
            
            if (nrow(sensor_row) > 0) {
              possible_time_cols <- c("pres_min[time]", "pres_min.time.", "pres_min.time")
              possible_value_cols <- c("pres_min[kPa]", "pres_min.kPa.", "pres_min.kPa")
              
              time_col <- NULL
              value_col <- NULL
              
              for (col in names(sensor_row)) {
                if (col %in% possible_time_cols) time_col <- col
                if (col %in% possible_value_cols) value_col <- col
              }
              
              if (!is.null(time_col) && !is.null(value_col)) {
                return(list(
                  time = as.numeric(sensor_row[[time_col]]),
                  value = as.numeric(sensor_row[[value_col]]),
                  available = TRUE
                ))
              }
            }
          }
        }
      }
      
      return(list(available = FALSE))
    })
    
    # Output for conditional panel
    output$nadir_available <- reactive({
      nadir_info()$available
    })
    outputOptions(output, "nadir_available", suspendWhenHidden = FALSE)
    
    # Calculate ROI times
    roi_times <- reactive({
      req(input$plot_sensor, roi_values$roi_config)
      nadir <- nadir_info()
      
      if (!nadir$available) return(NULL)
      
      nadir_time <- nadir$time
      config <- roi_values$roi_config
      
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
          `Start time` = c(paste(round(data_start, 4), "s"),
                           paste(round(roi1_start, 4), "s"),
                           paste(round(roi2_start, 4), "s"),
                           paste(round(roi3_start, 4), "s"),
                           paste(round(roi4_start, 4), "s"),
                           paste(round(roi5_start, 4), "s"),
                           paste(round(roi5_end, 4), "s")),
          `End Time` = c(paste(round(roi1_start, 4), "s"),
                         paste(round(roi1_end, 4), "s"),
                         paste(round(roi2_end, 4), "s"),
                         paste(round(roi3_end, 4), "s"),
                         paste(round(roi4_end, 4), "s"),
                         paste(round(roi5_end, 4), "s"),
                         paste(round(data_end, 4), "s")),
          Duration = c(paste(round(roi1_start - data_start, 4), "s"),
                       paste(round(config$roi1_ingress, 4), "s"),
                       paste(round(config$roi2_prenadir, 4), "s"),
                       paste(round(config$roi3_nadir, 4), "s"),
                       paste(round(config$roi4_postnadir, 4), "s"),
                       paste(round(config$roi5_outgress, 4), "s"),
                       paste(round(data_end - roi5_end, 4), "s")),
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
            pageLength = 10,
            scrollX = TRUE,
            dom = 't',
            ordering = FALSE
          ),
          rownames = FALSE,
          class = 'cell-border stripe'
        ) %>%
          DT::formatStyle(columns = 1:4, fontSize = '12px')
      }
    })
    
    # Status messages
    output$status_message <- renderText({
      nadir <- nadir_info()
      
      if (!nadir$available) {
        return("No nadir data available. Sensor must be listed in summary file.")
      }
      
      if (roi_values$delineated_created) {
        return("Delineated dataset created successfully!")
      }
      
      return("")
    })
    
    # Check if already delineated
    is_already_delineated <- reactive({
      req(input$plot_sensor)
      
      summary_files <- list.files(path = output_dir(), pattern = "batch_summary\\.csv$", full.names = TRUE)
      
      if (length(summary_files) > 0) {
        summary_file <- summary_files[which.max(file.info(summary_files)$mtime)]
        summary_df <- read.csv(summary_file)
        
        if ("delineated" %in% names(summary_df)) {
          sensor_row <- summary_df[summary_df$file == input$plot_sensor, ]
          if (nrow(sensor_row) > 0) {
            return(sensor_row$delineated == "Y")
          }
        }
      }
      
      return(FALSE)
    })
    
    # Create delineated dataset
    observeEvent(input$create_delineated, {
      req(input$plot_sensor, roi_times())
      
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
      removeModal()
      create_delineated_dataset()
    })
    
    # Function to create delineated dataset
    create_delineated_dataset <- function() {
      tryCatch({
        # Read original data
        file_path <- file.path(output_dir(), "csv", paste0(input$plot_sensor, "_min.csv"))
        sensor_data <- read.csv(file_path)
        
        # Create delineated folder
        delineated_dir <- file.path(output_dir(), "csv", "delineated")
        dir.create(delineated_dir, showWarnings = FALSE, recursive = TRUE)
        
        # Add ROI column
        times <- roi_times()
        boundaries <- times$boundaries
        
        sensor_data$roi <- cut(sensor_data$time_s, 
                               breaks = boundaries,
                               labels = c("trim_start", "roi1_ingress", "roi2_prenadir", 
                                          "roi3_nadir", "roi4_postnadir", "roi5_outgress", "trim_end"),
                               include.lowest = TRUE, right = FALSE)
        
        # Save delineated file
        output_file <- file.path(delineated_dir, paste0(input$plot_sensor, "_delineated.csv"))
        write.csv(sensor_data, output_file, row.names = FALSE)
        
        # Update summary file
        summary_files <- list.files(path = output_dir(), pattern = "batch_summary\\.csv$", full.names = TRUE)
        if (length(summary_files) > 0) {
          summary_file <- summary_files[which.max(file.info(summary_files)$mtime)]
          summary_df <- read.csv(summary_file)
          
          # Add delineated column if it doesn't exist
          if (!"delineated" %in% names(summary_df)) {
            summary_df$delineated <- "N"
          }
          
          # Update this sensor
          sensor_idx <- which(summary_df$file == input$plot_sensor)
          if (length(sensor_idx) > 0) {
            summary_df$delineated[sensor_idx] <- "Y"
            write.csv(summary_df, summary_file, row.names = FALSE)
          }
        }
        
        roi_values$delineated_created <- TRUE
        
        # Reset status after delay
        shinyjs::delay(3000, {
          roi_values$delineated_created <- FALSE
        })
        
      }, error = function(e) {
        showNotification(paste("Error creating delineated dataset:", e$message), 
                         type = "error")
      })
    }
    
    # Read selected sensor data
    selected_sensor_data <- reactive({
      req(input$plot_sensor)
      
      file_path <- file.path(output_dir(), "csv", paste0(input$plot_sensor, "_min.csv"))
      
      if (!file.exists(file_path)) {
        return(NULL)
      }
      
      data <- read.csv(file_path)
      return(data)
    })
    
    # Create ROI plot
    output$roi_plot <- renderPlotly({
      sensor_data <- selected_sensor_data()
      req(sensor_data)
      nadir <- nadir_info()
      
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
}