
plotsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Interactive Plot"),
    plotlyOutput(ns("sensor_plot"), height = "600px")
  )
}

plotsSidebarUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Plot Options"),
    selectInput(ns("plot_sensor"), "Select Sensor:", choices = NULL),
    selectInput(ns("left_y_var"), "Left Y-Axis:",
                choices = c("Pressure [kPa]" = "pressure_kpa",
                            "HIG Acceleration [g]" = "higacc_mag_g",
                            "Inertial Acceleration [m/s²]" = "inacc_mag_ms",
                            "Rotational Magnitude [deg/s]" = "rot_mag_degs"),
                selected = "pressure_kpa"),
    selectInput(ns("right_y_var"), "Right Y-Axis (Optional):",
                choices = c("None" = "",
                            "Pressure [kPa]" = "pressure_kpa",
                            "HIG Acceleration [g]" = "higacc_mag_g",
                            "Inertial Acceleration [m/s²]" = "inacc_mag_ms",
                            "Rotational Magnitude [deg/s]" = "rot_mag_degs"),
                selected = "higacc_mag_g"),
    checkboxInput(ns("show_nadir"), "Show Pressure Nadir", value = TRUE),
    checkboxInput(ns("show_legend"), "Show Legend", value = TRUE),
    
    hr(),
    
    h4("Plot Export Options"),
    checkboxInput(ns("use_default_export"), "Use Default Export Settings", value = FALSE),
    
    conditionalPanel(
      condition = paste0("!input.", ns("use_default_export")),
      selectInput(ns("plot_filetype"), "File Type:",
                  choices = c("PNG" = "png", 
                              "SVG" = "svg", 
                              "JPEG" = "jpeg"),
                  selected = "png"),
      numericInput(ns("plot_dpi"), "DPI:", 
                   value = 300, min = 72, max = 600, step = 1),
      fluidRow(
        column(6, numericInput(ns("plot_width_cm"), "Width (cm):", 
                               value = 16, min = 5, max = 100, step = 1)),
        column(6, numericInput(ns("plot_height_cm"), "Height (cm):", 
                               value = 10, min = 5, max = 100, step = 1))
      ),
      numericInput(ns("plot_font_size"), "Font Size (pt):", 
                   value = 10, min = 6, max = 24, step = 1),
      verbatimTextOutput(ns("plot_pixel_dimensions"))
    )
  )
}

plotsServer <- function(id, output_dir, summary_data, processing_complete = reactive(FALSE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Create a reactive expression to get the list of processed sensors
    processed_sensors <- reactive({
      # Add dependency on processing completion to trigger refresh
      processing_complete()
      
      min_files <- list.files(path = file.path(output_dir(), "csv"), 
                              pattern = "_min\\.csv$", full.names = FALSE)
      sensor_names <- gsub("_min\\.csv$", "", min_files)
      return(sensor_names)
    })
    
    # Update the sensor dropdown when processed sensors change
    observe({
      choices <- processed_sensors()
      current_choice <- input$plot_sensor
      
      if (length(choices) > 0) {
        # Preserve current selection if it still exists
        selected_value <- if (!is.null(current_choice) && current_choice %in% choices) {
          current_choice
        } else {
          choices[1]  # Default to first choice
        }
        
        updateSelectInput(session, "plot_sensor", 
                          choices = choices, 
                          selected = selected_value)
      }
    })
    
    # Create a reactive to read the selected sensor data
    selected_sensor_data <- reactive({
      req(input$plot_sensor)
      
      file_path <- file.path(output_dir(), "csv", paste0(input$plot_sensor, "_min.csv"))
      
      if (!file.exists(file_path)) {
        return(NULL)
      }
      
      data <- read.csv(file_path)
      return(data)
    })
    
    # Display calculated pixel dimensions for export
    output$plot_pixel_dimensions <- renderText({
      if (input$use_default_export) {
        return("")
      }
      
      width_px <- round(input$plot_width_cm / 2.54 * input$plot_dpi)
      height_px <- round(input$plot_height_cm / 2.54 * input$plot_dpi)
      
      paste0("Output dimensions: ", width_px, " × ", height_px, " pixels")
    })
    
    # Nadir info reactive function
    nadir_info <- reactive({
      req(input$plot_sensor)
      
      # Try to find nadir info in current session data first
      if (!is.null(summary_data()) && length(summary_data()) > 0) {
        selected_summary <- NULL
        for (summary in summary_data()) {
          if (summary$file == input$plot_sensor) {
            selected_summary <- summary
            break
          }
        }
        
        if (!is.null(selected_summary)) {
          return(list(
            time = as.numeric(selected_summary$`pres_min[time]`),
            value = as.numeric(selected_summary$`pres_min[kPa]`)
          ))
        }
      }
      
      # If not found in session data, try to read from most recent summary CSV
      summary_files <- list.files(path = output_dir(), pattern = "batch_summary\\.csv$", full.names = TRUE)
      
      if (length(summary_files) > 0) {
        file_info <- file.info(summary_files)
        summary_files <- summary_files[order(file_info$mtime, decreasing = TRUE)]
        
        summary_file <- summary_files[1]
        if (file.exists(summary_file)) {
          summary_df <- read.csv(summary_file, check.names = FALSE)
          
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
                  value = as.numeric(sensor_row[[value_col]])
                ))
              }
            }
          }
        }
      }
      
      return(NULL)
    })
    
    # Create the plot
    output$sensor_plot <- renderPlotly({
      sensor_data <- selected_sensor_data()
      req(sensor_data)
      
      var_names <- c("pressure_kpa", "higacc_mag_g", "inacc_mag_ms", "rot_mag_degs")
      var_labels <- c("Pressure [kPa]", "HIG Acceleration [g]", 
                      "Inertial Acceleration [m/s²]", "Rotational Magnitude [deg/s]")
      colors <- c("black", "red", "blue", "green")
      
      left_var <- input$left_y_var
      left_color <- colors[which(var_names == left_var)]
      left_label <- var_labels[which(var_names == left_var)]
      
      has_right_axis <- input$right_y_var != ""
      right_margin <- if (has_right_axis) 80 else 30
      
      p <- plot_ly() %>%
        layout(
          title = paste("Sensor Data:", input$plot_sensor),
          showlegend = input$show_legend,
          margin = list(l = 80, r = right_margin, t = 50, b = 50),
          xaxis = list(
            title = "Time [s]",
            showline = TRUE,
            linecolor = "black",
            linewidth = 1,
            showticklabels = TRUE,
            ticks = "outside",
            tickcolor = "black"
          ),
          yaxis = list(
            title = left_label,
            showline = TRUE,
            linecolor = "black",
            linewidth = 1,
            showticklabels = TRUE,
            ticks = "outside",
            tickcolor = "black"
          )
        )
      
      p <- p %>% add_trace(
        x = sensor_data$time_s,
        y = sensor_data[[left_var]],
        name = left_label,
        type = "scatter",
        mode = "lines",
        line = list(color = left_color)
      )
      
      if (has_right_axis) {
        right_var <- input$right_y_var
        right_color <- colors[which(var_names == right_var)]
        right_label <- var_labels[which(var_names == right_var)]
        
        p <- p %>% layout(
          yaxis2 = list(
            title = right_label,
            overlaying = "y",
            side = "right",
            showline = TRUE,
            linecolor = "black",
            linewidth = 1,
            showticklabels = TRUE,
            ticks = "outside",
            tickcolor = "black"
          )
        )
        
        p <- p %>% add_trace(
          x = sensor_data$time_s,
          y = sensor_data[[right_var]],
          name = right_label,
          yaxis = "y2",
          type = "scatter",
          mode = "lines",
          line = list(color = right_color)
        )
      }
      
      if (input$show_nadir && !is.null(nadir_info())) {
        nadir <- nadir_info()
        
        nadir_yaxis <- NULL
        if (left_var == "pressure_kpa") {
          nadir_yaxis <- "y"
        } else if (input$right_y_var == "pressure_kpa") {
          nadir_yaxis <- "y2"
        }
        
        if (!is.null(nadir_yaxis)) {
          p <- p %>% add_trace(
            x = nadir$time,
            y = nadir$value,
            name = "Pressure Nadir",
            type = "scatter",
            mode = "markers+text",
            marker = list(color = "orange", size = 10),
            text = paste("Nadir:", round(nadir$value, 2), "kPa"),
            textposition = "top right",
            textfont = list(color = "orange"),
            yaxis = nadir_yaxis
          )
        }
      }
      
      if (input$use_default_export) {
        p <- p %>% config(displaylogo = FALSE)
      } else {
        dpi_scale <- input$plot_dpi / 96
        
        p <- p %>% layout(
          font = list(size = input$plot_font_size),
          title = list(font = list(size = input$plot_font_size + 2)),
          xaxis = list(
            title = "Time [s]",
            showline = TRUE,
            linecolor = "black",
            linewidth = 1,
            showticklabels = TRUE,
            ticks = "outside",
            tickcolor = "black",
            tickfont = list(size = input$plot_font_size)
          ),
          yaxis = list(
            title = left_label,
            showline = TRUE,
            linecolor = "black",
            linewidth = 1,
            showticklabels = TRUE,
            ticks = "outside",
            tickcolor = "black",
            tickfont = list(size = input$plot_font_size)
          ),
          legend = list(font = list(size = input$plot_font_size))
        )
        
        if (has_right_axis) {
          p <- p %>% layout(
            yaxis2 = list(
              title = right_label,
              overlaying = "y",
              side = "right",
              showline = TRUE,
              linecolor = "black",
              linewidth = 1,
              showticklabels = TRUE,
              ticks = "outside",
              tickcolor = "black",
              tickfont = list(size = input$plot_font_size)
            )
          )
        }
        
        p <- p %>% config(
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = input$plot_filetype,
            filename = paste0(input$plot_sensor, "_plot"),
            width = round(input$plot_width_cm / 2.54 * input$plot_dpi),
            height = round(input$plot_height_cm / 2.54 * input$plot_dpi),
            scale = dpi_scale
          )
        )
      }
      
      return(p)
    })
  })
}