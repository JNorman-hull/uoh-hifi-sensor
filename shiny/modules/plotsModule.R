plotsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Complete sensor passage"),
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
    selectInput(ns("right_y_var"), "Right Y-Axis:",
                choices = c("None" = "none",
                            "Pressure [kPa]" = "pressure_kpa",
                            "HIG Acceleration [g]" = "higacc_mag_g",
                            "Inertial Acceleration [m/s²]" = "inacc_mag_ms",
                            "Rotational Magnitude [deg/s]" = "rot_mag_degs"),
                selected = "none"),
    
    hr(),
    
    h4("Pressure Nadir Options"),
    checkboxInput(ns("show_nadir"), "Show Pressure Nadir", value = TRUE),
    verbatimTextOutput(ns("current_nadir_display")),
    actionButton(ns("edit_nadir_btn"), "Select Pressure Nadir", class = "btn-warning btn-sm"),
    actionButton(ns("save_nadir_btn"), "Save Pressure Nadir", class = "btn-success btn-sm"),
    actionButton(ns("cancel_nadir_btn"), "Cancel", class = "btn-danger btn-sm"),
    textOutput(ns("nadir_status")),
    
    hr(),
    
    h4("Plot Export Options"),
    checkboxInput(ns("show_legend"), "Show Legend", value = TRUE),
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
    
    # Nadir editing values
    nadir_values <- reactiveValues(
      edit_mode = FALSE,
      selected_point = NULL,
      nadir_updated = 0,
      baseline_click = NULL  # Store the click data that exists when entering edit mode
    )
    
    # Get processed sensors using shared function
    processed_sensors <- reactive({
      processing_complete()
      get_processed_sensors(output_dir())
    })
    
    # Update the sensor dropdown when processed sensors change
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
    
    # Nadir info using shared function
    nadir_info <- reactive({
      req(input$plot_sensor)
      nadir_values$nadir_updated  # Force refresh when nadir is updated
      
      get_nadir_info(input$plot_sensor, output_dir())
    })
    
    # Display current nadir
    output$current_nadir_display <- renderText({
      nadir <- nadir_info()
      if (nadir$available) {
        paste0("Time: ", round(nadir$time, 3), "s\nPressure: ", round(nadir$value, 2), " kPa")
      } else {
        "No nadir data available"
      }
    })
    
    # Initialize button states
    observe({
      if (nadir_values$edit_mode) {
        shinyjs::disable("edit_nadir_btn")
        shinyjs::enable("save_nadir_btn")
        shinyjs::enable("cancel_nadir_btn")
      } else {
        shinyjs::enable("edit_nadir_btn")
        shinyjs::disable("save_nadir_btn")
        shinyjs::disable("cancel_nadir_btn")
      }
    })
    
    # Edit nadir button
    observeEvent(input$edit_nadir_btn, {
      nadir_values$edit_mode <- TRUE
      nadir_values$selected_point <- NULL
      # Store whatever click data currently exists as "baseline" to ignore
      nadir_values$baseline_click <- event_data("plotly_click", source = "nadir_plot")
    })
    
    # Cancel nadir button
    observeEvent(input$cancel_nadir_btn, {
      nadir_values$edit_mode <- FALSE
      nadir_values$selected_point <- NULL
    })
    
    # Save nadir button
    observeEvent(input$save_nadir_btn, {
      req(nadir_values$selected_point)
      
      summary_file <- get_latest_summary_file(output_dir())
      if (!is.null(summary_file) && file.exists(summary_file)) {
        summary_df <- read.csv(summary_file)
        row_idx <- which(summary_df$file == input$plot_sensor)
        
        if (length(row_idx) > 0) {
          summary_df[row_idx, "pres_min.time."] <- nadir_values$selected_point$x
          summary_df[row_idx, "pres_min.kPa."] <- nadir_values$selected_point$y
          write.csv(summary_df, summary_file, row.names = FALSE)
          
          nadir_values$nadir_updated <- nadir_values$nadir_updated + 1
          nadir_values$edit_mode <- FALSE
          nadir_values$selected_point <- NULL
        }
      }
    })
    
    # Handle click events for nadir selection
    observe({
      if (nadir_values$edit_mode) {
        click_data <- event_data("plotly_click", source = "nadir_plot")
        if (!is.null(click_data)) {
          # Only respond if this click is different from the baseline we stored
          if (is.null(nadir_values$baseline_click) ||
              click_data$x != nadir_values$baseline_click$x ||
              click_data$y != nadir_values$baseline_click$y) {
            nadir_values$selected_point <- list(x = click_data$x, y = click_data$y)
          }
        }
      }
    })
    
    # Status display
    output$nadir_status <- renderText({
      if (nadir_values$edit_mode) {
        if (!is.null(nadir_values$selected_point)) {
          paste0("Selected: ", round(nadir_values$selected_point$y, 2), " kPa at ", 
                 round(nadir_values$selected_point$x, 3), "s")
        } else {
          "Edit mode: Click on plot to select nadir"
        }
      } else {
        ""
      }
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
      
      has_right_axis <- input$right_y_var !="none"
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
      
      if (input$show_nadir) {
        nadir <- nadir_info()
        if (nadir$available) {
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
      }
      
      # Add selected point if in edit mode
      if (nadir_values$edit_mode && !is.null(nadir_values$selected_point)) {
        p <- p %>% add_trace(
          x = nadir_values$selected_point$x,
          y = nadir_values$selected_point$y,
          name = "Selected Nadir",
          type = "scatter",
          mode = "markers+text",
          marker = list(color = "purple", size = 12, symbol = "diamond"),
          text = paste("New:", round(nadir_values$selected_point$y, 2), "kPa"),
          textposition = "top center",
          textfont = list(color = "purple"),
          showlegend = FALSE
        )
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
      
      p$x$source <- "nadir_plot"
      p <- p %>% event_register("plotly_click")
      
      return(p)
    })
  })
}