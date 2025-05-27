plotsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Complete sensor passage"),
    plotlyOutput(ns("sensor_plot"), height = "600px")
  )
}

plotsSidebarUI <- function(id) {
  ns <- NS(id)
  
  # Get sensor variables for dropdown choices
  sensor_vars <- get_sensor_variables()
  var_choices <- setNames(sensor_vars$names, sensor_vars$labels)
  
  tagList(
    h4("Plot Options"),
    selectInput(ns("plot_sensor"), "Select Sensor:", choices = NULL),
    selectInput(ns("left_y_var"), "Left Y-Axis:",
                choices = var_choices,
                selected = "pressure_kpa"),
    selectInput(ns("right_y_var"), "Right Y-Axis:",
                choices = c("None" = "none", var_choices),
                selected = "none"),
    
    hr(),
    
    h4("Display Options"),
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
    
    # Get processed sensors using shared function
    processed_sensors <- reactive({
      processing_complete()
      get_processed_sensors(output_dir())
    })
    
    # Update sensor dropdown using shared function
    observe({
      update_sensor_dropdown(session, "plot_sensor", processed_sensors(), input$plot_sensor)
    })
    
    # Read sensor data using shared function
    selected_sensor_data <- reactive({
      req(input$plot_sensor)
      read_sensor_data(output_dir(), input$plot_sensor, "min")
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
    
    # Nadir info using shared function with reactivity
    nadir_info <- reactive({
      req(input$plot_sensor)
      get_nadir_info(input$plot_sensor, output_dir())
    })
    
    # Create the plot using shared plotting function
    output$sensor_plot <- renderPlotly({
      sensor_data <- selected_sensor_data()
      req(sensor_data)
      
      nadir <- nadir_info()
      
      # Create plot using shared function
      p <- create_sensor_plot(
        sensor_data = sensor_data,
        sensor_name = input$plot_sensor,
        plot_config = "standard",
        left_var = input$left_y_var,
        right_var = input$right_y_var,
        nadir_info = nadir,
        show_nadir = input$show_nadir,
        show_legend = input$show_legend,
        plot_source = "plots_nadir_plot"
      )
      
      # Configure export settings
      if (input$use_default_export) {
        p <- p %>% config(displaylogo = FALSE)
      } else {
        dpi_scale <- input$plot_dpi / 96
        
        p <- p %>% layout(
          font = list(size = input$plot_font_size),
          title = list(font = list(size = input$plot_font_size + 2))
        ) %>%
          config(
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