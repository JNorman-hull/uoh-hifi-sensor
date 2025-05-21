ui <- navbarPage(
  title = "UoH RAPID processing",
  
  # Combine shinyjs and JavaScript in the header
  header = tags$head(
    # Initialize shinyjs inside header
    shinyjs::useShinyjs(),
    
    # JavaScript for log updates
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateProcessLog', function(message) {
        var logElement = document.getElementById('process_log');
        if (logElement) {
          logElement.textContent = message.text;
          // Auto-scroll to bottom of log
          logElement.scrollTop = logElement.scrollHeight;
        }
      });
    "))
  ),
  
  # Navbar 1 - Main processing page
  tabPanel(
    "Process Sensors",
    
    # Keep sidebarLayout but make the sidebar content conditional
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        # File locations sidebar - only show when not on plots tab
        conditionalPanel(
          condition = "input.mainTabset !== 'plots'",
          h4("File Locations"),
          verbatimTextOutput("raw_data_location"),
          verbatimTextOutput("output_location"),
          
          hr(),
          
          # Add action button
          actionButton("process_btn", "Process Selected Sensors", 
                       class = "btn-primary btn-block")
        ),
        
        # Plot options sidebar - only show when on plots tab
        conditionalPanel(
          condition = "input.mainTabset === 'plots'",
          h4("Plot Options"),
          selectInput("plot_sensor", "Select Sensor:", choices = NULL),
          selectInput("left_y_var", "Left Y-Axis:",
                      choices = c("Pressure [kPa]" = "pressure_kpa",
                                  "HIG Acceleration [g]" = "higacc_mag_g",
                                  "Inertial Acceleration [m/s²]" = "inacc_mag_ms",
                                  "Rotational Magnitude [deg/s]" = "rot_mag_degs"),
                      selected = "pressure_kpa"),
          selectInput("right_y_var", "Right Y-Axis (Optional):",
                      choices = c("None" = "",
                                  "Pressure [kPa]" = "pressure_kpa",
                                  "HIG Acceleration [g]" = "higacc_mag_g",
                                  "Inertial Acceleration [m/s²]" = "inacc_mag_ms",
                                  "Rotational Magnitude [deg/s]" = "rot_mag_degs"),
                      selected = "higacc_mag_g"),
          checkboxInput("show_nadir", "Show Pressure Nadir", value = TRUE),
          
          hr(),
          
          # NEW - Plot Export Options
          h4("Plot Export Options"),
          selectInput("plot_filetype", "File Type:",
                      choices = c("PNG" = "png", 
                                  "SVG" = "svg", 
                                  "JPEG" = "jpeg"),
                      selected = "png"),
          numericInput("plot_dpi", "DPI:", 
                       value = 300, min = 72, max = 600, step = 1),
          fluidRow(
            column(6, numericInput("plot_width_cm", "Width (cm):", 
                                   value = 25, min = 5, max = 100, step = 1)),
            column(6, numericInput("plot_height_cm", "Height (cm):", 
                                   value = 15, min = 5, max = 100, step = 1))
          )
        )
      ),
      
      mainPanel(
        width = 9,
        
        # Add tabset panel in the main panel with proper ID
        tabsetPanel(
          id = "mainTabset",
          
          # Tab 1: File Selection - make sure to use exact tab values
          tabPanel(
            title = "File Selection",
            value = "file_selection", # Add a value for each tab
            h3("Sensor Selection"),
            
            # Select all checkbox
            checkboxInput("select_all", "Select All Sensors", value = FALSE),
            
            # Sensor selection UI
            uiOutput("sensor_checkboxes")
          ),
          
          # Tab 2: Processing Log
          tabPanel(
            title = "Processing Log",
            value = "processing_log", # This value will be used in updateTabsetPanel
            h3("Processing Log"),
            verbatimTextOutput("process_log")
          ),
          
          # Tab 3: Results Summary
          tabPanel(
            title = "Results Summary", 
            value = "results_summary",
            h3("Processing Results"),
            DT::dataTableOutput("results_table")
          ),
          
          # Tab 4: Plots - modified to be simpler since options moved to sidebar
          tabPanel(
            title = "Plots",
            value = "plots",
            h3("Interactive Plot"),
            plotlyOutput("sensor_plot", height = "600px")
          )
        )
      )
    )
  ),
  
  # Keep the existing placeholder tabs
  tabPanel(
    "Placeholder 1",
    h3("This page is reserved for future expansion")
  ),
  
  tabPanel(
    "Placeholder 2",
    h3("This page is reserved for future expansion")
  )
)