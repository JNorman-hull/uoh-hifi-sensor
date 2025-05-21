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
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        # Display file locations
        h4("File Locations"),
        verbatimTextOutput("raw_data_location"),
        verbatimTextOutput("output_location"),
        
        hr(),
        
        # Add action button
        actionButton("process_btn", "Process Selected Sensors", 
                     class = "btn-primary btn-block"),
        
        hr(),
        
        # Select all checkbox
        checkboxInput("select_all", "Select All Sensors", value = FALSE),
        
        # Sensor selection UI will be generated dynamically
        uiOutput("sensor_checkboxes")
      ),
      
      mainPanel(
        width = 9,
        
        # Processing logs
        h3("Processing Log"),
        verbatimTextOutput("process_log"),
        
        hr(),
        
        # Results table
        h3("Processing Results"),
        DT::dataTableOutput("results_table")
      )
    )
  ),
  
  # Navbar 2 - Placeholder
  tabPanel(
    "Placeholder 1",
    h3("This page is reserved for future expansion")
  ),
  
  # Navbar 3 - Placeholder
  tabPanel(
    "Placeholder 2",
    h3("This page is reserved for future expansion")
  )
)