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
    
    # Keep sidebarLayout
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
                     class = "btn-primary btn-block")
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
          
          # Tab 4: Plots (placeholder for now)
          tabPanel(
            title = "Plots",
            value = "plots",
            h3("Interactive Plots"),
            p("Plot functionality will be implemented in the next phase.")
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