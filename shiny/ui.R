ui <- navbarPage(
  title = "UoH RAPID processing",
  
  # Combine shinyjs and JavaScript in the header
  header = tags$head(
    # Initialize shinyjs inside header
    shinyjs::useShinyjs(),
    
    # JavaScript for log updates and processing completion
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateProcessLog', function(message) {
        var logElement = document.getElementById('processing-process_log');
        if (logElement) {
          logElement.textContent = message.text;
          // Auto-scroll to bottom of log
          logElement.scrollTop = logElement.scrollHeight;
        }
      });
      
      Shiny.addCustomMessageHandler('processingComplete', function(message) {
        // Can add additional UI feedback when processing completes
        console.log('Processing completed successfully');
      });
    "))
  ),
  
  # Main processing page
  tabPanel(
    "Process Sensors",
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        h4("File Locations"),
        verbatimTextOutput("raw_data_location"),
        verbatimTextOutput("output_location"),
        
        hr(),
        
        actionButton("process_btn", "Process Selected Sensors", 
                     class = "btn-primary btn-block")
      ),
      
      mainPanel(
        width = 9,
        
        tabsetPanel(
          id = "mainTabset",
          
          # File Selection Tab
          tabPanel(
            title = "File Selection",
            value = "file_selection",
            fileSelectionUI("file_selection")
          ),
          
          # Processing Log Tab
          tabPanel(
            title = "Processing Log",
            value = "processing_log",
            processingUI("processing")
          ),
          
          # Results Summary Tab
          tabPanel(
            title = "Results Summary", 
            value = "results_summary",
            resultsUI("results")
          )
        )
      )
    )
  ),
  
  # Data Visualization page (formerly Placeholder 1)
  tabPanel(
    "Data Visualization",
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        plotsSidebarUI("plots")
      ),
      
      mainPanel(
        width = 9,
        
        tabsetPanel(
          id = "visualizationTabset",
          
          # Plots Tab
          tabPanel(
            title = "Interactive Plots",
            value = "interactive_plots",
            plotsUI("plots")
          ),
          
          # Additional Analysis Tab
          tabPanel(
            title = "Data Analysis",
            value = "data_analysis",
            h3("Data Analysis"),
            p("This tab is reserved for additional data analysis features.")
          ),
          
          # Export Tab
          tabPanel(
            title = "Export Options",
            value = "export_options",
            h3("Export Options"),
            p("This tab is reserved for batch export and report generation features.")
          )
        )
      )
    )
  ),
  
  # Placeholder for future expansion
  tabPanel(
    "Settings",
    h3("This page is reserved for application settings and configuration")
  )
)