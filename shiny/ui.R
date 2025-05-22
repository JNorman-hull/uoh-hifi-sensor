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
            title = "Complete passage",
            value = "interactive_plots",
            plotsUI("plots")
          ),
          
          # Additional Analysis Tab
          tabPanel(
            title = "ROI delineation",
            value = "roi_delineation",
            h3("Trim and delineate sensor time series"),
            p("This tab is reserved for ROI delineation ")
          ),
          
          # Export Tab
          tabPanel(
            title = "Time normalization",
            value = "time_normalization",
            h3("Time normalization"),
            p("This tab is reserved for Time normalization.")
          )
        )
      )
    )
  ),
  
  # Placeholder for future expansion
  tabPanel(
    "Data analysis",
    h3("This page is reserved for data analysis")
  )
)