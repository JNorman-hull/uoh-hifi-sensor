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
  
  ## Sensor index and Dashboard page ##
  tabPanel(
    "Sensor dashboard",
    
    tabsetPanel(
      id = "dashboardTabset",
      
      # Placeholder tabs
      tabPanel(
        title = "Sensor index",
        value = "sensor_index",
        h3("Sensor Index"),
        p("This tab will display the sensor index information.")
      ),
      
      tabPanel(
        title = "Sensor dashboard", 
        value = "sensor_dashboard_tab",
        h3("Sensor Dashboard"),
        p("This tab will display sensor dashboard visualizations.")
      )
    )
  ),
  
  ## Sensor processing page##
  tabPanel(
    "Sensor processing",
    
    tabPanel(
      title = "Process raw data",
      value = "process_raw_data",
      
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
            id = "processingTabset",
            
            # ROI Delineation Tab
            tabPanel(
              title = "Process raw data",
              value = "process_raw_data",
              
              fileSelectionUI("file_selection"),
              
              hr(),
              
              # Processing Log in middle  
              processingUI("processing"),
              
              hr(),
              
              # Results Summary at bottom
              resultsUI("results"),
              
              hr()
            ),
            
            # Plots Tab
            tabPanel(
              title = "Add deployment information",
              value = "add_deployment_info",
              h3("Add Deployment Information"),
              p("This tab will allow users to add and edit deployment information for sensors.")
            )
          )
        )
       )
      )
    ),

  ## Time series page ##
  tabPanel(
    "Time series analysis",
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        uiOutput("dynamic_sidebar")
      ),
      
      mainPanel(
        width = 9,
        
        tabsetPanel(
          id = "visualizationTabset",
          
          # ROI Delineation Tab
          tabPanel(
            title = "Delineation and passage normalization",
            value = "roi_delineation",
            roiUI("roi")
          ),
          
          # Plots Tab
          tabPanel(
            title = "Plot and export",
            value = "interactive_plots",
            plotsUI("plots")
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Instrument analysis",
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
      ),
      
      mainPanel(
        width = 9,
        
        tabsetPanel(
          id = "InstrumentTabset",
          
          # Pressure Tab
          tabPanel(
            title = "Pressure analysis",
            value = "pres_analysis",
            h3("This page is reserved for pressure analysis")
            #roiUI("roi")
          ),
          
          # Plots Tab
          tabPanel(
            title = "Acceleration analysis",
            value = "acc_analysis",
            h3("This page is reserved for acceleration analysis")
            #plotsUI("plots")
          ),  
            
          # Plots Tab
          tabPanel(
            title = "Rotation analysis",
            value = "rot_analysis",
            h3("This page is reserved for rotation analysis")
          )
        )
      )
    )
  ),
  
  
  tabPanel(
    "Post-process analysis",
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
      ),
      
      mainPanel(
        width = 9,
        
        tabsetPanel(
          id = "PostprocessTabset",
          
          tabPanel(
            title = "Multivariate visualisation",
            value = "multivis_analysis",
            h3("This page is reserved for multivariate visualisation")
            #roiUI("roi")
          ),
          
          # Plots Tab
          tabPanel(
            title = "Data profiling analysis",
            value = "data_profiling",
            h3("This page is reserved for data profiling actions")
            #plotsUI("plots")
          ),  
          
          # Plots Tab
          tabPanel(
            title = "Statistical analysis",
            value = "statistical_analysis",
            h3("This page is reserved for statistical analysis")
          )
        )
      )
    )
  )
)