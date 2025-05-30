ui <- navbarPage(
  title = tags$div(
    tags$span( style = "color: red; vertical-align: sub; font-weight: bold; margin: 0; padding: 0;", "RAP",
              tags$sub(style = "color: blue;  vertical-align: sup; font-style: italic; ; margin: 0; padding: 0;", "pro"))
  ),
  id = "mainTabset",
  footer = tags$div(
    "Hull International Fisheries Institute (2025)", 
    style = "font-size: 12px; color: #666; text-align: center;"
  ),
  
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
    title = "Sensor processing",
    value = "sensor_processing",
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        uiOutput("processing_sidebar")
      ),
      
      mainPanel(
        width = 9,
        
        tabsetPanel(
          id = "processingTabset",
          
          tabPanel(
            title = "Process raw data",
            value = "process_raw_data",
            fileSelectionUI("file_selection"),
            hr(),
            processingUI("processing"),
            hr(),
            resultsUI("results"),
            hr()
          ),
          
          tabPanel(
            title = "Add deployment information",
            value = "add_deployment_info",
            deploymentUI("deployment_info")
          )
        )
      )
    )
  ),

  ## Time series page ##
  tabPanel(
    title = "Time series analysis",
    value = "time_series_analysis",
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        uiOutput("time_sidebar")
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
        pressureSidebarUI("pressure")
        
      ),
      
      mainPanel(
        width = 9,
        
        tabsetPanel(
          id = "InstrumentTabset",
          
          # Pressure Tab
          tabPanel(
            title = "Pressure analysis",
            value = "pres_analysis",
            pressureUI("pressure")
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