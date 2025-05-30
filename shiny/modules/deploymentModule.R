deploymentUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    h4("Blank"),
    #Main panel UI here
    fluidRow(
      column(12, h3("Add Deployment Information"),
             p("This tab will allow users to add and edit deployment information for sensors.")),
      
      hr(),

    )
  )
}


deploymentSidebarUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Deployment controls"),
    hr(),
    
    actionButton(ns("add_deploy_btn"), "Add deployment Information", 
                 class = "btn-primary btn-block")
                 
  )
}



deploymentServer <- function(id, raw_data_path, output_dir, processing_complete) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    
    # ============================= #
    # /// Reactive values \\\ ####  
    # ============================= #   
    
    
    
    # ============================= #
    # /// Event handlers \\\ ####  
    # ============================= # 
    
##header ####
    observeEvent(input$add_deploy_btn, {
      # Handle deployment info addition
      showNotification("Deployment info functionality coming soon!", type = "message")
    })
    
    # ============================= #
    # /// Helper functions \\\ ####  
    # ============================= # 
    
    
    
    
    # ============================= #
    # /// Output render \\\ ####  
    # ============================= #   
    
    # Return any reactive values other modules might need
    return(list())
  })
}

