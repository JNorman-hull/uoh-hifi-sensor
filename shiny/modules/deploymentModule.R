deploymentUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    h4("Blank"),
    #Main panel UI here
    fluidRow(
      column(8, h3("Add Deployment Information"),
             p("This tab will allow users to add and edit deployment information for sensors.")), 
      
      hr(),
      
      column(12, h3("Add Deployment Information"),
             p("This tab will allow users to add and edit deployment information for sensors.")),
      
      hr(),
      
      column(4, h3("Add Deployment Information"),
             p("This tab will allow users to add and edit deployment information for sensors."))
    )
  )
}


deploymentSidebarUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Deployment controls"),
    hr(),
    
    actionButton("add_deploy_btn", "Add deployment Information", 
                 class = "btn-primary btn-block")
  )
}



deploymentServer <- function(id, raw_data_path, output_dir = NULL, processing_complete = reactive(FALSE)) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    
  })
}

