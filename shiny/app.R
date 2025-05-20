# University of Hull RAPID import Shiny app

# Load the components of the Shiny app
source("global.R")  # This will load packages first
source("ui.R")      # This loads the UI definition
source("server.R")  # This loads the server function

# Launch the Shiny app
shinyApp(ui, server)