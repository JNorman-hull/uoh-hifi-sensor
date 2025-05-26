# File Selection Module

fileSelectionUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(8, h3("Sensor Selection")),
      column(4, 
             div(style = "text-align: right; margin-top: 15px;",
                 textOutput(ns("selection_summary"))
             )
      )
    ),
    
    fluidRow(
      column(12,
             wellPanel(
               fluidRow(
                 column(6, checkboxInput(ns("select_all"), "Select All Sensors", value = FALSE)),
                 column(6, 
                        div(style = "text-align: right; padding-top: 5px;",
                            actionButton(ns("clear_all"), "Clear All", class = "btn-sm")
                        )
                 )
               )
             )
      )
    ),
    
    # Sensor selection table
    DT::dataTableOutput(ns("sensor_table"))
  )
}

fileSelectionServer <- function(id, raw_data_path) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      sensor_data = NULL,
      selected_rows = integer(0)
    )
    
    # Simplified sensor info parsing using Python function directly
    parse_sensor_info <- function(sensor_names) {
      map(sensor_names, function(name) {
        tryCatch({
          py$parse_filename_info(name)
        }, error = function(e) {
          list(
            sensor = if(nchar(name) >= 3) substr(name, 1, 3) else name,
            date_deploy = "Unknown",
            time_deploy = "Unknown"
          )
        })
      })
    }
    
    # Create sensor data table when path changes
    observe({
      req(raw_data_path())
      
      sensor_names <- get_sensor_names(raw_data_path())
      
      if (length(sensor_names) > 0) {
        sensor_info <- parse_sensor_info(sensor_names)
        
        values$sensor_data <- tibble(
          No. = seq_along(sensor_names),
          Filename = sensor_names,
          Sensor = map_chr(sensor_info, "sensor"),
          Date = map_chr(sensor_info, "date_deploy"),
          Time = map_chr(sensor_info, "time_deploy")
        )
      } else {
        values$sensor_data <- NULL
      }
      
      # Reset selections when data changes
      values$selected_rows <- integer(0)
    })
    
    # Render sensor table (stable - doesn't re-render on selection changes)
    output$sensor_table <- DT::renderDataTable({
      req(values$sensor_data)
      
      DT::datatable(
        values$sensor_data,
        selection = list(mode = 'multiple'),
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          scrollY = "400px",
          dom = 'tip',
          stateSave = TRUE,
          columnDefs = list(
            list(width = '60px', targets = 0),
            list(width = '200px', targets = 1),
            list(width = '80px', targets = 2:4)
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover'
      ) %>%
        DT::formatStyle(columns = 1:5, fontSize = '14px')
    })
    
    # Update selected rows from user clicks
    observeEvent(input$sensor_table_rows_selected, {
      values$selected_rows <- input$sensor_table_rows_selected
    })
    
    # Helper function for proxy operations
    update_table_selection <- function(rows) {
      DT::dataTableProxy('sensor_table', session = session) %>%
        DT::selectRows(rows)
    }
    
    # Handle Select All checkbox
    observeEvent(input$select_all, {
      req(values$sensor_data)
      
      new_selection <- if(input$select_all) seq_len(nrow(values$sensor_data)) else integer(0)
      values$selected_rows <- new_selection
      update_table_selection(new_selection)
    })
    
    # Handle Clear All button
    observeEvent(input$clear_all, {
      values$selected_rows <- integer(0)
      updateCheckboxInput(session, "select_all", value = FALSE)
      update_table_selection(integer(0))
    })
    
    # Update Select All checkbox based on current selection
    observe({
      req(values$sensor_data)
      
      total_sensors <- nrow(values$sensor_data)
      selected_count <- length(values$selected_rows)
      
      isolate({
        should_be_checked <- selected_count == total_sensors && total_sensors > 0
        current_state <- input$select_all
        
        if (!is.null(current_state) && current_state != should_be_checked) {
          updateCheckboxInput(session, "select_all", value = should_be_checked)
        }
      })
    })
    
    # Selection summary text
    output$selection_summary <- renderText({
      req(values$sensor_data)
      
      total <- nrow(values$sensor_data)
      selected <- length(values$selected_rows)
      
      paste0(selected, " of ", total, " sensors selected")
    })
    
    # Return selected sensor filenames
    selected_sensors <- reactive({
      req(values$sensor_data, length(values$selected_rows) > 0)
      values$sensor_data$Filename[values$selected_rows]
    })
    
    # Return available sensor names
    sensor_names <- reactive({
      if (is.null(values$sensor_data)) NULL else values$sensor_data$Filename
    })
    
    return(list(
      selected_sensors = selected_sensors,
      sensor_names = sensor_names
    ))
  })
}