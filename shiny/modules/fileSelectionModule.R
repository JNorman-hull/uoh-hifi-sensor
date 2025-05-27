# File Selection Module - Rebuilt

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

fileSelectionServer <- function(id, raw_data_path, output_dir = NULL, processing_complete = reactive(FALSE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Single source of truth for all state
    state <- reactiveValues(
      sensor_data = NULL,
      selected_indices = integer(0)
    )
    
    processed_sensors <- reactive({
      if (is.null(output_dir)) return(character(0))
      
      processing_complete()  # Invalidate when processing completes
      
      index_file <- get_sensor_index_file(output_dir())
      if (is.null(index_file) || !file.exists(index_file)) {
        return(character(0))
      }
      
      tryCatch({
        index_df <- read.csv(index_file)
        return(index_df$file)
      }, error = function(e) {
        return(character(0))
      })
    })
    # Initialize sensor data once (simplified but keep functionality)
    observe({
      req(raw_data_path())
      
      sensor_names <- get_sensor_names(raw_data_path())
      
      if (length(sensor_names) > 0) {
        sensor_info <- map(sensor_names, function(name) {
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
        
        state$sensor_data <- tibble(
          No. = seq_along(sensor_names),
          Filename = sensor_names,
          Sensor = map_chr(sensor_info, "sensor"),
          Date = map_chr(sensor_info, "date_deploy"),
          Time = map_chr(sensor_info, "time_deploy")
        )
        
      }
      
      # Reset selection when data changes
      state$selected_indices <- integer(0)
    })
    
    # Render stable table (temporarily without highlighting to fix temp file error)
    output$sensor_table <- DT::renderDataTable({
      req(state$sensor_data)
      
      processed <- processed_sensors()
      
      dt <- DT::datatable(
        state$sensor_data,
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
      
      # Highlight processed sensors in orange
      if (length(processed) > 0) {
        processed_rows <- which(state$sensor_data$Filename %in% processed)
        if (length(processed_rows) > 0) {
          dt <- dt %>% DT::formatStyle(
            columns = 1:5,
            target = 'row',
            backgroundColor = DT::styleRow(processed_rows, 'orange')
          )
        }
      }
      
      return(dt)
    })
    
    # Update selection when user clicks table rows
    observeEvent(input$sensor_table_rows_selected, {
      state$selected_indices <- input$sensor_table_rows_selected %||% integer(0)
    }, ignoreNULL = FALSE)
    
    # Helper function for proxy operations
    update_table_selection <- function(rows) {
      DT::dataTableProxy('sensor_table', session = session) %>%
        DT::selectRows(rows)
    }
    
    # Handle Select All checkbox
    observeEvent(input$select_all, {
      req(state$sensor_data)
      
      if (input$select_all) {
        state$selected_indices <- seq_len(nrow(state$sensor_data))
        update_table_selection(state$selected_indices)
      } else {
        isolate({
          if (length(state$selected_indices) == nrow(state$sensor_data)) {
            state$selected_indices <- integer(0)
            update_table_selection(state$selected_indices)
          }
        })
      }
    })
    
    # Handle Clear All button
    observeEvent(input$clear_all, {
      state$selected_indices <- integer(0)
      
      # Update checkbox and table
      updateCheckboxInput(session, "select_all", value = FALSE)
      update_table_selection(integer(0))
    })
    
    # Update Clear All button state
    observe({
      if (length(state$selected_indices) > 0) {
        shinyjs::enable("clear_all")
      } else {
        shinyjs::disable("clear_all")
      }
    })
    
    # Auto-update Select All checkbox based on selection
    observe({
      req(state$sensor_data)
      
      total_sensors <- nrow(state$sensor_data)
      selected_count <- length(state$selected_indices)
      should_be_checked <- selected_count == total_sensors && total_sensors > 0
      
      # Only update if different from current state to avoid loops
      isolate({
        if (!is.null(input$select_all) && input$select_all != should_be_checked) {
          updateCheckboxInput(session, "select_all", value = should_be_checked)
        }
      })
    })
    
    # Selection summary
    output$selection_summary <- renderText({
      req(state$sensor_data)
      
      total <- nrow(state$sensor_data)
      selected <- length(state$selected_indices)
      
      paste0(selected, " of ", total, " sensors selected")
    })
    
    # Return selected sensors
    selected_sensors <- reactive({
      if (is.null(state$sensor_data) || length(state$selected_indices) == 0) {
        return(character(0))
      }
      state$sensor_data$Filename[state$selected_indices]
    })
    
    # Return all sensor names
    sensor_names <- reactive({
      if (is.null(state$sensor_data)) character(0) else state$sensor_data$Filename
    })
    
    return(list(
      selected_sensors = selected_sensors,
      sensor_names = sensor_names
    ))
  })
}