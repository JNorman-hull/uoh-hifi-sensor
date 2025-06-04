## File selection table UI ####
fileSelectionTableUI <- function(id, 
                                 title = "File Selection", 
                                 help_text = NULL,
                                 show_title = TRUE) {
  ns <- NS(id)
  
  tagList(
    if (show_title) {
      fluidRow(
        column(8, 
               h4(title),
               if (!is.null(help_text)) helpText(help_text)
        )
      )
    },
    DT::dataTableOutput(ns("selection_table"))
  )
}

## File selection controls UI (for sidebar) ####
fileSelectionControlsUI <- function(id, 
                                    show_select_all = TRUE,
                                    show_clear_all = TRUE,
                                    show_summary = TRUE,
                                    additional_controls = NULL) {
  ns <- NS(id)
  
  tagList(
    if (show_select_all) checkboxInput(ns("select_all"), "Select All", value = FALSE),
    if (show_clear_all) actionButton(ns("clear_all"), "Clear All", class = "btn-sm"),
    if (show_summary) textOutput(ns("selection_summary")),
    if (!is.null(additional_controls)) additional_controls
  )
}

## File selection table server ####
fileSelectionTableServer <- function(id, 
                                     sensor_data_reactive,
                                     highlight_sensors_reactive = reactive(NULL),
                                     enable_selection = TRUE,
                                     selection_mode = 'multiple',
                                     custom_formatting = NULL) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # State management
    state <- reactiveValues(
      selected_indices = integer(0)
    )
    
    # Render table
    output$selection_table <- DT::renderDataTable({
      sensor_data <- sensor_data_reactive()
      req(sensor_data)
      
      # Get rows to highlight
      highlight_sensors <- highlight_sensors_reactive()
      highlight_rows <- if (!is.null(highlight_sensors) && length(highlight_sensors) > 0 && "Filename" %in% names(sensor_data)) {
        which(sensor_data$Filename %in% highlight_sensors)
      } else {
        integer(0)
      }
      
      # Create highlight config
      highlight_config <- if (length(highlight_rows) > 0) {
        list(rows = highlight_rows, color = 'lightgreen')
      } else {
        list(rows = highlight_rows, color = 'orange')
      }
      
      # Create table using shared function
      dt <- create_sensor_table(
        table_data = sensor_data,
        enable_selection = enable_selection,
        selection_mode = selection_mode,
        highlight_config = highlight_config,
        page_length = 15,
        scroll_y = "560px",
        dom_options = 'tip',
        column_widths = get_sensor_table_column_widths(),
        state_save = TRUE,
        additional_formatting = custom_formatting
      )
      
      return(dt)
    })
    
    # Update selection when user clicks table rows
    observeEvent(input$selection_table_rows_selected, {
      state$selected_indices <- input$selection_table_rows_selected %||% integer(0)
    }, ignoreNULL = FALSE)
    
    # Helper function for proxy operations
    update_table_selection <- function(rows) {
      DT::dataTableProxy('selection_table', session = session) %>%
        DT::selectRows(rows)
    }
    
    # Handle Select All checkbox
    observeEvent(input$select_all, {
      sensor_data <- sensor_data_reactive()
      req(sensor_data)
      
      if (input$select_all) {
        state$selected_indices <- seq_len(nrow(sensor_data))
        update_table_selection(state$selected_indices)
      } else {
        isolate({
          if (length(state$selected_indices) == nrow(sensor_data)) {
            state$selected_indices <- integer(0)
            update_table_selection(state$selected_indices)
          }
        })
      }
    })
    
    # Handle Clear All button
    observeEvent(input$clear_all, {
      state$selected_indices <- integer(0)
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
    
    # Auto-update Select All checkbox
    observe({
      sensor_data <- sensor_data_reactive()
      req(sensor_data)
      
      total_sensors <- nrow(sensor_data)
      selected_count <- length(state$selected_indices)
      should_be_checked <- selected_count == total_sensors && total_sensors > 0
      
      isolate({
        if (!is.null(input$select_all) && input$select_all != should_be_checked) {
          updateCheckboxInput(session, "select_all", value = should_be_checked)
        }
      })
    })
    
    # Selection summary
    output$selection_summary <- renderText({
      sensor_data <- sensor_data_reactive()
      req(sensor_data)
      
      total <- nrow(sensor_data)
      selected <- length(state$selected_indices)
      
      paste0(selected, " of ", total, " sensors selected")
    })
    
    # Return values
    return(list(
      selected_indices = reactive(state$selected_indices),
      selected_items = reactive({
        sensor_data <- sensor_data_reactive()
        if (is.null(sensor_data) || length(state$selected_indices) == 0) {
          return(character(0))
        }
        if ("Filename" %in% names(sensor_data)) {
          sensor_data$Filename[state$selected_indices]
        } else {
          # Return first column if no Filename column
          sensor_data[[1]][state$selected_indices]
        }
      })
    ))
  })
}
