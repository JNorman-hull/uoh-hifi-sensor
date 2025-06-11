# File Selection Module - Using shared components

rawdataprocessingUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Add folder navigation section
    fluidRow(
      column(12,
             div(id = ns("breadcrumb_container"),
                 style = "margin-bottom: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
                 uiOutput(ns("breadcrumb_nav"))
             ),
             div(id = ns("folder_container"),
                 style = "margin-bottom: 15px;",
                 uiOutput(ns("folder_browser"))
             )
      )
    ),
    
    fileSelectionTableUI(
      ns("sensor_table"),
      help_text = "Index of RAW RAPID data files. Green = sensor processed. Orange = sensor requires processing."
    )
  )
}

rawdataprocessingsidebarUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Raw data prcoessing controls"),

    div(style = "color: #666; font-style: italic; margin-bottom: 15px;",
        "Select sensor(s) to process binary RAPID data."),
    div(style = "color: #666; font-style: bold; margin-bottom: 15px;",
        "Raw RAPID sensor data (.imp and .hig) should be added to ./raw_sens_data."),
    
    hr(),
    
    processinghelperUI(ns("processing_helper")),
    
    hr(),
    
    # Use shared controls
    fileSelectionControlsUI(
      ns("sensor_table"),
      show_select_all = TRUE,
      show_clear_all = TRUE,
      show_summary = TRUE
    ),
    
    br(),
    
    actionButton(ns("process_btn"), "Process Selected Sensors", 
                 class = "btn-primary btn-block")
  )
}

rawdataprocessingServer <- function(id, raw_data_path, output_dir, processing_complete = reactive(FALSE),
                                    global_sensor_state, trigger_data_update, trigger_summary_update, trigger_processing_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Add folder navigation state
    folder_state <- reactiveValues(
      current_path = "",  # Relative path from raw_data_path
      folder_history = character(0)
    )
    
    # Get current full path
    current_full_path <- reactive({
      base_path <- raw_data_path()
      if (folder_state$current_path == "") {
        return(base_path)
      } else {
        return(file.path(base_path, folder_state$current_path))
      }
    })
    
    # Get folders in current directory
    current_folders <- reactive({
      req(current_full_path())
      
      if (!dir.exists(current_full_path())) {
        return(character(0))
      }
      
      all_items <- list.files(current_full_path(), full.names = FALSE, include.dirs = TRUE)
      folders <- all_items[file.info(file.path(current_full_path(), all_items))$isdir]
      return(folders[!is.na(folders)])
    })
    
    folder_stats <- reactive({
      folders <- current_folders()
      processed <- processed_sensors()
      
      map(folders, function(folder) {
        folder_path <- file.path(current_full_path(), folder)
        
        # Count sensor files (.IMP files)
        imp_files <- list.files(folder_path, pattern = "\\.IMP$", full.names = TRUE, recursive = TRUE)
        sensor_count <- length(imp_files)
        
        # Calculate total size
        if (sensor_count > 0) {
          file_sizes <- file.info(imp_files)$size
          total_size_mb <- round(sum(file_sizes, na.rm = TRUE) / (1024^2), 1)
        } else {
          total_size_mb <- 0
        }
        
        # Get sensor names and count processed
        sensor_names <- tools::file_path_sans_ext(basename(imp_files))
        processed_count <- sum(sensor_names %in% processed)
        
        list(
          folder = folder,
          sensor_count = sensor_count,
          total_size_mb = total_size_mb,
          processed_count = processed_count
        )
      })
    })
    
    # Get processed sensors
    processed_sensors <- reactive({
      if (is.null(output_dir)) return(character(0))
      
      # Add global state invalidation
      global_sensor_state$summary_updated
      global_sensor_state$processing_updated
      
      tryCatch({
        processing_complete()
      }, error = function(e) {})
      
      index_df <- get_sensor_index_file(output_dir(), read_data = TRUE)
      if (is.null(index_df)) {
        return(character(0))
      }
      
      tryCatch({
        return(index_df$file)
      }, error = function(e) {
        return(character(0))
      })
    })
    
    # Prepare sensor data
    sensor_data <- reactive({
      req(current_full_path())
      global_sensor_state$processing_updated
      
      sensor_names <- get_sensor_names(current_full_path())
      
      if (length(sensor_names) == 0) return(NULL)
      
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
      
      tibble(
        No. = seq_along(sensor_names),
        Filename = sensor_names,
        Sensor = map_chr(sensor_info, ~ .x$sensor %||% "Unknown"),
        Date = map_chr(sensor_info, ~ .x$date_deploy %||% "Unknown"),
        Time = map_chr(sensor_info, ~ .x$time_deploy %||% "Unknown"),
        Status = ifelse(sensor_names %in% processed_sensors(), "Processed", "Requires Processing")
      )
    })
    
    
    
    # Use the shared table module
    # Add custom formatting function
    custom_table_formatting <- function(dt, table_data) {
      if (is.null(table_data) || nrow(table_data) == 0) {
        return(DT::datatable(
          data.frame(Message = "No raw sensor data found. Add .IMP and .HIG files to ./raw_sens_data"),
          options = list(dom = 't', ordering = FALSE),
          rownames = FALSE,
          selection = 'none'
        ))
      }
      
      # Apply conditional formatting based on Status column values
      dt <- dt %>% DT::formatStyle(
        "Status",  # Target the Status column
        target = 'row',  # Apply to entire row
        backgroundColor = DT::styleEqual(
          c("Processed", "Requires Processing"), 
          c("lightgreen", "orange")
        )
      )
      
      return(dt)
    }
    
    # Use custom formatting instead of highlight_sensors_reactive
    table_results <- fileSelectionTableServer(
      "sensor_table",
      sensor_data_reactive = sensor_data,
      highlight_sensors_reactive = reactive(NULL),  # No longer needed
      enable_selection = TRUE,
      selection_mode = 'multiple',
      custom_formatting = custom_table_formatting
    )
    
    observe({
      selected_sensors <- table_results$selected_items()
      is_processing <- processing_helper$is_processing()
      
      # Enable button only when sensors are selected AND not processing
      can_process <- length(selected_sensors) > 0 && !is_processing
      
      if (can_process) {
        shinyjs::enable("process_btn")
      } else {
        shinyjs::disable("process_btn")
      }
    })
    
    observeEvent(input$process_btn, {
      shinyjs::disable("process_btn")  # Disable immediately on click
      processing_helper$process_sensors()
    })
    
    observe({
      if (processing_helper$processing_complete()) {
        # Clear selection by updating the table proxy
        DT::dataTableProxy('sensor_table-selection_table', session = session) %>%
          DT::selectRows(integer(0))
      }
    })
    
    selected_sensors <- table_results$selected_items
    
    # Then call processinghelperServer
    processing_helper <- processinghelperServer("processing_helper", 
                                                selected_sensors, 
                                                current_full_path,
                                                output_dir,
                                                global_sensor_state,
                                                trigger_data_update,
                                                trigger_summary_update,
                                                trigger_processing_update)
    
    
    # Render breadcrumb navigation
    output$breadcrumb_nav <- renderUI({
      path_parts <- if (folder_state$current_path == "") {
        "Raw sensor data"
      } else {
        strsplit(folder_state$current_path, "/")[[1]]
      }
      
      breadcrumb_items <- list()
      breadcrumb_items[[1]] <- actionLink(ns("nav_root"), "Raw sensor data", style = "color: #007bff;")
      
      if (length(path_parts) > 0 && path_parts[1] != "Raw sensor data") {
        for (i in seq_along(path_parts)) {
          cumulative_path <- paste(path_parts[1:i], collapse = "/")
          breadcrumb_items[[i + 1]] <- span(" / ")
          breadcrumb_items[[i + 2]] <- actionLink(ns(paste0("nav_", i)), path_parts[i], 
                                                  onclick = paste0("Shiny.setInputValue('", ns("navigate_to"), "', '", cumulative_path, "', {priority: 'event'});"),
                                                  style = "color: #007bff;")
        }
      }
      
      do.call(tagList, breadcrumb_items)
    })
    
    # Render folder browser
    output$folder_browser <- renderUI({
      folders <- current_folders()
      stats <- folder_stats()
      
      # Create navigation elements
      nav_elements <- list()
      
      # Add "Back to Parent" button if not in root
      if (folder_state$current_path != "") {
        parent_path <- if (grepl("/", folder_state$current_path)) {
          dirname(folder_state$current_path)
        } else {
          ""
        }
        
        nav_elements <- append(nav_elements, list(
          div(
            class = "parent-nav-tile",
            style = "
          border: 2px solid #6c757d;
          border-radius: 8px;
          padding: 15px;
          margin: 8px;
          background-color: #f8f9fa;
          cursor: pointer;
          transition: all 0.3s ease;
          min-width: 200px;
          display: inline-block;
          vertical-align: top;
        ",
            onclick = paste0("Shiny.setInputValue('", ns("navigate_to"), "', '", parent_path, "', {priority: 'event'});"),
            onmouseover = "this.style.backgroundColor = '#e9ecef'; this.style.transform = 'translateY(-2px)';",
            onmouseout = "this.style.backgroundColor = '#f8f9fa'; this.style.transform = 'translateY(0px)';",
            
            # Back icon and title
            div(
              style = "margin-bottom: 8px;",
              icon("arrow-left", style = "color: #6c757d; font-size: 24px; margin-right: 8px;"),
              span("Back to Parent", style = "font-weight: bold; font-size: 16px; color: #333;")
            ),
            
            # Parent directory info
            div(
              if (parent_path == "") "Return to root directory" else paste("Return to", basename(parent_path)),
              style = "font-style: italic; color: #666; font-size: 13px;"
            )
          )
        ))
      }
      
      # Add folder tiles if any exist
      if (length(folders) > 0) {
        folder_tiles <- map(stats, function(stat) {
          # Create content summary
          content_text <- if (stat$sensor_count > 0) {
            paste0(stat$sensor_count, " sensor files. Total size = ", stat$total_size_mb, "MB")
          } else {
            "No sensor files"
          }
          
          # Create processing status
          process_text <- if (stat$sensor_count > 0) {
            paste0(stat$processed_count, "/", stat$sensor_count, " sensor files processed")
          } else {
            ""
          }
          
          # Create tile
          div(
            class = "folder-tile",
            style = "
          border: 2px solid #007bff;
          border-radius: 8px;
          padding: 15px;
          margin: 8px;
          background-color: #f8f9fa;
          cursor: pointer;
          transition: all 0.3s ease;
          min-width: 200px;
          display: inline-block;
          vertical-align: top;
        ",
            onclick = paste0("
          Shiny.setInputValue('", ns("enter_folder"), "', '", stat$folder, "', {priority: 'event'});
          this.style.backgroundColor = '#e3f2fd';
          setTimeout(() => this.style.backgroundColor = '#f8f9fa', 200);
        "),
            onmouseover = "this.style.backgroundColor = '#e7f3ff'; this.style.transform = 'translateY(-2px)';",
            onmouseout = "this.style.backgroundColor = '#f8f9fa'; this.style.transform = 'translateY(0px)';",
            
            # Folder icon and title
            div(
              style = "margin-bottom: 8px;",
              icon("folder", style = "color: #007bff; font-size: 24px; margin-right: 8px;"),
              span(stat$folder, style = "font-weight: bold; font-size: 16px; color: #333;")
            ),
            
            # Content summary
            if (content_text != "") {
              div(
                content_text,
                style = "font-style: italic; color: #666; font-size: 13px; margin-bottom: 5px;"
              )
            },
            
            # Processing status
            if (process_text != "") {
              div(
                process_text,
                style = paste0(
                  "font-size: 12px; font-weight: 500; color: ",
                  if (stat$processed_count == stat$sensor_count) "#28a745" else "#6c757d",
                  ";"
                )
              )
            }
          )
        })
        
        nav_elements <- append(nav_elements, folder_tiles)
      }
      
      # Show message if no folders and in subdirectory
      if (length(folders) == 0 && folder_state$current_path != "") {
        nav_elements <- append(nav_elements, list(
          p("No subfolders in this directory", style = "color: #666; font-style: italic; margin: 8px;")
        ))
      } else if (length(folders) == 0 && folder_state$current_path == "") {
        nav_elements <- append(nav_elements, list(
          p("No subfolders found", style = "color: #666; font-style: italic; margin: 8px;")
        ))
      }
      
      tagList(
        p("Raw RAPID data index:", style = "margin-bottom: 15px; font-weight: bold;"),
        div(
          style = "display: flex; flex-wrap: wrap; gap: 5px;",
          nav_elements
        )
      )
    })
    
    # Handle folder navigation
    observeEvent(input$enter_folder, {
      new_path <- if (folder_state$current_path == "") {
        input$enter_folder
      } else {
        file.path(folder_state$current_path, input$enter_folder)
      }
      folder_state$current_path <- new_path
    })
    
    observeEvent(input$nav_root, {
      folder_state$current_path <- ""
    })
    
    observeEvent(input$navigate_to, {
      folder_state$current_path <- input$navigate_to
    })
    
    
    return(list(
      selected_sensors = table_results$selected_items,
      sensor_names = reactive({
        data <- sensor_data()
        if (is.null(data)) character(0) else data$Filename
      }),
      process_trigger = reactive(input$process_btn),
      # Return processing helper outputs
      processing_complete = processing_helper$processing_complete,
      newly_processed_sensors = processing_helper$newly_processed_sensors,
      summary_data = processing_helper$summary_data
    ))
  })
}