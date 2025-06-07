accelerationUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Introductory text at the top
    tagList(
      h3("Acceleration Analysis"),
      plotModuleUI(ns("acceleration_plot"), height = "600px"),
      br(),
      
      # Two smaller boxes side by side
      fluidRow(
        column(
          width = 6,
          div(
            style = "background-color: #f8f9fa; border: 1px solid #ccc; padding: 20px; 
                   border-radius: 5px; margin-bottom: 20px; margin-right: 10px;",
            tags$h4("Strike calculator", style = "margin-top: 0; color: #333;"),
            p("Build strike tool here and peak finding here")
          )
        ),
        column(
          width = 6,
          div(
            style = "background-color: #f8f9fa; border: 1px solid #ccc; padding: 20px; 
                   border-radius: 5px; margin-bottom: 20px;",
            summarytableModuleUI(ns("acceleration_summary"))
          )
        )
      ),
      
      fluidRow(
        column(
          width = 6,
          div(
            style = "background-color: #f8f9fa; border: 1px solid #ccc; padding: 20px; 
                   border-radius: 5px; margin-bottom: 20px; margin-right: 10px;",
            tags$h4("Sensor acceleration summary", style = "margin-top: 0; color: #333;"),
            p("Provide all the summary information for the currently selected sensor here.")
          )
        ),
        column(
          width = 6,
          div(
            style = "background-color: #f8f9fa; border: 1px solid #ccc; padding: 20px; 
                   border-radius: 5px; margin-bottom: 20px;",
            tags$h4("Acceleration", style = "margin-top: 0; color: #333;"),
            p("Misc box")
          )
        )
      )
    ))
}



accelerationSidebarUI <- function(id) {
  ns <- NS(id)
  
  # CSS for subtle scrollbar that appears on interaction
  scroll_css <- HTML("
    <style>
      /* Hide scrollbar by default */
      .scrollable-sidebar::-webkit-scrollbar {
        width: 8px;
        background-color: transparent;
      }
      
      /* Show scrollbar track on hover or when scrolling */
      .scrollable-sidebar:hover::-webkit-scrollbar-track,
      .scrollable-sidebar:active::-webkit-scrollbar-track,
      .scrollable-sidebar:focus::-webkit-scrollbar-track {
        background: rgba(240,240,240,0.5);
        border-radius: 4px;
      }
      
      /* Show scrollbar thumb on hover or when scrolling */
      .scrollable-sidebar:hover::-webkit-scrollbar-thumb,
      .scrollable-sidebar:active::-webkit-scrollbar-thumb,
      .scrollable-sidebar:focus::-webkit-scrollbar-thumb {
        background: rgba(180,180,180,0.5);
        border-radius: 4px;
      }
      
      /* Show scrollbar thumb when scrolling */
      .scrollable-sidebar::-webkit-scrollbar-thumb:vertical:active {
        background: rgba(150,150,150,0.7);
      }
    </style>
  ")
  
  tagList(
    scroll_css,
    div(class = "scrollable-sidebar", 
        style = "height: 90vh; overflow-y: auto; padding-right: 5px;",
        
        h4("Acceleration controls"),
        
        div(style = "color: #666; font-style: italic; margin-bottom: 15px;",
            "Select a sensor to begin acceleration analysis."),
        
        #status display
        statusSidebarUI(ns("status_display"),
                        show_acc_processed = TRUE,
                        show_acc_processed_sum = TRUE),
        
        enhancedSensorSelectionUI(ns("sensor_selector"), status_filter_type = "acc_processed"),
        
        summarytableSidebarUI(ns("acceleration_summary")),
        
        hr(), h4("Plot controls"),
        plotSidebarUI(ns("acceleration_plot"), 
                      show_left_var = TRUE,   
                      show_right_var = TRUE,    
                      show_normalized = TRUE,   
                      show_nadir = TRUE,      
                      show_roi_markers = TRUE,   
                      show_legend = TRUE,
                      show_plot_width = TRUE,
                      show_plot_height = TRUE,
                      default_plot_height = 8,
                      default_plot_width = 16,
                      default_show_normalized = FALSE,
                      default_show_nadir = TRUE,
                      default_show_roi_markers = TRUE,
                      default_show_legend = FALSE,
                      default_left_var = "pressure_kpa",
                      default_right_var = "higacc_mag_g"),    
        
        hr(),
        
        actionButton(ns("add_deploy_btn"), "Add acceleration Information", 
                     class = "btn-primary btn-block")
    )
  )
}

accelerationServer <- function(id, raw_data_path, output_dir, processing_complete, session_state = NULL, 
                               global_sensor_state, 
                               trigger_data_update, trigger_summary_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #needs to load the instrument index and get all the acceleration variables, perhaps should be a global function to load instrument data? not sure
    # needs to be able to write back to the instrument index, same logic as reading/writing the sensor index file for each operation we do
    # so perhaps has a global function which loads instrument index, identifies the necessary instrument variables (pres_ acc_, rot_), 
    # then any function we need can read and write the relevant instrument_var = for any operation required
    
    # ============================= #
    # /// Reactive values \\\ ####  
    # ============================= #   
    
    # acceleration state ####
    acceleration_values <- reactiveValues(
      data_updated = 0            # Counter to trigger data refresh
    )
    
    # Get roi boundaries ####
    roi_boundaries <- reactive({
      get_roi_boundaries(sensor_selector$selected_sensor(), output_dir(), TRUE)
    })
    
    sensor_status <- reactive({
      req(sensor_selector$selected_sensor())
      global_sensor_state$summary_updated  # Use global
      global_sensor_state$data_updated     # Use global
      get_sensor_status(sensor_selector$selected_sensor(), output_dir())
    })
    # ============================= #
    # /// Data loading & processing  \\\ ####  
    # ============================= # 
    
    sensor_selector <- enhancedSensorSelectionServer("sensor_selector", output_dir,
                                                     processing_complete,
                                                     status_filter_type = "acc_processed",
                                                     session_state = session_state)
    
    # Read selected sensor data
    selected_sensor_data <- reactive({
      req(sensor_selector$selected_sensor())
      global_sensor_state$data_updated  
      
      
      # Check for delineated file first, fall back to minimal data
      delineated_data <- read_sensor_data(output_dir(), sensor_selector$selected_sensor(), "delineated")
      if (!is.null(delineated_data)) {
        return(delineated_data)
      }
      
      return(read_sensor_data(output_dir(), sensor_selector$selected_sensor(), "min"))
    })
    
    # Get nadir info using shared function
    nadir_info <- reactive({
      req(sensor_selector$selected_sensor())
      global_sensor_state$summary_updated  # Use global
      get_nadir_info(sensor_selector$selected_sensor(), output_dir())
    })
    
    # ============================= #
    # /// UI State management \\\ ####  
    # ============================= # 
    
    
    
    # Enable/disable normalized checkbox based on sensor status  
    observe({
      req(sensor_selector$selected_sensor())
      status <- get_sensor_status(sensor_selector$selected_sensor(), output_dir())
      
      if (status$normalized) {
        shinyjs::enable(paste0("acceleration_plot-show_normalized"))
      } else {
        shinyjs::disable(paste0("acceleration_plot-show_normalized"))
        updateCheckboxInput(session, "acceleration_plot-show_normalized", value = FALSE)
      }
    })
    
    # Enable/disable normalized checkbox based on sensor status  
    observe({
      req(sensor_selector$selected_sensor())
      status <- get_sensor_status(sensor_selector$selected_sensor(), output_dir())
      
      if (status$delineated) {
        shinyjs::enable(paste0("acceleration_plot-show_roi_markers"))
      } else {
        shinyjs::disable(paste0("acceleration_plot-show_roi_markers"))
        updateCheckboxInput(session, "acceleration_plot-show_roi_markers", value = FALSE)
      }
    })  
    
    # Auto-uncheck nadir when normalized is checked
    observeEvent(input$`acceleration_plot-show_normalized`, {
      if (input$`acceleration_plot-show_normalized`) {
        updateCheckboxInput(session, "acceleration_plot-show_nadir", value = FALSE)
      }
    })
    
    
    
    # ============================= #
    # /// Event handlers \\\ ####  
    # ============================= # 
    
    # Handle acceleration info addition
    observeEvent(input$add_deploy_btn, {
      if (!is.null(sensor_selector$selected_sensor()) && sensor_selector$selected_sensor() != "") {
        showNotification(paste("Adding acceleration info for:", sensor_selector$selected_sensor()), type = "message")
      } else {
        showNotification("Please select a sensor first", type = "warning")
      }
    })
    
    # ============================= #
    # /// Helper functions \\\ ####  
    # ============================= # 
    
    # Add helper functions here as needed
    
    # ============================= #
    # /// Output render \\\ ####  
    # ============================= #    
    
    
    
    # acceleration status display ####
    status_controls <- statusModuleServer("status_display",
                                          sensor_name_reactive = reactive(sensor_selector$selected_sensor()),
                                          output_dir_reactive = reactive(output_dir()),
                                          check_types = c("acc_processed", "acc_processed_sum"),
                                          invalidation_trigger = reactive(global_sensor_state$summary_updated),
                                          individual_outputs = TRUE)
    
    # acceleration summary display ####
    summary_table <- summarytableModuleServer("acceleration_summary", 
                                              sensor_reactive = reactive(sensor_selector$selected_sensor()),
                                              output_dir_reactive = reactive(output_dir()),
                                              instrument_variable = "acc",
                                              global_sensor_state = global_sensor_state,
                                              trigger_data_update = trigger_data_update,
                                              trigger_summary_update = trigger_summary_update)
    
    # acceleration plot ####
    plot_controls <- plotModuleServer("acceleration_plot", 
                                      sensor_data = selected_sensor_data,
                                      sensor_name = reactive(sensor_selector$selected_sensor()),
                                      nadir_info = nadir_info,
                                      roi_boundaries = roi_boundaries,
                                      right_var = reactive(input$`acceleration_plot-right_y_var`),
                                      left_var = reactive(input$`acceleration_plot-left_y_var`),
                                      plot_width = reactive(input$`acceleration_plot-plot_width`),
                                      plot_height = reactive(input$`acceleration_plot-plot_height`),
                                      show_nadir = reactive(input$`acceleration_plot-show_nadir`),
                                      show_legend = reactive(input$`acceleration_plot-show_legend`),
                                      show_normalized = reactive(input$`acceleration_plot-show_normalized`),
                                      show_roi_markers = reactive(input$`acceleration_plot-show_roi_markers`),
                                      title_prefix = "Acceleration Analysis",
                                      plot_source = "acceleration_plot"
    )
    return(list(
      selected_sensor = reactive(sensor_selector$selected_sensor())
    ))
    
  })
}  
