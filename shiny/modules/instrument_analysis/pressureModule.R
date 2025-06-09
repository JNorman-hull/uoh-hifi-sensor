pressureUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Introductory text at the top
    tagList(
      h3("Pressure Analysis"),
      plotModuleUI(ns("pressure_plot"), height = "600px"),
      br(),
    
    # Two smaller boxes side by side
    fluidRow(
      column(
        width = 6,
        div(
          style = "background-color: #f8f9fa; border: 1px solid #ccc; padding: 20px; 
                   border-radius: 5px; margin-bottom: 20px; margin-right: 10px;",
          tags$h4("RPC and LRPC Calculator", style = "margin-top: 0; color: #333;"),
          p("Calculate rate and ratio pressure change here. Add the pressure config file, check what value the surface and depth is set to. Have a slidebar to set the depth if known.
            Have two buttons - one for calculate rate pressure change (find max pressure 1s < nadir time, measure change). The output which is saved is printed under the button  'Max pressure (nadir - 1s) = max_pres_1s' It's only shown if the value does not = NA. Rate pressure change = x Kpa/s-1, again only shows value if not NA.
            One for calcxulate Log ratio pressure change (log of acclimation prersssure/nadir pressure). This is caculated using two acclimation pressures - one for surface, one for depth.
            Start over button which gives modal message for replacing previous pressure analysis if any of the check values = Y. When pressed it resets the pres sttus variables and sets any calculations back to NA")
        )
      ),
      column(
        width = 6,
        div(
          style = "background-color: #f8f9fa; border: 1px solid #ccc; padding: 20px; 
                   border-radius: 5px; margin-bottom: 20px;",
        summarytableModuleUI(ns("pressure_summary"))
        )
      )
    ),
    
    fluidRow(
      column(
        width = 6,
        div(
          style = "background-color: #f8f9fa; border: 1px solid #ccc; padding: 20px; 
                   border-radius: 5px; margin-bottom: 20px; margin-right: 10px;",
          tags$h4("Sensor pressure summary", style = "margin-top: 0; color: #333;"),
          p("Provide all the summary information for the currently selected sensor here. RPC, LRPC (surface, depth), pressure nadir, .")
        )
      ),
      column(
        width = 6,
        div(
          style = "background-color: #f8f9fa; border: 1px solid #ccc; padding: 20px; 
                   border-radius: 5px; margin-bottom: 20px;",
          tags$h4("Barotrauama assesment", style = "margin-top: 0; color: #333;"),
          p("Hav a species index table (baro.csv) which has (species, age, size, sample size, mortality threshold). Asses our paramters against all thresholds and produce a summary report which is printed in the box, as well as saved  in ./assesment.
          Only active when all calculations have bene done.
          Uses same logic as elsewhere for managing when data is overwritten etc.")
        )
      )
    )
  ))
}

#Method 1: helpText() - styled for instructions
#helpText("This sidebar controls the pressure configuration for sensor data."),

# Method 2: p() - regular paragraph
#p("Configure pressure parameters below:"),


pressureSidebarUI <- function(id) {
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
        
        h4("Pressure controls"),
        
        div(style = "color: #666; font-style: italic; margin-bottom: 15px;",
            "Select a sensor to begin pressure analysis."),
        
        #status display
        statusSidebarUI(ns("status_display"),
                        show_pres_processed = TRUE,
                        show_pres_processed_sum = TRUE),
        
        enhancedSensorSelectionUI(ns("sensor_selector"), status_filter_type = "pres_processed"),
        
        summarytableSidebarUI(ns("pressure_summary")),
        
        hr(),
        configurationSidebarUI(ns("pressure_config"), config_type = "pres", 
                               label = "Pressure configuration:"),
        
        h4("Pressure Parameters"),
        div(style = "margin-bottom: 10px;",
            numericInput(ns("acclim_pres_surface"), "Surface acclimation (kPa):", 
                         value = NULL, min = 0, max = 200, step = 0.1, width = "100%")),
        
        div(style = "margin-bottom: 10px;",
            numericInput(ns("acclim_pres_depth"), "Depth acclimation (kPa):", 
                         value = NULL, min = 0, max = 200, step = 0.1, width = "100%")),
        
        div(style = "margin-bottom: 10px;",
            numericInput(ns("hydrostatic_pressure"), "Hydrostatic pressure:", 
                         value = NULL, min = 0, max = 20, step = 0.01, width = "100%")),
        
        div(style = "margin-bottom: 15px;",
            textInput(ns("pressure_config_label"), "Configuration label:", 
                      value = "", width = "100%", placeholder = "e.g., Custom_depth_config")),
        
        actionButton(ns("save_pressure_config"), "Save Configuration", 
                     class = "btn-success btn-block"),
        
        textOutput(ns("pressure_config_status")),
        

        hr(), h4("Plot controls"),
        plotSidebarUI(ns("pressure_plot"), 
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
                      default_right_var = "none"),    
        
        hr(),
        
        actionButton(ns("add_deploy_btn"), "Add pressure Information", 
                     class = "btn-primary btn-block")
    )
  )
}

pressureServer <- function(id, raw_data_path, output_dir, processing_complete, 
                           session_state = NULL, global_sensor_state, 
                           trigger_data_update, trigger_summary_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #needs to load the instrument index and get all the pressure variables, perhaps should be a global function to load instrument data? not sure
    # needs to be able to write back to the instrument index, same logic as reading/writing the sensor index file for each operation we do
    # so perhaps has a global function which loads instrument index, identifies the necessary instrument variables (pres_ acc_, rot_), 
    # then any function we need can read and write the relevant instrument_var = for any operation required
    
    # ============================= #
    # /// Reactive values \\\ ####  
    # ============================= #   

# pressure state ####
    pressure_values <- reactiveValues(
      pressure_config = NULL,
      baseline_config = NULL,
      inputs_changed = FALSE
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
                                                     status_filter_type = "pres_processed",
                                                     session_state = session_state,
                                                     global_sensor_state = global_sensor_state,
                                                     trigger_summary_update = trigger_summary_update)  
    # Read selected sensor data
    selected_sensor_data <- reactive({
      req(sensor_selector$selected_sensor())
      global_sensor_state$data_updated
      global_sensor_state$summary_updated 

      
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
    # /// Pressure configuration \\\ ####  
    # ============================= #
    
    # Load pressure configurations
    pressure_config <- configurationServer("pressure_config",
                                           output_dir = output_dir,
                                           config_type = "pres",
                                           sensor_name = reactive(sensor_selector$selected_sensor()),
                                           auto_select_sensor_config = TRUE)
    
    # Store current config and populate inputs
    observe({
      pressure_values$pressure_config <- pressure_config$current_config()
      pressure_values$baseline_config <- pressure_values$pressure_config
      pressure_values$inputs_changed <- FALSE
      
      config <- pressure_values$pressure_config
      if (!is.null(config)) {
        updateTextInput(session, "pressure_config_label", value = config$label)
        updateNumericInput(session, "acclim_pres_surface", value = config$acclim_pres_surface)
        updateNumericInput(session, "acclim_pres_depth", value = config$acclim_pres_depth)
        updateNumericInput(session, "hydrostatic_pressure", value = config$hydrostatic_pressure)
      }
    })
    
    # Track changes in inputs
    observe({
      if (!is.null(pressure_values$baseline_config)) {
        config <- pressure_values$baseline_config
        
        inputs_changed <- (
          input$pressure_config_label != (config$label %||% "") ||
            input$acclim_pres_surface != (config$acclim_pres_surface %||% 0) ||
            input$acclim_pres_depth != (config$acclim_pres_depth %||% 0) ||
            input$hydrostatic_pressure != (config$hydrostatic_pressure %||% 0)
        )
        
        pressure_values$inputs_changed <- inputs_changed
      } else {
        # For no config, check if any field has content
        inputs_changed <- (
          nchar(trimws(input$pressure_config_label)) > 0 ||
            !is.null(input$acclim_pres_surface) ||
            !is.null(input$acclim_pres_depth) ||
            !is.null(input$hydrostatic_pressure)
        )
        
        pressure_values$inputs_changed <- inputs_changed
      }
    })
    
    # ============================= #
    # /// UI State management \\\ ####  
    # ============================= # 
    
    
    # Store current config in reactive values
    observe({
      pressure_values$pressure_config <- pressure_config$current_config()
    })
    

    # Enable/disable normalized checkbox based on sensor status  
    observe({
      req(sensor_selector$selected_sensor())
      status <- get_sensor_status(sensor_selector$selected_sensor(), output_dir())
      
      if (status$normalized) {
        shinyjs::enable(paste0("pressure_plot-show_normalized"))
      } else {
        shinyjs::disable(paste0("pressure_plot-show_normalized"))
        updateCheckboxInput(session, "pressure_plot-show_normalized", value = FALSE)
      }
    })
    
    # Enable/disable normalized checkbox based on sensor status  
    observe({
      req(sensor_selector$selected_sensor())
      global_sensor_state$summary_updated  # Add this line
      global_sensor_state$data_updated     # Add this line
      
      status <- get_sensor_status(sensor_selector$selected_sensor(), output_dir())
      
      if (status$delineated) {
        shinyjs::enable(paste0("pressure_plot-show_roi_markers"))
      } else {
        shinyjs::disable(paste0("pressure_plot-show_roi_markers"))
        updateCheckboxInput(session, "pressure_plot-show_roi_markers", value = FALSE)
      }
    })  
    
 
    # ============================= #
    # /// Event handlers \\\ ####  
    # ============================= # 
    
    # Auto-uncheck nadir when normalized is checked
    observeEvent(input$`pressure_plot-show_normalized`, {
      if (input$`pressure_plot-show_normalized`) {
        updateCheckboxInput(session, "pressure_plot-show_nadir", value = FALSE)
      }
    })
    
    
    # Handle pressure info addition
    observeEvent(input$add_deploy_btn, {
      if (!is.null(sensor_selector$selected_sensor()) && sensor_selector$selected_sensor() != "") {
        showNotification(paste("Adding pressure info for:", sensor_selector$selected_sensor()), type = "message")
      } else {
        showNotification("Please select a sensor first", type = "warning")
      }
    })
    
    # Save pressure configuration
    observeEvent(input$save_pressure_config, {
      config_name <- trimws(input$pressure_config_label)
      
      if (nchar(config_name) == 0) {
        showNotification("Please enter a configuration label", type = "error")
        return()
      }
      
      # Check if config already exists
      existing_configs <- pressure_config$all_configs()
      if (!is.null(existing_configs) && config_name %in% names(existing_configs)) {
        showModal(modalDialog(
          title = "Configuration Exists",
          paste("Configuration '", config_name, "' already exists. Replace existing configuration?"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_save_pressure_config"), "Replace", class = "btn-warning")
          ),
          size = "m"
        ))
      } else {
        save_pressure_configuration()
      }
    })
    
    # Confirm save pressure configuration
    observeEvent(input$confirm_save_pressure_config, {
      removeModal()
      save_pressure_configuration()
    })
    
    # Button state management for save pressure config
    observe({
      can_save <- pressure_values$inputs_changed && 
        !is.null(input$pressure_config_label) && 
        nchar(trimws(input$pressure_config_label)) > 0
      
      if (can_save) {
        shinyjs::enable("save_pressure_config")
      } else {
        shinyjs::disable("save_pressure_config")
      }
    })
    
    # ============================= #
    # /// Helper functions \\\ ####  
    # ============================= # 
    
    # Save pressure configuration function
    save_pressure_configuration <- function() {
      config_name <- trimws(input$pressure_config_label)
      
      # Create pressure configuration values
      pressure_config_values <- c(
        input$acclim_pres_surface,
        input$acclim_pres_depth,
        input$hydrostatic_pressure
      )
      
      # Save configuration using shared function
      success <- save_config_value(
        output_dir = output_dir(),
        config_type = "pres",
        key = config_name,
        value = pressure_config_values
      )
      
      if (success) {
        # Reload configurations and trigger global updates
        pressure_config$reload_configs()
        trigger_summary_update()
        
        # Reset change tracking
        pressure_values$inputs_changed <- FALSE
        pressure_values$baseline_config <- list(
          label = config_name,
          acclim_pres_surface = input$acclim_pres_surface,
          acclim_pres_depth = input$acclim_pres_depth,
          hydrostatic_pressure = input$hydrostatic_pressure
        )
        
        showNotification("Pressure configuration saved successfully!", type = "message")
      } else {
        showNotification("Failed to save pressure configuration", type = "error")
      }
    }
    
    # ============================= #
    # /// Output render \\\ ####  
    # ============================= #   
    
    # Pressure config status
    output$pressure_config_status <- renderText({
      if (pressure_values$inputs_changed) {
        "Configuration modified - click Save Configuration to save changes"
      } else {
        ""
      }
    })
  
# Pressure status display ####
    status_controls <- statusModuleServer("status_display",
                                          sensor_name_reactive = reactive(sensor_selector$selected_sensor()),
                                          output_dir_reactive = reactive(output_dir()),
                                          check_types = c("pres_processed", "pres_processed_sum"),
                                          invalidation_trigger = reactive(global_sensor_state$summary_updated),
                                          individual_outputs = TRUE)
    
# Pressure summary display ####
    summary_table <- summarytableModuleServer("pressure_summary", 
                                              sensor_reactive = reactive(sensor_selector$selected_sensor()),
                                              output_dir_reactive = reactive(output_dir()),
                                              instrument_variable = "pres",
                                              global_sensor_state = global_sensor_state,
                                              trigger_data_update = trigger_data_update,
                                              trigger_summary_update = trigger_summary_update)
    
# Pressure plot ####
    plot_controls <- plotModuleServer("pressure_plot", 
                                      sensor_data = selected_sensor_data,
                                      sensor_name = reactive(sensor_selector$selected_sensor()),
                                      nadir_info = nadir_info,
                                      roi_boundaries = roi_boundaries,
                                      right_var = reactive(input$`pressure_plot-right_y_var`),
                                      left_var = reactive(input$`pressure_plot-left_y_var`),
                                      plot_width = reactive(input$`pressure_plot-plot_width`),
                                      plot_height = reactive(input$`pressure_plot-plot_height`),
                                      show_nadir = reactive(input$`pressure_plot-show_nadir`),
                                      show_legend = reactive(input$`pressure_plot-show_legend`),
                                      show_normalized = reactive(input$`pressure_plot-show_normalized`),
                                      show_roi_markers = reactive(input$`pressure_plot-show_roi_markers`),
                                      title_prefix = "Pressure Analysis",
                                      plot_source = "pressure_plot"
    )
    return(list(
      selected_sensor = reactive(sensor_selector$selected_sensor())
    ))
    
  })
}  
  