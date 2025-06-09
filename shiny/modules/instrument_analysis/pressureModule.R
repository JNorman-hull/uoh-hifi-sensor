## Pressure helpers ####

# Function: rpc_find_max_pres 
# Find max pressure 1s before nadir
# Steps:
# For selected sensor, get pres_min.time. and pres_min.kPa. value for roi = roi4_nadir
# In the sensor data (_delineated), identify the maximum pressure_kpa 1 second before the nadir (2000 rows). Name values as pres_max_1s_nadir.kPa.and pres_max_1s_nadir.time.

# Function: rpc_calculate
# Calculate the rate of pressure change
# Steps: 
# For selected sensor, use rpc_find_max_pres to Find max pressure 1s before nadir
# Using values passed from rpc_find_max_pres, calculate rate pressure change as pres_min.kPa. - pres_max_1s_nadir.kPa. Name it as pres_rpc

# Function: lrpc_get_acclim
# Identify surface and depth acclimation values from the configuration
# Steps: 
# Retrieve the config values loaded from the configuration loader: acclim_pres_depth, acclim_pres_surface

#Function lrpc_calculate
# Retrieve the nadir value using value passed from rpc_find_max_pres
# Calculate Log Ratio Pressure change for each acclimation (surface, depth) passed by lrpc_get_acclim. Name them pres_lrpc_surface	pres_lrpc_depth
# Calculation is log(acclimation /nadir)


# Function: rate_ratio_analysis
#Calculate Rate and Log ratio pressure change 
# Use rpc_find_max_pres, rpc_calculate, lrpc_get_acclim and lrpc_calculate to calculate rate pressure change and ratio pressure change (surface, depth)
# return the values and Safely update the global instrument index and trigger global state function
#Use the status function to update the sensor status in the global index file, pres_rpc_processed,	pres_lrpc_processed we can also update all_pres_processed, as we require the pressure summary to be done ebfore rate/ratio

# Function: generate_pressure_text
# Generate the output text used in the Sensor pressure summary box
# Make sure global status tracked
# Get the required information from the instrument index 
# pres_lrpc_surface,	pres_lrpc_depth, pres_rpc, nadir = pres_min.kPa.(roi4_nadir)
# populate text fields " Pressure nadir = x kPa" "Rate pressure change = x kPa/s-1", "Log ratio pressure change: Surface acclimated = x, Depth acclimated = x"


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
          tags$h4("Empty", style = "margin-top: 0; color: #333;"),
          p("Empty")
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
          p("Provide all the summary information for the currently selected sensor here. RPC, LRPC (surface, depth), pressure nadir, ."),
          #tags$p(textOutput(ns("pressure_nadir_text"))),
          #tags$p(textOutput(ns("rpc_text"))),
          #tags$p(textOutput(ns("lrpc_surface_text")),
          #tags$p(textOutput(ns("lrpc_depth_text"))
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
                        show_pres_processed_sum = TRUE,
                        show_pres_processed_rpc = TRUE,
                        show_pres_processed_lrpc = TRUE),
        
        enhancedSensorSelectionUI(ns("sensor_selector"), status_filter_type = "pres_processed"),
        
        h4(""),
        
        summarytableSidebarUI(ns("pressure_summary")),
        
        actionButton(ns("rpc_lrpc_btn"), "Calculate RPC and LRPC", 
                     class = "btn-primary btn-block"),
        
        textOutput(ns("current_rpc_lrpc")),
        
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
                      default_right_var = "none") 
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
    
    #Modify this button state management from summarytableModule
    ## Requires pres_sum_processed status to be Y (e.g., use status function) before button is available
    # rpc_lrpc_btn only available when status$pres_sum_processed
    # if instrument_variable, "pres_rpc_processed" and "pres_lrpc_processed", then button state is "Recalculate RPC and LRPC"
    # observe({
    #   req(sensor_reactive(), instrument_variable)
    #   status <- sensor_status()
    #   
    #   # Check if summary already processed
    #   status_col <- paste0(instrument_variable, "_sum_processed")
    #   already_processed <- status[[status_col]] %||% FALSE
    #   
    #   # Button state: enabled when delineated and trimmed
    #   can_process <- status$delineated && status$trimmed
    #   
    #   button_states <- list(
    #     "process_summary" = can_process
    #   )
    #   
    #   manage_button_states(session, button_states)
    #   
    #   # Update button appearance and text
    #   if (already_processed) {
    #     updateActionButton(session, "process_summary", 
    #                        label = "Recalculate summary information")
    #     shinyjs::removeClass("process_summary", "btn-success")
    #     shinyjs::addClass("process_summary", "btn-warning")
    #   } else {
    #     updateActionButton(session, "process_summary", 
    #                        label = "Process summary information")
    #     shinyjs::removeClass("process_summary", "btn-warning") 
    #     shinyjs::addClass("process_summary", "btn-success")
    #   }
    # })
    
    #Modify this button state management from summarytableModule
    #if instrument_variable, "pres_rpc_processed" and "pres_lrpc_processed", then give modal to say "RPC and LRPC already calculated. Replace existing calculations?"
    
    # observeEvent(input$process_summary, {
    #   req(sensor_reactive(), instrument_variable)
    #   
    #   # Check if data already exists
    #   status <- sensor_status()
    #   status_col <- paste0(instrument_variable, "_sum_processed")
    #   already_processed <- status[[status_col]] %||% FALSE
    #   
    #   if (already_processed) {
    #     showModal(modalDialog(
    #       title = "Summary Data Exists",
    #       paste("Summary data already exists for", sensor_reactive(), 
    #             ". Replace existing summary data?"),
    #       footer = tagList(
    #         modalButton("Cancel"),
    #         actionButton(ns("confirm_replace_summary"), "Replace", class = "btn-warning")
    #       )
    #     ))
    #   } else {
    #     calculate_and_save_summary()
    #   }
    # })
    # 
    # observeEvent(input$confirm_replace_summary, {
    #   removeModal()
    #   calculate_and_save_summary()
    # })
    
 
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
    
    # Modify these outputs to use the helper function for pressure summary text
    # output$passage_duration_text <- renderText({
    #   generate_duration_text("passage_duration.mm.ss.", "Overall passage duration")
    # })
    # 
    # output$ingress_nadir_text <- renderText({
    #   generate_duration_text("ingress_nadir_duration.mm.ss.", "Sensor ingress to nadir")
    # })
    # 
    # output$nadir_outgress_text <- renderText({
    #   generate_duration_text("nadir_outgress_duration.mm.ss.", "Nadir to sensor outgress")
    # })
    
    
    #Modify this to be output status for RPC and LRPC calculation
    # output$current_rpc_lrpc <- renderText({
    #   req(sensor_reactive(), instrument_variable)
    #   status <- sensor_status()
    #   status_col <- paste0(instrument_variable, "_sum_processed")
    #   
    #   if (status[[status_col]] %||% FALSE) {
    #     paste("Summary information calculated for", sensor_reactive())
    #   } else {
    #     ""
    #   }
    # })
    
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
  