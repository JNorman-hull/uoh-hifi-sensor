# ============================= #
# /// Shared Plot Module \\\ ####  
# ============================= #

## Variable definitions #####
get_sensor_variables <- function() {
  list(
    names = c("pressure_kpa", "higacc_mag_g", "inacc_mag_ms", "rot_mag_degs"),
    labels = c("Pressure [kPa]", "HIG Acceleration [g]", 
               "Inertial Acceleration [m/s²]", "Rotational Magnitude [deg/s]"),
    colors = c("black", "red", "blue", "green")
  )
}

# Get ROI boundaries for plots ####

## Get ROI boundaries ####
get_roi_boundaries <- function(sensor_name, output_dir, show_roi = FALSE) {
  if (!show_roi || is.null(sensor_name) || sensor_name == "") {
    return(NULL)
  }
  
  # Check if sensor is delineated and trimmed
  status <- get_sensor_status(sensor_name, output_dir)
  if (!status$delineated || !status$trimmed) {
    return(NULL)
  }
  
  # Read delineated data
  sensor_data <- read_sensor_data(output_dir, sensor_name, "delineated")
  if (is.null(sensor_data) || !"roi" %in% names(sensor_data)) {
    return(NULL)
  }
  
  # ROI levels for trimmed data
  roi_levels <- c("roi1_sens_ingress", "roi2_inflow_passage", 
                  "roi3_prenadir", "roi4_nadir", "roi5_postnadir", 
                  "roi6_outflow_passage", "roi7_sens_outgress")
  
  # Create boundaries array to match plot function expectations (10 elements)
  boundaries <- numeric(10)
  boundaries[1] <- min(sensor_data$time_s)  # Data start
  boundaries[10] <- max(sensor_data$time_s)  # Data end
  
  # Find ROI start times
  for (i in seq_along(roi_levels)) {
    roi_data <- sensor_data[sensor_data$roi == roi_levels[i], ]
    if (nrow(roi_data) > 0) {
      boundaries[i + 1] <- min(roi_data$time_s)  # boundaries[2] through boundaries[8]
    }
  }
  
  # ROI 7 end time (boundary[9])
  roi7_data <- sensor_data[sensor_data$roi == "roi7_sens_outgress", ]
  if (nrow(roi7_data) > 0) {
    boundaries[9] <- max(roi7_data$time_s)
  }
  
  return(boundaries)
}

plotModuleUI <- function(id, height = "600px") {
  ns <- NS(id)
  
  tagList(
    plotlyOutput(ns("plot"), height = height)
  )
}

plotSidebarUI <- function(id, 
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
                          default_show_roi_markers = FALSE,
                          default_show_legend = FALSE,
                          default_left_var = "pressure_kpa",
                          default_right_var = "none") {
  ns <- NS(id)
  
  # Get sensor variables for dropdown choices
  sensor_vars <- get_sensor_variables()
  var_choices <- setNames(sensor_vars$names, sensor_vars$labels)
  
  tagList(
    if (show_left_var) {
      selectInput(ns("left_y_var"), "Left Y-Axis:",
                  choices = var_choices,
                  selected = default_left_var)
    },
    
    if (show_right_var) {
      selectInput(ns("right_y_var"), "Right Y-Axis:",
                  choices = c("None" = "none", var_choices),
                  selected = default_right_var)
    },
    
    if (show_plot_width) {
      div(style = "color: #666; font-style: italic; margin-bottom: 15px;",
          "Enter plot width for figure export.")
      textInput(ns("plot_width"), "Export plot width:", value = default_plot_width*37.8)
    },
    #37.8 = cm x pixels at 96 DPI
    
    if (show_plot_height) {
      div(style = "color: #666; font-style: italic; margin-bottom: 15px;",
                               "Enter plot height for figure export.")
      textInput(ns("plot_height"), "Export plot height:", value = default_plot_height*37.8)
    },
    
    
    if (show_normalized) {
      checkboxInput(ns("show_normalized"), "Show normalized time series", value = default_show_normalized)
    },
    
    if (show_nadir) {
      checkboxInput(ns("show_nadir"), "Show Pressure Nadir", value = default_show_nadir)
    },
    
    if (show_roi_markers) {
      checkboxInput(ns("show_roi_markers"), "Show ROI markers", value = default_show_roi_markers)
    },
    
    if (show_legend) {
      checkboxInput(ns("show_legend"), "Show Legend", value = default_show_legend)
    }
  )
}

plotModuleServer <- function(id, 
                             sensor_data,
                             sensor_name,
                             nadir_info = reactive(NULL),
                             selected_nadir = reactive(NULL),
                             roi_boundaries = reactive(NULL),
                             left_var = reactive("pressure_kpa"),
                             right_var = reactive("none"),
                             show_nadir = reactive(FALSE),
                             show_legend = reactive(FALSE),
                             show_normalized = reactive(FALSE),
                             show_roi_markers = reactive(FALSE),
                             plot_width = reactive(16*37.8),
                             plot_height = reactive(8*37.8),
                             plot_source = "plot",
                             title_prefix = "Sensor Data",
                             # Internal logic parameters (not user controls)
                             custom_edit_mode = reactive(FALSE),
                             is_normalized_view = reactive(FALSE),
                             custom_traces = reactive(NULL)) {
  
  moduleServer(id, function(input, output, session) {
    
    # Determine which time variable to use
    time_var <- reactive({
      if (show_normalized() && !is.null(sensor_data()) && "time_norm" %in% names(sensor_data())) {
        "time_norm"
      } else {
        "time_s"
      }
    })
    
    # Internal logic for suppressing ROI lines (not user controlled)
    suppress_roi_lines <- reactive({
      current_time_var <- time_var()
      isTRUE(custom_edit_mode()) || (current_time_var == "time_norm")
    })
    
    # Determine if ROI markers should be shown (user control + boundaries exist + not suppressed)
    show_roi_lines <- reactive({
      user_wants_roi <- if("show_roi_markers" %in% names(input)) input$show_roi_markers else FALSE
      has_boundaries <- !is.null(roi_boundaries())
      not_suppressed <- !suppress_roi_lines()
      
      user_wants_roi && has_boundaries && not_suppressed
    })
    
    # Render the plot
    output$plot <- renderPlotly({
      data <- sensor_data()
      req(data)
      
      # Get current settings
      sensor_vars <- get_sensor_variables()
      current_left_var <- left_var()
      current_right_var <- right_var()
      current_time_var <- time_var()
      
      # Determine left axis
      left_idx <- which(sensor_vars$names == current_left_var)
      left_color <- sensor_vars$colors[left_idx]
      left_label <- sensor_vars$labels[left_idx]
      
      # Determine right axis
      has_right_axis <- current_right_var != "none"
      right_margin <- if (has_right_axis) 80 else 30
      
      # Set x-axis label
      x_label <- if (current_time_var == "time_norm") "Normalized Time" else "Time [s]"
      
      # Create base plot
      p <- plot_ly() %>%
        layout(
          title = paste(title_prefix, sensor_name()),
          showlegend = show_legend(),
          margin = list(l = 80, r = right_margin, t = 50, b = 50),
          xaxis = list(
            title = x_label,
            showline = TRUE,
            linecolor = "black",
            linewidth = 1,
            showticklabels = TRUE,
            ticks = "outside",
            tickcolor = "black"
          ),
          yaxis = list(
            title = left_label,
            showline = TRUE,
            linecolor = "black",
            linewidth = 1,
            showticklabels = TRUE,
            ticks = "outside",
            tickcolor = "black"
          )
        )%>%
        config(
          toImageButtonOptions = list(
            format = 'svg',
            filename = paste0(sensor_name(), "_plot"),
            width = input$plot_width,
            height = input$plot_height,
            scale = 1
          )
        )
      
      # Add left axis trace
      p <- p %>% add_trace(
        x = data[[current_time_var]],
        y = data[[current_left_var]],
        name = left_label,
        type = "scatter",
        mode = "lines",
        line = list(color = left_color)
      )
      
      # Add right axis if configured
      if (has_right_axis) {
        right_idx <- which(sensor_vars$names == current_right_var)
        right_color <- sensor_vars$colors[right_idx]
        right_label <- sensor_vars$labels[right_idx]
        
        p <- p %>% layout(
          yaxis2 = list(
            title = right_label,
            overlaying = "y",
            side = "right",
            showline = TRUE,
            linecolor = "black",
            linewidth = 1,
            showticklabels = TRUE,
            ticks = "outside",
            tickcolor = "black"
          )
        )
        
        p <- p %>% add_trace(
          x = data[[current_time_var]],
          y = data[[current_right_var]],
          name = right_label,
          yaxis = "y2",
          type = "scatter",
          mode = "lines",
          line = list(color = right_color)
        )
      }
      
      # Add nadir marker if configured and available
      if (show_nadir() && !is.null(nadir_info()) && nadir_info()$available) {
        nadir_yaxis <- NULL
        if (current_left_var == "pressure_kpa") {
          nadir_yaxis <- "y"
        } else if (has_right_axis && current_right_var == "pressure_kpa") {
          nadir_yaxis <- "y2"
        }
        
        if (!is.null(nadir_yaxis)) {
          p <- p %>% add_trace(
            x = nadir_info()$time,
            y = nadir_info()$value,
            name = "Pressure Nadir",
            type = "scatter",
            mode = "markers+text",
            marker = list(color = "orange", size = 10),
            text = paste("Nadir:", round(nadir_info()$value, 2), "kPa"),
            textposition = "top right",
            textfont = list(color = "orange"),
            yaxis = nadir_yaxis
          )
        }
      }
      
      if (!is.null(custom_traces()) && length(custom_traces()) > 0) {
        for (trace in custom_traces()) {
          p <- p %>% add_trace(
            x = trace$x,
            y = trace$y,
            type = trace$type %||% "scatter",
            mode = trace$mode %||% "lines",
            line = trace$line,
            name = trace$name %||% "",
            showlegend = trace$showlegend %||% FALSE,
            hovertext = trace$hovertext,
            hoverinfo = trace$hoverinfo %||% "text"
          )
        }
      }
      
      # Add selected nadir point if provided (for editing mode)
      if (!is.null(selected_nadir()) && !is.null(selected_nadir()$x)) {
        p <- p %>% add_trace(
          x = selected_nadir()$x,
          y = selected_nadir()$y,
          name = "Selected Nadir",
          type = "scatter",
          mode = "markers+text",
          marker = list(color = "purple", size = 12, symbol = "diamond"),
          text = paste("New:", round(selected_nadir()$y, 2), "kPa"),
          textposition = "top center",
          textfont = list(color = "purple"),
          showlegend = FALSE
        )
      }
      
      # Add ROI boundary lines if all conditions met
      if (show_roi_lines()) {
        roi_labels <- c("", "ROI 1", "ROI 2", "ROI 3", "ROI 4", "ROI 5", "ROI 6", "ROI 7", "")
        
        for (i in 2:9) {  # Skip first and last boundaries (data start/end)
          p <- p %>% add_segments(
            x = roi_boundaries()[i], xend = roi_boundaries()[i],
            y = min(data[[current_left_var]], na.rm = TRUE), 
            yend = max(data[[current_left_var]], na.rm = TRUE),
            line = list(color = "blue", width = 2, dash = "dash"),
            showlegend = FALSE,
            hoverinfo = "text",
            text = paste(roi_labels[i])
          )
        }
      }
      
      # Set plot source for event handling
      p$x$source <- plot_source
      p <- p %>% event_register("plotly_click")
      
      return(p)
    })
    
    # Return reactive inputs for external access
    return(list(
      plot_width = reactive(input$plot_width),
      plot_height = reactive(input$plot_height),
      left_var = reactive(input$left_y_var),
      right_var = reactive(input$right_y_var),
      show_normalized = reactive(input$show_normalized),
      show_nadir = reactive(input$show_nadir),
      show_roi_markers = reactive(input$show_roi_markers),
      show_legend = reactive(input$show_legend)
    ))
  })
}

#Module usage

#add main UI call
#plotModuleUI(ns("pressure_plot"), height = "600px"),

# Add desired sidebar content
# hr(), h4("Plot controls"),
# plotSidebarUI(ns("pressure_plot"), 
#               show_left_var = TRUE,
# show_right_var = TRUE, 
# show_normalized = TRUE,
# show_nadir = TRUE,
# show_roi_markers = TRUE,
# show_legend = TRUE,
# default_show_normalized = FALSE,
# default_show_nadir = TRUE,
# default_show_roi_markers = FALSE,
# default_show_legend = FALSE,
# default_left_var = "pressure_kpa",
# default_right_var = "none")

# Defaults can be set here, or fallback to server level defaults if sidebar content hidden

#Then add server controls to output of module

# plot_controls <- plotModuleServer("pressure_plot", 
#                                   sensor_data = selected_sensor_data,
#                                   sensor_name = reactive(sensor_selector$selected_sensor()),
#                                   nadir_info = nadir_info,
#                                   roi_boundaries = roi_boundaries,
#                                   #don't need to provide left and right var as default = pressure
#                                   show_nadir = reactive(input$`pressure_plot-show_nadir`),
#                                   show_legend = reactive(input$`pressure_plot-show_legend`),
#                                   show_normalized = reactive(input$`pressure_plot-show_normalized`),
#                                   show_roi_markers = reactive(input$`pressure_plot-show_roi_markers`),
#                                   title_prefix = "Pressure Analysis",
#                                   plot_source = "pressure_plot"
# )

#Default fallback
#plotModuleServer <- function(id, 
# sensor_data,
# sensor_name,
# nadir_info = reactive(NULL),
# selected_nadir = reactive(NULL),
# roi_boundaries = reactive(NULL),
# left_var = reactive("pressure_kpa"),
# right_var = reactive("none"),
# show_nadir = reactive(FALSE),
# show_legend = reactive(FALSE),
# show_normalized = reactive(FALSE),
# show_roi_markers = reactive(FALSE),
# plot_source = "plot",
# title_prefix = "Sensor Data"

# this means all plots are loaded with 'everything', and can be reactive to any sidebar changes without reloading entire plot



