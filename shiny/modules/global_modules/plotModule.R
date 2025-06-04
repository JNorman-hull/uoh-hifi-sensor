# ============================= #
# /// Shared Plot Module \\\ ####  
# ============================= #

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
                             plot_source = "plot",
                             title_prefix = "Sensor Data",
                             # Internal logic parameters (not user controls)
                             custom_edit_mode = reactive(FALSE),
                             is_normalized_view = reactive(FALSE)) {
  
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



