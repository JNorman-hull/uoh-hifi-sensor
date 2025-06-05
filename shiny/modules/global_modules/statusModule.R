# ============================= #
# /// Shared Status Module \\\ ####  
# ============================= #

# ============================= #
# /// Status Helper Functions \\\ ####  
# ============================= #

## Define status levels ####
get_status_level_config <- function() {
  list(
    levels = list(
      "0" = list(color = "gray", label = "None", priority = 0),
      "1" = list(color = "red", label = "Error", priority = 1),
      "2" = list(color = "orange", label = "Warning", priority = 2), 
      "3" = list(color = "blue", label = "Info", priority = 3),
      "4" = list(color = "green", label = "Success", priority = 4)
    )
  )
}

## Define status check types ####
get_status_check_definitions <- function() {
  list(
    bad_sensor = list(
      name = "Sensor Quality",
      checks = list(
        list(condition = function(status) status$bad_sens,
             level = 1, message = "Sensor marked as 'bad'"),
        list(condition = function(status) !status$bad_sens,
             level = 4, message = "Sensor marked as 'good'")
      )
    ),
    
    deployment_info = list(
      name = "Deployment Information",
      checks = list(
        list(condition = function(status) status$deployment_info,
             level = 4, message = "Deployment information complete"),
        list(condition = function(status) !status$deployment_info,
             level = 1, message = "Deployment information required")
      )
    ),
    
    delineation = list(
      name = "Delineation Status",
      checks = list(
        list(condition = function(status) !status$delineated, 
             level = 1, message = "Sensor requires delineation"),
        list(condition = function(status) status$delineated && !status$trimmed, 
             level = 2, message = "Sensor file delineated (not trimmed)"),
        list(condition = function(status) status$trimmed, 
             level = 4, message = "Sensor file delineated and trimmed")
      )
    ),
    
    normalization = list(
      name = "Normalization Status", 
      checks = list(
        list(condition = function(status) status$normalized,
             level = 4, message = "Time series normalized"),
        list(condition = function(status) !status$normalized,
             level = 1, message = "Time series requires normalization")
      )
    ),
    
    passage_times = list(
      name = "Passage Times",
      checks = list(
        list(condition = function(status) status$passage_times,
             level = 4, message = "Passage times calculated"),
        list(condition = function(status) !status$passage_times,
             level = 1, message = "Passage times require calculation")
      )
    ),
    
    pres_processed = list(
      name = "Pressure analysis",
      checks = list(
        list(condition = function(status) status$all_pres_processed,
             level = 4, message = "All pressure analysis complete"),
        list(condition = function(status) !status$all_pres_processed,
             level = 1, message = "Pressure processing incomplete ")
      )
    ),
    
    pres_processed_sum = list(
      name = "Pressure analysis (summary)",
      checks = list(
        list(condition = function(status) status$pres_sum_processed,
             level = 4, message = "Pressure sumary calculated"),
        list(condition = function(status) !status$pres_sum_processed,
             level = 1, message = "Pressure summary requires calculation")
      )
    ),
    
    pres_processed_rpc = list(
      name = "Pressure analysis (rpc)",
      checks = list(
        list(condition = function(status) status$pres_rpc_processed,
             level = 4, message = "Rate pressure change calculated"),
        list(condition = function(status) !status$pres_rpc_processed,
             level = 1, message = "Rate pressure change requires calculation")
      )
    ),
    
    pres_processed_lrpc = list(
      name = "Pressure analysis (lrpc)",
      checks = list(
        list(condition = function(status) status$pres_lrpc_processed,
             level = 4, message = "Log ratio pressure change calculated"),
        list(condition = function(status) !status$pres_lrpc_processed,
             level = 1, message = "Log ratio pressure change requires calculation")
      )
    ),
    
    acc_processed = list(
      name = "Acceleration analysis",
      checks = list(
        list(condition = function(status) status$all_acc_processed,
             level = 4, message = "All acceleration analysis complete"),
        list(condition = function(status) !status$all_acc_processed,
             level = 1, message = "Acceleration processing incomplete ")
      )
    ),
    
    acc_processed_sum = list(
      name = "Acceleration analysis (summary)",
      checks = list(
        list(condition = function(status) status$acc_sum_processed,
             level = 4, message = "Acceleration sumary calculated"),
        list(condition = function(status) !status$acc_sum_processed,
             level = 1, message = "Acceleration summary requires calculation")
      )
    ),
    
    acc_processed_peaks = list(
      name = "Acceleration peaks",
      checks = list(
        list(condition = function(status) status$acc_hig_peaks_processed,
             level = 4, message = "Acceleration peaks processed"),
        list(condition = function(status) !status$acc_hig_peaks_processed,
             level = 1, message = "Acceleration peaks requires calculation")
      )
    ),
    
    acc_processed_strikes = list(
      name = "Acceleration strikes",
      checks = list(
        list(condition = function(status) status$acc_strike_processed,
             level = 4, message = "Acceleration strikes processed"),
        list(condition = function(status) !status$acc_strike_processed,
             level = 1, message = "Acceleration strikes requires calculation")
      )
    ),
    
    acc_processed_collision = list(
      name = "Acceleration collisions",
      checks = list(
        list(condition = function(status) status$acc_collision_processed,
             level = 4, message = "Acceleration collisions processed"),
        list(condition = function(status) !status$acc_collision_processed,
             level = 1, message = "Acceleration collisions requires calculation")
      )
    ),
    
    rotation_processing = list(
      name = "Rotation Analysis",
      checks = list(
        list(condition = function(status) status$all_rot_processed,
             level = 4, message = "All rotation analysis complete"),
        list(condition = function(status) status$rot_sum_processed,
             level = 2, message = "Rotation summary calculated"),
        list(condition = function(status) !status$all_rot_processed,
             level = 1, message = "Rotation analysis requires calculation")
      )
    )
  )
}

## Evaluate status checks ####
evaluate_status_checks <- function(sensor_name, output_dir, 
                                   check_types = c("bad_sens", "deployment_info", "delineation", "normalization",
                                                   "passage_times", "pres_processed", "pres_processed_sum", "pres_processed_rpc",
                                                   "pres_processed_lrpc", "acc_processed", "acc_processed_sum", "acc_processed_peaks",
                                                   "acc_processed_strikes", "acc_processed_collision", "rotation_processing"
                                   )) {
  if (is.null(sensor_name) || sensor_name == "") {
    return(list(
      level = 0,
      messages = "No sensor selected",
      color = "gray",
      label = "None"
    ))
  }
  
  # Get sensor status
  status <- get_sensor_status(sensor_name, output_dir)
  
  # Get check definitions and level config
  check_definitions <- get_status_check_definitions()
  level_config <- get_status_level_config()
  
  # Evaluate all requested check types
  results <- list()
  highest_level <- 0
  
  for (check_type in check_types) {
    if (check_type %in% names(check_definitions)) {
      check_def <- check_definitions[[check_type]]
      
      # Find first matching condition
      for (check in check_def$checks) {
        if (check$condition(status)) {
          results[[check_type]] <- list(
            level = check$level,
            message = check$message,
            name = check_def$name
          )
          highest_level <- max(highest_level, check$level)
          break
        }
      }
    }
  }
  
  # Get styling for highest level
  highest_config <- level_config$levels[[as.character(highest_level)]]
  
  # Combine messages
  messages <- sapply(results, function(x) x$message)
  messages <- messages[messages != ""]  # Remove empty messages
  
  return(list(
    level = highest_level,
    messages = paste(messages, collapse = "\n"),
    color = highest_config$color,
    label = highest_config$label,
    details = results
  ))
}

## Apply styling to UI #### 
apply_status_styling <- function(session, element_id, color, ns = NULL) {
  full_id <- if (!is.null(ns)) paste0("#", ns(element_id)) else paste0("#", element_id)
  
  shinyjs::runjs(paste0("
    $('", full_id, "').css({
      'color': '", color, "', 
      'font-weight': 'bold'
    });
  "))
}




statusModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    textOutput(ns("status_display"))
  )
}

statusSidebarUI <- function(id,
                            show_bad_sensor = FALSE,
                            show_deployment_info = FALSE, 
                            show_delineation = FALSE,
                            show_normalization = FALSE,
                            show_passage_times = FALSE,
                            show_pres_processed = FALSE,
                            show_pres_processed_sum = FALSE,
                            show_pres_processed_rpc = FALSE,
                            show_pres_processed_lrpc = FALSE,
                            show_acc_processed = FALSE,
                            show_acc_processed_sum = FALSE,
                            show_acc_processed_peaks = FALSE,
                            show_acc_processed_strikes = FALSE,
                            show_acc_processed_collision = FALSE,
                            show_rotation_processing = FALSE,
                            label_prefix = "") {
  ns <- NS(id)
  
  # Build list of status items to show
  status_items <- list()
  
  if (show_bad_sensor) {
    status_items[["bad_sensor"]] <- paste0(label_prefix, "Sensor Quality")
  }
  if (show_deployment_info) {
    status_items[["deployment_info"]] <- paste0(label_prefix, "Deployment Information")
  }
  if (show_delineation) {
    status_items[["delineation"]] <- paste0(label_prefix, "Delineation Status")
  }
  if (show_normalization) {
    status_items[["normalization"]] <- paste0(label_prefix, "Normalization Status")
  }
  if (show_passage_times) {
    status_items[["passage_times"]] <- paste0(label_prefix, "Passage Times")
  }
  if (show_pres_processed) {
    status_items[["pres_processed"]] <- paste0(label_prefix, "Pressure Analysis")
  }
  if (show_pres_processed_sum) {
    status_items[["pres_processed_sum"]] <- paste0(label_prefix, "Pressure Summary")
  }
  if (show_pres_processed_rpc) {
    status_items[["pres_processed_rpc"]] <- paste0(label_prefix, "RPC Analysis")
  }
  if (show_pres_processed_lrpc) {
    status_items[["pres_processed_lrpc"]] <- paste0(label_prefix, "LRPC Analysis")
  }
  if (show_acc_processed) {
    status_items[["acc_processed"]] <- paste0(label_prefix, "Acceleration Analysis")
  }
  if (show_acc_processed_sum) {
    status_items[["acc_processed_sum"]] <- paste0(label_prefix, "Acceleration Summary")
  }
  if (show_acc_processed_peaks) {
    status_items[["acc_processed_peaks"]] <- paste0(label_prefix, "Acceleration Peaks")
  }
  if (show_acc_processed_strikes) {
    status_items[["acc_processed_strikes"]] <- paste0(label_prefix, "Acceleration Strikes")
  }
  if (show_acc_processed_collision) {
    status_items[["acc_processed_collision"]] <- paste0(label_prefix, "Acceleration Collisions")
  }
  if (show_rotation_processing) {
    status_items[["rotation_processing"]] <- paste0(label_prefix, "Rotation Analysis")
  }
  
  # Create UI elements for each status
  tagList(
    lapply(names(status_items), function(status_key) {
      textOutput(ns(paste0(status_key, "_status")))
    })
  )
}

statusModuleServer <- function(id,
                               sensor_name_reactive,
                               output_dir_reactive,
                               check_types = c("bad_sensor", "deployment_info", "delineation"),
                               invalidation_trigger = reactive(NULL),
                               individual_outputs = TRUE) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Combined status info for all checks
    combined_status_info <- reactive({
      # Include invalidation trigger if provided
      if (!is.null(invalidation_trigger)) {
        invalidation_trigger()
      }
      
      sensor_name <- sensor_name_reactive()
      output_dir <- output_dir_reactive()
      
      if (is.null(sensor_name) || sensor_name == "") {
        return(list(level = 0, messages = "", color = "gray"))
      }
      
      # Evaluate all requested check types
      result <- evaluate_status_checks(sensor_name, output_dir, check_types = check_types)
      return(result)
    })
    
    # Create individual status displays if requested
    if (individual_outputs) {
      for (check_type in check_types) {
        local({
          current_check <- check_type
          output_id <- paste0(current_check, "_status")
          
          # Individual status reactive
          individual_status <- reactive({
            if (!is.null(invalidation_trigger)) {
              invalidation_trigger()
            }
            
            sensor_name <- sensor_name_reactive()
            output_dir <- output_dir_reactive()
            
            if (is.null(sensor_name) || sensor_name == "") {
              return(list(level = 0, message = "", color = "gray"))
            }
            
            result <- evaluate_status_checks(sensor_name, output_dir, check_types = c(current_check))
            
            if (current_check %in% names(result$details)) {
              detail <- result$details[[current_check]]
              level_config <- get_status_level_config()
              color <- level_config$levels[[as.character(detail$level)]]$color
              
              return(list(
                level = detail$level,
                message = detail$message,
                color = color
              ))
            } else {
              return(list(level = 0, message = "", color = "gray"))
            }
          })
          
          # Text output
          output[[output_id]] <- renderText({
            individual_status()$message
          })
          
          # Styling observer
          observe({
            info <- individual_status()
            if (info$message != "") {
              apply_status_styling(session, output_id, info$color, ns)
            }
          })
        })
      }
    }
    
    # Combined status display
    output$status_display <- renderText({
      combined_status_info()$messages
    })
    
    # Combined styling
    observe({
      info <- combined_status_info()
      if (info$messages != "") {
        apply_status_styling(session, "status_display", info$color, ns)
      }
    })
    
    # Return reactive values for external access
    return(list(
      combined_status = combined_status_info,
      individual_check_types = reactive(check_types)
    ))
  })
}