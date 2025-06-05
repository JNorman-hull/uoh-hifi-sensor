summarytableModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    h4("Descriptive statistics and central tendancies")
  
  # Summary DT table is always shown and populated by global_processed_instrument_data for the relevant instrument variable
  # that way, when theres no data (NA) or blank, we will have a blank table
    
    #Action button for Process summary information
    # Textouput under button to say that summary information has been calculated
    
    
  )
}


summarytableModuleServer <- function(id, instrument_variable = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ============================= #
    # /// Reactive values \\\ ####  
    # ============================= #   
    
    #status flags I think
    

    # ============================= #
    # /// Data loading & processing  \\\ ####  
    # ============================= # 
    
    # function needs to have an instrument_variable input so it knows which summary variables we're building
    # this logic will not be here, it will come from a global instrument reading/writing function
    # usage intention is instrument_variable = "pres", "acc" "rot"
    
    #if the instrument variable we set = acc, then we know we're going to process summary data for acc_hig and acc_inacc.
    # like all other functions, the summary table uses the operating modules selected sensor e.g., if we place summarytableModuleServer in pressureModule, then it willbe passed the selected sensor from there
    # no need to repeat logic here

    # ============================= #
    # /// UI State management \\\ ####  
    # ============================= # 
    
    #Action button (green) for Process summary information only available when delineated and trimmed status = Y
    # If acc_sum_processed = Y, then button state is red and says 'Recalculate summary information'

    
    # ============================= #
    # /// Event handlers \\\ ####  
    # ============================= # 

    # When user clicks 'Process summary information', we process it and save it to the instrument_index
    # User receives a message saying it was processed and written to index for sensor
    # If data already exists for the sensor, modal message to say we're going to replace it or cancel (same as other modules)
    
    #If summary information processing successful the update the status flag for instrument e.g., pres_sum_processed, acc_sum_processed, rot_sum_processed,
    #should be able to inherit the instrument_variable value as status flag matches instrument_index format
    
    
    # ============================= #
    # /// Helper functions \\\ ####  
    # ============================= #
    
    #Just a rough example of what the summary function could be
    # Summary finction identifies the roi boundaries and calculates the min, max, median and iqr of the selected instrument variable 
    
    calc_summary_stats <- function(sensor_name, output_dir, isntrument = NULL){
      # Function to calculate summary statistics to be used across instrument analysis types
      
      roi_times <- get_roi_boundaries(sensor_name, output_dir)
      #these refer to the columns present in the _delineated file for the selected sensor
      #instrument names match the instrument_variable input we provide at server level
      isntrument <- list(
        pres = "pressure_kpa",
        acc = list("higacc_mag_g",
                   "inacc_mag_ms"
        ),
        rot = "rot_mag_degs"
      )
      
      # for each roi defined by start and end of boundary), calculate summary statistics
      
      # Create an 'overall' summary too, which does summary on all data
      
      instrument_summary <- bind_rows(
        data %>%
          group_by(roi) %>%
          summarise(
            med = median(isntrument),
            min = min(isntrument),
            max = max(isntrument),
            IQR = IQR(isntrument),
            .groups = "drop"
          ), data %>%
          mutate(roi = "overall") %>%
          group_by(roi) %>%
          summarise(
            med = median(isntrument),
            min = min(isntrument),
            max = max(isntrument),
            IQR = IQR(isntrument),
            .groups = "drop"
          )
      )
            return(instrument_summary)
      #to be used to display the summary in a tidy DT Table in respective instrument module 
    }
    
    
    # ============================= #
    # /// Output render \\\ ####  
    # ============================= #    
    
    # Build the DT table here
    # It is constructed from the instrument_index file
    # Columns = ROI, min, max, median, iqr for the relevant instrument and relevant sensor only
    # render the textoutput  too 
    
  })
} 