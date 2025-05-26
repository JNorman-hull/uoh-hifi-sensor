resultsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Processing Results"),
    DT::dataTableOutput(ns("results_table"))
  )
}

resultsServer <- function(id, summary_data, processing_complete) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Display results table
    output$results_table <- DT::renderDataTable({
      req(processing_complete())
      req(length(summary_data()) > 0)
      
      tryCatch({
        # Convert the list of dictionaries to an R data frame
        summary_df <- do.call(rbind, lapply(summary_data(), function(x) {
          df <- as.data.frame(x, stringsAsFactors = FALSE)
          
          # Reorder to put 'file' first
          col_order <- c("file", setdiff(names(df), "file"))
          df[, col_order]
        }))
        
        # If conversion worked and we have data, display it
        if (is.data.frame(summary_df) && nrow(summary_df) > 0) {
          DT::datatable(
            summary_df,
            options = list(
              pageLength = 10,
              scrollX = TRUE,
              scrollY = "400px",
              dom = 'frtip',
              columnDefs = list(
                list(width = '150px', targets = 0)  # File column
              )
            ),
            filter = 'top',
            rownames = FALSE,
            class = 'cell-border stripe hover'
          ) %>%
            DT::formatStyle(columns = 1:ncol(summary_df), fontSize = '12px')
        } else {
          # If conversion failed, show empty table with message
          NULL
        }
      }, error = function(e) {
        # Error in data conversion - log and return NULL
        warning("Error displaying results table: ", e$message)
        return(NULL)
      })
    })
  })
}