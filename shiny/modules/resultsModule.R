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
            scrollX = TRUE
          )
        )
      } else {
        # If conversion failed, show a message
        return(NULL)
      }
    })
  })
}