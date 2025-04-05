#' Apply Label Encoding to Data
#'
#' This function iterates through each column in a data frame. 
#' If a column is not numeric, it applies label encoding, 
#' converting categories into integers. A progress bar is displayed
#' showing the progress and the column currently being processed.
#' @param data A data frame where some columns may be non-numeric.
#' @return A data frame with all columns converted to numeric type, 
#'         where non-numeric columns have been label encoded.
#' @importFrom progress progress_bar
#' @export
apply_label_encoding <- function(data) {
  # Ensure the progress package is available
  if (!requireNamespace("progress", quietly = TRUE)) {
    stop("The 'progress' package is required but not installed. Please install it.")
  }
  
  data_converted <- data
  
  # Initialize the progress bar
  pb <- progress::progress_bar$new(
    format = "[:bar] :percent | Processing :current/:total :what",
    total = length(names(data_converted)), clear = FALSE
  )
  
  for (col_name in names(data_converted)) {
    # Update the progress bar with the current column name
    pb$tick(tokens = list(what = col_name))
    
    col_data <- data_converted[[col_name]]
    if (!is.numeric(col_data) && any(!is.na(col_data))) {
      # Extract the non-NA values
      non_na_values <- col_data[!is.na(col_data)]
      
      # Define a function to extract the non-numeric part of a string
      extract_non_numeric <- function(x) gsub("\\d+", "", x)
      
      # Apply this function to all non-NA values
      non_numeric_parts <- sapply(non_na_values, extract_non_numeric)
      
      # Check if all non-numeric parts are the same, implying a consistent pattern
      if (length(unique(non_numeric_parts)) == 1) {
        # If so, extract and use the numbers as labels
        numbers <- as.numeric(gsub("[^0-9]+", "", non_na_values))
        numeric_labels <- rep(NA, length(col_data))
        numeric_labels[!is.na(col_data)] <- numbers
      } else {
        # If not, convert the whole column to a factor and then to numeric
        numeric_labels <- as.numeric(factor(col_data))
      }
      
      # Preserve NA values
      numeric_labels[is.na(col_data)] <- NA
      
      data_converted[[col_name]] <- numeric_labels
    }
  }
  return(data_converted)
}
