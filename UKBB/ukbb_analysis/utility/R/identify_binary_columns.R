#' Identify Binary Columns
#'
#' This function iterates through each column in a data frame. It identifies columns that contain only the values 0 and 1,
#' which are considered binary columns. It then returns the names of these binary columns.
#' It includes a progress bar to indicate the progress of identification.
#' @param data A data frame that may contain binary columns.
#' @return A vector containing the names of all columns that are binary (only containing values 0 and 1).
#' @importFrom progress progress_bar
#' @export
identify_binary_columns <- function(data) {
  # Ensure the progress package is available
  if (!requireNamespace("progress", quietly = TRUE)) {
    stop("The 'progress' package is required but not installed. Please install it.")
  }
  
  binary_columns <- c()  # Initialize an empty vector to store the names of binary columns
  
  # Initialize the progress bar
  pb <- progress::progress_bar$new(
    format = "[:bar] :percent | Processing :current/:total :what",
    total = length(names(data)), clear = FALSE
  )
  
  for (col_name in names(data)) {
    # Update the progress bar with the current column name
    pb$tick(tokens = list(what = col_name))
    
    unique_values <- unique(data[[col_name]])
    # Check if the column only contains the values 0 and 1
    if (all(unique_values %in% c(0, 1)) && length(unique_values) == 2) {
      binary_columns <- c(binary_columns, col_name)
    }
  }
  
  return(binary_columns)
}


    