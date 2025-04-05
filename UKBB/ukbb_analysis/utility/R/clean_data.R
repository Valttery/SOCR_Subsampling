
#' Clean Data by Removing Columns with Excessive Missing Values
#'
#' This function examines each column in a data frame and removes columns where the proportion of missing values exceeds a specified threshold.
#' @param data A data frame that may contain missing values.
#' @param m The maximum allowable proportion of missing values for a column to be retained.
#' @return A data frame with columns having a higher proportion of missing values than the specified threshold removed.
#' @export
clean_data <- function(data, m){
  setDT(data)  # Ensure data is in data.table format
  
  cols_to_remove <- c()  # Initialize an empty vector to store indices of columns to remove
  for (i in 1:ncol(data)) {
    column <- data[[i]]
    invalid_count <- sum(is.na(column))  # Count the number of missing values in the column
    invalid_ratio <- invalid_count / length(column)  # Calculate the proportion of missing values
    
    if (invalid_ratio > m) {
      cols_to_remove <- c(cols_to_remove, i)  # Add the column index to the list if the proportion exceeds m
    }
  }
  
  if (length(cols_to_remove) > 0) {
    data <- data[, !cols_to_remove, with = FALSE]  # Remove columns with too many missing values
  }
  return(data)
}
