#' Convert Date Columns to Numeric
#'
#' This function iterates through each column in a data frame. If a column is recognized as a date,
#' it converts the dates to a numeric value representing the number of days since the earliest date in the column.
#' Requires the 'lubridate' package for the 'ymd' function.
#' @param data A data frame that may contain date columns.
#' @return A data frame with all date columns converted to numeric values representing days since the base date.
#' @export
convert_dates_to_numeric <- function(data) {
  # Ensure lubridate and progress are available
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("The 'lubridate' package is required but not installed. Please install it.")
  }
  if (!requireNamespace("progress", quietly = TRUE)) {
    stop("The 'progress' package is required but not installed. Please install it.")
  }
  
  # Initialize the progress bar
  pb <- progress::progress_bar$new(
    format = "[:bar] :percent | Column :current/:total (:eta remaining)",
    total = length(names(data)), clear = FALSE
  )
  
  data_converted <- data
  for (col_name in names(data_converted)) {
    # Attempt to convert the column to a date using lubridate::ymd
    column_as_date <- lubridate::ymd(data_converted[[col_name]])
    if (!all(is.na(column_as_date))) {
      # If conversion is successful and not all values are NA
      base_date <- min(column_as_date, na.rm = TRUE)  # Find the earliest date
      # Convert dates to numeric values representing days since the base date
      data_converted[[col_name]] <- as.numeric(difftime(column_as_date, base_date, units = "days"))
    }
    # Update the progress bar
    pb$tick()
  }
  return(data_converted)
}

