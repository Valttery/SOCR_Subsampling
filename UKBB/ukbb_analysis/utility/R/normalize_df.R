#' Normalize columns in a data frame
#'
#' This function takes a data frame and normalizes its numeric columns to a range of 0 to 1, ignoring NA values.
#' It calculates the range for each numeric column and scales the values accordingly.
#' For columns with a range of 0 (all values are the same), it returns a vector of zeros for non-NA values.
#' @param df A data frame with numeric and/or other type columns.
#' @return A data frame with numeric columns normalized to the range 0 to 1, other columns remain unchanged.
#' @export
normalize_df <- function(df) {
  for(col_name in names(df)) {
    # Check if the column is numeric
    if(is.numeric(df[[col_name]])) {
      # Extract non-NA values
      values <- df[[col_name]][!is.na(df[[col_name]])]
      
      # Calculate non-NA minimum and maximum values
      min_val <- min(values, na.rm = TRUE)
      max_val <- max(values, na.rm = TRUE)
      
      # If the range is zero, return a vector of zeros for non-NA values
      if(max_val == min_val) {
        df[[col_name]][!is.na(df[[col_name]])] <- 0
      } else {
        # Otherwise, normalize the non-NA values
        df[[col_name]][!is.na(df[[col_name]])] <- (values - min_val) / (max_val - min_val)
      }
    }
  }
  return(df)  # Return the modified data frame
}
