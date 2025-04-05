#' Search and Select Specific Fields from Data
#'
#' This function searches through the column names of a data frame and selects columns that match a specified pattern.
#' The patterns are constructed from a list of base names provided by the user, aimed to match a specific naming convention.
#' @param data A data frame from which to select columns.
#' @param name_list A vector of base names to construct the search patterns from.
#' @return A data frame containing only the columns that match the constructed search patterns.
#' @export
search_fields <- function(data, name_list){
  all_columns <- colnames(data)
  matched_columns <- c()
  for(name in name_list) {
    # Construct a regex pattern based on the provided name
    pattern <- paste0("^", name, "_f[0-9]+(_[0-9]+){2}$")
    # Find all matches to the pattern in the column names
    matches <- grep(pattern, all_columns, value = TRUE)
    # Append the matched column names to the result
    matched_columns <- c(matched_columns, matches)
  }
  # Return a subset of the data containing only the matched columns
  return(data[, matched_columns])
}

    