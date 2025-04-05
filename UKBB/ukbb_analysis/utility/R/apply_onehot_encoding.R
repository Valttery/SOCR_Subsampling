#' Apply One-Hot Encoding to Data
#'
#' This function converts categorical variables in a data frame to a series of binary variables (one-hot encoding). 
#' It handles NA values and keeps track of the initial and final row counts. 
#' Any non-numeric columns will be transformed into multiple binary columns, one for each category.
#' @param data A data frame that may contain categorical variables.
#' @return A data frame where all categorical variables have been replaced with one-hot encoded columns.
#'         The number of columns may increase significantly depending on the number of categories.
#' @importFrom progress progress_bar
#' @export
apply_onehot_encoding <- function(data) {
  # Ensure the progress package is available
  if (!requireNamespace("progress", quietly = TRUE)) {
    stop("The 'progress' package is required but not installed. Please install it.")
  }
  
  data_final <- data.frame(matrix(nrow = nrow(data), ncol = 0))
  
  print(paste("Initial row count:", nrow(data)))
  
  empty_rows <- apply(data, 1, function(row) all(is.na(row)))
  
  # Initialize the progress bar
  pb <- progress::progress_bar$new(
    format = "[:bar] :percent | Processing :current/:total :what",
    total = length(names(data)), clear = FALSE
  )
  
  for (col_name in names(data)) {
    # Update the progress bar with the current column name
    pb$tick(tokens = list(what = col_name))
    
    if (any(!is.na(data[[col_name]]))) {
      tryCatch({
        if (!is.numeric(data[[col_name]])) {
          temp_col <- as.character(data[[col_name]])
          temp_col[is.na(temp_col)] <- "NA_value"
          
          onehot_encoded <- model.matrix(~ temp_col - 1)
          colnames(onehot_encoded) <- gsub("temp_col", col_name, colnames(onehot_encoded))
          
          onehot_encoded[empty_rows, ] <- NA
          data_final <- cbind(data_final, onehot_encoded)
        } else {
          numeric_col <- data[[col_name]]
          numeric_col[empty_rows] <- NA
          data_final <- cbind(data_final, numeric_col)
        }
      }, error = function(e) {
        warning(paste("Error processing column", col_name, ":", e$message))
      })
    } else {
      warning(paste("Column", col_name, "is empty or all NA. Skipping..."))
    }
  }
  
  print(paste("Final row count:", nrow(data_final)))
  
  return(data_final)
}

