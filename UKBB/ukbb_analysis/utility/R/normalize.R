#' Normalize a vector
#'
#' This function takes a numeric vector and normalizes it to a range of 0 to 1.
#' It adds a small epsilon to the range to avoid division by zero.
#' It includes a progress bar to indicate the progress of normalization.
#' @param x A numeric vector that you want to normalize.
#' @return A numeric vector of the same length, with values normalized to the range 0 to 1.
#' @importFrom progress progress_bar
#' @export
normalize <- function(x) {
  # Ensure the progress package is available
  if (!requireNamespace("progress", quietly = TRUE)) {
    stop("The 'progress' package is required but not installed. Please install it.")
  }
  
  epsilon <- 1e-5  # A small number to avoid division by zero
  range <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE) + epsilon  # Calculate the range and avoid singularities
  
  # Initialize the progress bar
  pb <- progress::progress_bar$new(
    format = "[:bar] :percent",
    total = length(x), clear = FALSE
  )
  
  if(range == 0) {
    # If the range is zero after adding epsilon, return a vector of zeros
    result <- rep(0, length(x))
  } else {
    # Otherwise, return the normalized vector
    result <- (x - min(x, na.rm = TRUE)) / range
  }
  
  # Update progress after each element is processed
  for(i in 1:length(x)) {
    pb$tick()
  }
  
  return(result)
}


    