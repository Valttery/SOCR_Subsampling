#' Read a Subset of a Large Dataset
#'
#' This function reads a subset of rows and/or columns from a large dataset. 
#' It allows sampling of a specific number of rows (k) and/or a specific number of fields (p). 
#' It's particularly useful when dealing with very large files where reading the entire file into memory is not feasible.
#' @param fn The file name of the large dataset.
#' @param fn_fields The file name containing the field names.
#' @param k The number of rows to sample from the dataset (optional).
#' @param p The number of fields to sample from the dataset (optional).
#' @param total The total number of rows in the dataset (optional, will be calculated if not provided).
#' @return A data frame containing the sampled subset of the dataset.
#' @export
read_subset <- function(fn, fn_fields, k = NA, p = NA, total = NA){
  require(data.table)  # Ensure data.table is available for fread
  require(dplyr)       # Ensure dplyr is available for sample_n and pull
  
  fields_df <- fread(fn_fields, header = F)
  
  if (!is.na(p)) {
    fields_sub <- fields_df %>% dplyr::sample_n(p, replace = F) %>% dplyr::pull(V1)
    idx <- which(fields_df$V1 %in% fields_sub)
  }
  if (is.na(total)) {
    total <- nrow(fread(fn))
  }
  
  if (!is.na(k)) {
    set.seed(123)  # Set seed for reproducibility
    row_idx <- sample(1:total, k)
  } else {
    row_idx <- 1:total
  }
  if (!is.na(p)) {
    data <- fread(fn, select = idx)[row_idx, ]
  } else{
    data <- fread(fn)[row_idx, ]
  }
  return(data)
}
