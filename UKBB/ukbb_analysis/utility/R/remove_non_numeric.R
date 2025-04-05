#' Remove Non-Numeric Columns from Data
#'
#' This function iterates through each column in a data frame and removes columns that are not numeric.
#' It's useful for preparing data for analyses that require numeric input.
#' @param data A data frame that may contain non-numeric columns.
#' @return A data frame containing only the columns that are numeric.
#' @export
remove_non_numeric <- function(data){
  setDT(data)  # 确保数据是data.table对象
  numeric_cols <- sapply(data, is.numeric)  # 确定每列是否是数值型
  data <- data[, numeric_cols, with = FALSE]  # 选择数值型列
  return(data)
}