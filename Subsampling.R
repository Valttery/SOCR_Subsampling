rm(list = ls(all = TRUE))  # Remove all objects
gc()                       # Force garbage collection

library(Rcpp)
library(tictoc)
library(parallel)

file_path <- "ukb44534_compiled_tab-001.csv"
#file_path <- "UKBB_10k_1k.csv"
#file_path <- "Binomial_10k_1k.txt"
#file_path <- "Binomial_append_100k_10k.txt"
#file_path <- "Binomial_append_1M_10k_2sd.txt"
#file_path <- PATH_VALUE

# Define parameters for dataset sampling. N,M,K_values are placeholders for the shell script
#n <- N_VALUE  # Number of rows to sample per dataset
#k <- K_VALUE   # Number of columns to sample per dataset
#m <- M_VALUE   # Number of datasets to sample
n <- 300  # Number of rows to sample per dataset
k <- 30   # Number of columns to sample per dataset
m <- 2000   # Number of datasets to sample
targetMissingness <- 0.1  # acceptable missingness value
dir <- "subsamples" # Target directory to put subsampled data
targetOutput <- "cancer_diagnosed_by_doctor_f2453_0_0"


# Create the subdirectory if it does not exist
if (!dir.exists(dir)) {
  dir.create(dir)
}

# Use one core for the operating system and the rest for sampling
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)



# ----------------- Subsampling function definitions ---------------------------

# Source the external C++ function parse_line_cpp()
Rcpp::sourceCpp(file = "parse_line.cpp")

# -----------------------------------------------------------------------------
# Function: build_line_index_binary
#
# Scans the entire file in binary mode to find the positions of all newline ('\n')
# characters. Returns a vector of byte offsets where each newline is found.
# -----------------------------------------------------------------------------
build_line_index_binary <- function(file_path) {
  con <- file(file_path, "rb")
  on.exit(close(con))
  
  chunk_size <- 65536
  pos <- 0
  offsets <- numeric(1e6)  # initial guess; will expand if needed
  count <- 0
  
  repeat {
    buf <- readBin(con, what = "raw", n = chunk_size)
    nread <- length(buf)
    if (nread == 0) break
    
    nl_positions <- which(buf == as.raw(0x0A))  # newline character '\n'
    for (nlpos in nl_positions) {
      count <- count + 1
      if (count > length(offsets)) {
        offsets <- c(offsets, numeric(length(offsets)))
      }
      offsets[count] <- pos + (nlpos - 1)
    }
    
    pos <- pos + nread
    if (nread < chunk_size) break
  }
  
  offsets[1:count]
}

# -----------------------------------------------------------------------------
# Function: build_line_offsets
#
# Converts the newline offsets into line start offsets.
# For the first line, the start is byte 0, and for subsequent lines the start
# is one byte after the previous newline.
# -----------------------------------------------------------------------------
build_line_offsets <- function(file_path) {
  nl_offsets <- build_line_index_binary(file_path)
  num_lines <- length(nl_offsets)
  if (num_lines == 0) return(numeric(0))
  
  line_offsets <- numeric(num_lines)
  line_offsets[1] <- 0
  if (num_lines > 1) {
    line_offsets[2:num_lines] <- nl_offsets[1:(num_lines - 1)] + 1
  }
  line_offsets
}

# -----------------------------------------------------------------------------
# Function: read_line_at
#
# Reads a specific line from the file based on its starting byte offset.
# It calculates how many bytes to read by using the next line's start offset
# or the file size for the last line.
#
# Parameters:
#   - fileCon: open file connection (binary mode)
#   - line_offsets: vector of starting offsets for each line
#   - i: the index of the line to read
#   - fsize: total size of the file in bytes
#
# Returns:
#   - The text content of the line, with trailing newline characters removed.
# -----------------------------------------------------------------------------
read_line_at <- function(fileCon, line_offsets, i, fsize) {
  start_pos <- line_offsets[i]
  end_pos <- if (i < length(line_offsets)) line_offsets[i + 1] - 1 else (fsize - 1)
  if (end_pos < start_pos) stop("Invalid line offsets, end_pos < start_pos.")
  
  length_to_read <- end_pos - start_pos + 1
  seek(fileCon, where = start_pos, origin = "start")
  raw_data <- readBin(fileCon, what = "raw", n = length_to_read)
  line_str <- rawToChar(raw_data)
  
  # Normalize line endings: remove any trailing \r, \n, or \r\n
  line_str <- sub("\r?\n$", "", line_str)
  line_str
}

# -----------------------------------------------------------------------------
# Function: sample_dataset
#
# Encapsulates the sampling process for a single dataset. It randomly samples
# n rows from the file and k columns from the CSV header, then builds a data frame.
#
# Parameters:
#   - file_path: path to the CSV file
#   - line_offsets: precomputed vector of line start offsets
#   - fsize: total file size in bytes
#   - headerLN: a character vector of header column names
#   - n: number of rows to sample
#   - k: number of columns to sample
#
# Returns:
#   - A data frame with the sampled data.
# -----------------------------------------------------------------------------
sample_dataset <- function(file_path, line_offsets, fsize, headerLN, n, k, 
                           metatable = NULL, outcomeCol = NULL, targetMissingness = 0) {
  if (is.null(metatable)) {
    # If no metatable, sample randomly from all columns.
    all_cols <- seq_along(headerLN)
    if (!is.null(outcomeCol)) {
      outcome_index <- match(outcomeCol, headerLN)
      if (is.na(outcome_index))
        stop("Outcome column not found in headerLN.")
      # Exclude the outcome column from random sampling.
      all_cols <- setdiff(all_cols, outcome_index)
    }
    sampled_cols <- sample(all_cols, k)
  } else {
    # With metatable, first read valid columns.
    criteria <- read.csv(metatable, header = FALSE, stringsAsFactors = FALSE)
    valid_names <- criteria[criteria[[2]] < targetMissingness & criteria[[3]] == 0, 1]
    valid_idx <- which(headerLN %in% valid_names)
    
    if (!is.null(outcomeCol)) {
      outcome_index <- match(outcomeCol, headerLN)
      if (is.na(outcome_index))
        stop("Outcome column not found in headerLN.")
      # Remove outcome column from valid indices to avoid duplicating it.
      valid_idx <- setdiff(valid_idx, outcome_index)
    }
    
    if (length(valid_idx) < k) {
      stop("Good columns are not enough to sample from.")
    }
    sampled_cols <- sample(valid_idx, k)
  }
  
  # Sample n rows at random.
  na_strings <- c("", "NA", "NULL")
  num_rows <- length(line_offsets)
  r <- sample(num_rows, n)
  
  fileCon <- file(file_path, "rb")
  on.exit(close(fileCon))
  
  rowElements <- vector("list", n)  # List to store each sampled row
  
  for (j in 1:n) {
    i <- r[j]  # Get the j-th random row index
    this_line_txt <- read_line_at(fileCon, line_offsets, i, fsize)
    ln <- parse_line_cpp(this_line_txt, sep = "\t", quote = "\"")
    # ln <- parse_line_cpp(this_line_txt, sep = "\t", quote = "\"")
    ln[ln %in% na_strings] <- NA
    
    # Ensure the row has enough columns; pad with NA if necessary.
    max_needed <- max(c(sampled_cols, if (!is.null(outcomeCol)) outcome_index else 0))
    if (length(ln) < max_needed) {
      ln <- c(ln, rep(NA, max_needed - length(ln)))
    }
    
    if (!is.null(metatable)) {
      # If outcomeCol is provided, include its value as the second column.
      if (!is.null(outcomeCol)) {
        rowElements[[j]] <- c(i, ln[outcome_index], ln[sampled_cols])
      } else {
        rowElements[[j]] <- c(i, ln[sampled_cols])
      }
    } else {
      # For non-metatable cases, adjust similarly (depending on how you want to handle the row index).
      if (!is.null(outcomeCol)) {
        # Here you might choose a different order if no row index is needed.
        rowElements[[j]] <- c(ln[sampled_cols[1]], ln[outcome_index], ln[sampled_cols[-1]])
      } else {
        rowElements[[j]] <- ln[sampled_cols]
      }
    }
  }
  
  # Convert the list of rows to a data frame.
  df <- data.frame(do.call(rbind, rowElements), stringsAsFactors = FALSE)
  
  if (!is.null(metatable)) {
    if (!is.null(outcomeCol)) {
      colnames(df) <- c("RowIndex", outcomeCol, headerLN[sampled_cols])
    } else {
      colnames(df) <- c("RowIndex", headerLN[sampled_cols])
    }
  } else {
    if (!is.null(outcomeCol)) {
      colnames(df) <- c(headerLN[sampled_cols[1]], outcomeCol, headerLN[sampled_cols[-1]])
    } else {
      colnames(df) <- headerLN[sampled_cols]
    }
  }
  
  return(df)
}


# ----------------- Integrity checking function definitions --------------------

# -----------------------------------------------------------------------------
# Function: check_na
# Calculates the percentage of missing values in each column of a dataframe.
#
# Parameters:
#   - df: A dataframe to be checked for missing values.
#
# Returns:
#   - A 2-column dataframe with:
#       * features: names of the columns;
#       * percent: the percentage of missing values in each column.
# -----------------------------------------------------------------------------
check_na <- function(df) {
  # Initialize vector to store percentage of missing values
  percent <- c()
  
  # (Unused variable preserved from original code)
  idxs <- df[1, ]
  
  # Loop through each column to compute missing value ratio
  for (col in 1:ncol(df)) {
    na_count <- sum(is.na(df[, col]))
    percent <- c(percent, na_count / nrow(df))
  }
  
  # Build and return the result data frame
  result <- data.frame(features = names(df), percent = percent)
  return(result)
}


# -----------------------------------------------------------------------------
# Function: is_constant
#
# Determines if a given vector is constant, meaning it contains only a single
# unique value after omitting missing values.
#
# Parameters:
#   - d: A vector (e.g., a dataframe column) to be checked.
#
# Returns:
#   - TRUE if the vector is constant (or empty after NA removal), FALSE otherwise.
# -----------------------------------------------------------------------------
is_constant <- function(d) {
  d <- na.omit(d)
  unq_list <- unique(d)
  if (length(unq_list) <= 1)
    return(TRUE)
  else
    return(FALSE)
}

# -----------------------------------------------------------------------------
# Function: check_constant
#
# Checks each column of a dataframe to determine whether it is constant.
#
# Parameters:
#   - df: A dataframe to be checked.
#
# Returns:
#   - A 2-column dataframe with:
#       * features: names of the columns;
#       * is_constant: indicator (1 if the column is constant, 0 if not).
# -----------------------------------------------------------------------------
check_constant <- function(df) {
  # Initialize vector to store constant indicators
  constant_sign <- c()
  
  # Loop through each column to check if it is constant
  for (i in 1:ncol(df)) {
    if (is_constant(df[, i]))
      constant_sign <- c(constant_sign, 1)
    else
      constant_sign <- c(constant_sign, 0)
  }
  
  # Build and return the result data frame
  result <- data.frame(features = names(df), is_constant = constant_sign)
  return(result)
}


# -----------------------------------------------------------------------------
# Main Processing: Sampling m Datasets
# -----------------------------------------------------------------------------
tic.clearlog()
tic("Whole process")
fsize <- file.size(file_path)

tic("Building line offsets")
line_offsets <- build_line_offsets(file_path)
num_rows <- length(line_offsets)
#num_rows <- as.integer(system(paste("wc -l", file_path, "| awk '{print $1}'"), intern = TRUE))
cat("Number of rows:", num_rows, "\n")
toc(log = TRUE, quiet = TRUE)

# Read the CSV header (first line) to get the column names.
fileCon <- file(file_path, "rb")
header_line <- read_line_at(fileCon, line_offsets, 1, fsize)
headerLN <- parse_line_cpp(header_line, "\t", "\"")
#headerLN <- parse_line_cpp(header_line, ",", "\"")
cat("Total columns in file:", length(headerLN), "\n")
close(fileCon)

# Delete all the pre-existing files in the target directory
items <- list.files(dir, full.names = TRUE)
unlink(items, recursive = TRUE)

cat(sprintf("Generating %d samples of %d rows and %d columns.\n", m, n, k))
# Create a list to store the m datasets.
datasets <- vector("list", m)

tic(sprintf("First round"))
cat(sprintf("Using %d cores.\n", num_cores))
# Sampling using multiple cores
clusterExport(cl, varlist = c("file_path", "line_offsets", "fsize", "headerLN", "n", "k", "targetMissingness", "dir",
                              "sample_dataset", "read_line_at", "check_na", "is_constant", "check_constant", "targetOutput"))
clusterEvalQ(cl, {
  library(Rcpp)
  sourceCpp(file = "parse_line.cpp")
})
datasets <- parLapply(cl, 1:m, function(ds) {
  ds_data <- sample_dataset(file_path, line_offsets, fsize, headerLN, n, k)
  na_df <- check_na(ds_data)
  constant_df <- check_constant(ds_data)
  output_file <- file.path(dir, sprintf("subsampled_%d.csv", ds))
  output_table <- file.path(dir, sprintf("Goodness_%d.csv", ds))
  write.csv(ds_data, output_file, row.names = FALSE)
  goodness_table <- merge(na_df, constant_df, by = "features")
  write.csv(goodness_table, output_table, row.names = FALSE)
  
  ds_data
})
toc(log = TRUE, quiet = TRUE)

tic("Combining metatable")
df_list <- vector("list", m)
for(i in 1:m){
  df_list[[i]] <- read.csv(file.path(dir, paste0("Goodness_", i, ".csv")))
}
meta_data <- bind_rows(df_list)
meta_data <- meta_data %>%
  group_by(features) %>%
  summarise(
    missing_percent = mean(percent),
    is_constant = round(mean(is_constant))
  )
write.csv(meta_data, "meta_data.csv", row.names = FALSE)
toc(log = TRUE, quiet = TRUE)

cat(sprintf("First round sampling finished. Starting second round.\n"))

criteria <- read.csv("meta_data.csv", header = FALSE, stringsAsFactors = FALSE)
valid_names <- criteria[criteria[[2]] < targetMissingness & criteria[[3]] == 0, 1]
valid_idx <- which(headerLN %in% valid_names)
cat(sprintf("A total of %d good features found:\n", length(valid_idx)))

tic("Second round sampling")
datasets <- parLapply(cl, 1:m, function(ds) {
  ds_data <- sample_dataset(file_path, line_offsets, fsize, headerLN, n, k, "meta_data.csv", targetOutput, targetMissingness)
  output_file <- file.path(dir, sprintf("subsampled_v2_%d.csv", ds))
  write.csv(ds_data, output_file, row.names = FALSE)

  ds_data
})
toc(log = TRUE, quiet = TRUE)




clusterCall(cl, gc)
stopCluster(cl)

toc(log = TRUE, quiet = TRUE)
cat(sprintf("A total of %d datasets have been sampled.\n", m))

log_data <- tic.log(format = TRUE)
print(log_data)

