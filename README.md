# Subsampling

This repository contains an R script that efficiently samples multiple datasets from a large CSV (or similarly delimited) file and a supporting C++ function. 

This script utilizes:

- `Rcpp` for seamless integration of C++ functions into R.
- `tictoc` for performance logging.
- `parallel` for distributing work across multiple CPU cores

## Overview

The script performs the following tasks:
- **Efficient File Scanning:** Opens the file in binary mode and identifies the byte offsets of newline characters to quickly locate the beginning of each line.
- **Random Sampling:** Randomly selects a specified number of rows and columns from the file. The first line is used as the header to extract column names.
- **C++ Integration:** Uses an external C++ function (`parse_line_cpp` defined in `parse_line.cpp`) to parse each line, ensuring high performance when processing large files.
- **Parallel Processing:** Distributes the sampling work across multiple cores using R's `parallel` package.
- **Output Generation:** Writes each subsampled dataset to a separate CSV file within a designated output directory.

## Dependencies

Before running the script, ensure that the following R packages are installed:
- **Rcpp**
- **tictoc**
- **parallel**

You also need the C++ source file `parse_line.cpp` in the same directory as the R script, as it provides the `parse_line_cpp` function used for parsing lines.

## Configuration

At the top of the script, you can adjust key parameters:

- `file_path`: Path to the dataset.
- `num_cores`: The number of cores to use (default: `#of_Core_on_your_computer - 1`). 
- Sampling Parameters:
  - `n`: Number of rows to sample per dataset.
  - `k`: Number of columns to sample per dataset.
  - `m`: Number of datasets to sample.

- Output Directory:
  - `dir`: The directory where the subsampled CSV files will be saved (default: `"./subsamples"`).


## Execution

- Make sure that input parameter of the `parse_line_cpp` function is correct. This parameter can be different for different dataset files. Common forms are `\t` and `,`.
```
headerLN <- parse_line_cpp(header_line, "\t", "\"")
headerLN <- parse_line_cpp(header_line, ",", "\"")
```

- Source the script in R:

```
source("Subsampling.R")
```

## Process Flow
 - The script first computes the byte offsets for the start of each line in the file.
 - It reads the header to determine column names.
 - It then randomly samples rows and columns to generate multiple subsamples using multiple cores, saving each as a CSV file in the `subsamples` directory.
 - Execution times for different steps are logged using the `tictoc` package.

## Output

The subsampled datasets are saved as individual CSV files in the specified output directory (`subsamples`). Each file is named in the format `subsampledData_<dataset_number>.csv`.


## Author

  **Runhan Wang**  
  Email: [runhanw@umich.edu](mailto:runhanw@umich.edu)  

