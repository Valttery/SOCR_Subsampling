#!/bin/bash
# run_r.sh
# This script runs the R program "Subsampling.R" multiple times by replacing placeholders in the template.
# In addition to setting m, n, and k, it now includes an extra parameter p (a filename) taken from an array of filenames.
# All output is collected in the file results.txt.

# Define the output file
output_file="results.txt"
# Clear or create the output file
> "$output_file"

# Define an array containing multiple filenames
# p_array=("Binomial_append_1M_10k_2sd.txt")
p_array=("Binomial_10k_1k.txt" "Binomial_100k_10k.txt" "Binomial_append_100k_1k.txt" "Binomial_append_1M_1k.txt" "Binomial_append_1M_10k_2sd.txt")

# Loop through the parameters:
# p is taken from the filename array;
# m takes the values: 5000, 10000;
# n takes the values: 300, 500;
# k is calculated as n/10 (n = rows, k = columns, m = samples).
for p in "${p_array[@]}"; do
  for m in 10000 5000; do
    for n in 500 300; do
      killall R
      k=$(awk "BEGIN {printf \"%d\", $n/10}")
      echo "Running Subsampling.R with parameters: p=${p}, m=${m}, n=${n}, k=${k}" | tee -a "$output_file"
      # Use sed to replace placeholders in the template and generate a temporary R script file: tmp_script.R
      sed -e "s/M_VALUE/${m}/g" \
          -e "s/N_VALUE/${n}/g" \
          -e "s/K_VALUE/${k}/g" \
          -e "s|PATH_VALUE|\"${p}\"|g" Subsampling.R > tmp_script.R
      # Run the generated R script
      Rscript tmp_script.R >> "$output_file" 2>&1
      echo "--------------------------------------" >> "$output_file"
    done
  done
done

# Remove the temporary file
rm tmp_script.R

echo "All runs are completed. Please check the results in ${output_file}."
