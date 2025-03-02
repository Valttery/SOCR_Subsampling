cat("GENERATING THE BINOMIAL 1million - 1K\n\n")
start_time_total <- Sys.time()

for (i in 1:1000) {
  start_time <- Sys.time()
  print(i)
  
  n <- 1000      # Number of observations in each batch
  p <- 1000      # Number of variables per observation
  
  # Generate a matrix of normally distributed random numbers (rounded to 3 decimals)
  X1 <- matrix(round(rnorm(n * p), 3), nrow = n, ncol = p)
  
  amplitude <- 2.0
  # Define the indices for non-zero coefficients (ensure they are within 1:p)
  nonzero <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
  beta <- amplitude * (1:p %in% nonzero)
  
  # Compute the linear combination with added noise
  ztemp <- function() X1 %*% beta + rnorm(n)
  z <- ztemp()
  
  pr <- 1 / (1 + exp(-z))  # Apply the inverse logit function
  Ytemp <- rbinom(n, 1, pr)  # Generate the Bernoulli response variable
  
  # Generate IDs for the current batch
  IDs_temp <- seq((1 + n * (i - 1)), i * n, 1)
  
  # Combine IDs, response variable, and predictor variables
  X2 <- cbind(IDs_temp, Ytemp, X1)
  X2 <- as.data.frame(X2)
  X2$IDs_temp <- as.character(X2$IDs_temp)
  
  # Append the data to the output file
  write.table(X2,
              file = "Binomial_append_1M_1k.txt",
              append = TRUE,
              eol = "\r\n",
              row.names = FALSE,
              col.names = FALSE,
              sep = ",",
              fileEncoding = "UTF-8")
  
  end_time <- Sys.time()
  print(end_time - start_time)
}
end_time_total <- Sys.time()
print(end_time_total - start_time_total)


cat("GENERATING THE BINOMIAL 100K - 1K\n\n")
start_time_total <- Sys.time()

for (i in 1:100) {
  start_time <- Sys.time()
  print(i)
  
  n <- 1000      # Number of observations per batch
  p <- 1000      # Number of variables per observation
  
  # Generate an n*p matrix of random normal numbers, rounded to three decimal places
  X1 <- matrix(round(rnorm(n * p), 3), nrow = n, ncol = p)
  
  amplitude <- 3.5
  # Set some variables to have nonzero coefficients (ensure the indices are within 1:p)
  nonzero <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
  beta <- amplitude * (1:p %in% nonzero)
  
  # Define a linear combination and add noise
  ztemp <- function() X1 %*% beta + rnorm(n)
  z <- ztemp()
  
  pr <- 1 / (1 + exp(-z))  # Apply the inverse logit function
  Ytemp <- rbinom(n, 1, pr)  # Binary response variable from a binomial distribution
  
  # Generate batch-specific IDs
  IDs_temp <- seq((1 + n * (i - 1)), i * n, 1)
  
  # Combine IDs, response variable, and predictors
  X2 <- cbind(IDs_temp, Ytemp, X1)
  X2 <- as.data.frame(X2)
  X2$IDs_temp <- as.character(X2$IDs_temp)
  
  # Append the data to a file
  write.table(X2,
              file = "Binomial_append_100k_1k.txt",
              append = TRUE,
              eol = "\r\n",
              row.names = FALSE,
              col.names = FALSE,
              sep = ",",
              fileEncoding = "UTF-8")
  
  end_time <- Sys.time()
  print(end_time - start_time)
}
end_time_total <- Sys.time()
print(end_time_total - start_time_total)
