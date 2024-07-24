# Load necessary libraries
library(quantmod)

# Function to find the common start date for a list of ETFs
find_common_start_date <- function(etfs) {
  
  # Initialize a vector to store the first date of each ETF
  first_dates <- vector()
  names_list <- vector()
  
  # Loop through each ETF to get its first date
  for (etf in etfs) {
    # Retrieve the stock data
    stock_data <- try(getSymbols(etf, src = "yahoo", auto.assign = FALSE), silent = TRUE)
    
    # Check if data retrieval was successful
    if (inherits(stock_data, "try-error")) {
      warning(paste("Failed to retrieve data for", etf))
      next
    }
    
    stock_data <- na.omit(stock_data)
    
    # Get the first date of available data
    first_date <- try(index(stock_data)[1], silent = TRUE)
    
    # Check if date retrieval was successful
    if (inherits(first_date, "try-error")) {
      warning(paste("Failed to extract first date for", etf))
      next
    }
    
    # Append the first date to the vector
    first_dates <- c(first_dates, first_date)
    names_list <- c(names_list, etf)
  }
  
  # Find the earliest first date
  earliest_date_index <- which.max(as.Date(first_dates, origin = "1970-01-01"))
  earliest_date <- as.Date(first_dates[earliest_date_index])
  earliest_etf <- names_list[earliest_date_index]
  
  # Print the ETF with the earliest start date
  print(paste("The asset with the earliest start date is", earliest_etf, "with a start date of", earliest_date,", index: ", earliest_date_index))
  
  # Find the latest of the first dates
  common_start_date <- max(as.Date(first_dates, origin = "1970-01-01"))
  
  return(common_start_date)
}
