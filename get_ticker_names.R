# Load necessary libraries
library(quantmod)
library(rvest)
library(dplyr)

# Function to get the list of S&P 500 tickers
get_sp500_tickers <- function() {
  # URL to scrape the S&P 500 tickers
  url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
  
  # Scrape the data
  sp500_page <- read_html(url)
  sp500_table <- sp500_page %>% html_table(fill = TRUE)
  
  # Extract the tickers
  sp500_tickers <- sp500_table[[1]] %>% pull(Symbol)
  sp500_tickers[63] <- "BRK-B"
  sp500_tickers[78] <- "BF-B"
  
  # Return the tickers
  return(sp500_tickers)
}

# Function to get a list of common ETFs (example from ETFdb.com)
get_common_etfs <- function() {
  # URL to scrape the ETFs list
  url <- "https://etfdb.com/compare/market-cap/"
  
  # Scrape the data
  etfs_page <- read_html(url)
  etfs_table <- etfs_page %>% html_table(fill = TRUE)
  
  # Extract the tickers
  etfs_tickers <- etfs_table[[1]] %>% pull(Symbol)
  
  # Return the tickers
  return(etfs_tickers)
}
