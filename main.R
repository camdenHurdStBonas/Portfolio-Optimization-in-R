#   Set up

#Cleared the work space
rm(list=ls())
#sets the variable "path" to the name of Working Directory
path <- "/set/your/path/here"
#Set the Working Directory using the variable "path"
setwd(path)

source("portfolio.R")

source("find_common_start_date.R")

source("get_ticker_names.R")

stock_list <- get_common_etfs()

common_start_date <- find_common_start_date(stock_list)

weights <- c(1, rep(0, length(stock_list) - 1))

test <- portfolio(stock_list, from = common_start_date)

test$summary(weights, A= 4)
