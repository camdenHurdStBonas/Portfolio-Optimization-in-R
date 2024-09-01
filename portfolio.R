# Load necessary libraries
library(quantmod) # For Stock Data
library(quadprog) # For Quadratic Programming 
library(ggplot2)  # For Visualization
library(moments)  # For skewness and kurtosis
library(MASS)     # For fitdistr function
library(reshape2) # For melt function
suppressWarnings({
  library(ggrepel)# For frepel function
  })
library(dplyr)

# Define the portfolio function
portfolio <- function(names_list=NULL, from=NULL,rf=NULL, frequency = "Daily") {
  
  
  ### Set up:
  
  
  # Create an environment to store local variables
  obj <- new.env()
  
  # Function to calculate additional statistics for given returns
  additional_stats <- function(returns) {
    
    # creates a list of additional statistics
    add_stats <- list(
      skewness = skewness(returns, na.rm = TRUE), # Skewness
      kurtosis = kurtosis(returns, na.rm = TRUE), # Kurtosis
      VaR = log(quantile(returns, probs = 0.05, na.rm = TRUE)+1), # geometric Value at Risk
      ES = log(mean(returns[returns <= quantile(returns, probs = 0.05, na.rm = TRUE)], na.rm = TRUE)+1), # geometric Estimated Shortfall
      lower_partial_sd = log(sd(returns[returns < mean(returns, na.rm = TRUE)], na.rm = TRUE)+1), # geometric lower partial standard deviation
      sortino_ratio = ((log(mean(returns)+1)) - rf) / (log(sd(returns[returns < mean(returns, na.rm = TRUE)], na.rm = TRUE)+1)), # geometric Sortino ratio
      CAGR = (prod(1 + returns, na.rm = TRUE)^(1 / (length(returns) / freq_multiplier)) - 1), # compound annual growth rate
      ADD = log( mean( cumprod(1 + returns) / cummax( cumprod(1 + returns) ) ) ),
      MDD = min( cumprod(1 + returns) / cummax( cumprod(1 + returns)) - 1,na.rm = TRUE), # Maximum draw down
      count = length(returns) # count
    )
    
    # return list
    return(add_stats)
    
  }
  
  # Function to calculate a matrix of statistics given portfolio weights
  obj$weighted_stats<- function(port_weights,name=NULL){
    
    # Compute the weighted returns of the tangency portfolio
    weighted_returns <- excess_log_return_matrix %*% port_weights
    
    # gather additional statistics
    stats <- additional_stats(weighted_returns)
    
    # create the statistic matrix
    stats_matrix <- matrix(unlist(stats),ncol=1)
    colnames(stats_matrix) <- paste(name,"Weighted Port")
    stats_matrix <- rbind(
      log(mean(weighted_returns)+1),
      (log(mean(weighted_returns)+1))-rf,
      log(sd(weighted_returns,na.rm = TRUE)+1),
      ((log(mean(weighted_returns)+1))-rf)/(log(sd(weighted_returns,na.rm = TRUE)+1)),
      stats_matrix)
    rownames(stats_matrix) <- c("Geo Mean","Geo Risk Premium","Geo Sd","Geo Sharpe","Skewness", "Kurtosis", "Geo VaR", "Geo ES", "Geo Lower Partial SD","Geo Sortino Ratio", "CAGR","Geo ADD","MDD","Count")
    
    # return matrix
    return(stats_matrix)
    
  }
  
  # Set the frequency multiplier based on user input (only used for risk free rate division and CAGR)
  if (frequency == "Daily") {
    freq_multiplier <- 252  
  } else if (frequency == "Monthly") {
    freq_multiplier <- 12
  } else if (frequency == "Quarterly") {
    freq_multiplier <- 4
  } else if (frequency == "Yearly") {
    freq_multiplier <- 1
  } else {
    stop("Invalid frequency type. Please choose from 'Daily', 'Monthly', 'Quarterly', or 'Yearly'.")
  }
  
  
  ### Data manipulation and Error Handling:
  
  
  #suppress the warning that IRX contains NA's
  suppressWarnings({
    
  # Fetch historical risk-free rates from Yahoo Finance (e.g., 13-week Treasury bill yield)
  rf_data <- getSymbols("^IRX", src = "yahoo", from = as.Date(from), auto.assign = FALSE)
  
  })
  
  #Extracting the adjusted close
  rf_series <- ((Ad(rf_data) / 100)+1) ^ (1 / freq_multiplier)-1  # Adjusting to decimal and to the time period
  
  # Replace NA values with the previous rate or average rate
  rf_series <- rf_series[-1]  # Remove the first element because it is Na in excess Ln returns
  rf_series <- na.locf(rf_series) # Replace NA's with previous data point
  average_rf_rate <- mean(rf_series, na.rm = TRUE)
  rf_series <- na.fill(rf_series,average_rf_rate) # Replace any leftover Na's with the average
  
  # Choose the risk-free rate
  if (is.null(rf)) { # if there is no risk free rate given by userr
    # Use historical rates
    risk_free_rates <- rf_series
    rf <- average_rf_rate
  } else { # if there is a risk free rate given by user
    # Use a constant rate
    rf <- (rf+1)^(1/freq_multiplier)-1 # User has to input an anual rate because we divide it for them
    risk_free_rates <- xts(rep(rf, nrow(rf_series)), order.by = index(rf_series))
  }
  
  # Store the risk rate list and average risk free rate in local variables
  obj$rf <- rf
  obj$rf_df <- as.data.frame(risk_free_rates)
  colnames(obj$rf_df) <- c("^IRX")
  
  # Initialize an empty list to store the adjusted close prices and log returns
  adjusted_close_prices_list <- list()
  excess_log_returns_list <- list()
  
  # Parse through the names list to get the excess log returns
  for (i in names_list) {
    
    # Retrieve the stock data
    stock_data <- try(getSymbols(i, src = "yahoo", from = as.Date(from), auto.assign = FALSE), silent = TRUE)
    
    # Check if data retrieval was successful
    if (inherits(stock_data, "try-error")) {
      warning(paste("Failed to retrieve data for", i))
      next
    }
    
    # Extract the adjusted close prices
    adj_close <- try(Ad(stock_data), silent = TRUE)
    
    # Check if extraction was successful
    if (inherits(adj_close, "try-error")) {
      warning(paste("Failed to extract adjusted close prices for", i))
      next
    }
    
    # Check for NA values in the adjusted close prices
    if (any(is.na(adj_close))) {
      stop(paste("NA values found in the adjusted close prices for", i))
    }
    
    # Store the adjusted close prices in the list
    adjusted_close_prices_list[[i]] <- adj_close
    
    # Calculate the log returns
    log_returns <- diff(log(adj_close))
    
    # remove the first value it will be Na
    log_returns <- log_returns[-1,]
    
    # Ensure the risk-free rates align with the dates of the log returns
    rf_aligned <- window(risk_free_rates, start = start(log_returns), end = end(log_returns))
    
    # Calculate excess log returns
    excess_log_return <- log_returns - rf_aligned
    
    # Check if log return calculation was successful
    if (inherits(excess_log_returns_list, "try-error")) {
      warning(paste("Failed to calculate log returns for", i))
      next
    }
    
    # add this excess Ln returns to the list of all excess Ln returns
    excess_log_returns_list[[i]] <- excess_log_return
    
  }
  
  # Check if all datasets have the same length
  lengths <- sapply(excess_log_returns_list, length)
  if (length(unique(lengths)) != 1) {
    stop("Datasets have different lengths. Please ensure all datasets cover the same time period.")
  }
  
  # Create a matrix for the adjusted close prices and stores it in the local environment
  adjusted_close_prices_matrix <- do.call(cbind, adjusted_close_prices_list)
  colnames(adjusted_close_prices_matrix) <- names_list
  obj$adjusted_close_prices_matrix <-  adjusted_close_prices_matrix
  obj$adjusted_close_prices_list_df <- as.data.frame(adjusted_close_prices_matrix)
  
  # Create a matrix for the excess log returns and stores it in the local environment
  excess_log_return_matrix <- do.call(cbind, excess_log_returns_list)
  colnames(excess_log_return_matrix) <- names_list
  obj$excess_log_return_matrix <- excess_log_return_matrix
  obj$excess_log_return_df <- as.data.frame(excess_log_return_matrix)
  
  
  ### Calculations:
  
  
  # Compute the geometric mean of the excess log returns
  geometric_means <- apply(excess_log_return_matrix, 2, function(x) log(mean(x, na.rm = TRUE)+1))
  
  # Compute the geometric standard deviation of the excess log returns
  geometric_standard_deviations <- apply(excess_log_return_matrix, 2, function(x) {
    log(sd(x, na.rm = TRUE)+1)
  })
  
  # Compute the sharpe of the excess log returns
  sharpe_ratios <- (geometric_means - rf) / geometric_standard_deviations
  
  # Compute the Excess returns over the risk-free rate
  excess_returns <- geometric_means - rf
  
  # Compute the number of observations for each stock
  n = length(excess_log_returns_list)
  
  dates_list <- index(excess_log_return_matrix)
  end_date <- as.Date(dates_list[length(dates_list)])
  
  # Compute additional stats for each stock
  stock_stats <- lapply(excess_log_returns_list, additional_stats)
  
  # Calculate the Equal Weight portfolio weights
  equal_weights <- rep(1 / length(excess_returns), length(excess_returns))
  
  # Compute the covariance matrix of the excess log returns
  covariance_matrix <- cov(excess_log_return_matrix, use = "complete.obs")
  
  # Regularize the covariance matrix
  epsilon <- 1e-6  # Small regularization parameter
  covariance_matrix <- covariance_matrix + epsilon * diag(nrow(covariance_matrix))
  
  # Compute the correlation matrix of the log returns
  correlation_matrix <- cor(excess_log_return_matrix, use = "complete.obs")
  
  
  ### Quadratic Programming:
  
  
  # Solve the quadratic programming problem for the tangency portfolio
  Dmat <- 2 * covariance_matrix
  dvec <- rep(0, length(excess_returns))
  
  # Set up constraints: weights sum to 1 and weights >= 0
  Amat <- cbind(excess_returns, diag(length(excess_returns)))
  bvec <- c(1, rep(0, length(excess_returns)))
  
  result <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
  
  # Tangency portfolio weights
  tangency_weights <- result$solution
  
  # Set small weights to zero
  threshold <- 1e-10  # Define a threshold for small weights
  tangency_weights[abs(tangency_weights) < threshold] <- 0
  
  # Normalize weights to ensure they sum to 1
  tangency_weights <- tangency_weights / sum(tangency_weights)
  
  
  ### Storage and more Calculations:
  
  
  # Calculate geometric mean of the tangency portfolio
  tangency_geometric_mean <- sum(tangency_weights * geometric_means)
  
  # Calculate standard deviation of the tangency portfolio
  tangency_variance <- t(tangency_weights) %*% covariance_matrix %*% tangency_weights
  tangency_sd <- log(sqrt(tangency_variance)+1)
  
  # Calculate Sharpe ratio of the tangency portfolio
  tangency_sharpe_ratio <- (tangency_geometric_mean - rf) / tangency_sd
  
  # Store correlation matrix
  obj$correlation_matrix <- correlation_matrix
  obj$correlation_df <- as.data.frame(correlation_matrix)
  
  # Store tangncy weights
  obj$tangency_weights <- tangency_weights
  obj$tangency_weights_df <- as.data.frame(tangency_weights)
  rownames(obj$tangency_weights_df) <- names_list
  rownames(obj$tangency_weights_df,names_list)
  
  # Store equal weights
  obj$equal_weights <- equal_weights
  obj$equal_weights_df <- as.data.frame(equal_weights)
  rownames(obj$equal_weights_df,names_list)
  
  # Store stocks statistics matrices
  stock_stats_matrix <- sapply(stock_stats, function(x) unlist(x))
  rownames(stock_stats_matrix) <- c("Skewness", "Kurtosis", "Geo VaR", "Geo ES", "Geo Lower Partial SD","Geo Sortino Ratio", "CAGR","Geo ADD","MDD","Count")
  stock_stats_matrix <- t(stock_stats_matrix)
  stock_stats_matrix <- cbind(
    "Geo Mean"= geometric_means,
    "Geo Risk Premium"= excess_returns,
    "Geo SD"= geometric_standard_deviations,
    "Geo Sharpe"= sharpe_ratios, 
    stock_stats_matrix,
    "Tangency Weights" = tangency_weights)
  
  # Store statistics matrices into local variables
  obj$stock_stats_matrix <-stock_stats_matrix
  obj$stock_stats_df <- as.data.frame(stock_stats_matrix)
  obj$tangency_stats_matrix <-  obj$weighted_stats(tangency_weights,name="Tangency")
  obj$tangency_stats_df <-  as.data.frame(obj$weighted_stats(tangency_weights,name="Tangency"))
  obj$equal_weight_stats_matrix <-  obj$weighted_stats(equal_weights,name="Equal")
  obj$equal_weight_stats_df <-  as.data.frame(obj$weighted_stats(equal_weights,name="Equal"))

  
  ### Functions:
  
    
  # Define the function to visualize the efficient frontier function
  obj$ef <- function() {
    
    # Efficient frontier
    returns_seq <- seq(min(excess_returns), max(geometric_means), length.out = 100)
    efficient_frontier <- data.frame(Return = numeric(100), Risk = numeric(100))
    
    for (j in 1:length(returns_seq)) {
      # Solve the quadratic programming problem for each target return
      dvec_ef <- rep(0, length(excess_returns))
      Amat_ef <- cbind(1, excess_returns, diag(length(excess_returns)))
      bvec_ef <- c(1, returns_seq[j], rep(0, length(excess_returns)))
      
      result_ef <- try(solve.QP(Dmat, dvec_ef, Amat_ef, bvec_ef, meq = 2),silent = TRUE)
      
      if (inherits(result_ef, "try-error")) {
        warning(paste("No solution for return:", returns_seq[j]))
        next
      }
      
      weights_ef <- result_ef$solution
      weights_ef[abs(weights_ef) < threshold] <- 0
      weights_ef <- weights_ef / sum(weights_ef)  # Normalize weights to sum to 1
      
      # Calculate return and risk for the efficient frontier
      return_ef <- sum(weights_ef * geometric_means)
      risk_ef <- log(sqrt(t(weights_ef) %*% covariance_matrix %*% weights_ef)+1)
      
      efficient_frontier$Return[j] <- return_ef
      efficient_frontier$Risk[j] <- risk_ef
    }
    
    # Remove rows where Risk or Return is zero
    efficient_frontier <- efficient_frontier[!(efficient_frontier$Risk == 0 & efficient_frontier$Return == 0), ]
    
    # Visualization
    ef_plot <- ggplot(data = efficient_frontier, aes(x = Risk * 100, y = Return * 100)) +
      geom_line(aes(color = "Efficient Frontier")) +
      geom_point(aes(x = tangency_sd * 100, y = tangency_geometric_mean * 100, color = "Tangency Portfolio"), size = 4) +
      geom_abline(aes(intercept = rf * 100, slope = tangency_sharpe_ratio, color = "Capital Allocation Line"), linetype = "dashed") +
      ggtitle(paste("Efficient Frontier with Tangency Portfolio and Capital Allocation Line of ",frequency," Excess Ln Returns\n(",from," to ",end_date,")",sep="")) +
      xlab("Geometric Standard Deviation in P.P. (%)") +
      ylab("Geometric Mean in P.P. (%)") +
      theme_minimal() +
      scale_color_manual(name = "Legend",
                         values = c("Efficient Frontier" = "blue",
                                    "Tangency Portfolio" = "red",
                                    "Capital Allocation Line" = "green"),
                         labels = c(
                           paste("Capital Allocation Line\n(Rf= ",round(rf*100,2),"%, Geo Sharpe= ",round(tangency_sharpe_ratio,4),")",sep=""),
                           "Efficient Frontier",
                           paste("Tangency Portfolio\n(",round(tangency_geometric_mean * 100,2),"% , ",round(tangency_sd * 100,2),"%)",sep="")
                         ))
    
    print(ef_plot)
    
  }
  
  # Define the function to visualize the complete portfolio function
  obj$cal <- function(A=4) {
    
    # Check that A is not zero
    if (A == 0) {
      stop("The value of A cannot be zero. Please provide a non-zero value for A.")
    }
    
    # Calculate the percentage of bankroll to invest in the tangency portfolio
    percentage_invested <- (tangency_geometric_mean - rf) / (A/5 * tangency_sd)
    
    # Calculate the geometric mean and variance of the complete portfolio
    complete_port_geometric_return <- (1 - percentage_invested) * rf + percentage_invested * tangency_geometric_mean
    complete_port_sd <- percentage_invested * tangency_sd
    
    # Calculate the utility of the complete portfolio
    utility <- complete_port_geometric_return - (24/2) * A * complete_port_sd^2
    
    # Create a data frame for the indifference curve
    indifference_curve <- data.frame(
      Risk = seq(0, 2 * tangency_sd, length.out = 100),
      Return = utility + (24/2) * A * seq(0, 2 * tangency_sd, length.out = 100)^2
    )
    
    # Visualization
    cal_plot <- ggplot(data = indifference_curve, aes(x = Risk*100, y = Return*100)) +
      geom_line(aes(color = "Indifference Curve")) +
      geom_point(aes(x = tangency_sd*100, y = tangency_geometric_mean*100, color = "Tangency Portfolio"), size = 4) +
      geom_point(aes(x = complete_port_sd*100, y = complete_port_geometric_return*100, color = "Complete Portfolio"), size = 4) +
      geom_abline(aes(intercept = rf*100, slope = tangency_sharpe_ratio, color = "Capital Allocation Line"), linetype = "dashed") +
      ggtitle(paste("Indifference Curve with Complete Portfolio, Tangency Portfolio, and Capital Allocation Line of ",frequency," Excess Ln Returns (",round(percentage_invested*100,2),"% Invested) (A= ",A,")\n(",from," to ",end_date,")",sep="")) +
      xlab("Geometric Standard Deviation in P.P. (%)") +
      ylab("Geometric Mean in P.P. (%)") +
      theme_minimal() +
      scale_color_manual(name = "Legend",
                         values = c("Indifference Curve" = "blue",
                                    "Tangency Portfolio" = "red",
                                    "Complete Portfolio" = "purple",
                                    "Capital Allocation Line" = "green"),
                         labels = c(
                           paste("Capital Allocation Line\n(Rf= ",round(rf*100,2),"%, Geo Sharpe= ",round(tangency_sharpe_ratio,4),")",sep=""),
                           paste("Complete Portfolio\n(",round(complete_port_geometric_return * 100,2),"%, ",round(complete_port_sd * 100,2),"%)",sep=""),
                           paste("Indifference Curve\n(Utility= ",round(utility*100,2),"%)",sep=""),
                           paste("Tangency Portfolio\n(",round(tangency_geometric_mean * 100,2),"% , ",round(tangency_sd * 100,2),"%)",sep="")
                         ))
    
    print(cal_plot)
    
  }
  
  # Define the function to visualize weighted returns
  obj$weighted_returns <- function(port_weights,name=NULL) {
    
    # Compute the weighted returns of the tangency portfolio
    weighted_returns <- excess_log_return_matrix %*% port_weights
    
    # Calculate the cumulative returns
    cumulative_returns <- cumprod(weighted_returns+1)-1
    
    # Fit a lognormal distribution to the tangency portfolio returns
    fit <- fitdistr(weighted_returns+1, "lognormal")
    meanlog <- fit$estimate["meanlog"]
    sdlog <- fit$estimate["sdlog"]
    
    # Convert to data frame for plotting
    dates <- index(excess_log_return_matrix)
    weighted_returns_df <- data.frame(Date = dates, Weighted_Returns = as.numeric(weighted_returns))
    cumulative_returns_df <- data.frame(Date = dates, Cumulative_Returns = as.numeric(cumulative_returns))
    
    # Calculate the geometric mean and geometric standard deviation
    geom_mean <- log(mean(weighted_returns)+1)
    
    # Calculate the average cumulative return line
    cumulative_returns_df$Average_Cumulative_Return <- cumprod(rep(1 + geom_mean, length(weighted_returns))-rf) - 1
    
    # Create a dataframe for the plot
    weights_df <- data.frame(
      Stock = colnames(excess_log_return_matrix),
      Weight = port_weights
    )
    
    # Sort the dataframe by weight in descending order
    weights_df <- weights_df[order(-weights_df$Weight), ]
    
    # Generate a color palette
    num_stocks <- nrow(weights_df)
    colors <- rainbow(num_stocks)
    names(colors) <- weights_df$Stock
    
    # Create the bar plot
    weights_plot <- ggplot(weights_df, aes(x = reorder(Stock, -Weight), y = Weight * 100, fill = Stock)) +
      geom_bar(stat = "identity") +
      ggtitle(paste("Investment Weights in ", name, " Portfolio", sep = "")) +
      xlab("Stock") +
      ylab("Weight (%)") +
      scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, color = colors[weights_df$Stock])) +
      scale_fill_manual(values = colors) +
      theme(legend.position = "none")  # Hide the legend, as we are using color for axis labels
    
    print(weights_plot)
    
    # Create the time series plot of cumulative returns
    cumulative_returns_plot <- ggplot(cumulative_returns_df, aes(x = Date)) +
      geom_line(aes(y = Cumulative_Returns * 100, color = "Compounded Cumulative Excess Ln Returns")) +
      geom_line(aes(y = Average_Cumulative_Return * 100, color = "Geometric Mean"), linetype = "dashed") +
      ggtitle(paste("Time series of Compounded Cumulative ",frequency," Excess Ln Returns of ", name, " Weighted Portfolio (Final Value: ", round(tail(cumulative_returns, 1) * 100, 2), "%)\n(", from, " to ",end_date,")", sep = "")) +
      xlab("Date") +
      ylab(paste("Compounded Cumulative",frequency,"Excess Ln Return in P.P. (%)")) +
      theme_minimal() +
      scale_color_manual(name = "Legend",
                         values = c("Compounded Cumulative Excess Ln Returns" = "blue",
                                    "Geometric Mean" = "green"),
                                    #"Mean * 2 SD" = "red",
                                    #"Mean / 2 SD" = "red"),
                         labels = c(
                           paste("Compounded Cumulative ",frequency," Excess Ln Returns",sep = ""),
                           paste("Risk Premium (", round((log(mean(weighted_returns)+1)-rf) * 100, 2), "%)", sep = "")
                         ))
    
    print(cumulative_returns_plot)
    
    # Create the histogram of weighted returns
    hist(weighted_returns+1, breaks = 50, probability = TRUE, col = "lightblue", xlab = paste(frequency," Excess Ln Growth Rates",sep=""), main = paste("Histogram of ",name," Weighted Portfolio ",frequency," Excess Ln Returns with a Lognormal Curve\n(",from," to ",end_date,")",sep=""))
    curve(dlnorm(x, meanlog = meanlog, sdlog = sdlog), add = TRUE, col = "red", lwd = 2)
    # Draw vertical lines for mean and ±2 standard deviations
    abline(v = log(mean(weighted_returns)+1)+1, col = "green", lwd = 2, lty = 2)  # Mean
    abline(v = (log(mean(weighted_returns)+1)+1) * (log(sd(weighted_returns)+1)+1)^2, col = "red", lwd = 2, lty = 2)  # Mean + 2 SD
    abline(v = (log(mean(weighted_returns)+1)+1) / (log(sd(weighted_returns)+1)+1)^2, col = "red", lwd = 2, lty = 2)  # Mean - 2 SD
    abline(v = (log(mean(weighted_returns)+1)+1) * (log(sd(weighted_returns)+1)+1), col = "yellow", lwd = 2, lty = 2)  # Mean + 2 SD
    abline(v = (log(mean(weighted_returns)+1)+1) / (log(sd(weighted_returns)+1)+1), col = "yellow", lwd = 2, lty = 2)  # Mean - 2 SD
    grid()
    # Add a legend
    legend("topright",
           legend = c(
             paste("Geometric Mean (",round(log(mean(weighted_returns)+1)+1,4),")",sep=""),
             paste("Geo Mean * Geo SD ^2 (",round((log(mean(weighted_returns)+1)+1) * (log(sd(weighted_returns)+1)+1)^2,4),"%)",sep=""),
             paste("Geo Mean / Geo SD ^2 (",round((log(mean(weighted_returns)+1)+1) / (log(sd(weighted_returns)+1)+1)^2,4),"%)",sep=""),
             paste("Geo Mean * Geo SD (",round((log(mean(weighted_returns)+1)+1) * (log(sd(weighted_returns)+1)+1),4),"%)",sep=""),
             paste("Geo Mean / Geo SD (",round((log(mean(weighted_returns)+1)+1) / (log(sd(weighted_returns)+1)+1),4),"%)",sep="")
             ), 
           col = c( "green", "red", "red","yellow","yellow"),
           lwd = 2,
           lty = 2,
           cex = 0.8,
           bty = "n")
    
    # Create dummy data for legend
    legend_data <- data.frame(
      Line_Type = c("Excess Ln Returns","Geometric Mean","Mean * SD", "Mean / SD", "Mean * 2 SD", "Mean / 2 SD"),
      yintercept = c(
        NA,
        log(mean(weighted_returns)+1),
        (log(mean(weighted_returns)+1)+1) * (log(sd(weighted_returns)+1)+1) - 1,
        (log(mean(weighted_returns)+1)+1) / (log(sd(weighted_returns)+1)+1) - 1,
        (log(mean(weighted_returns)+1)+1) * (log(sd(weighted_returns)+1)+1)^2 - 1,
        (log(mean(weighted_returns)+1)+1) / (log(sd(weighted_returns)+1)+1)^2 - 1
      )
    )
    
    # Create the time series plot of weighted returns
    weighted_returns_ts_plot <- ggplot(weighted_returns_df, aes(x = Date, y = Weighted_Returns*100)) +
      geom_line(color = "blue") +
      geom_hline(data = legend_data, aes(yintercept = yintercept*100, color = Line_Type), linetype = "dashed", size = 1) +
      scale_color_manual(name = "Legend", 
                         values = c(
                           "Excess Ln Returns" = "blue",
                           "Geometric Mean" = "Green",
                           "Mean * SD" = "yellow",
                           "Mean / SD" = "yellow",
                           "Mean * 2 SD" = "red",
                           "Mean / 2 SD" = "red"
                           ),
                         labels = c(
                           paste(frequency," Excess Ln Returns",sep=""),
                           paste("Geometric Mean (",round((log(mean(weighted_returns)+1))*100,2),"%)",sep=""),
                           paste("Geo Mean * Geo SD ^2 (",round(((log(mean(weighted_returns)+1)+1) * (log(sd(weighted_returns)+1)+1)^2 - 1)*100,2),"%)",sep=""), 
                           paste("Geo Mean * Geo SD (",round(((log(mean(weighted_returns)+1)+1) * (log(sd(weighted_returns)+1)+1) - 1)*100,2),"%)",sep=""),
                           paste("Geo Mean / Geo SD ^2 (",round(((log(mean(weighted_returns)+1)+1) / (log(sd(weighted_returns)+1)+1)^2 - 1)*100,2),"%)",sep=""),
                           paste("Geo Mean / Geo SD (",round(((log(mean(weighted_returns)+1)+1) / (log(sd(weighted_returns)+1)+1) - 1)*100,2),"%)",sep="")
                           )
                         ) +
      ggtitle(paste("Time Series of ",name, " Weighted Portfolio ",frequency," Excess Ln Returns\n(", from, " to ",end_date,")", sep = "")) +
      xlab("Date") +
      ylab(paste(frequency," Excess Ln Returns in P.P. (%)",sep="")) +
      theme_minimal()
    
    print(weighted_returns_ts_plot)
    
    # Calculate the drawdowns
    drawdowns <- (cumulative_returns+1) / cummax(cumulative_returns+1) - 1
    
    # Calculate the average drawdown
    average_drawdown <- log(mean(drawdowns, na.rm = TRUE)+1)
    
    # Calculate the average drawdown
    sd_drawdown <- log(sd(drawdowns, na.rm = TRUE)+1)+1
    
    # Convert to data frame for plotting
    drawdowns_df <- data.frame(Date = dates_list, Drawdown = as.numeric(drawdowns))
    
    # Create the time series plot of drawdowns
    drawdown_plot <- ggplot(drawdowns_df, aes(x = Date, y = Drawdown * 100)) +
      geom_line(aes(color = "Drawdown")) +
      geom_hline(aes(yintercept = average_drawdown * 100, color = "Average Drawdown"), linetype = "dashed", size = 1) +
      geom_hline(aes(yintercept = ((average_drawdown + 1) / sd_drawdown - 1) * 100, color = "Average Drawdown / SD"), linetype = "dashed", size = 1) +
      geom_hline(aes(yintercept = ((average_drawdown + 1) / sd_drawdown^2 - 1) * 100, color = "Average Drawdown / SD^2"), linetype = "dashed", size = 1) +
      ggtitle(paste("Time Series of ", name, " Weighted Portfolio of ", frequency, " Excess Ln Drawdowns\n(", from, " to ", end_date, ")", sep = "")) +
      xlab("Date") +
      ylab(paste(frequency, " Excess Ln Drawdown in P.P. (%)", sep = "")) +
      theme_minimal() +
      scale_color_manual(name = "Legend",
                         values = c("Drawdown" = "blue", 
                                    "Average Drawdown" = "green", 
                                    "Average Drawdown / SD" = "yellow", 
                                    "Average Drawdown / SD^2" = "red"),
                         labels = c(
                           paste("Geo Mean Drawdown: ", round(average_drawdown * 100, 2), "%", sep = ""),
                           paste("Geo Mean Drawdown / Geo SD: ", round(((average_drawdown + 1) / sd_drawdown - 1) * 100, 2), "%", sep = ""),
                           paste("Geo Mean Drawdown / Geo SD^2: ", round(((average_drawdown + 1) / sd_drawdown^2 - 1) * 100, 2), "%", sep = ""),
                           paste(frequency," Excess Ln Drawdown",sep="")
                         )) +
      theme(legend.position = "right")
    
    print(drawdown_plot)
    
  }
  
  # Define the function to visualize tangency returns
  obj$tangency_returns <- function() {
    
    obj$weighted_returns(tangency_weights,name = "Tangency")
    
  }
  
  # Define the function to visualize tangency returns
  obj$equal_weight_returns <- function() {
    
    obj$weighted_returns(equal_weights, name= "Equal")
    
  }
  
  # Define the function to visualize a comparison plot
  obj$comparison_plot <- function(weights) {
    
    # Create a dataframe for the plot
    df <- data.frame(
      Date = index(excess_log_return_matrix),
      Tangency = excess_log_return_matrix %*% tangency_weights,
      Weighted = excess_log_return_matrix %*% weights,
      Equal = excess_log_return_matrix %*% equal_weights)
    
    # Melt the dataframe for ggplot2
    df_melt <- melt(df, id.vars = "Date", variable.name = "Portfolio", value.name = "Log_Return")
    
    # Calculate cumulative returns for each portfolio
    df_melt$Cumulative_Return <- ave(df_melt$Log_Return, df_melt$Portfolio, FUN = function(x) cumprod(x + 1) - 1)
    
    # Extract the last cumulative return value for each portfolio
    final_cumulative_returns <- aggregate(Cumulative_Return ~ Portfolio, data = df_melt, FUN = function(x) tail(x, 1))
    
    # Prepare labels for the legend
    labels <- with(final_cumulative_returns, paste(Portfolio,
                                                   "\n(Final Value: ", round(Cumulative_Return * 100, 2), "%)", sep = ""))
    
    # Calculate geometric means
    geom_means <- df %>%
      summarise(across(Tangency:Equal, ~ log(mean(.)+1)))
    
    # Create a dataframe for the geometric mean lines
    geom_mean_df <- data.frame(
      Date = index(excess_log_return_matrix),
      Tangency_Geo_Mean = cumprod(rep(1 + geom_means$Tangency, nrow(df))-rf) - 1,
      Weighted_Geo_Mean = cumprod(rep(1 + geom_means$Weighted, nrow(df))-rf) - 1,
      Equal_Geo_Mean = cumprod(rep(1 + geom_means$Equal, nrow(df))-rf) - 1
    )
    
    # Melt the geometric mean dataframe for ggplot2
    geom_mean_melt <- melt(geom_mean_df, id.vars = "Date", variable.name = "Portfolio", value.name = "Geo_Mean_Cumulative_Return")
    
    # Adjust the Portfolio labels to match the main data
    geom_mean_melt$Portfolio <- gsub("_Geo_Mean", "", geom_mean_melt$Portfolio)
    
    # Plot the cumulative returns
    comparison_plot <- ggplot(df_melt, aes(x = Date, y = Cumulative_Return * 100, color = Portfolio)) +
      geom_line(size = 1) +
      geom_line(data = geom_mean_melt, aes(x = Date, y = Geo_Mean_Cumulative_Return * 100, color = Portfolio), linetype = "dotted", size = 1) +
      labs(title = paste("Time Series of Portfolio Compounded Cumulative ", frequency, " Excess Ln Return Comparison\n(", from, " to ", end_date, ")", sep = ""), 
           x = "Date", 
           y = paste("Compounded Cumulative ", frequency, " Excess Ln Return in P.P. (%)", sep = "")) +
      theme_minimal() +
      scale_color_manual(values = c("Tangency" = "blue", "Weighted" = "red", "Equal" = "green"), labels = labels)
    
    print(comparison_plot)
    
  }
  
  # Define the function to visualize correlation heat map
  obj$plot_correlation_heatmap <- function() {
    
    # Melt the correlation matrix to long format for ggplot2
    correlation_matrix_melted <- melt(correlation_matrix)
    
    # Create the heatmap plot
    heatmap_plot <- ggplot(data = correlation_matrix_melted, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient(low = "green", high = "red", name = "Correlation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + # Ensure vertical text
      ggtitle(paste(frequency," Excess Ln Returns Correlation Matrix Heatmap (",from," to ",end_date,")",sep="")) +
      xlab("Assets") +
      ylab("Assets")
    
    print(heatmap_plot)
  }
  
  # Define the function to visualize Risk-Return Scatter Plot
  obj$risk_return_scatter <- function() {
    
    # Compute the average Sharpe ratio
    average_sharpe <- mean(sharpe_ratios, na.rm = TRUE)
    
    # Create a data frame for plotting
    risk_return_df <- data.frame(
      Asset = colnames(excess_log_return_matrix),
      Geometric_Mean = geometric_means * 100,
      Geometric_SD = geometric_standard_deviations * 100
    )
    
    # Plotting
    risk_return_plot <- ggplot(risk_return_df, aes(x = Geometric_SD, y = Geometric_Mean, label = Asset)) +
      geom_point(color = "blue") +
      geom_text(vjust = -0.5, hjust = 0.5) +
      geom_abline(aes(intercept = rf * 100, slope = average_sharpe, color = "Average Sharpe Line"), linetype = "dashed") +
      ggtitle(paste(frequency, " Excess Ln Returns Risk-Return Scatter Plot\n(", from, " to ", end_date, ")", sep = "")) +
      xlab("Geometric Standard Deviation in P.P. (%)") +
      ylab("Geometric Mean in P.P. (%)") +
      theme_minimal() +
      scale_color_manual(name = "Legend",
                         values = c("Average Sharpe Line" = "red"),
                         labels = c(paste("Average Sharpe Line\n(Rf= ",round(rf*100,2),"%, Avg Geo Sharpe= ", round(average_sharpe, 4),")", sep = ""))) +
      theme(legend.position = "right")
    
    print(risk_return_plot)
  }
  
  # Define the function to visualize cumulative returns
  obj$plot_cumulative_returns <- function() {
    
    # Calculate the compounded cumulative returns for each stock
    cumulative_returns_matrix <- cumprod(1 + excess_log_return_matrix) - 1
    
    # Convert to data frame for plotting
    cumulative_returns_df <- as.data.frame(cumulative_returns_matrix)
    cumulative_returns_df$Date <- dates_list
    
    # Melt the data frame for ggplot
    cumulative_returns_long <- melt(cumulative_returns_df, id.vars = "Date", variable.name = "Stock", value.name = "Cumulative_Return")
    
    # Plotting
    cumulative_returns_plot <- ggplot(cumulative_returns_long, aes(x = Date, y = Cumulative_Return * 100, color = Stock)) +
      geom_line(alpha = 0.5) +
      geom_text_repel(data = cumulative_returns_long[cumulative_returns_long$Date == end_date, ],
                      aes(label = Stock),
                      size = 3,
                      box.padding = 0.3,
                      point.padding = 0.5,
                      segment.color = 'grey50') +
      ggtitle(paste("Time Series Compounded Cumulative ",frequency," Excess Returns\n(", from, " to ", end_date, ")", sep = "")) +
      xlab("Date") +
      ylab(paste("Compounded Cumulative ",frequency," Ecxess Ln Return in P.P. (%)",sep="")) +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_color_viridis_d(option = "C")
    
    print(cumulative_returns_plot)
  }
  
  # Define the function to visualize and print a summary of the given weights, equal weights, and tangency portfolio
  obj$summary <- function(weights=tangency_weights,A=4) {
    
    suppressWarnings({
    
    obj$ef()
    
    obj$cal(A= A)
    
    obj$tangency_returns()
    
    obj$plot_correlation_heatmap()
    
    obj$risk_return_scatter()
    
    obj$plot_cumulative_returns()
    
    obj$equal_weight_returns()
    
    obj$weighted_returns(weights)
    
    obj$comparison_plot(weights)
    
    })
    
    #print(obj$correlation_matrix)
    
    #print(obj$stock_stats_matrix)
    
    print(obj$tangency_stats_matrix)
    
    print(obj$equal_weight_stats_matrix)
    
    print(obj$weighted_stats(weights))
    
    obj$weighted_stats_matrix <- obj$weighted_stats(weights)
    
    obj$weighted_stats_df <- as.data.frame(obj$weighted_stats_matrix)
    
    # Store equal weights
    obj$weights <- weights
    obj$weights_df <- as.data.frame(weights)
    rownames(obj$weights_df) <- names_list
    
  }
  
  
  ### END
  
  
  return(obj)
  
}
