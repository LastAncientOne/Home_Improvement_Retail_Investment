library(quantmod)
library(PerformanceAnalytics)
library(dygraphs)

monthly_returns <- function(ticker, base_year)
{
  # Obtain stock price data from Yahoo! Finance
  stock <- getSymbols(ticker, src = "yahoo", auto.assign = FALSE) 
  # Remove missing values
  stock <- na.omit(stock)
  # Keep only adjusted closing stock prices
  stock <- stock[, 6]
  
  # Confine our observations to begin at the base year and end at the last available trading day
  horizon <- paste0(as.character(base_year), "/", as.character(Sys.Date()))
  stock <- stock[horizon]
  
  # Calculate monthly arithmetic returns
  data <- periodReturn(stock, period = "monthly", type = "arithmetic")
  
  # Assign to the global environment to be accessible
  assign(ticker, data, envir = .GlobalEnv)
}

# Call our function for each stock
monthly_returns("CTRE", 2017)
monthly_returns("HD", 2017)
monthly_returns("LEN", 2017)
monthly_returns("LGIH", 2017)
monthly_returns("LOW", 2017)
monthly_returns

# Get S&P 500 Data
monthly_returns("SPY", 2015)

# Merge all the data and rename columns
returns <- merge.xts(CTRE, HD, LEN, LGIH, LOW, NVR, SP500)
colnames(returns) <- c("CTRE", "HD", "LEN", "LGIH", "LOW", "NVR", "SP500")

# remove N/A
returns <- na.omit(returns)

# Produce interactive chart of stock returns
dygraph(returns, main = "Stocks Returns") %>%
  dyAxis("y", label = "Return", valueRange = c(-1,0.5)) %>%
  dyRangeSelector(dateWindow = c("2017-01-01", "2022-09-02")) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set2")) 

# Print last 5 rows of the data, rounded to 4 decimal places
round(tail(returns, n = 5), 4)


# Assign weights
wts <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)

# Construct a portfolio using our returns object and weights
# Only select first three columns to isolate our individual stock data
portfolio_returns <- Return.portfolio(R = returns[,1:6], weights = wts, wealth.index = TRUE)

# Then isolate our S&P 500 data
benchmark_returns <- Return.portfolio(R = returns[,6], wealth.index = TRUE)

# Merge the two
comp <- merge.xts(portfolio_returns, benchmark_returns)
colnames(comp) <- c("Portfolio", "Benchmark")

# Build an interactive graph to compare performance
dygraph(comp, main = "Portfolio Performance vs. Benchmark") %>%
  dyAxis("y", label = "Amount ($)")