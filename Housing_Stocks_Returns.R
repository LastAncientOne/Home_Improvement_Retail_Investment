# Construct Multi-Asset Portfolio
# libraries 
library(quantmod)
library(TTR)
library(pander)
library(ggplot2)
library(tidyr)
library(ggthemes)

# create a vector of stocks
stocks = c("CTRE", "HD", "LEN", "LGIH", "LOW", "NVR")

# download prices and create returns from Adjusted Prices
data1 = lapply(stocks, FUN = function(x) {
  ROC(Ad(getSymbols(x, from = "2017-01-01", to = "2022-09-02", auto.assign = FALSE)),
      type = "discrete") * 100
})  #%returns

# convert to data frame
ret1 = as.data.frame(do.call(merge, data1))
# change columna names
colnames(ret1) = gsub(".AX.Adjusted", "", colnames(ret1))
# remove the first row of missing values
ret1 = ret1[-1, ]
# add dates column
returns = data.frame(Date = as.Date(row.names(ret1)), ret1)
row.names(ret1) = NULL

pander(head(returns), split.table = Inf)

# convert to long
ret_long = pivot_longer(returns, cols = -c(Date), values_to = "Return", names_to = "Stock")

# plot
port_p1 = ggplot(ret_long, aes(Date, Return, color = Stock)) + geom_path(stat = "identity") +
  facet_grid(Stock ~ .) + theme_minimal() + labs(x = "Date", y = "Returns")
port_p1