library(quantmod)
library(TTR)
library(PortfolioAnalytics)
library(fPortfolio)
# library(PerformanceAnalytics)

# create a vector of stocks
stocks <- c("CTRE", "HD", "LEN", "LGIH", "LOW", "NVR")

# download stock prices and create returns from Adjusted Prices
data = lapply(stocks, FUN = function(x) {
  ROC(Ad(getSymbols(x, from = "2017-01-01", to = "2022-09-02", auto.assign = FALSE)),
      type = "discrete") * 100
})  #%returns

# convert to data frame
rets = as.data.frame(do.call(merge, data))

# change columns names
colnames(rets) = gsub(".Adjusted", "", colnames(rets))
# remove the first row of missing values
rets = rets[-1, ]
# add dates column
rets = data.frame(Date = as.Date(row.names(rets)), rets)
row.names(rets) = NULL

data_p2 = zoo(rets[, -1], order.by = as.Date(rets$Date))
# create specification

port = portfolio.spec(assets = c(colnames(data_p2)))

# add long only constraint
port = add.constraint(portfolio = port, type = "long_only")
# add full investment constraint
port = add.constraint(portfolio = port, type = "full_investment")

# objective: maximize risk
port_rnd = add.objective(portfolio = port, type = "risk", name = "StdDev")

# objective: maximize return
port_rnd = add.objective(portfolio = port_rnd, type = "return", name = "mean")

chart.RiskReward(rand_p, risk.col = "StdDev", return.col = "mean", chart.assets = TRUE)

port_msd = add.objective(portfolio = port, type = "risk", name = "StdDev")
minvar = optimize.portfolio(R = data_p2, portfolio = port_msd, optimize_method = "ROI",
                             trace = TRUE)
minvar


plot(minvar, risk.col = "StdDev", main = "Mean Variance Portfolio", chart.assets = TRUE)


# efficient frontier
minvar_ef = create.EfficientFrontier(R = data_p2, portfolio = port_msd,
                                     type = "mean-StdDev")
chart.EfficientFrontier(minvar_ef, match.col = "StdDev", type = "l", tangent.line = FALSE,
                        chart.assets = TRUE)