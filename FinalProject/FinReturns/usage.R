library(FinReturns)
library(ggplot2)

devtools::document()

# 3. Test the new functionality
data(sp500)
asset <- new_asset_returns(sp500, date_col = "Date", return_col = "Return", asset_name = "SP500")

# Daily Aggregation
plot(asset, type = "hist", frequency = "daily")
plot(asset, type = "time", frequency = "daily")

# Monthly Aggregation
plot(asset, type = "hist", frequency = "monthly")
plot(asset, type = "time", frequency = "monthly")

# Yearly Aggregation
plot(asset, type = "time", frequency = "yearly")
plot(asset, type = "hist", frequency = "yearly")

