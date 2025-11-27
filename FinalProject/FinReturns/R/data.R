#' S&P 500 Simulated Returns (1980-2024)
#'
#' A dataset containing simulated daily returns for the S&P 500 from 1980 to 2024.
#' It includes over 11,000 observations to demonstrate long-term time series analysis
#' and includes a synthetic market crash outlier.
#'
#' @format A data frame with approx 11,000 rows and 2 variables:
#' \describe{
#'   \item{Date}{Date of the observation (Weekdays only)}
#'   \item{Return}{Daily log return (decimal)}
#' }
#' @source Simulated data based on historical S&P 500 statistical moments.
"sp500"
