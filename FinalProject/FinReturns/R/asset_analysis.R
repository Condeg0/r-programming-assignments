#' Calculate Skewness (Internal)
#'
#' @param x A numeric vector of returns.
#' @keywords internal
calc_skewness <- function(x) {
  n <- length(x)
  m3 <- sum((x - mean(x, na.rm = TRUE))^3) / n
  s3 <- sd(x, na.rm = TRUE)^3
  return(m3 / s3)
}

#' Calculate Kurtosis (Internal)
#'
#' @param x A numeric vector of returns.
#' @keywords internal
calc_kurtosis <- function(x) {
  n <- length(x)
  m4 <- sum((x - mean(x, na.rm = TRUE))^4) / n
  s4 <- sd(x, na.rm = TRUE)^4
  return((m4 / s4) - 3) # Excess kurtosis
}

#' Create an asset_returns Object
#'
#' Converts a data frame into an S3 object of class 'asset_returns'.
#' Performs defensive checks to ensure data validity.
#'
#' @param data A data frame containing date and return columns.
#' @param date_col String. Name of the date column.
#' @param return_col String. Name of the return column.
#' @param asset_name String. Name of the asset (e.g., "SP500").
#'
#' @return An object of class "asset_returns".
#' @export
#' @examples
#' data(sp500)
#' my_asset <- new_asset_returns(sp500, date_col = "Date", return_col = "Return")
new_asset_returns <- function(data, date_col = "Date", return_col = "Return", asset_name = "Asset") {

  # Defensive Programming: Validate Inputs
  if (!is.data.frame(data)) stop("Input 'data' must be a data frame.")
  if (!all(c(date_col, return_col) %in% names(data))) stop("Specified columns not found in data.")

  dates <- data[[date_col]]
  returns <- data[[return_col]]

  if (!is.numeric(returns)) stop("Return column must be numeric.")

  # Constructor
  obj <- list(
    data = data.frame(Date = dates, Return = returns),
    asset_name = asset_name
  )

  class(obj) <- "asset_returns"
  return(obj)
}

#' Summary Method for Asset Returns
#'
#' Prints statistical moments (Mean, Volatility, Skewness, Kurtosis).
#'
#' @param object An object of class asset_returns.
#' @param ... Additional arguments.
#' @export
summary.asset_returns <- function(object, ...) {
  ret <- object$data$Return
  cat("--- Asset Analysis: ", object$asset_name, " ---\n")
  cat("Observations: ", length(ret), "\n")
  cat("Mean Return:  ", round(mean(ret, na.rm = TRUE), 5), "\n")
  cat("Volatility:   ", round(sd(ret, na.rm = TRUE), 5), "\n")
  cat("Skewness:     ", round(calc_skewness(ret), 4), "\n")
  cat("Kurtosis:     ", round(calc_kurtosis(ret), 4), "\n")
  cat("----------------------------\n")
}

#' Plot Method for Asset Returns
#'
#' Generates visualizations using ggplot2 with adjustable frequency.
#'
#' @param x An object of class asset_returns.
#' @param type String. "time", "hist", or "qq".
#' @param frequency String. "daily", "monthly", or "yearly". Default is "daily".
#' @param ... Additional arguments.
#' @import ggplot2
#' @export
plot.asset_returns <- function(x, type = "time", frequency = "daily", ...) {

  df <- x$data

  # Frequency Aggregation Logic (Base R)
  if (frequency == "monthly") {
    df$Group <- format(df$Date, "%Y-%m")
    # Aggregate returns (Summing log returns)
    df_agg <- aggregate(Return ~ Group, data = df, sum)
    # Restore Date object for plotting (First day of month)
    df_agg$Date <- as.Date(paste0(df_agg$Group, "-01"))
    plot_data <- df_agg
    freq_label <- "Monthly"

  } else if (frequency == "yearly") {
    df$Group <- format(df$Date, "%Y")
    df_agg <- aggregate(Return ~ Group, data = df, sum)
    df_agg$Date <- as.Date(paste0(df_agg$Group, "-01-01"))
    plot_data <- df_agg
    freq_label <- "Yearly"

  } else {
    # Default to daily
    plot_data <- df
    freq_label <- "Daily"
  }

  # Plotting Logic
  if (type == "time") {
    p <- ggplot(plot_data, aes(x = Date, y = Return)) +
      geom_line(color = "#2c3e50", linewidth = 0.5) + # Clean line, no points
      labs(title = paste(x$asset_name, "-", freq_label, "Returns Series"),
           y = paste(freq_label, "Return")) +
      theme_minimal()

  } else if (type == "hist") {
    p <- ggplot(plot_data, aes(x = Return)) +
      geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "gray90", color = "gray40") +
      # Use aes(color=...) to generate legend, manual scale below defines the color
      geom_density(aes(color = "Density"), linewidth = 1) +
      scale_color_manual(name = "", values = c("Density" = "red")) +
      labs(title = paste(x$asset_name, "-", freq_label, "Distribution"),
           y = "Density") +
      theme_minimal() +
      theme(legend.position = "top")

  } else if (type == "qq") {
    p <- ggplot(plot_data, aes(sample = Return)) +
      stat_qq() +
      stat_qq_line(color = "red") +
      labs(title = paste(x$asset_name, "-", freq_label, "Q-Q Plot")) +
      theme_minimal()

  } else {
    stop("Invalid plot type. Choose 'time', 'hist', or 'qq'.")
  }

  return(p)
}
