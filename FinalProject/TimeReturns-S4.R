# --- S4 Class Definition ---
#' An S4 Class to Represent Financial Time Series Returns
#' 
#' @slot dates Date. The time index for the returns.
#' @slot returns numeric. The daily return values.
#' @slot asset_name character. The name of the asset.
setClass("TimeReturns", 
         slots = list(
           dates = "Date",
           returns = "numeric",
           asset_name = "character"
         ))

# --- Constructor Function ---
#' Constructor for the TimeReturns Class
#' 
#' @param returns A numeric vector of daily returns.
#' @param dates A Date vector corresponding to the returns.
#' @param asset_name A character string for the asset name.
#' @return A new S4 object of class TimeReturns.
new_TimeReturns <- function(returns, dates, asset_name) {
  # Defensive Checks (Module 11)
  if (!is.numeric(returns)) {
    stop("Input 'returns' must be a numeric vector.")
  }
  if (!inherits(dates, "Date")) {
    stop("Input 'dates' must be a Date vector.")
  }
  if (length(returns) != length(dates)) {
    stop("Returns and Dates vectors must be the same length.")
  }
  
  new("TimeReturns", 
      returns = returns, 
      dates = dates, 
      asset_name = asset_name)
}

# --- S4 Method 1: show_returns (Generic and Method) ---
#' Show Key Information about a TimeReturns object
#' 
#' @param object An object of class TimeReturns.
setGeneric("show_returns", function(object) standardGeneric("show_returns"))

#' @rdname show_returns
#' @export
setMethod("show_returns", "TimeReturns", function(object) {
  cat("--- TimeReturns Summary ---\n")
  cat("Asset Name:", object@asset_name, "\n")
  cat("Time Period:", min(object@dates), "to", max(object@dates), "\n")
  cat("Observations:", length(object@returns), "\n")
  cat("Mean Daily Return:", round(mean(object@returns, na.rm = TRUE) * 100, 4), "%\n")
})

# --- S4 Method 2: chart_returns (Generic and Method) ---
#' Create a comprehensive return visualization (Time Series and Density Plot)
#' 
#' @param object An object of class TimeReturns.
setGeneric("chart_returns", function(object) standardGeneric("chart_returns"))

#' @rdname chart_returns
#' @export
setMethod("chart_returns", "TimeReturns", function(object) {
  # Defensive Check (Module 11 - on internal data)
  if (length(object@returns) == 0) {
    stop("Cannot chart: TimeReturns object has no data.")
  }
  
  df <- data.frame(Date = object@dates, Return = object@returns)
  
  # 1. Time Series Plot (Retained from previous version)
  p1 <- ggplot(df, aes(x = Date, y = Return)) +
    ggplot2::geom_line(color = "steelblue", alpha = 0.7) +
    ggplot2::labs(title = paste("Time Series: Daily Returns for", object@asset_name),
         x = "Date",
         y = "Daily Return") +
    ggplot2::theme_minimal()
  
  # 2. Density Plot vs. Normal Distribution (NEW and Required)
  mean_ret <- mean(df$Return, na.rm = TRUE)
  sd_ret <- sd(df$Return, na.rm = TRUE)
  
  p2 <- ggplot(df, aes(x = Return)) +
    # Histogram scaled to density
    ggplot2::geom_histogram(aes(y = after_stat(density)), binwidth = sd_ret / 10, fill = "lightblue", color = "black") +
    # Empirical Density Curve
    ggplot2::geom_density(color = "blue", linewidth = 1) +
    # Theoretical Normal Distribution (Required Comparison)
    ggplot2::stat_function(fun = dnorm, 
                  args = list(mean = mean_ret, sd = sd_ret), 
                  color = "red", 
                  linetype = "dashed", 
                  linewidth = 1) +
    ggplot2::labs(title = paste("Distribution of Returns vs. Normal Curve"),
         subtitle = "Red dashed line shows a theoretical Normal Distribution",
         x = "Daily Return",
         y = "Density") +
    ggplot2::theme_minimal()
  
  # Use the 'patchwork' package layout (requires installation/import)
  # For final project, you would use: p1 / p2
  print(p1)
  print(p2) 
})
