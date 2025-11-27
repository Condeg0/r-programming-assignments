# --- Utility Function 3: calculate_skewness ---
#' Calculate Skewness of Returns
#' 
#' @param returns A numeric vector of returns.
#' @return The skewness of the distribution.
#' @export
calculate_skewness <- function(returns) {
  # Defensive Check (Module 11)
  if (!is.numeric(returns)) {
    stop("Input 'returns' must be a numeric vector.")
  }
  if (length(returns) < 3) {
    stop("Skewness requires at least 3 data points.")
  }
  
  # Skewness formula (using mean and standard deviation)
  n <- length(returns)
  m3 <- sum((returns - mean(returns))^3) / n
  s3 <- sd(returns)^3
  skew = m3 / s3
  return(skew)
}

# --- Utility Function 4: calculate_max_drawdown ---
#' Calculate Maximum Drawdown
#' 
#' @param returns A numeric vector of returns.
#' @return The maximum drawdown value.
#' @export
calculate_max_drawdown <- function(returns) {
  # Defensive Check (Module 11)
  if (!is.numeric(returns)) {
    stop("Input 'returns' must be a numeric vector.")
  }
  
  # Calculate cumulative wealth index
  wealth_index <- cumprod(1 + returns)
  
  # Calculate maximum drawdown
  peak <- cummax(wealth_index)
  drawdown <- (peak - wealth_index) / peak
  return(max(drawdown))
}

# --- Utility Function 5: annualize_returns ---
#' Annualize Daily Returns
#' 
#' @param daily_returns A numeric vector of daily returns.
#' @param trading_days An integer for the number of trading days per year (default is 252).
#' @return The annualized return rate.
#' @export
annualize_returns <- function(daily_returns, trading_days = 252) {
  # Defensive Check (Module 11)
  if (!is.numeric(daily_returns) || trading_days < 1) {
    stop("Inputs must be numeric; trading_days must be positive.")
  }
  
  # Annualization formula
  (prod(1 + daily_returns)^(trading_days / length(daily_returns))) - 1
}
# --- Utility Function 6 (UPGRADED): monte_carlo_var --> calculate_comprehensive_var ---
#' Comprehensive Value-at-Risk (VaR) Estimation
#' 
#' @param returns A numeric vector of historical returns.
#' @param confidence_level The VaR confidence level (e.g., 0.99 for 99%).
#' @param method A character string specifying the method: "Historical" or "Parametric".
#' @param n_simulations Number of simulation paths to run (only used for Monte Carlo).
#' @return A list containing the estimated VaR for the specified method(s).
#' @export
calculate_comprehensive_var <- function(returns, confidence_level = 0.99, method = "Historical", n_simulations = 5000) {
  # Defensive Checks (Module 11)
  if (!is.numeric(returns) || confidence_level <= 0 || confidence_level >= 1) {
    stop("Returns must be numeric, and confidence_level must be between 0 and 1.")
  }
  
  VaR_results <- list()
  alpha <- 1 - confidence_level
  
  # --- 1. Historical VaR (Non-Parametric) ---
  if (method %in% c("Historical", "All")) {
    # VaR is the quantile of the actual historical loss distribution
    historical_var <- quantile(returns, alpha, na.rm = TRUE)
    VaR_results$Historical_VaR <- historical_var
  }
  
  # --- 2. Parametric VaR (Assumes Normal Distribution) ---
  if (method %in% c("Parametric", "All")) {
    mu <- mean(returns, na.rm = TRUE)
    sigma <- sd(returns, na.rm = TRUE)
    # Parametric VaR relies on the qnorm function (quantile of normal distribution)
    parametric_var <- mu + qnorm(alpha) * sigma
    VaR_results$Parametric_VaR <- parametric_var
  }
  
  # --- 3. Monte Carlo VaR (Retained/Refined) ---
  if (method %in% c("MonteCarlo", "All")) {
    mu <- mean(returns, na.rm = TRUE)
    sigma <- sd(returns, na.rm = TRUE)
    simulated_returns <- rnorm(n_simulations, mean = mu, sd = sigma)
    monte_carlo_var <- quantile(simulated_returns, alpha)
    VaR_results$MonteCarlo_VaR <- monte_carlo_var
  }
  
  return(VaR_results)
}
