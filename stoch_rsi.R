# Requires rsi.R and sma.R to be sourced first.
stoch_rsi <- function(data, period, k_period, d_period) {
  if (!exists("rsi", mode = "function") || !exists("sma", mode = "function")) stop("Source rsi.R and sma.R first")
  if (any(c(k_period, d_period) < 1) || any(c(k_period, d_period) != as.integer(c(k_period, d_period)))) stop("k_period and d_period must be positive integers")
  rsi_values <- rsi(data, period)
  valid <- rsi_values[!is.na(rsi_values)]
  if (length(valid) < (k_period + d_period - 1)) stop("Not enough observations for StochRSI smoothing")
  min_rsi <- min(valid)
  max_rsi <- max(valid)
  normalized <- if (max_rsi == min_rsi) rep(0, length(valid)) else (valid - min_rsi) / (max_rsi - min_rsi)
  k_line <- sma(normalized, k_period)
  d_line <- sma(k_line, d_period)
  result <- list(k_line = k_line, d_line = d_line)
  return(result)
}
