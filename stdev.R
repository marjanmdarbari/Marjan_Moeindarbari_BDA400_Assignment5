stdev <- function(data) {
  if (!is.numeric(data) || length(data) < 1 || any(!is.finite(data))) stop("data must contain finite numbers")
  mean_value <- sum(data) / length(data)
  differences <- data - mean_value
  squared_differences <- differences^2
  variance <- sum(squared_differences) / length(data) # population stdev (not sample sd)
  standard_deviation <- sqrt(variance)
  return(standard_deviation)
}
