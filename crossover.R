# Directional labels follow the assignment pseudocode on page 18.
crossover <- function(arr1, arr2) {
  if (length(arr1) != length(arr2)) stop("Both arrays should have the same length")
  if (!is.numeric(arr1) || !is.numeric(arr2)) stop("Both arrays must be numeric")
  n <- length(arr1)
  crossover_signals <- rep("None", n)
  if (n >= 2) for (i in 2:n) {
    if (anyNA(c(arr1[i-1], arr1[i], arr2[i-1], arr2[i]))) next
    if (arr1[i] > arr2[i] && arr1[i-1] <= arr2[i-1]) crossover_signals[i] <- "Up"
    else if (arr1[i] < arr2[i] && arr1[i-1] >= arr2[i-1]) crossover_signals[i] <- "Down"
  }
  return(crossover_signals)
}
