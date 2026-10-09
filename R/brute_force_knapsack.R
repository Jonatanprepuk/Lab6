#' Brute Force Algorithm for Knapsack Problem
#'
#' @param x A \code{\link[base]{data.frame}} with values and weights
#'   for all items. Each row represents one item and must contain
#'   the following columns with positive values:
#'   \describe{
#'     \item{\code{w}}{A \code{numeric} representing the weight.}
#'     \item{\code{v}}{A \code{numeric} representing the value.}
#'   }
#' @param W A positive \code{\link[base]{numeric}} giving the capacity
#'   of the knapsack.
#'
#' @return A \code{\link[base]{list}} with elements:
#'   \describe{
#'     \item{\code{value}}{Total value of the optimal knapsack.}
#'     \item{\code{elements}}{Row indices in \code{x} of the items
#'       included in the knapsack.}
#'   }
#'
#' @examples
#' x <- data.frame(w = c(2, 3, 4), v = c(3, 4, 5))
#' knapsack_brute_force(x, W = 5)
#'
#' @export
brute_force_knapsack <- function(x, W) {
  stopifnot(is.data.frame(x), is.numeric(W))

  n <- nrow(x)
  highest_values <- 0

  for (i in 1:(2^n - 1)) {
    subset <- as.logical(intToBits(i)[1:n])

    weight <- sum(x$w[subset])
    value <- sum(x$v[subset])

    if (weight < W && value > highest_values) {
      elements <- which(subset)
      highest_values <- value
    }
  }
  list(value = highest_values, elements = elements)
}
