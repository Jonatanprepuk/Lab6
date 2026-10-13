#' Dynamic programming approach for the Knapsack problem
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
#' @export


knapsack_dynamic <- function(x, W) {
  stopifnot(is.data.frame(x))
  n <- nrow(x)
  weight <- x$w
  value <- x$v
  stopifnot(
    all(c(sign(c(weight, value, W))) == 1) # check that weight, value and W are all positive numerics
  )

  mat <- matrix(0, nrow = n + 1, ncol = W + 1)

  for (i in 1:n) {
    for (capacity in 0:W) {
      if (weight[i] > capacity) {
        mat[i + 1, capacity + 1] <- mat[i, capacity + 1]
      } else {
        mat[i + 1, capacity + 1] <- max(
          mat[i, capacity + 1],
          value[i] + mat[i, capacity - weight[i] + 1]
        )
      }
    }
  }

  # Find the optimal items for the knapsack
  selected <- c()
  capacity <- W

  for (i in n:1) {
    if (mat[i + 1, capacity + 1] != mat[i, capacity + 1]) {
      selected <- c(selected, i)
      capacity <- capacity - weight[i]
    }
  }

  maximumValue <- mat[n + 1, W + 1]
  elements <- sort(selected)

  return(list(
    value = maximumValue,
    elements = elements
  ))
}


# ------------------- test the function -------------
# suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
#
# set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
# n = 2000
# knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE),
#                                v = runif(n = n, 0, 10000))
# rm(n)
#
# knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
#
# knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
