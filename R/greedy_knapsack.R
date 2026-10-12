#' Greedy Approximation for the Knapsack Problem
#'
#' Solves the knapsack problem using Dantzig's greedy approximation.
#' Items are sorted by value per unit of weight and added in that order
#' until the next item no longer fits. The result is the better of this
#' greedy selection and the single item that did not fit.
#'
#' The algorithm does not guarantee an optimal solution, but it is
#' guaranteed to achieve at least half of the optimal value.
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
#' greedy_knapsack(x, W = 5)
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm}
#'
#' @export
greedy_knapsack <- function(x, W) {
  stopifnot(is.data.frame(x), is.numeric(W), W > 0)
  x <- x[x$w <= W, ]

  if (nrow(x) == 0) {
    return(list(value = 0, elements = NA))
  }

  x$v_w <- x$v / x$w
  x <- sort_by(x, ~ list(-v_w))
  s1 <- list(value = 0, elements = c())
  i <- 1
  w <- 0

  while (w + x$w[i] < W && i <= nrow(x)) {
    w <- w + x$w[i]
    s1$value <- s1$value + x$v[i]
    s1$elements <- c(s1$elements, as.integer(rownames(x)[i]))

    i <- i + 1
  }

  if (i <= nrow(x)) {
    s2 <- list(value = x$v[i], elements = as.integer(rownames(x)[i]))
  } else {
    s2 <- list(value = 0, elements = NA)
  }

  if (s1$value > s2$value) {
    return(s1)
  }
  return(s2)
}
