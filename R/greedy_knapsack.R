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
#' @param fast A \code{\link[base]{logical}} that runs Rcpp implementation
#'  when true, otherwise R implementation.
#' @param ... Additional parameters, used to compare Rcpp
#' implementation with \code{order()} with shell sort.
#'
#' @return A \code{\link[base]{list}} with elements:
#'   \describe{
#'     \item{\code{value}}{Total value of the optimal knapsack.}
#'     \item{\code{elements}}{Row indices in \code{x} of the items
#'       included in the knapsack.}
#'   }
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#'
#' @examples
#' x <- data.frame(w = c(2, 3, 4), v = c(3, 4, 5))
#' greedy_knapsack(x, W = 5)
#'
#' @export
greedy_knapsack <- function(x, W, fast = FALSE, ...) {
  stopifnot(
    is.data.frame(x),
    all(c("w", "v") %in% colnames(x)),
    is.numeric(x$w),
    is.numeric(x$v),
    all(is.finite(x$w)),
    all(is.finite(x$v)),
    all(x$w > 0),
    all(x$v > 0),
    is.numeric(W),
    W > 0,
    is.logical(fast),
    length(fast) == 1
  )

  if (!any(x$w <= W)) {
    return(list(value = 0, elements = NA))
  }

  if (fast) {
    return(fast_greedy_knapsack(x$v, x$w, W))
  }

  ord <- order(x$v / x$w, decreasing = TRUE, ...)
  k <- sum(cumsum(as.numeric(x$w[ord])) <= W)
  s1_elements <- ord[seq_len(k)]

  s1_value <- sum(x$v[ord][seq_len(k)])

  if (k == nrow(x)) {
    return(list(value = s1_value, elements = s1_elements))
  }

  s2_index <- ord[k + 1]
  s2_value <- x$v[s2_index]

  if (s2_value > s1_value && x$w[s2_index] <= W) {
    return(list(value = s2_value, elements = s2_index))
  }

  return(list(value = s1_value, elements = s1_elements))
}
