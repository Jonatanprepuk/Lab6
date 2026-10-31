#' Dynamic programming approach for the Knapsack problem 
#' 
#' @param x A \code{\link[base]{data.frame}} with values and weights
#'   for all items. Each row represents one item and must contain
#'   the following columns with positive values:
#'   \describe{
#'     \item{\code{w}}{A \code{numeric} representing the weight (integer).}
#'     \item{\code{v}}{A \code{numeric} representing the value.}
#'   }
#' @param W A positive \code{\link[base]{numeric}} (integer) giving the capacity
#'   of the knapsack.
#'
#' @return A \code{\link[base]{list}} with elements:
#'   \describe{
#'     \item{\code{value}}{Total value of the optimal knapsack.}
#'     \item{\code{elements}}{Row indices in \code{x} of the items
#'       included in the knapsack.}
#'   }
#'
#'@export


knapsack_dynamic <- function(x, W){
 n = nrow(x)
 weight <- x$w
 value <- x$v
 
 # check input, now checks for columns v/w, numeric type, NA 
 #  and integer weights & W
 stopifnot(
   is.data.frame(x),
   all(c("w", "v") %in% colnames(x)),  # check required columns exist
   nrow(x) > 0,  # Check x is not empty
   is.numeric(x$w),
   is.numeric(x$v),
   all(is.finite(x$w)),  # no NA, NaN and Inf allowed 
   all(is.finite(x$v)),  # no NA, NaN and Inf allowed 
   all(x$w > 0),
   all(x$v > 0),
   all(x$w == floor(x$w)),  # weights must be whole numbers
   is.numeric(W),
   length(W) == 1,  # W must be a single value
   is.finite(W),  #  no NA, NaN and Inf allowed 
   W > 0,
   W == floor(W)  # W must be a whole number
 )
 
 mat <- matrix(0, nrow = n+1, ncol = W +1)
 
 for (i in 1:n) {
   for (capacity in 0:W) {
     
     if (weight[i] > capacity) {
       mat[i + 1, capacity + 1] <-mat[i, capacity + 1]
       
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
 
 for(i in n:1){
   if(mat[i + 1, capacity +1 ] != mat[i, capacity +1]){
     selected <- c(selected, i)
     capacity <- capacity - weight[i]
   }
   
 }
 
 maximumValue = mat[n + 1, W +1]
 elements = sort(selected)
 
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
