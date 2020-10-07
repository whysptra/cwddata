#' Normalise a vector of numbers to sum to 1
#'
#' @aliases stack_normalize
#'
#' @param x A vector of numbers
#'
#' @return
#' A normalised scale of vectors which sum to 1
#' 
#'
#' @examples
#' x <- c(10, 30, 40)
#' stack_normalize(x)
# c(0.125, 0.375, .5)
#' x <- c(75, 0, 5, 20, NA)
#' stack_normalise(x)
# c(0.75, 0, 0.05, 0.2, NA)
#' x <- c(NA, NA, 10)
#' stack_normalize(x)
# c(NA, NA, 1)

stack_normalise <- function(x){
  x/sum(x, na.rm = TRUE)
  sum_x <- sum(x, na.rm = TRUE)
  if(sum_x <= 0) stop("x must sum to a positive number")
  x/sum_x
}

#' @export
stack_normalize <- stack_normalise