#' make regular sequence from range of numeric vector
#'
#' @param x numeric vector
#' @param n length of sequence (default is 51)
#'
#' @return a numeric vector
#' @export
#'
#' @details function takes a vector, calculates range (removing \code{NA}s) and returns sequence between the range limits
#'
#' @examples
#' X <- rnorm(100)
#' Y <- mseq(X)
mseq <- function(x, n = 51) {
  xrange <- range(x, na.rm = TRUE)
  seq(xrange[1], xrange[2], length.out = n)
}


