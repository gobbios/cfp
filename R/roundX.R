#' special rounding
#'
#' @param x numeric vector
#' @param roundto numeric of length 1, round to nearest ...
#'
#' @return numeric vector of the same length as \code{x}
#' @export
#'
#' @examples
#' x <- runif(20, 0, 1)
#' plot(roundX(x, 0.2), x)
#'
#' x <- rnorm(100, 0, 1)
#' plot(roundX(x, 0.5), x)
#'

roundX <- function(x, roundto = 0) {
  round(x / roundto) * roundto
}


