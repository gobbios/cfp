
#' Expand matrix to data.frame via row and column names
#'
#' @param x a matrix with row and column names
#' @return a data.frame with row names in the \code{Rx} column, and the column names in the \code{Cx} column, and values in the \code{val} column
#' @export
#' @examples
#' x <- matrix(1:20, ncol=4)
#' rownames(x) <- paste0("r", 1:5)
#' colnames(x) <- paste0("c", 1:4)
#' mat2long(x)

mat2long <- function(x) {
  if(is.null(rownames(x))) stop("no row names")
  if(is.null(colnames(x))) stop("no column names")

  return(data.frame(Rx = rep(rownames(x), ncol(x)), Cx = rep(colnames(x), each=nrow(x)), val = as.numeric(x)))
}




