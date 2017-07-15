
#' add alpha to colours
#'
#' @param x character, can be a character vector, contains either names of colors (e.g. "red", "darkgreen") or their hexadecimal representation
#' @param alpha numeric between 0 and 1, sets transparency (0 = transparent, 1 = opaque)
#' @importFrom grDevices col2rgb rgb
#' @return color names in hexadecimal
#' @export
#'
#' @examples
#' coloralpha(c("#FF0000", "#FFD700"), alpha = 0.3)
#' coloralpha(c("red", "green"), alpha = 0.3)
#' coloralpha("#FF0000", alpha = 0.3)
#' coloralpha("red", alpha = 0.3)
#' mycols <- c("#F1BB7B", "#FD6467", "#5B1A18", "#D67236")
#' mycols <- coloralpha(mycols)
#' plot(1:4, 1:4, pch=16, cex=10, col=mycols)
#' points(1:4, 1:4+0.2, pch=16, cex=10, col=mycols)

coloralpha <- function(x, alpha = 0.5) {
  if(nchar(x)[1] == 9 & substr(x[1], 1, 1) == "#") {
    alpha <- substr(rgb(1, 1, 1, alpha), 8, 9)
    paste0(x, alpha)
  } else {
    apply(col2rgb(x)/255, 2, function(X) rgb(X[1], X[2], X[3], alpha = alpha))
  }
}



