#' add a circle to a plot
#'
#' draws a circle with vertical alignment at bottom
#'
#' @param x numeric, x location
#' @param y numeric, y location
#' @param rad numeric, radius
#' @param npoints integer, number of points along the circumference
#' @param border color for circumference, by default omitted (i.e. \code{NA})
#' @param ... other graphical parameters, e.g. \code{col}
#'
#' @importFrom graphics polygon
#' @return adds to current plot
#' @export
#'
#' @examples
#' plot(0, 0, asp=1, xlim=c(-2,2), ylim=c(-2,2), "n")
#' # by radius
#' drawcircle(-2, -2, 2, col="red")
#' drawcircle(-2, -2, 1, col="blue")
#' drawcircle(-2, -2, 0.5, col="gold")
#' # by area
#' drawcircle(2, -2, sqrt(2*2*pi/pi), col="red")
#' drawcircle(2, -2, sqrt(2*1*pi/pi), col="blue")
#' drawcircle(2, -2, sqrt(2*0.5*pi/pi), col="gold")


drawcircle <- function(x, y, rad, npoints = 70, border = NA, ...) {
  rvals <- seq(0, 2*pi, length.out = npoints+1)
  xp <- rad * cos(rvals) + x
  yp <- rad * sin(rvals) + y + rad
  polygon(xp, yp, border = border, ...)
}

