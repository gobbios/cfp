#' histogram with overlay
#'
#' @param num numerical vector
#' @param fac vector with factor or character
#' @param bins numeric, the number of bins for the histogram
#' @param colours colours to be used for categories (by default four different colours)
#' @param xlab character, label for x-axis
#' @param numrange numeric of length 2, range for numerical values to consider
#' @details \code{numrange} is by default \code{c(NA, NA)}, which does not constrain the range along the numeric variable
#' @return a plot
#' @export
#'
#' @importFrom grDevices adjustcolor
#' @importFrom graphics title
#' @examples
#' xdata <- data.frame(var = NA, fac = sample(letters[1:3], size = 20000, replace = TRUE))
#' xdata$var[xdata$fac == "a"] <- rnorm(n = table(xdata$fac)["a"], mean = -2)
#' xdata$var[xdata$fac == "b"] <- rnorm(n = table(xdata$fac)["b"], mean = 0)
#' xdata$var[xdata$fac == "c"] <- rnorm(n = table(xdata$fac)["c"], mean = 1)
#'
#' histogram(num = xdata$var, fac = xdata$fac, bins = 10)
#' histogram(num = xdata$var, fac = xdata$fac, bins = 50)
#' histogram(num = xdata$var, fac = xdata$fac, bins = 100)
#' histogram(num = xdata$var, fac = xdata$fac, bins = 50, numrange = c(-2, 2))
#' histogram(num = xdata$var, fac = xdata$fac, bins = 100, colours = rainbow(3))

histogram <- function(num, fac, bins = 50, colours = c("red", "blue", "grey", "gold"), xlab = "", numrange = c(NA, NA)) {
  # check that there are not more than four categories
  nfacs <- length(table(fac))
  if(nfacs > length(colours)) stop("too many categories: add colours", call. = FALSE)
  # make dataframe
  xdata <- data.frame(num, fac)
  facs <- levels(xdata$fac)
  # limit to range
  if(!is.na(numrange[1])) xdata <- xdata[xdata$num >= numrange[1], ]
  if(!is.na(numrange[2])) xdata <- xdata[xdata$num <= numrange[2], ]
  # calculate bins
  breaks <- seq(min(xdata$num), max(xdata$num), length.out = bins)
  # get maximum value for y-axis
  mx <- max(unlist(lapply(tapply(X = xdata$num, INDEX = xdata$fac, FUN = hist, breaks = breaks, plot = FALSE),
                          FUN = function(x)max(x$counts))))
  # set up plot
  plot(0, 0, type = "n", xlim = range(xdata$num), ylim = c(0, mx*1.05), ann = FALSE,
       yaxs = "i", las = 1)
  title(xlab = xlab, ylab = "Frequency")
  for(i in 1:nfacs) {
    hist(xdata$num[xdata$fac == facs[i]], breaks = breaks, add = TRUE,
         col = adjustcolor(colours[i], 0.5), border = NA)
  }
}
