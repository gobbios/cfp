#' create jitter in a normal-shaped envelope
#'
#' @param y numeric, the response variable (along vertical axis)
#' @param grouping character or factor, optional, a grouping factor for \code{y}
#' @param maxwidth positive numeric, the maximum width along the horizontal axis
#' @importFrom stats rnorm sd
#' @return a numeric vector of the same length as \code{y} that represents a relative offset to be added to the actual x-coordinates
#' @export
#'
#' @examples
#' xdata <- data.frame(id = sample(letters[1:2], 500, TRUE))
#' xdata$resp[xdata$id == "a"] <- rnorm(n = sum(xdata$id == "a"), mean = -5, sd = 4)
#' xdata$resp[xdata$id == "b"] <- rnorm(n = sum(xdata$id == "b"), mean = 2, sd = 1)
#'
#' plot(0, 0, type = "n", xlim = c(0.5, 2.5), ylim = c(-15, 7))
#' xdata$xcoord <- as.numeric(xdata$id)
#' points(xdata$xcoord, xdata$resp)
#'
#' plot(0, 0, type = "n", xlim = c(0.5, 2.5), ylim = c(-15, 7))
#' xdata$xcoord2 <- xdata$xcoord + violinjitter(xdata$resp, grouping = xdata$id, maxwidth = 0.4)
#' points(xdata$xcoord2, xdata$resp, cex = 0.5)

violinjitter <- function(y, grouping = NULL, maxwidth = 1) {
  res <- numeric(length(y))
  if (is.null(grouping)) {
    grouping <- factor(rep("x", length(res)))
  } else {
    if (is.factor(grouping)) {
      grouping <- droplevels(grouping)
    } else {
      grouping <- as.factor(grouping)
    }
  }

  for (i in levels(grouping)) {
    temp <- y[grouping == i]
    xmean <- mean(temp)
    xsd <- sd(temp)
    offs <- dnorm(temp, mean = xmean, sd = xsd)
    offs <- rnorm(n = length(temp), sd = offs)
    offs <- offs/max(abs(offs))
    res[grouping == i] <- offs * maxwidth
  }

  res
}
