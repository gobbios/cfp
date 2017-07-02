#' Plot raw data for binomial data
#'
#' @param xdata a data frame
#' @param response character, the column name with the response variable (y-axis)
#' @param pred character, the column name with the response variable (x-axis)
#' @param pstructure optional, a named list the values for subsetting. Names of that list have to correspond to column names in \code{xdata}
#' @param agg logical, should data be first aggregated over a third variable (e.g. individual)
#' @param aggcol character, the column name for aggregation
#' @param xlimsglobal logical of length 1, or numeric of length two
#' @param cexfac expansion factor for points
#' @param bin optional, numeric of length 1 or longer, if supplied the data will be binned along 'pred' in either a specific number of bins (if length 1) or along a specific break points (of length >1)
#' @param pch optional point character (by default 16)
#' @param col optional colour (by default transparent grey)
#' @param ylim optional vector of length 2 for y-axis limits (by default c(0, 1))
#' @param \dots more arguments for the plot, for example \code{xlab=}
#' @importFrom grDevices grey
#' @importFrom graphics plot
#' @importFrom stats aggregate
#' @export
#' @return A plot
#' @examples
#' xdata <- data.frame(resp = sample(c(0,1), 1000, TRUE), ID = sort(sample(letters[1:20], 1000, TRUE)),
#' pred=sort(rnorm(1000))+rnorm(1000), cat1 = sample(1:3, 1000, TRUE), cat2 = sample(1:3, 1000, TRUE),
#' cat3 = sample(1:3, 1000, TRUE))
#' plotbinary(xdata = xdata, response = "resp", pred = "pred", agg = TRUE, aggcol = "ID")
#' plotbinary(xdata = xdata, response = "resp", pred = "pred", agg = TRUE, aggcol = "ID", cexfac = 1)
#'
#' plotbinary(xdata = xdata, response = "resp", pred = "pred", agg = FALSE)
#' plotbinary(xdata = xdata, response = "resp", pred = "pred", agg = FALSE, bin=30)



plotbinary <- function(xdata, response, pred, pstructure=NULL, agg=FALSE, aggcol=NULL, xlimsglobal=FALSE, cexfac=pi, bin = NULL, pch=NULL, col=NULL, ylim=NULL, ...) {
  # get some graphical parameters out of the way (in case one wants to modify pch, etc)
  # note that the size of symbols only makes sense for circles, i.e. pch=1 or 16 or 21
  if(is.null(pch)) PCH <- 16 else PCH <- pch
  if(is.null(col)) COL <- grey(level = 0.3, alpha = 0.4) else COL <- col
  if(is.null(ylim)) YLIM <- c(0,1) else YLIM <- ylim

  # and return explanation if xlim or cex was supplied
  # not working yet...
  #if(hasArg("cex")) stop("please cexfac instead of cex")
  #if(hasArg("xlim")) stop("please xlimsglobal instead of xlim")
  # if("cex" %in% names(match.call())) stop("please cexfac instead of cex")
  # if("xlim" %in% names(match.call())) stop("please xlimsglobal instead of xlim")


  # select subset according to 'pstructure'
  pdata <- xdata

  if(!is.null(pstructure)) {
    for(i in 1:length(pstructure)) pdata <- pdata[ pdata[, names(pstructure)[i] ] == pstructure[[i]], ]
  }



  if(agg == TRUE) {
    # get averaged predictor and response
    # actually proportions, but mean works here too
    # get also how often each individual is represented
    pd <- aggregate(pdata[, c(response, pred)], by=list(XX = pdata[, aggcol]), function(X) return(c(mean(X), length(X)))  )
    pd$ps <- sqrt((pd[,2][,2]*cexfac)/pi)


    # get x axis range and take global range if not supplied otherwise
    if(length(xlimsglobal) == 1) {
      if(xlimsglobal) xlims <- range(xdata[, pred])
      if(!xlimsglobal) xlims <- range(pdata[, pred])
    } else {
      xlims <- xlimsglobal
    }


    plot(pd[, 3][, 1], pd[, 2][, 1], xlim=xlims, ylim=YLIM, pch=PCH, cex=pd$ps, col=COL, ...)

  }


  if(agg == FALSE) {

    # should binning be done; if so, how many bins or specific break values
    if(is.null(bin)) {
      pd <- data.frame(table(pdata[, response], pdata[, pred]))
      pd[, 1] <- as.numeric(as.character(pd[, 1]))
      pd[, 2] <- as.numeric(as.character(pd[, 2]))
      pd$ps <- sqrt((pd$Freq*cexfac)/pi)

      # get x axis range and take global range if not supplied otherwise
      if(length(xlimsglobal) == 1) {
        if(xlimsglobal) xlims <- range(xdata[, pred])
        if(!xlimsglobal) xlims <- range(pdata[, pred])
      } else {
        xlims <- xlimsglobal
      }

      plot(pd[, 2], pd[, 1], xlim=xlims, ylim=YLIM, pch=PCH, cex=pd$ps, col=COL, ...)


    } else {
      # use bin
      # bin = 5
      # bin <- seq(-4.5, 3, by=0.5)

      pdata$breakpoints <- cut(pdata[, pred], breaks = bin)
      pd <- data.frame(table(pdata[, response], pdata$breakpoints))
      pd$ps <- sqrt((pd$Freq*cexfac)/pi)
      pd$xvals <- (as.numeric( sub("\\((.+),.*", "\\1", pd[, 2]) ) + as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", pd[, 2]) )) / 2
      pd[, 1] <- as.numeric(as.character(pd[, 1]))


      # get x axis range and take global range if not supplied otherwise
      if(length(xlimsglobal) == 1) {
        if(xlimsglobal) xlims <- range(xdata[, pred])
        if(!xlimsglobal) xlims <- range(pdata[, pred])
      } else {
        xlims <- xlimsglobal
      }

      plot(pd$xvals, pd[, 1], xlim=xlims, ylim=YLIM, pch=PCH, cex=pd$ps, col=COL, ...)

    }
  }

}
