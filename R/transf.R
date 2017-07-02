
#' Create histograms of several transformations
#'
#' @param vec a numerical vector
#' @param xround numeric, used to round \code{vec} first and the plot a line histogram
#' @importFrom graphics plot axis par hist par text
#' @export
#' @return A plot
#' @examples
#' x <- rnorm(1000, mean=100)
#' transf(x)
#' transf(x, 1)
#' x <- rpois(1000, lambda=7) + rnorm(1000)
#' transf(x)
#' transf(x, 1)

# vec <- rnorm(1000, 100)

transf <- function(vec, xround=NULL) {
  mypars <- par(no.readonly = T)
  par(mfcol=c(3,2))

  minx <- min(vec)


  if(is.null(xround)) {
    hist(vec, main="original")
  } else {
    plot(table(round(vec, xround)), "h", axes=F, ylab="Frequency", xlab="vec")
    axis(1); axis(2)
  }


  # roots
  if(minx >= 0) {
    x <- vec - minx
    if(is.null(xround)) {
      hist(x^0.5, main=paste0("square root (-", round(minx, 2), ")"), xlab="vec")
      hist(x^0.25, main=paste0("fourth root (-", round(minx, 2), ")"), xlab="vec")
    } else {
      plot(table(round(x^0.5, xround)), "h", axes=F, ylab="Frequency", xlab="vec", main=paste0("square root (-", round(minx, 2), ")"))
      axis(1); axis(2)
      plot(table(round(x^0.25, xround)), "h", axes=F, ylab="Frequency", xlab="vec", main=paste0("fourth root (-", round(minx, 2), ")"))
      axis(1); axis(2)
    }
  }

  if(minx < 0) {
    x <- vec + abs(minx)
    if(is.null(xround)) {
      hist(x^0.5, main=paste0("square root (+", round(abs(minx), 2), ")"), xlab="vec")
      hist(x^0.25, main=paste0("fourth root (+", round(abs(minx), 2), ")"), xlab="vec")
    } else {
      plot(table(round(x^0.5, xround)), "h", axes=F, ylab="Frequency", xlab="vec", main=paste0("square root (+", round(abs(minx), 2), ")"))
      axis(1); axis(2)
      plot(table(round(x^0.25, xround)), "h", axes=F, ylab="Frequency", xlab="vec", main=paste0("fourth root (+", round(abs(minx), 2), ")"))
      axis(1); axis(2)
    }
  }

  # logarithmic



  if(min(vec) < 0) hist(log(vec+abs(min(vec))), main=paste("log (+", round(abs(min(vec)),2), "...)", sep=""))
  if(min(vec)==0) hist(log(vec + min(vec[-c(which(vec==0))])), main=paste("log +",  min(vec[-c(which(vec==0))])))
  if(min(vec)>0) hist(log(vec), main="log")



  # inverse
  if(min(vec)>0) hist(1/vec, main="inverse (1/x)")
  # if(min(vec)<0) hist(1/vec, main="inverse (1/x)")
  # if(min(vec)==0) hist(1/(vec + min(vec[-c(which(vec==0))])), main=paste("inverse (1/ +",  round(min(vec[-c(which(vec==0))]),2) ,"...)",sep="")  )
  if(min(vec)<=0) {
    x <- vec+ abs(min(vec))
    offs <- abs(min(vec))
    if(0 %in% x) {
      offs <- offs + min(x[-c(which(x==0))])
      x <- x + min(x[-c(which(x==0))])
    }
    hist(x, main=paste("inverse (1/ +",  round(offs,2) ,"...)",sep=""))
  }

  # arc sin
  if((min(vec)>=0 & max(vec)<=1) == T) hist(asin(vec), main="arcsin")
  #if((min(vec)>=0 & max(vec)<=1) == F) plot(vec, type="n", main="no arcsin possible")
  if((min(vec)>=0 & max(vec)<=1) == F) {
    plot(0,0, type="n", ann=F, axes=F)
    text(0,0, label="no arcsin possible", cex=2)
  }

  par(mypars)
}


# transf(rnorm(100))



