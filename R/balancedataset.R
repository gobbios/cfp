#' balance a data set according to some grouping factor(s)
#'
#' @param xdata a \code{data.frame}
#' @param whattobalance a character vector with column names. The corresponding columns typically are either factor or character.
#' @param n integer, the number of cases to select for each factor level (or combination of factor levels)
#' @details the function requires either one or two factors to be balanced over
#'
#' if \code{n} is larger than the largest possible number, there will be a warning to that effect and \code{n} will be reset to the largest possible number, i.e. the function behaves as if \code{n = NULL} (the default)
#'
#' @author Christof Neumann
#' @return a list with 5 items
#' \itemize{
#' \item \code{$seldata} the subset of \code{xdata} with the selected rows
#' \item \code{$unseldata} the subset of \code{xdata} with the rows that were not selected
#' \item \code{$sel} the row indices of the selected rows
#' \item \code{$unsel} the row indices of the rows not selected
#' \item \code{$factors} the balance factor(s) (= \code{whattobalance})
#' }
#' @export
#'
#' @examples
#' set.seed(1)
#' xdata <- data.frame(ID = sample(letters[1:4], 30, replace = TRUE),
#' context = sample(LETTERS[21:22], 30, replace = TRUE),
#' var1 = rnorm(30), var2 = rnorm(30))
#' table(xdata$ID, xdata$context)
#' balancedataset(xdata = xdata, whattobalance = c("context"), n = 2)$seldata
#' balancedataset(xdata = xdata, whattobalance = c("context"), n = 3)$seldata
#' balancedataset(xdata = xdata, whattobalance = c("context"))$seldata
#'
#' # with two factors
#' balancedataset(xdata = xdata, whattobalance = c("context", "ID"), n = 1)$seldata
#'
#' # a case where one combination occurs only once (hence n = 1): row 13 has to be in each data set
#' xdata2 <- xdata[-9, ]
#' table(xdata2$ID, xdata2$context)
#' x <- sapply(1:50, function(X){
#'   row.names(balancedataset(xdata = xdata2, whattobalance = c("context", "ID"))$seldata)
#' })
#' table(x)
#'
#'

# set.seed(1)
# xdata <- data.frame(ID = sample(letters[1:4], 30, replace = TRUE), context = sample(LETTERS[21:22], 30, replace = TRUE), var1 = rnorm(30), var2 = rnorm(30))
# table(xdata$ID, xdata$context)
#
# whattobalance <- c("context", "ID")
# whattobalance <- c("ID")

balancedataset <- function(xdata, whattobalance, n = NULL) {
  if(sum(whattobalance %in% colnames(xdata)) != length(whattobalance)) stop("not all factors found in data set", call. = FALSE)
  # transform into character, so as not having to deal with (empty) factor levels
  fac1 <- as.character(xdata[, whattobalance[1]])

  # if there is only one such factor to balance over
  if(length(whattobalance) == 1) {
    # get max n
    maxn <- min(table(fac1))
    if(!is.null(n)) {
      if(n > maxn) {
        warning(paste0("'n' is larger than the largest possible value, which is ", maxn, ", and has been set to this value"), call. = FALSE)
        n <- maxn
      }
    }
    if(is.null(n)) n <- maxn
    # if maxn >= 2 means that we can sample without issue
    # if maxn == 1 means that if there is only one possibility, then sample(36, size = 1) would not return 36 each time but a value between 1 and 36, which is undesired behaviour and I haven't found a way around this yet (sample.int()?)
    if(maxn > 1) {
      x <- as.matrix(data.frame(table(fac1)))
      sel <- as.numeric(apply(x, 1, function(X) sample(which(fac1 == X[1]), size = n)))
    }
    if(maxn == 1) {
      x <- as.matrix(data.frame(table(fac1)))
      sel <- numeric(nrow(x))
      for(i in 1:nrow(x)) {
        if(x[i, "Freq"] == "1") sel[i] <- which(fac1 == x[i, 1])
        if(x[i, "Freq"] != "1") sel[i] <- sample(which(fac1 == x[i, 1]), size = 1)
      }
    }
  }



  if(length(whattobalance) == 2) {
    fac2 <- as.character(xdata[, whattobalance[2]])
    maxn <- min(table(fac1, fac2))
    if(maxn == 0) {
      stop("at least one combination does not occur in data", call. = FALSE)
    }
    if(!is.null(n)) {
      if(n > maxn) {
        warning(paste0("'n' is larger than the largest possible value, which is ", maxn, ", and has been set to this value"), call. = FALSE)
        n <- maxn
      }
    }
    if(is.null(n)) n <- maxn
    if(maxn > 1) {
      x <- as.matrix(data.frame(table(fac1, fac2)))[, 1:2]
      # X <- x[1, ]
      sel <- as.numeric(apply(x, 1, function(X) sample(which(fac1 == X[1] & fac2 == X[2] ), size = n)))
    }
    if(maxn == 1) {
      x <- as.matrix(data.frame(table(fac1, fac2)))
      sel <- numeric(nrow(x))
      for(i in 1:nrow(x)) {
        if(x[i, "Freq"] == "1") sel[i] <- which(fac1 == x[i, 1] & fac2 == x[i, 2])
        if(x[i, "Freq"] != "1") sel[i] <- sample(which(fac1 == x[i, 1] & fac2 == x[i, 2] ), size = 1)
      }
    }
  }


  # prepare results
  nr <- 1:nrow(xdata)
  # unselected rows
  unsel <- nr[!nr %in% sel]
  res <- list()
  res$seldata <- xdata[sort(sel), ]
  res$unseldata <- xdata[unsel, ]
  res$sel <- sort(sel)
  res$unsel <- unsel
  res$factors <- whattobalance

  return(res)
}
