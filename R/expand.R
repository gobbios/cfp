#' expand grid with numerical component
#'
#' expand combinations with range of numerical variable
#'
#' @param formula a formula
#' @param data the data set
#' @param res numerical, the resolution for the numerical variable, i.e. the length of the sequence between minimum and maximum
#' @param ... further columns to be added with their value
#'
#' @return a data frame with columns as specified in \code{formula} and if given as in \code{\dots}
#' @export
#' @details this function is an extension of expand.grid, i.e. it takes all the possible combinations of factors and binary variables in the data set and matches the numerical variable accordingly along a sequence from its minimum to its maximum
#'
#' only combinations that actually occur in the data are returned
#'
#' if \code{res = 2}, the numerical variable will be returned as range
#'
#' @examples
#' set.seed(123)
#' xdata <- data.frame(pred1 = rnorm(100),
#'                     pred2 = sample(c(0, 1), 100, TRUE),
#'                     fac1 = sample(letters[1:3], 100, TRUE),
#'                     fac2 = sample(LETTERS[7:8], 100, TRUE))
#' expand(~pred1 + fac1 + fac2, xdata, res = 3)
#' expand(~pred2 + fac1 + fac2, xdata, res = 3)
#' # adding columns
#' expand(~pred2 + fac1 + fac2, xdata, res = 3, x = -17, y = "gg")

expand <- function(formula, data, res, ...) {
  allvars <- all.vars(formula)
  # first, determine which variables are categorical (factors or binary (0/1))
  # and which are numeric
  facs <- c()
  bins <- c()
  nums <- c()
  cats <- list()
  i = 2
  for (i in 1:length(allvars)) {
    if (is.factor(data[, allvars[i]])) {
      facs <- c(facs, allvars[i])
      cats[[length(cats) + 1]] <- levels(data[, allvars[i]])
      names(cats)[length(cats)] <- allvars[i]
    } else {
      if (length(table(data[, allvars[i]])) == 2) {
        if (identical(c("0", "1"), names(table(data[, allvars[i]])))) {
          bins <- c(bins, allvars[i])
          facs <- c(facs, allvars[i])
          cats[[length(cats) + 1]] <- c(0, 1)
          names(cats)[length(cats)] <- allvars[i]
        }
      } else {
        nums <- c(nums, allvars[i])
      }
    }
  }

  # use expand grid with categorical predictors to get combinations
  res1 <- expand.grid(cats)

  # create output object
  outres <- matrix(ncol = length(allvars), nrow = 0)
  colnames(outres) <- c(names(cats), nums)

  # a function to extend the range according to 'res'
  foo <- function(X) seq(min(X), max(X), length.out = res)
  # add numerical variables according to the range for the specific combination
  i = 1
  for(i in 1:nrow(res1)) {
    # find the rows in 'data' that correspond to the given combination of categorical predictors
    k = names(res1)[1]
    ind <- list()
    for(k in names(res1)){
      ind[[length(ind) + 1]] <- which(data[, k] == res1[i, k])
    }
    xtab <- table(unlist(ind))
    xtab <- as.numeric(names(xtab[which(xtab == ncol(res1))]))

    # only continue if there actually are combinations in the data
    if(length(xtab) > 0) {
      temp <- data[xtab, nums, drop = FALSE]
      m <- matrix(rep(apply(res1[i, , drop = FALSE], 2, as.character), res), nrow = res, byrow = TRUE)
      x <- apply(temp, 2, foo)
      tempres <- cbind(m, x)
      outres <- rbind(outres, tempres)
    }
  }

  # reformat columns in data frame
  res <- data.frame(outres)
  for(i in 1:ncol(res)) {
    if(colnames(res)[i] %in% c(nums, bins)) res[, i] <- as.numeric(as.character(res[, i]))
  }

  # add additional columns if required
  add <- list(...)
  if(length(add) >= 1) {
    for(i in 1:length(add)) {
      res <- cbind(res, add[[i]])
      colnames(res)[ncol(res)] <- names(add)[i]
    }
  }

  return(res)
}
