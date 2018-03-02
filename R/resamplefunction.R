#' resample function
#'
#' Resampling model estimates from glmmADMB fits
#' @param model a model fit from \code{glmmadmb}
#' @param dat the data set for the fit (typically a \code{data.frame})
#' @param N the number of (re)samples to take
#' @param termsref character with 'control' variables. These variables will be set to zero (numeric predictors) or their reference level (factors).
#' @param useparallel use the \code{doParallel} package?
#' @param stepsalong the resolution of steps along the range of numerical predictors
#' @param allcores should all available cores be used (which might slow down any activity other than R substantially). If \code{FALSE} (default), all but one core are assigned to R.
#'
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom foreach foreach '%dopar%'
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom iterators icount
#' @importFrom stats predict update
#' @return a list with two items:
#'  \itemize{
#'   \item a \code{data.frame} with the predictor variables at the values for which the prediction was made
#'   \item a \code{matrix} with the same number of rows as the first element of the list, and \code{N} columns, i.e. the predicted values for each sample
#'  }
#' @export
#' @examples
#' \dontrun{
#' library(glmmADMB)
#' set.seed(12345)
#' N <- 100
#' resp <- rnorm(n = N)
#' pred1 <- sample(letters[1:2], length(resp), T)
#' pred2 <- sample(LETTERS[3:4], length(resp), T)
#' pred3 <- rnorm(n = N)
#' ref <- sample(letters[15:20], length(resp), T)
#' xdata <- data.frame(resp, pred1, pred2, pred3, ref)
#' res <- glmmadmb(resp ~ pred1*pred2 + pred3 + (1|ref), data = xdata, family = "gaussian")
#' summary(res)
#'
#' # predict along different values of $pred3
#' resamplefunction(res, xdata, N = 2)
#' # keep $pred3 at 0
#' resamplefunction(res, xdata, N = 2, termsref = "pred3")
#' # keep $pred2 at its reference level ("C")
#' resamplefunction(res, xdata, N = 2, termsref = "pred2")
#' # keep $pred1 at reference and $pred3 at 0
#' resamplefunction(res, xdata, N = 2, termsref = c("pred1", "pred3"))
#' }
#'
resamplefunction <- function(model, dat, N, termsref = NULL, useparallel = TRUE, stepsalong = 11, allcores = FALSE) {
  # take all terms to vary
  xterms <- attr(x = model$terms, which = "term.labels")
  # get data classes
  dc <- attr(x = model$terms, which = "dataClasses")
  # get main effects names
  xterms <- dc[names(dc) %in% xterms]
  # get values for predictions depending on the term's data class
  predvals <- list()
  for(i in 1:length(xterms)) {
    if(xterms[i] == "factor") {
      predvals[[length(predvals) + 1]] <- levels(dat[, names(xterms)[i]])
    }
    if(xterms[i] == "numeric" | xterms[i] == "integer") {
      predvals[[length(predvals) + 1]] <- seq(from = min(dat[, names(xterms)[i]]), to = max(dat[, names(xterms)[i]]), length.out = stepsalong)
    }
    names(predvals)[i] <- names(xterms)[i]
  }

  # if terms were specified, take those that are not specified at value 0 (for numeric) or reference level (for factor)
  if(!is.null(termsref)) {
    for(i in 1:length(termsref)) {
      if(dc[termsref[i]] == "numeric" | dc[termsref[i]] == "integer") {
        predvals[termsref[i]] <- 0
      }
      if(dc[termsref[i]] == "factor") {
        predvals[termsref[i]] <- levels(dat[, termsref[i]])[1]
      }
    }
  }

  ndata <- expand.grid(predvals)
  # re'factor' levels
  if("factor" %in% xterms) {
    xnames <- names(xterms)[xterms == "factor"]
    for(i in xnames) {
      ndata[, i] <- factor(ndata[, i], levels = levels(dat[, i]))
    }
  }


  if(useparallel) {
    # initiate multicore
    core <- detectCores()
    if(allcores) {
      cl <- makeCluster(core)
    } else {
      cl <- makeCluster(core-1)
    }
    registerDoParallel(cl)

    allres <- foreach(1:N, .packages = "glmmADMB", .combine = 'cbind') %dopar% {
      tempdata <- dat
      tempdata <- tempdata[sample(1:nrow(tempdata), replace = TRUE), ]
      xres <- update(model, data = tempdata)
      predict(xres, newdata = ndata, re.form = NULL, type = "response")
    }
    on.exit(stopCluster(cl))
  }

  if(!useparallel) {
    allres <- matrix(ncol = N, nrow = nrow(ndata))
    for(i in 1:N) {
      tempdata <- dat
      tempdata <- tempdata[sample(1:nrow(tempdata), replace = TRUE), ]
      xres <- update(model, data = tempdata)
      allres[, i] <- predict(xres, newdata = ndata, re.form = NULL, type = "response")
    }
  }

  return(list(ndata, allres))
}
