#' calculate an auto-correlation term
#'
#' @param modres the model result object from \code{(g)lmer}
#' @param xdata the data.frame used to fit the model
#' @param corrvar character, the column name in \code{xdata} for the variable relevant for the correlation (e.g. date)
#' @param contrfac character, the column name in \code{xdata} for the control factor (e.g. ID or group)
#' @param stdzcorr logical, should the correlation-variable \code{corrvar} be z-transformed
#' @param doplot logical, should a plot be produced
#' @param plot_sds numeric of length 2, the SDs for visualization if required
#' @param opti_sds numeric of length 2, the SDs for the optimization
#' @param opti_n numeric, the number of steps for the visualization
#'
#' @details This function is heavily based on code by Roger Mundry.
#'
#' The difference between \code{plotSDs} and \code{optiSDs} is that the former is only used for the visualization, and generally should cover at least equal if not wider range than the latter
#' @return a list (and a plot)
#' @export
#' @importFrom stats AIC dnorm optimize weighted.mean
#'
#' @examples
#' # generate model
#' set.seed(123)
#' n = 1000
#' xdata <- data.frame(pred1 = rnorm(n), pred2 = rnorm(n))
#' xdata$ID <- factor(sample(LETTERS[1:20], size = n, replace = TRUE))
#' re <- rnorm(20)
#' names(re) <- LETTERS[1:20]
#' xdata$resp <- re[as.numeric(xdata$ID)] + xdata$pred1 * 0.3 + xdata$pred2 * (-0.4) +
#'               diffinv(rnorm(n, sd = 1.5))[-n] + rnorm(n, sd = 0.5)
#' \dontrun{
#' library(lme4)
#' res <- lmer(resp ~ pred1 + pred2 + (1|ID), data = xdata)
#' summary(res)
#' # visual
#' autocorr(modres = res, xdata = xdata, corrvar = "pred1", contrfac = "ID", plot_sds = c(0.001, 1))
#' # narrow optimization range and suppress plot
#' xres <- autocorr(modres = res, xdata = xdata, corrvar = "pred1", contrfac = "ID",
#'                  opti_sds = c(0.05, 0.25), doplot = FALSE)
#' # add location of local minimum to previous plot
#' abline(v = xres$opti$minimum, col = "red")
#' xdata$ac <- xres$optiac
#' plot(xdata$ac, xdata$resp)
#' res2 <- update(res, .~. +ac)
#' summary(res2)
#' }

autocorr <- function(modres, xdata, corrvar, contrfac, doplot = TRUE,
                     plot_sds = c(0.001, 2), opti_sds = c(0.001, 1),
                     opti_n = 51, stdzcorr = TRUE) {

  if (stdzcorr) {
    corrvar <- scale(xdata[, corrvar])
  } else {
    corrvar <- xdata[, corrvar]
  }

  RE <- xdata[, contrfac] # contr.fac
  RElevels <- levels(RE) # c.fac.lev
  Ncases <- nrow(xdata)
  resis <- resid(modres)
  # ac.sd=2
  acinternal <- function(ac.sd, returnAIC = TRUE) {
    all.ac <- numeric(Ncases)

    # i=1
    # ac.sd=1
    for (i in 1:length(RElevels)){
      ind_resis <- resis[RE == RElevels[i]]
      ind_corrfac <- corrvar[RE == RElevels[i]]
      xx <- unlist(lapply(1:length(ind_resis), function(X) {
        ac_weights <- dnorm(x = sqrt( (ind_corrfac[X] - ind_corrfac[-X]) ^ 2),
                            mean = 0, sd = ac.sd)
        ac_weights[is.nan(ac_weights)] <- 0
        weighted.mean(ind_resis[-X], w = ac_weights)
      }))
      all.ac[RE == RElevels[i]] <- xx
    }
    all.ac[is.nan(all.ac)] <- 0
    all.ac[is.na(all.ac)] <- 0
    if (returnAIC) {
      xdata$X <- scale(all.ac)
      xres <- AIC(update(modres, .~. + X, data = xdata))
      return(xres)
    } else {
      return(as.numeric(scale(all.ac)))
    }
  }

  if (doplot) {
    sds <- seq(plot_sds[1], plot_sds[2], length.out = opti_n)
    aics <- sapply(sds, acinternal)
    plot(sds, aics, typ = "l", lwd = 3)
    # abline(v = optiSDs, lty = 3)
    # abline(v = optires$minimum, lwd = 3, col = "red")
  } else {
    optires <- optimize(acinternal, lower = opti_sds[1], upper = opti_sds[2])
    acvals <- acinternal(optires$minimum, returnAIC = FALSE)

    allres <- list(opti = optires, optiac = as.numeric(acvals))
    return(allres)
  }
}
