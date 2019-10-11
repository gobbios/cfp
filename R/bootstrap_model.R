#' refit model with bootstrapped data set
#'
#' @param model a model fit from \code{lme4}
#' @param dat the data set for the fit (typically a \code{data.frame})
#' @param N the number of (re)samples to take
#' @param useparallel use the \code{doParallel} package?
#' @param allcores should all available cores be used (which might slow down any activity other than R substantially). If \code{FALSE} (default), all but one core are assigned to R.
#'
#' @return a list with model fits
#' @export
#'
#' @examples
#' library(lme4)
#' (fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy))
#'

# model = fm1; dat=sleepstudy; N=5;useparallel = TRUE; allcores = FALSE
bootstrap_model <- function(model, dat, N, useparallel = TRUE, allcores = FALSE) {
  if(useparallel) {
    # initiate multicore
    core <- detectCores()
    if(allcores) {
      cl <- makeCluster(core)
    } else {
      cl <- makeCluster(core-1)
    }
    registerDoParallel(cl)

    if(class(model) == "lmerMod" | class(model) == "glmerMod") {
      allres <- foreach(1:N, .packages = "lme4") %dopar% {
        tempdata <- dat[sample(1:nrow(dat), replace = TRUE), ]
        xres <- update(model, data = tempdata)
        xres
        # predict(xres, newdata = ndata, re.form = NA, type = "response")
      }
    }
    on.exit(stopCluster(cl))
  }

  # if parallel is turned off use a simple loop
  if(!useparallel) {
    allres <- list()
    for(i in 1:N) {
      tempdata <- dat[sample(1:nrow(dat), replace = TRUE), ]
      allres[[i]] <- update(model, data = tempdata)
    }
  }

  allres
}
