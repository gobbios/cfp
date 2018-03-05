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
#' @importFrom lme4 ranef
#' @return a list with two items:
#'  \itemize{
#'   \item a \code{data.frame} with the predictor variables at the values for which the prediction was made
#'   \item a \code{matrix} with the same number of rows as the first element of the list, and \code{N} columns, i.e. the predicted values for each sample
#'  }
#' @export
#' @details the function needs data sets in which columns have one dimension only (at least on the predictor side). If you get a warning or error, please check \code{unlist(lapply(<yourdata>, ncol))} to find columns that have more than one dimension. This could happen, for example, if you applied \code{scale()} to a numerical column. You could use \code{<yourdata>$mycol <- as.numeric(<yourdata>$mycol)} to fix this issue. Also, the model has to be refitted after such an operation.
#'
#' In the case of glmmADMB, if the model fitting fails in one or more of the resampling runs, this would normally break the loop. The function currently uses a form of error catching that returns \code{NA} as predicted values for such cases. The function will produce a message informing you that this happend. You might want to increase \code{N} so that you actually end up with the number of valid samples intended.
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
#'
#' epil2$subject <- factor(epil2$subject)
#' (fm <- glmmadmb(y ~ Base*trt + Age + Visit + (Visit|subject),
#'                 data = epil2, family = "nbinom"))
#' resamplefunction(model = fm, dat = epil2, N = 3, stepsalong = 3)
#'
#' library(lme4)
#' (fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy))
#' resamplefunction(model = fm1, dat = sleepstudy, N = 3, stepsalong = 3)
#'
#' (gm1 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
#'               data = cbpp, family = binomial))
#' resamplefunction(model = gm1, dat = cbpp, N = 3, stepsalong = 3)
#' }
#'
resamplefunction <- function(model, dat, N, termsref = NULL, useparallel = TRUE, stepsalong = 11, allcores = FALSE) {
  # sometimes, data columns in the data set might be matrices with 1 column (which appear as 'nmatrix.1' data classes in the model objects)
  # eg. when data were subjected to 'scale()'
  # for now, make sure that columns are vectors (not matrices)
  if(class(model) == "lmerMod" | class(model) == "glmerMod") {
    datclasses <- as.character(unlist(lapply(model@frame, class)))
  }
  if(class(model) == "glmmadmb") {
    datclasses <- as.character(unlist(lapply(model$frame, class)))
  }
  if("matrix" %in% datclasses) {
    warning("You have at least one matrix in your data set. Please transform into a vector.", call. = FALSE)
  }
  rm(datclasses)
  # tempclasses <- unlist(lapply(dat, class))
  # if("matrix" %in% tempclasses) {
  #   repl <- which(tempclasses == "matrix")
  #   for(i in repl) {
  #     dat[, names(tempclasses)[i]] <- as.numeric(dat[, names(tempclasses)[i]])
  #   }
  #   model <- update(model, data = dat)
  # }
  # rm(tempclasses, repl)



  # get the terms and combinations of values for the prediction, depending on model type (glmmadmb or lmer/glmer)

  if(class(model) == "lmerMod" | class(model) == "glmerMod") {
    # check out lme4:::terms.merMod()


    # m <- model@frame
    # str(m)
    # attr(m, "terms")
    # unlist(attr(attr(m, "terms"), "predvars.random"))

    # all terms for predictor variables (including interactions)
    allterms <- attr(attr(model@frame, "terms"), "term.labels")
    # data classes (for main effects, but also includes the response and the random effects [and maybe even an offset if present?])
    datclasses <- attr(attr(model@frame, "terms"), "dataClasses")
    # get main effect names (still includes random effects)
    prednames <- datclasses[names(datclasses) %in% allterms]
    # exclude random effects
    ref <- names(ranef(model))
    for(i in ref) prednames <- prednames[-c(which(names(prednames) == i))]
  }

  if(class(model) == "glmmadmb") {
    # all terms for predictor variables (including interactions)
    allterms <- attr(x = model$terms, which = "term.labels")
    # data classes (for main effects, but also includes the response [and maybe even an offset if present?])
    datclasses <- attr(x = model$terms, which = "dataClasses")
    # get main effect names
    prednames <- datclasses[names(datclasses) %in% allterms]

    # # take all terms to vary
    # xterms <- attr(x = model$terms, which = "term.labels")
    # # get data classes
    # dc <- attr(x = model$terms, which = "dataClasses")
    # # get main effects names
    # xterms <- dc[names(dc) %in% xterms]
  }



  # get values for predictions depending on the term's data class
  predvals <- list()

  for(i in 1:length(prednames)) {
    if(prednames[i] == "factor") {
      predvals[[length(predvals) + 1]] <- levels(dat[, names(prednames)[i]])
    }
    if(prednames[i] == "numeric" | prednames[i] == "integer") {
      # if a numeric variable has only two realizations, most commonly 0 and 1, assume it's actually categorical (e.g. manual contrast coding)
      if(length(table(dat[, names(prednames)[i]])) == 2) {
        predvals[[length(predvals) + 1]] <- as.numeric(names(table(dat[, names(prednames)[i]])))
      } else {
        predvals[[length(predvals) + 1]] <- seq(from = min(dat[, names(prednames)[i]]), to = max(dat[, names(prednames)[i]]), length.out = stepsalong)
      }
    }
    # assign name to the list element
    names(predvals)[i] <- names(prednames)[i]
  }

  # if 'termsref' were specified, take those at value 0 (for numeric), reference level (for factor) or at it's smaller value in case of two numerical values
  if(!is.null(termsref)) {
    for(i in 1:length(termsref)) {
      if(datclasses[termsref[i]] == "numeric" | datclasses[termsref[i]] == "integer") {
        if(length(table(dat[, termsref[i]])) == 2) {
          predvals[termsref[i]] <- as.numeric(names(table(dat[, termsref[i]])))[1]
        } else {
          predvals[termsref[i]] <- 0
        }
      }
      if(datclasses[termsref[i]] == "factor") {
        # make sure that the variable remains a factor
        # predvals[termsref[i]] <- factor(levels(dat[, termsref[i]]))
        predvals[termsref[i]] <- factor(levels(dat[, termsref[i]])[1], levels = levels(dat[, termsref[i]]))
      }
    }
  }

  # make a data frame for combinations of values to predict over (only main effects)
  ndata <- expand.grid(predvals)

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
      allres <- foreach(1:N, .packages = "lme4", .combine = 'cbind') %dopar% {
        tempdata <- dat
        tempdata <- tempdata[sample(1:nrow(tempdata), replace = TRUE), ]
        xres <- update(model, data = tempdata)
        predict(xres, newdata = ndata, re.form = NA, type = "response")
      }
      on.exit(stopCluster(cl))
    }
    if(class(model) == "glmmadmb") {
      allres <- foreach(1:N, .packages = "glmmADMB", .errorhandling = "pass") %dopar% { #, .combine = 'cbind'
        tempdata <- dat
        tempdata <- tempdata[sample(1:nrow(tempdata), replace = TRUE), ]
        xres <- update(model, data = tempdata)
        predict(xres, newdata = ndata, re.form = NA, type = "response")
      }
      on.exit(stopCluster(cl))

      # some data handling if there were errors during some of the resampling runs:
      errors <- which(unlist(lapply(allres, function(X) "message" %in% names(X))))
      if(length(errors) >= 1) {
        for(i in errors) {
          allres[[i]] <- rep(NA, nrow(ndata))
        }
        message("during ", errors, " runs there were problems and the results for these runs are returned as NA")
      }
      allres <- do.call("cbind", allres)
    }

  }

  # if parallel is turned off use a simple loop
  if(!useparallel) {
    allres <- matrix(ncol = N, nrow = nrow(ndata))
    for(i in 1:N) {
      tempdata <- dat
      tempdata <- tempdata[sample(1:nrow(tempdata), replace = TRUE), ]
      xres <- update(model, data = tempdata)
      allres[, i] <- predict(xres, newdata = ndata, re.form = NA, type = "response")
    }
  }

  # predict for original data
  ndata$orifit <- predict(model, newdata = ndata, re.form = NA, type = "response")
  return(list(ndata, allres))
}

