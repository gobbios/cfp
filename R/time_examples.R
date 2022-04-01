#' time it takes to run examples in a package
#'
#' @param package character, path to package (default is current working
#'                directory)
#' @param foo character, the name(s) of the function(s) to time. Default is set
#'            to include all functions that are found to be exported by the
#'            package.
#' @param includedontrun logical, include examples that are set to 'dontrun',
#'                       default is \code{FALSE}
#' @details This is a very experimental function. Use it with caution.
#' @return a data frame
#' @importFrom devtools as.package
#' @importFrom utils read.table example
#' @export
#'

time_examples <- function(package = ".",
                          foo = NULL,
                          includedontrun = FALSE) {
  pkg <- devtools::as.package(package)
  suppressPackageStartupMessages(require(package = pkg$package,
                                         character.only = TRUE))

  foos <- as.character(read.table(file.path(pkg$path, "NAMESPACE"))[, 1])
  foos <- foos[grepl(pattern = "^export\\(",
                     foos) &
                 grepl(pattern = "\\)$",
                       foos)]
  foos <- gsub("^export\\(", "", foos)
  foos <- gsub("\\)$", "", foos)

  if (is.null(foo)) {
    xfoos <- foos
  } else {
    xfoos <- unique(foos[foos %in% foo])
  }

  if (length(xfoos) == 0) {
    return("no valid function found")
  }

  xxres <- data.frame(foo = xfoos, time = NA, warn = NA, error = NA)
  for (f in 1:length(xfoos)) {
    cat("running examples in:", xfoos[f], "\n")
    x <- system.time({
      y <- tryCatch({
        example(topic = xfoos[f],
                character.only = TRUE,
                ask = FALSE,
                echo = FALSE,
                verbose = FALSE,
                run.dontrun = includedontrun)
      },
      error = function(e) return("error"),
      warning = function(w) return("warning"))
      })
    if (length(y) == 1) {
      if (y == "error") xxres$error[f] <- TRUE
      if (y == "warning") xxres$warn[f] <- TRUE
    }
    xxres$time[f] <- round(as.numeric(x[3]), 1)
    rm(x, y)
  }
  xxres
}
