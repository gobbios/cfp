#' load knitr chunk cache interactively
#'
#' @param chunk character, the name of the chunk
#' @param path character, the path to the cache location (by default it's the one set up by knitr)
#' @param envir character, the environment to load the objects into (default is the parent of the function)
#'
#' @keywords knitr
#' @details The function borrows heavily from the \code{qwraps2} package (via https://github.com/dewittpe/qwraps2).
#'
#' The messages that display which objects, if any, were loaded and which class they belong to might not be entirely reliable.
#' @return objects are loaded and messages returned
#' @export
#'

cacheload <- function(chunk, path = knitr::opts_chunk$get("cache.path"), envir = parent.frame()) {
  xpat <- paste0("^(", paste(chunk, collapse = "|"), ")_[0-9a-f]{32}\\.rdx$")
  x <- list.files(path = path, pattern = xpat, full.names = TRUE)
  x <- gsub("\\.rdx$", "", x)
  if (length(x) == 1) {
    ls_before <- ls(envir = envir)
    lazyLoad(x, envir = envir)
    ls_after <- ls(envir = envir)
    if (length(ls_after) > length(ls_before)) {
      newobs <- ls_after[!ls_after %in% ls_before]
      newobs_class <- as.character(unlist(sapply(newobs, function(X)class(get(X))[1])))
      out <- paste(paste(newobs, newobs_class, sep = ": "), collapse = "\n")
      message(paste("apparent success. the following objects were loaded:\n", out))
    }
  } else {
    if (length(x) == 0) {
      message("no cache file found, nothing loaded")
      return(NULL)
    } else {
      message("more than one cache file found, nothing loaded")
      return(NULL)
    }
  }
}


