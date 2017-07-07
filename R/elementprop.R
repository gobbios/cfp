#' Proportion of elements in a sequence
#'
#' @param S character of length 1 or longer, contains the sequence
#' @param target character of length 1: occurence of which element to model
#'
#' @return the porportion of \code{target} elements in \code{S}
#' @export
#' @details sequences cannot contain \code{NA}s at this point.
#'
#' @examples
#' elementprop("AAABBB", "B") #should be 0.5
#' elementprop("AAAB", "A") # should be 0.75
#' elementprop("AB", "A") # should be 0.5
#'
#' S <- c("A", "A", "A", "B", "B", "B")
#' elementprop(S, target="A")
#' elementprop(S, target="X")


# S <- "AAABBB"
# target <- "A"

elementprop <- function(S, target) {
  # split sequence
  if(length(S) == 1) S <- unlist(strsplit(S, ""))

  if(target %in% S) {
    res <- as.numeric(table(S)[target] / length(S))
  } else {
    res <- 0
  }
  return(res)
}

# S <- "AAABBB"
# elementprop(S, "A")
# elementprop(S, "X")


