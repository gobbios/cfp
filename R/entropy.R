#' Shannon entropy
#'
#' @param S character of length 1 or longer, contains the sequence
#' @param n numeric of length 1, first- or second-order entropy (\code{n=1} or \code{n=2})
#'
#' @return a numeric value
#' @export
#' @details see also the \code{\link[entropy]{entropy}} function in the \pkg{entropy}
#' @examples
#' S <- "AABAACA"
#' entropy(S, n = 1)
#' \dontrun{
#' # entropy's version only works with table counts
#' St <- table(unlist(strsplit(S, "")))
#' entropy::entropy(St, method = "ML" , unit="log2")
#' }
#' entropy(S, n = 2)





entropy <- function(S, n=1) {
  # split sequence
  if(length(S) == 1) S <- unlist(strsplit(S, ""))

  if(n == 1) {
    St <- table(S)
    pr <- as.numeric(St)/sum(St)
    res <- sum(pr * (-1) * log(pr, 2))
  }

  if(n == 2) {
    St <- table(sapply(1:(length(S)-1), function(x)paste(S[x], S[x+1], sep = "-")))
    pr <- as.numeric(St)/sum(St)
    res <- sum(pr * (-1) * log(pr, 2))
  }

  return(res)
}

