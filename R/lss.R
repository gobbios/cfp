#' Levenshtein similarity (based on Levenshtein distance)
#'
#' @param S1 character of length 1 or longer, contains the sequence
#' @param S2 character of length 1 or longer, contains the sequence
#'
#' @details we use the \code{\link[utils]{adist}} function, and normalize it according to \code{1-d(S1, S2) / max(S1l, S2l)}, where \code{D} is the \code{adist} function and \code{S1l} and \code{S2l} are the lengths of the two sequences
#'
#' @return numeric value of Levenshtein similarity
#' @importFrom utils adist
#' @export
#' @examples
#' S1 <- "AAAABBAAAC"
#' S2 <- "ABAABBBAAA"
#' lss(S1, S2)
#' lss(S1, S1)
#' \dontrun{
#' RecordLinkage::levenshteinSim(S1, S2)
#' }
#' S3 <- "CBX"
#' lss(S1, S3)
#' lss(S2, S3)



# S1 <- "AAAABBAAAC"
# S2 <- "ABAABBBAAA"


lss <- function(S1, S2){
  if(length(S1) > 1 | length(S2) > 1) stop("works only if length(S1) == length(S2) == 1")
  LD <- as.numeric(adist(S1, S2))
  return(1 - (LD / max(c(nchar(S1), nchar(S2)))))
  # alternatively: use levenshteinSim() of the RecordLinkage package...
}



# S1 <- "AAAAAA"
# S2 <- "BBA"
#
# lss(S1, S2)
# RecordLinkage::levenshteinSim(S1, S2)
# lss(S1, S1)







