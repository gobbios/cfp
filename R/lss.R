#' Levenshtein similarity (based on Levenshtein distance)
#'
#' @param S1 character of length 1 or longer, contains the sequence
#' @param S2 character of length 1 or longer, contains the sequence
#' @param normalize logical, should the distance measure be normalized (by default: \code{TRUE})
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
#' # and with character vectors:
#' S1 <- c("one", "two", "three")
#' S2 <- c("eins", "two", "three")
#' lss(S1, S2)
#' lss("ABC", "DBC")



# S1 <- "AAAABBAAAC"
# S2 <- "ABAABBBAAA"
# S1 <- c("BB", "AA", "CC")
# S2 <- c("AA", "BB", "CC", "CC")
# S1 <- c("one", "two", "three", "A", "A")
# S2 <- c("ein s", "two", "three", "A", "A")
# S3 <- c("eins", "two", "three", "A", "A")
# lss(S1, S2)
# lss(S1, S3)
# lss("abcAA", "XbcAA")

lss <- function(S1, S2, normalize = TRUE){
  if(length(S1) > 1 | length(S2) > 1) {
    # stop("works only if length(S1) == length(S2) == 1")
    allchars <- unique(c(as.character(S1), as.character(S2)))
    # assign single letters/numbers to each
    l <- length(allchars)
    if(l > 62) stop("too many different strings")
    repl <- c(letters, LETTERS, 0:9)[1:l]
    # names(allchars) <- repl
    names(repl) <- allchars
    repl[allchars]
    S1 <- paste0(repl[S1], collapse = "")
    S2 <- paste0(repl[S2], collapse = "")
  }

  LD <- as.numeric(adist(S1, S2))
  if(normalize) return(1 - (LD / max(c(nchar(S1), nchar(S2)))))
  if(!normalize) return(LD)
  # alternatively: use levenshteinSim() of the RecordLinkage package...
}



# S1 <- "AAAAAA"
# S2 <- "BBA"
#
# lss(S1, S2)
# RecordLinkage::levenshteinSim(S1, S2)
# lss(S1, S1)







