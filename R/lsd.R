
#' Levenshtein distance (or rather similarity)
#'
#' @param S1 character of length 1 or longer, contains the sequence
#' @param S2 character of length 1 or longer, contains the sequence
#'
#' @details one thing to notice is that we assume that if both sequences are uniform the mutual information is 1 (regardless of length, but in our titi data set sequence length is constant...). Also, the function will return \code{NaN} or \code{NA} if one of the sequences is uniform (exists of only one syllable/call type). Whether this is correct behaviour or a bug, I can't say at the moment...
#'
#' @return numeric
#' @export



lsd <- function(S1, S2){
  # library(RecordLinkage)

  #levenshteinSim()

}

