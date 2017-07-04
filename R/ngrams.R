
#' *n*-grams of sequences
#'
#' @param S character of length 1 or longer, contains the sequence
#' @param n numeric of length 1, syllable length, can only be 1, 2 or 3
#' @param xsep character, how should syllables be separated for 2-grams or 3-grams
#' @details there is a substantially faster function in the \code{\link[tau]{textcnt}} function in the \pkg{tau} package...
#' @export
#' @return a data.frame with two columns, the first describes the syllables and the second is the count
#' @examples
#' SS <- "AAAABBBA"
#' ngrams(SS, 1)
#' ngrams(SS, 2)
#' ngrams(SS, 3)
#' ngrams(SS, 3, xsep="-")


# library(tau)
# S <- SO <- "AABA"
# textcnt(SO, n=3, method="ngram")
# n=3
# xsep=""
ngrams <- function(S, n, xsep="") {
  if(n > 3) stop("only n=1, n=2 or n=3 allowed...")

  # split sequence
  if(length(S) == 1) S <- unlist(strsplit(S, ""))
  S

  Su <- unique(S)
  if(n == 1) {
    templ <- as.matrix(expand.grid(Su))

    res <- data.frame(table(S))
    colnames(res) <- c("syl", "cnt")

  }

  if(n > 1) {

    if(n == 2) templ <- as.matrix(expand.grid(Su, Su))
    if(n == 3) templ <- as.matrix(expand.grid(Su, Su, Su))

    xres <- numeric(nrow(templ))
    i=1
    for(i in 1:(length(S)-(n-1))) {
      xpos <- apply(templ, 1, function(x) sum(x == S[c(i : (i+n-1))]) == n)
      xres[xpos] <- xres[xpos] + 1
    }
    res<- data.frame(syl=apply(templ, 1, paste, collapse=xsep), cnt=xres)


  }
  return(res)

}

