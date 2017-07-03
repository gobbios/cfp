#' Transitions between elements of a sequence
#'
#' @param S character, the sequence
#' @param xstart logical, should start state be included
#' @param xstop logical, should end state be included
#' @param strucmat an optional matrix with column and row names (see details and examples)
#' @param out character: either \code{"ml"} or \code{"bayes"}
#' @details usage of \code{strucmat} might be helpful for at least tow reasons:
#' 1) if there are constraints on the possible transitions. For example, a start cannot be followed by a stop (that wouldn't be a sequence). Such impossible transitions are indicated by 0 in this matrix.
#' 2) if sequences are analysed that not all contain the same/complete set of elements. This might be useful if you want to generate matching outputs even though the actual observed sequences (and hence the result of the function) might differ (e.g. in the case of letter transitions in words)
#' @export
#' @references
#' Alger, S. J., Larget, B. R., & Riters, L. V. (2016). A novel statistical method for behaviour sequence analysis and its application to birdsong. Animal behaviour, 116, 181-193.
#' @examples
#' transitions("AAAAABBC", out="ml")
#' transitions("AAAAABBC", out="bayes")
#'
#' smat <- matrix(1, ncol=3, nrow=4);
#' colnames(smat) <- c("A", "B", "C"); rownames(smat) <- c("start", "A", "B", "C")
#'
#' transitions("AAAAABBC", out="ml", strucmat = smat)
#' transitions("AAAAABBC", out="bayes", strucmat = smat)
#'
#' # errors:
#' # transitions("AAAAABBC", out="ml", strucmat = smat, xstop = TRUE)
#' # transitions("AAAAABBC", out="bayes", strucmat = smat, xstop = TRUE)
#'
#' # add a stop column, but constrain that starts cannot be followed by stops
#' smat <- cbind(smat, 1); colnames(smat)[4] <- "stop"; smat[1, 4] <- 0
#' transitions("AAAAABBC", out="ml", strucmat = smat, xstop = TRUE)
#' transitions("AAAAABBC", out="bayes", strucmat = smat, xstop = TRUE)



transitions <- function(S, strucmat=NULL, xstart=TRUE, xstop=FALSE, out="ml") {
  # split sequence
  if(length(S) == 1) S <- unlist(strsplit(S, ""))

  # unique elements
  Su <- unique(S)

  # sanity check
  if(!is.null(strucmat)) {
    if(xstop & !"stop" %in% colnames(strucmat)) stop("structure matrix does not contain 'stop' state (in columns)")
    if(xstart & !"start" %in% rownames(strucmat)) stop("structure matrix does not contain 'start' state (in rows)")
    # all other states should occur in both row and column names
    if(sum(Su %in% colnames(strucmat)) != length(Su)) stop("not all states in the structure columns")
    if(sum(Su %in% rownames(strucmat)) != length(Su)) stop("not all states in the structure rows")
  }




  if(xstart) S <- c("start", S)
  if(xstop) S <- c(S, "stop")


  if(is.null(strucmat)) {
    strucmat <- matrix(1, ncol=length(Su), nrow=length(Su))
    colnames(strucmat) <- rownames(strucmat) <- Su
    if(xstart) {
      strucmat <- rbind(1, strucmat)
      rownames(strucmat)[1] <- "start"
    }
    if(xstop) {
      strucmat <- cbind(strucmat, 1)
      colnames(strucmat)[ncol(strucmat)] <- "stop"
    }
  }



  obsmat <- strucmat - strucmat

  i=1
  for(i in 1:(length(S)-1)) {
    obsmat[S[i], S[i+1]] <- obsmat[S[i], S[i+1]] +1
  }


  if(out == "bayes") {
    # 'Bayesian' matrix...
    bmat <- obsmat + strucmat
    res <- bmat / rowSums(bmat)

    if(NaN %in% res) res[is.nan(res)] <- NA
    if(Inf %in% res) res[is.infinite(res)] <- NA

    # sanity check: all rowsums should add up to 1 (or zero)
    if(sum(round(rowSums(res, na.rm = T), 5) %in%  c(0, 1)) != nrow(res)) warning("warning 1: row sums don't add up to 1 (or 0)")
  }

  if(out == "ml") {
    res <- obsmat / rowSums(obsmat)

    if(NaN %in% res) res[is.nan(res)] <- NA
    if(Inf %in% res) res[is.infinite(res)] <- NA

    # sanity check: all rowsums should add up to 1 (or zero)
    if(sum(round(rowSums(res, na.rm = T), 5) %in% c(0, 1)) != nrow(res)) warning("warning 2: row sums don't add up to 1 (or 0)")
  }

  return(res)
}




