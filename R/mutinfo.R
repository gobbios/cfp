#' Mutual information
#'
#' @param S1 character of length 1 or longer, contains the sequence
#' @param S2 character of length 1 or longer, contains the sequence
#'
#' @details one thing to notice is that we assume that if both sequences are uniform the mutual information is 1 (regardless of length, but in our titi data set sequence length is constant...). Also, the function will return \code{NaN} or \code{NA} if one of the sequences is uniform (exists of only one syllable/call type). Whether this is correct behaviour or a bug, I can't say at the moment...
#'
#' @source Matlab function by A. Kershenbaum
#' @return numeric
#' @export
#'
#' @examples
#' S1 <- "AAABBCA"
#' S2 <- "AAACCAB"
#' mutinfo(S1, S2)
#'
#' S1 <- "AAAAAAA"
#' S2 <- "AAAAAAA"
#' mutinfo(S1, S2)
#'
#' S1 <- "AAAAAAB"
#' S2 <- "BBBBBAA"
#' mutinfo(S1, S2)
#'
#' S1 <- "AAAAAAA"
#' S2 <- "BBBCCAA"
#' mutinfo(S1, S2)



# C <- c("A", "A", "B", "A")
# C0 <- c("C", "D", "A", "A")


mutinfo <- function(S1, S2){
  style="original"
  C <- S1
  C0 <- S2


  # function provided in MatLab form by A. Kershenbaum to M. Berthet


  # transform character vectors into numeric
  if(length(C) == 1) C <- unlist(strsplit(C, ""))
  if(length(C0) == 1) C0 <- unlist(strsplit(C0, ""))
  Su <- unique(c(C, C0))
  C <- as.numeric(sapply(C, function(x)which(Su == x)))
  C0 <- as.numeric(sapply(C0, function(x)which(Su == x)))
  C
  C0


  if(length(table(C)) == 1 & length(table(C0)) == 1) {
    res <- 1
  } else {

    un <- unique(C0)
    ul <- unique(C)
    N <- length(C) #Number of objects


    # Calculate the entropies of the two classifications

    if(style=="original") {
      Sn=0
      n=1
      for (n in 1:length(un)){
        nn <- length(which(C0==un[n]))
        Sn <- Sn+(nn*log(nn/N))# Is negative !!
      }
      Sl=0
      for (l in 1:length(ul)){
        nl <- length(which(C==ul[l]))
        if (nl>0) Sl <- Sl+(nl*log(nl/N))
        # l+1
      }
    }
    if(style=="chris") {
      Sn <- sum(table(C0)*log(table(C0)/N))
      Sl <- sum(table(C)*log(table(C)/N))
    }

    # up to here it does not make a difference whether we use letters or numbers...


    # Combine these to the mutual information
    if(style=="original") {
      C
      Snl=0
      n=1
      l=4
      for (n in 1:length(un)){
        nn <- length(which(C0==un[n]))
        #nn2 <- length(which(letters[C0]==letters[un][n]))
        for (l in 1:length(ul)){
          nl <- length(which(C==ul[l]))
          #nl2 <- length(which(letters[C]==letters[ul][l]))
          (nnl <- length(intersect(which(C0==un[n]), which(C==ul[l]))))
          #(nnl2 <- length(intersect(which(letters[C0]==letters[un][n]), which(letters[C]==letters[ul][l]))))
          if (nnl>0) Snl <- Snl+nnl*log(N*nnl/(nn*nl))
          #if(nnl != nnl2) stop()
        }
      }


    }

    #intersect(table(C), table(C0))


    res <- Snl/sqrt(Sn*Sl)
  }



  {
    # % Calculates the normalised mutual information between two sets of symbols
    # % INPUTS:
    #   % C, C0: Two numerical arrays of equal length, representing the two
    # %        categorisations. In the literature, C0 is considered the "true"
    # %        class, and C is the clustering output. Note that these are
    # %        numerical arrays - even if the symbols themselves are letters,
    # %        they must first be converted into integers.
    # % OUTPUTS:
    #   % NMI:   The calculated normalised mutual information, in the range 0-1
    # %
    # % For more information, see:
    #   % Zhong, S. & Ghosh, J. 2005, Generative model-based document clustering:
    #   % a comparative study Knowl Inf Syst, 8:374-384.
    #
  }


  return(res)
}






