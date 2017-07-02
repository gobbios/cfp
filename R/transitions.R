#' Transitions between elements of a sequence
#'
#' @param S character, the sequence
#' @param start logical, should start state be included
#' @param stop logical, should end state be included
#' @param restemplate an optional matrix with two columns, with the transitions to look for (see details)
#' @details usage of \code{restemplate} might be helpful if sequences are analysed that not all contain the same/complete set of elements. This might be useful if you want to generate matching outputs even though the actual observed sequences (and hence the result of the function) might differ (e.g. in the case of letter transitions in words)
#' @export
#' @examples
#' S <- "AAAABB"
#' transitions(S)
#' transitions(S, start=FALSE, stop=FALSE)





# restemplate <- as.matrix(expand.grid(letters[1:4], letters[1:4]))
# S <- paste(sample(letters[1:3], 3, T), collapse = "")


# S <- "AAAABB"
# start <- stop <- TRUE
transitions <- function(S, start=TRUE, stop=TRUE, restemplate=NULL) {
  if(length(S) == 1) S <- unlist(strsplit(S, ""))
  S

  # unique elements
  if(is.null(restemplate)) {
    Su <- unique(S)
  } else {
    Su <- unique(c(restemplate[, 1], restemplate[, 2]))
  }
  Su

  # add start and stop elements
  if(start) S <- c("start", S)
  if(stop) S <- c(S, "stop")
  S


  if(is.null(restemplate)) {
    res <- as.matrix(expand.grid(Su, Su))
  } else {
    res <- restemplate
  }
  res

  if(start) res <- rbind(cbind("start", Su), res)
  if(stop) res <- rbind(res, cbind( Su, "stop"))
  res
  counts <- numeric(nrow(res))
  i=1
  for(i in 1:(length(S)-1)) {
    xrow <- which(res[, 1] == S[i] & res[, 2] == S[i+1])
    counts[xrow] <- counts[xrow] + 1
  }


  res <- data.frame(e1=res[, 1], e2=res[, 2], count = counts)
  # calculate overall transitions (incl start and stop)
  res$transall <- res$count/sum(res$count)
  # calculate transitions excluding start and stop
  xrow <- which( (res$e1 != "start" & res$e1 != "stop") & (res$e2 != "start" & res$e2 != "stop") )
  res$transstrict <- NA
  res$transstrict[xrow] <- res$count[xrow]/sum(res$count[xrow])

  return(res)
}


# S <- "AAAABB"
# transitions(S)
# transitions(S, start=F, stop=F)




# restemplate <- as.matrix(expand.grid(letters[1:3], letters[1:3]))
# word <- paste(sample(letters[1:3], 25, T), collapse = "")
# transitions(S = word, start = T, stop = T, restemplate = restemplate)





