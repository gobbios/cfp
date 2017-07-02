# function to calculate slope of a titi monkey AB sequence
# works with single length sequences (e.g. AAAABBBA) or vectors of sequence elements
# returns NA if length of sequence is one element
# returns if logistic regression runs into complete separation (e.g. with sequences like AAAABBBB)

#' 'Slope' of elements within a sequence according to position in sequence
#'
#' @param s character of length 1 or longer, contains the sequence
#' @param type character (linear or logistic): which type of regression to use
#' @param target character of length 1: occurence of which element to model
#' @importFrom stats binomial coefficients glm lm
#' @export
#' @return numeric of length 1
#' @details \code{s} can be provided via a single character, which only works if the elements are themselves only one character, for example "A" in "AAABGACCAAFW"
#' @details \code{s} can also be provided via a character vector, which will work also if the elements are themselves longer than one character, for example "Ax" in c("Ax", "B", "A", "A", "Ax", "C")
#' @examples
#' S <- "AAAAABBBA"
#' elementslope(S, "linear", target="A")
#' elementslope(S, "logistic", target="A")
#' S <- c("A", "A", "B", "A", "A", "A")
#' elementslope(S, "linear")
#' elementslope(S, "logistic")
#' S <- c("Ax", "Ax", "B", "Ax", "Ax", "Ax")
#' elementslope(S, "linear", target = "Ax")
#' elementslope(S, "logistic", target = "Ax")
#' S <- "AAAAABBB"
#' elementslope(S, "linear", target="A")
#' elementslope(S, "logistic", target="A")
#' # NA because of complete separation, i.e. logistic model cannot be identified



# s <- "AAAAABBBA"
# s <- "A"
# s <- c("A", "A", "B", "A", "A", "A")
# s <- "AAAAABBBB"
#
#
# type="linear"
# target = "A"

elementslope <- function(s, type="linear", target="A") {


  if(length(s) == 1) {
    if(nchar(s) == 1) {
      res <- NA
    } else {
      s <- unlist(strsplit(s, ""))
    }
  }

  resp <- as.numeric(s == target)
  pred <- as.numeric(scale(1:length(s)))


  if(type == "linear") {
    res <- lm(resp ~ pred)
    res <- as.numeric(coefficients(res)[2])
  }

  # https://stackoverflow.com/questions/4948361/how-do-i-save-warnings-and-errors-as-output-from-a-function


  if(type == "logistic") {
    myTryCatch <- function(expr) {
      warn <- err <- NULL
      value <- withCallingHandlers(
        tryCatch(expr, error=function(e) {
          err <<- e
          NULL
        }), warning=function(w) {
          warn <<- w
          invokeRestart("muffleWarning")
        })
      list(value=value, warning=warn, error=err)
    }

    res <- myTryCatch(glm(resp ~ pred, family=binomial))
    if(is.null(res[[2]]) & is.null(res[[3]])) {
      res <- as.numeric(coefficients(res[[1]])[2])
    } else {
      res <- NA
    }

  }
  return(res)
}
