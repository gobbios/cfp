#' repeated DFAs with balanced subsets
#'
#' can be used to determine variables that discriminate in the context of permuted DFA
#' @param xdata a \code{data.frame} with the data
#' @param testfactor character, the name of the column for the test factor (grouping factor)
#' @param balancefactor character of length 1 or 2, the factor(s) used for balancing
#' @param varnames character vector, column names of the numeric variables to be used for DFA
#' @param npercomb the number of cases per level of \code{balancefactor} (if one balance factor) or the number of cases per combination of \code{balancefactor} (if two balance factors)
#' @param nrand numeric, the number of DFAs to be calculated
#'
#' @return a \code{data.frame} with information on the first (and if appropriate also second) discriminant function
#' \itemize{
#'   \item name of the variable with the highest absolute loading
#'   \item loading of the variable on the given function (can be either negative or positive)
#'   \item proportion of variance explained by the function
#' }
#' @details information for second function will only be returned if there were at least 2 functions calculated
#' @export
#' @import candisc
#' @author Christof Neumann
#' @references Berthet, M., Neumann, C., Mesbahi, G., Cäsar, C., & Zuberbühler, K. (2018). Contextual encoding in titi monkey alarm call sequences. Behavioral Ecology and Sociobiology, 72(1), 8.
#'
#' Mundry, R., & Sommer, C. (2007). Discriminant function analysis with nonindependent data: consequences and an alternative. Animal Behaviour, 74(4), 965-976.
#' @seealso \code{\link{balancedataset}}
#' @examples
#' data(iris)
#' res <- repDFA(xdata = iris, testfactor = "Species", balancefactor = "Species",
#'               varnames = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
#'               npercomb = 5, nrand = 50)
#' table(res$df1_best)
#' table(res$df2_best)

repDFA <- function(xdata, testfactor, balancefactor, varnames, npercomb = NULL, nrand = 20) {
  # empty matrix for storing the results
  res <- matrix(ncol = 7, nrow = nrand)
  colnames(res) <- c("rand", "df1_best", "df1_load", "df1_pct", "df2_best", "df2_load", "df2_pct")

  for(i in 1:nrand) {
    res[i, "rand"] <- i
    # create balanced data set
    tempdata <- balancedataset(xdata = xdata, whattobalance = balancefactor, n = npercomb)$seldata
    table(tempdata$Ind, tempdata$Ctxt)
    # calculate DFA
    dfares <- candisc(lm(as.matrix(tempdata[, varnames]) ~ tempdata[, testfactor]))
    strucmat <- dfares$structure
    pcts <- round(dfares$pct/100, 3)
    # extract relevant info
    res[i, "df1_best"] <- rownames(strucmat)[which.max(abs(strucmat[, 1]))]
    res[i, "df1_load"] <- round(strucmat[, 1][which.max(abs(strucmat[, 1]))], 3)
    res[i, "df1_pct"] <- pcts[1]

    # do so for the second function if there is one
    if(ncol(strucmat) >= 2) {
      res[i, "df2_best"] <- rownames(strucmat)[which.max(abs(strucmat[, 2]))]
      res[i, "df2_load"] <- round(strucmat[, 2][which.max(abs(strucmat[, 2]))], 3)
      res[i, "df2_pct"] <- pcts[2]
    }
  }

  # transform into data frame for output
  res <- data.frame(res)
  res[, c("rand", "df1_load", "df1_pct", "df2_load", "df2_pct")] <- apply(res[, c("rand", "df1_load", "df1_pct", "df2_load", "df2_pct")], 2, as.numeric)
  return(res)
}


