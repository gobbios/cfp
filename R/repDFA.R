#' repeated DFAs with different subsets
#'
#' @param xdata a \code{data.frame} with the data
#' @param testfactor character, the name of the column for the test factor
#' @param balancefactor character of length 1 or 2, the factors that balanced data sets should be created
#' @param varnames character vector, column names of the numeric variables to be used for LDA
#' @param npercomb the number of cases per level of \code{balancefactor} (if one balance factor) or the number of cases per combination of \code{balancefactor} (if two balance factors)
#' @param nrand numeric, the number of randomized LDAs to be calculated
#'
#' @return a \code{data.frame} with 7 columns (the name of variable, the loading of the function, and the percentage of variance explaine by the first (and second discriminant function))
#' @details information for second function will only be returned if there were at least 2 functions calculated
#' @export
#' @import candisc
#' @examples
#' data(iris)
#' res <- repDFA(xdata = iris, testfactor = "Species", balancefactor = "Species",
#'               varnames = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
#'               npercomb = 5, nrand = 50)
#' table(res$df1_best)
#' table(res$df2_best)

repDFA <- function(xdata, testfactor, balancefactor, varnames, npercomb = NULL, nrand = 20) {
  # empty matrices for storing the loadings of the first and second functions' loadings
  mat1 <- mat2 <- matrix(ncol = length(varnames), nrow = nrand, dimnames = list(NULL, varnames))

  res <- matrix(ncol = 7, nrow = nrand)
  colnames(res) <- c("rand", "df1_best", "df1_load", "df1_pct", "df2_best", "df2_load", "df2_pct")

  for(i in 1:nrand) {
    res[i, "rand"] <- i
    # create balanced data set
    tempdata <- balancedataset(xdata = xdata, whattobalance = balancefactor, n = npercomb)$seldata
    table(tempdata$Ind, tempdata$Ctxt)
    # calculate LDA
    ldares <- candisc(lm(as.matrix(tempdata[, varnames]) ~ tempdata[, testfactor]))
    strucmat <- ldares$structure
    pcts <- round(ldares$pct/100, 3)

    res[i, "df1_best"] <- rownames(strucmat)[which.max(abs(strucmat[, 1]))]
    res[i, "df1_load"] <- round(strucmat[, 1][which.max(abs(strucmat[, 1]))], 3)
    res[i, "df1_pct"] <- pcts[1]

    if(ncol(strucmat) >= 2) {
      res[i, "df2_best"] <- rownames(strucmat)[which.max(abs(strucmat[, 2]))]
      res[i, "df2_load"] <- round(strucmat[, 2][which.max(abs(strucmat[, 2]))], 3)
      res[i, "df2_pct"] <- pcts[2]
    }
  }

  res <- data.frame(res)
  res[, c("rand", "df1_load", "df1_pct", "df2_load", "df2_pct")] <- apply(res[, c("rand", "df1_load", "df1_pct", "df2_load", "df2_pct")], 2, as.numeric)
  return(res)
}


