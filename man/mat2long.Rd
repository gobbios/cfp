% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mat2long.R
\name{mat2long}
\alias{mat2long}
\title{Expand matrix to data.frame via row and column names}
\usage{
mat2long(x)
}
\arguments{
\item{x}{a matrix with row and column names}
}
\value{
a data.frame with row names in the \code{Rx} column, and the column names in the \code{Cx} column, and values in the \code{val} column
}
\description{
Expand matrix to data.frame via row and column names
}
\examples{
x <- matrix(1:20, ncol=4)
rownames(x) <- paste0("r", 1:5)
colnames(x) <- paste0("c", 1:4)
mat2long(x)
}
