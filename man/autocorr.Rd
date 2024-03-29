% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autocorr.R
\name{autocorr}
\alias{autocorr}
\title{calculate an auto-correlation term}
\usage{
autocorr(
  modres,
  xdata,
  corrvar,
  contrfac,
  doplot = TRUE,
  plot_sds = c(0.001, 2),
  opti_sds = c(0.001, 1),
  plot_n = 51,
  stdzcorr = TRUE
)
}
\arguments{
\item{modres}{the model result object from \code{(g)lmer}}

\item{xdata}{the data.frame used to fit the model}

\item{corrvar}{character, the column name in \code{xdata} for the variable relevant for the correlation (e.g. date)}

\item{contrfac}{character, the column name in \code{xdata} for the control factor (e.g. ID or group)}

\item{doplot}{logical, should a plot be produced}

\item{plot_sds}{numeric of length 2, the SDs for visualization if required}

\item{opti_sds}{numeric of length 2, the SDs for the optimization}

\item{plot_n}{numeric, the number of steps for the visualization}

\item{stdzcorr}{logical, should the correlation-variable \code{corrvar} be z-transformed}
}
\value{
a list (and a plot)
}
\description{
calculate an auto-correlation term
}
\details{
This function is heavily based on code by Roger Mundry.

The difference between \code{plotSDs} and \code{optiSDs} is that the former is only used for the visualization, and generally should cover at least equal if not wider range than the latter
}
\examples{
# generate model
set.seed(123)
n = 1000
xdata <- data.frame(pred1 = rnorm(n), pred2 = rnorm(n))
xdata$ID <- factor(sample(LETTERS[1:20], size = n, replace = TRUE))
re <- rnorm(20)
names(re) <- LETTERS[1:20]
xdata$resp <- re[as.numeric(xdata$ID)] + xdata$pred1 * 0.3 + xdata$pred2 * (-0.4) +
              diffinv(rnorm(n, sd = 1.5))[-n] + rnorm(n, sd = 0.5)
\dontrun{
library(lme4)
res <- lmer(resp ~ pred1 + pred2 + (1|ID), data = xdata)
summary(res)
# visual
autocorr(modres = res, xdata = xdata, corrvar = "pred1", contrfac = "ID", plot_sds = c(0.001, 1))
# narrow optimization range and suppress plot
xres <- autocorr(modres = res, xdata = xdata, corrvar = "pred1", contrfac = "ID",
                 opti_sds = c(0.05, 0.25), doplot = FALSE)
# add location of local minimum to previous plot
abline(v = xres$opti$minimum, col = "red")
xdata$ac <- xres$optiac
plot(xdata$ac, xdata$resp)
res2 <- update(res, .~. +ac)
summary(res2)
}
}
