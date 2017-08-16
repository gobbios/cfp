#' graphical model checks
#'
#' @param mod a model object (\code{\link[lme4]{merMod}})
#'
#' @return a plot with histogram and qq plot of model residuals and a plot with fitted versus residuals
#' @export
#' @importFrom graphics box
#' @importFrom stats fitted qqline qqnorm resid
#'
#' @examples
#' x <- data.frame(x = rnorm(100), y = rnorm(100), ID = sample(letters, 100, replace = TRUE))
#' mod <- lme4::lmer(y ~ x + (1|ID), data = x)
#' modcheck(mod = mod)

modcheck <- function(mod) {
  # save graphical parameters (to reset them at the end of the function)
  mypars <- par(no.readonly = TRUE)
  par(mfrow = c(2, 2))

  resis <- resid(mod)
  hist(resis, xlab = "residuals", main = "", las = 1)
  box()

  plot(fitted(mod), resis, xlab = "fitted values", ylab = "residuals", las = 1)

  qqnorm(resis)
  qqline(resis)

  par(mypars)
}


