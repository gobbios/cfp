# check whether the balancedataset function works

test_that("balanced data sets", {
  data(iris)
  xdata <- iris[sample(1:nrow(iris), 70), ]
  temp <- balancedataset(xdata, "Species")$seldata
  tab <- table(temp$Species)

  # check 1 - all values the same in the output data frame
  check1 <- length(unique(tab)) == 1
  # check 2 - value is the minimum of counts in original xdata
  check2 <- tab[1] == min(table(xdata$Species))

  expect_true(check1)
  expect_true(check2)

  # another one with combinations
  # set.seed(123)
  xdata <- data.frame(var = rnorm(500), fac1 = sample(letters[1:3], 500, TRUE), fac2 = sample(LETTERS[20:22], 500, TRUE))
  xtab <- min(table(xdata$fac1, xdata$fac2))
  res <- balancedataset(xdata, whattobalance = c("fac1", "fac2"))$seldata
  tab <- table(res$fac1, res$fac2)
  expect_equal(max(tab), xtab)
  expect_equal(min(tab), xtab)

  res <- balancedataset(xdata, whattobalance = c("fac1", "fac2"), n = 2)$seldata
  tab <- table(res$fac1, res$fac2)
  expect_equal(max(tab), 2)
  expect_equal(min(tab), 2)

  # require too large a number of cases -> warning and resetting of n
  expect_warning(res <- balancedataset(xdata, whattobalance = c("fac1", "fac2"), n = 100)$seldata)
  tab <- table(res$fac1, res$fac2)
  expect_equal(max(tab), xtab)
  expect_equal(min(tab), xtab)

  # return identical data if the supplied data are already perfectly balanced
  newdata <- balancedataset(xdata, whattobalance = c("fac1", "fac2"))$seldata
  res <- balancedataset(newdata, whattobalance = c("fac1", "fac2"))$seldata
  expect_identical(newdata, res)
  newdata <- balancedataset(xdata, whattobalance = c("fac1", "fac2"), n = 1)$seldata
  res <- balancedataset(newdata, whattobalance = c("fac1", "fac2"))$seldata
  expect_identical(newdata, res)


  # remove one combination so that the function will fail
  xdata <- xdata[-c(which(xdata$fac1 == "a" & xdata$fac2 == "U")), ]
  expect_error(balancedataset(xdata, whattobalance = c("fac1", "fac2")))

})
