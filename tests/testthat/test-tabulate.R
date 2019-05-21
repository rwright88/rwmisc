test_that("tabulate2 returns correct values when negative, positive, or NA", {
  x <- c(-10:10, NA)
  res <- tabulate2(x)
  exp <- as.integer(rep(1, 22))
  names(exp) <- c(NA, -10:10)
  expect_identical(res, exp)
})
