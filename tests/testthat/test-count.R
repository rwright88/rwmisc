test_that("count returns correct values when negative, positive, or NA", {
  x <- c(-10:10, NA)
  res <- count(x)
  exp <- list(key = c(NA, -10:10), count = as.integer(rep(1, 22)))
  expect_identical(res, exp)
})
