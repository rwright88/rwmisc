test_that("wtd_quantile returns expected length numeric vector", {
  out <- wtd_quantile(runif(10), w = runif(10), probs = c(0.25, 0.5, 0.75))
  expect_is(out, "numeric")
  expect_length(out, 3)
})
