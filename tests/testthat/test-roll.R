test_that("roll_ema returns expected length numeric vector", {
  out <- roll_ema(runif(100), n = 10, alpha = 0.1)
  expect_is(out, "numeric")
  expect_length(out, 100)
})
