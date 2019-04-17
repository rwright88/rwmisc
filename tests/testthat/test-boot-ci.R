context("test-boot-ci")

test_that("boot_ci returns expected length numeric vector", {
  out <- boot_ci(runif(10), times = 100, probs = c(0.025, 0.975))
  expect_is(out, "numeric")
  expect_length(out, 2)
})
