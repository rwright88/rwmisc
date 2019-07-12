test_that("equal() returns correct logical for numeric vectors", {
  expect_identical(equal(c(1, 1, 1)), TRUE)
  expect_identical(equal(c(1, 1, 2)), FALSE)
})
