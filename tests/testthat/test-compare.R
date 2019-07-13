test_that("equal_all() returns correct logical for numeric vectors", {
  expect_identical(equal_all(c(1, 1, 1)), TRUE)
  expect_identical(equal_all(c(1, 1, 2)), FALSE)
})
