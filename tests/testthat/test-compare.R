test_that("equal_all() correct return for logical vectors", {
  expect_identical(equal_all(c(TRUE, TRUE, TRUE)), TRUE)
  expect_identical(equal_all(c(TRUE, TRUE, FALSE)), FALSE)
})

test_that("equal_all() correct return for integer vectors", {
  expect_identical(equal_all(c(1L, 1L, 1L)), TRUE)
  expect_identical(equal_all(c(1L, 1L, 2L)), FALSE)
})

test_that("equal_all() correct return for numeric vectors", {
  expect_identical(equal_all(c(1.1, 1.1, 1.1)), TRUE)
  expect_identical(equal_all(c(1.1, 1.1, 2.1)), FALSE)
})

test_that("equal_all() correct return for character vectors", {
  expect_identical(equal_all(c("a", "a", "a")), TRUE)
  expect_identical(equal_all(c("a", "a", "b")), FALSE)
})

test_that("equal_all() correct return for list vectors", {
  expect_identical(equal_all(list(1L, 1L, 1L)), TRUE)
  expect_identical(equal_all(list(1L, 1L, 2L)), FALSE)
})
