test_that("identical_all has same return as repeating identical()", {
  x <- list(1, 1, 1)
  expect_identical(
    identical_all(x),
    identical(x[[1]], x[[2]]) && identical(x[[1]], x[[3]])
  )
})
