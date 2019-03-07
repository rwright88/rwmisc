context("test-summary2")

test_that("returns a data frame with correct columns", {
  dat <- data.frame(
    a = 1L,
    b = 1,
    c = TRUE,
    d = "a",
    e = Sys.Date(),
    f = factor(1)
  )

  out <- summary2(dat)

  expect_is(out, "data.frame")
  expect_identical(
    names(out),
    c("name", "type", "n", "d_na", "n_unique", "mean", "p0", "p25", "p50", "p75", "p100")
  )
})
