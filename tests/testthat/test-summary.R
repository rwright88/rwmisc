test_that("summary2 returns a data frame with correct columns", {
  dat <- data.frame(
    a = 1L,
    b = 1,
    c = TRUE,
    d = "a",
    e = Sys.Date(),
    f = factor(1)
  )

  out <- summary2(dat)
  nms <- sort(names(out))
  nms_exp <- sort(c(
    "name", "type", "n", "d_na", "n_unique",
    "mean", "p0", "p25", "p50", "p75", "p100"
  ))

  expect_is(out, "data.frame")
  expect_identical(nms, nms_exp)
})
