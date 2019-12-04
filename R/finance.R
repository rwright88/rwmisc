# TODO
# fix dealing with different sized inputs
# add tests

#' Calculate present value
#'
#' @param pay Payment per period
#' @param rate Rate of interest per period
#' @param n Number of compounding periods
#' @return Present value
#' @export
fin_pv <- function(pay, rate, n) {
  len <- length(pay)
  if (len != length(rate) || len != length(n)) {
    stop("`pay`, `rate`, and `n` must have the same length", call. = FALSE)
  }
  temp <- (1 + rate) ^ n
  z <- (rate == 0)
  r <- !z
  pv <- vector("numeric", len)
  pv[z] <- -pay[z] * n[z]
  pv[r] <- -pay[r] * (temp[r] - 1) / (rate[r] * temp[r])
  pv
}

#' Calculate payment per period
#'
#' @param pv Present value
#' @param rate Rate of interest per period
#' @param n Number of compounding periods
#' @return Payment per period
#' @export
fin_pay <- function(pv, rate, n) {
  len <- length(pv)
  if (len != length(rate) || len != length(n)) {
    stop("`pv`, `rate`, and `n` must have the same length", call. = FALSE)
  }
  temp <- (1 + rate) ^ n
  z <- (rate == 0)
  r <- !z
  pay <- vector("numeric", len)
  pay[z] <- -pv[z] / n[z]
  pay[r] <- -pv[r] * (rate[r] * temp[r]) / (temp[r] - 1)
  pay
}

#' Calculate rate of interest per period
#'
#' @param pv Present value
#' @param pay Payment per period
#' @param n Number of compounding periods
#' @param guess Starting guess for solving rate of interest, default 0.1
#' @param tol Tolerance for solution
#' @param max_iter Maximum iterations to find solution
#' @return Rate of interest per period
#' @export
fin_rate <- function(pv, pay, n, guess = 0.1, tol = 1e-5, max_iter = 100) {
  rn <- guess
  iterator <- 0
  close <- FALSE

  while (iterator < max_iter && close == FALSE) {
    rnp1 <- rn - g_div_gp(rn, n, pay, pv)
    diff <- abs(rnp1 - rn)
    close <- all(diff < tol)
    iterator <- iterator + 1
    rn <- rnp1
  }

  rn
}

# https://github.com/numpy/numpy/blob/master/numpy/lib/financial.py
g_div_gp <- function(r, n, p, x) {
  t1 = (r + 1) ^ n
  t2 = (r + 1) ^ (n - 1)
  (t1 * x + p * (t1 - 1) / r) /
    (n * t2 * x - p * (t1 - 1) / (r ^ 2) + n * p * t2 / r)
}

#' Calculate number of compounding periods
#'
#' @param pv Present value
#' @param pay Payment per period
#' @param rate Rate of interest per period
#' @return Number of compounding periods
#' @export
fin_n <- function(pv, pay, rate) {
  len <- length(pv)
  if (len != length(pay) || len != length(rate)) {
    stop("`pv`, `pay`, and `rate` must have the same length", call. = FALSE)
  }
  z <- (rate == 0)
  r <- !z
  n <- vector("numeric", len)
  n[z] <- -pv[z] / pay[z]
  n[r] <- -log(1 + rate[r] * pv[r] / pay[r]) / log(1 + rate[r])
  n
}
