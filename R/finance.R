# TODO: fix index subset when rate is 0 or not
# TODO: tests
# https://github.com/numpy/numpy/blob/master/numpy/lib/financial.py

#' Calculate present value
#'
#' @param pay Payment per period
#' @param rate Rate of interest per period
#' @param n Number of compounding periods
#' @return Present value
#' @export
fin_pv <- function(pay, rate, n) {
  len <- max(length(pay), length(rate), length(n))
  pv <- vector("numeric", len)
  temp <- (1 + rate) ^ n
  pv[rate == 0] <- -pay * n
  pv[rate != 0] <- -pay * (temp - 1) / (rate * temp)
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
  len <- max(length(pv), length(rate), length(n))
  pay <- vector("numeric", len)
  temp <- (1 + rate) ^ n
  pay[rate == 0] <- -pv / n
  pay[rate != 0] <- -pv * (rate * temp) / (temp - 1)
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
  len <- max(length(pv), length(pay), length(rate))
  n <- vector("numeric", len)
  n[rate == 0] <- -pv / pay
  n[rate != 0] <- log(1 - rate * pv / pay) / log(1 + rate)
  n
}
