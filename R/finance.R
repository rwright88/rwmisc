# TODO
# arg checking
# fin_rate() is unfinished
# https://github.com/numpy/numpy/blob/master/numpy/lib/financial.py

#' Calculate payment of amortization
#'
#' @param principal Principal starting amount
#' @param rate Interest rate per period
#' @param n Number of periods
#' @return Numeric vector of payment per period
#' @export
fin_pay <- function(principal, rate, n) {
  if (rate < 0) {
    stop("`rate` must be greater than or equal to 0.", call. = FALSE)
  } else if (rate == 0) {
    return(principal / n)
  }
  if (n < 1) {
    stop("`n` must be greater than or equal to 1.", call. = FALSE)
  }

  pay <- principal * (rate * (1 + rate) ^ n) / ((1 + rate) ^ n - 1)
  pay
}

#' Calculate estimated interest rate of amortization
#'
#' @param principal Principal starting amount, single value
#' @param payment Payment per period, single value
#' @param n Number of periods, single value
#' @param tol Tolerance, single value
#' @return Single value numeric vector of interest rate per period
#' @export
fin_rate <- function(principal, payment, n, tol = 1e-5) {
  stopifnot(
    length(principal) == 1,
    length(payment) == 1,
    length(n) == 1,
    length(tol) == 1
  )
  if (payment * n < principal) {
    stop("`payment` * `n` must be greater than or equal to `principal`.", call. = FALSE)
  }

  rate_lower <- 0
  rate_upper <- (payment * n) / principal - 1
  rate_guess <- min(0.05, rate_upper / 2)

  pay_calc <- fin_pay(principal, rate_guess, n)
  pay_diff <- (pay_calc - payment) / payment

  while (abs(pay_diff) > tol) {
    if (pay_diff < 0) {
      rate_lower <- rate_guess
      rate_guess <- (rate_guess + rate_upper) / 2
    } else if (pay_diff > 0) {
      rate_upper <- rate_guess
      rate_guess <- (rate_guess + rate_lower) / 2
    }
    pay_calc <- fin_pay(principal, rate_guess, n)
    pay_diff <- (pay_calc - payment) / payment
  }

  rate_guess
}
