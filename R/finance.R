# TODO
# fin_amort_rate() is unfinished

#' Calculate payment of amortization
#'
#' @param principal principal starting amount
#' @param rate interest rate per period
#' @param n number of periods
#' @return numeric vector of payment per period
#' @export
fin_amort_payment <- function(principal, rate, n) {
  principal * (rate * (1 + rate) ^ n) / ((1 + rate) ^ n - 1)
}

#' Calculate interest rate of amortization
#'
#' @param principal principal starting amount
#' @param payment payment per period
#' @param n number of periods
#' @return numeric vector of interest rate per period
#' @export
fin_amort_rate <- function(principal, payment, n) {
  rate_lower <- 0
  rate_upper <- (payment * n) / principal - 1
  rate_guess <- min(0.05, rate_upper / 2)

  pay_calc <- fin_amort_payment(principal, rate_guess, n)
  pay_diff <- (pay_calc - payment) / payment

  while (abs(pay_diff) > 1e-5) {
    if (pay_diff < 0) {
      rate_lower <- rate_guess
      rate_guess <- (rate_guess + rate_upper) / 2
    } else if (pay_diff > 0) {
      rate_upper <- rate_guess
      rate_guess <- (rate_guess + rate_lower) / 2
    }
    pay_calc <- calc_payment(principal, rate_guess, n)
    pay_diff <- (pay_calc - payment) / payment
  }

  rate_guess
}
