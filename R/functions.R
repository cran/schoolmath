####################################################
# In order to let roxygen2() create all man pages, #
# you must run the following command:              #
#       roxygen2::roxygenise()                     #
####################################################



#' cancel a fraction to the smallest numbers
#'
#' @param numerator the fraction's numerator
#' @param denominator the fraction's denominator
#' #'
#' @return An integer vector of length 2 containing the reduced numerator and denominator.
#'
#' @examples
#' cancel.fraction(40,15)
#' cancel.fraction(42, 56)
#'
#' @export
#---------------------------------------------------------------
cancel.fraction <- function(numerator, denominator) {
  cpp_cancel_fraction(numerator, denominator)
}
#---------------------------------------------------------------






#---------------------------------------------------------------

#' Check whether a vector contains prime numbers
#'
#' @param x The number or vector to check.
#'
#' @return A logical vector indicating whether values are prime.
#'
#' @examples
#' is.prim(8)
#' is.prim(11)
#'
#' x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
#' is.prim(x)
#'
#' @export
is.prim <- function(x) {
  cpp_is_prim(x)
}






#' generate prime-numbers in a range from START to END
#'
#' @param start the number to start from
#' @param end the number to end
#' @param live should primes additionally be printed out to stdout when found? (default is FALSE)
#'
#' @return a vector of prime numbers
#'
#' @examples
#' primes(12,150) # list prime-numbers between 12 and 150
#'
#' @export
#---------------------------------------------------------------
primes <- function(start=1, end=9999, live=FALSE) {
  cpp_primes(start, end, live)
}


#---------------------------------------------------------------


#' This function calculates the prime-factors of a number
#'
#' @param n the number to be checked
#' #'
#' @return a vector with the prime factors
#'
#' @examples
#' prime.factor(21)
#' prime.factor(100)
#'
#' @export
#---------------------------------------------------------------
prime.factor <- function(n) {
  cpp_prime_factor(n)
}
#---------------------------------------------------------------














#' convert a decimal-number into fraction
#'
#' @param decimal the decimal number to be converted, given without an repeating ending
#' @param period if the decimal places have an repeating ending (period), set the period here. See examples.
#' #'
#' @return An integer vector of length 2 containing the numerator and denominator
#'
#' @examples
#' ## converting 23.4323
#' decimal2fraction(23.4323)
#'
#' ## converting a number with decimal period, e.g. 12.12344444444444444444
#' decimal2fraction(12.123, 4)
#'
#' @export
#---------------------------------------------------------------
decimal2fraction <- function(decimal, period = 0) {
  decimal <- as.character(decimal)
  cpp_decimal2fraction(decimal, period)
}
#---------------------------------------------------------------




#' Greatest common divisor of two numbers
#'
#' @param x first number
#' @param y second number
#' #'
#' @return numeric greatest common divisor
#'
#' @examples
#' gcd(42, 56)
#'
#' @export
#---------------------------------------------------------------
gcd <- function(x, y) {
  cpp_gcd(x, y)
}
#---------------------------------------------------------------




#' calculating the smallest common multiple of two numbers
#'
#' @param x first number
#' @param y second number
#' #'
#' @return numeric least common multiple
#'
#' @examples
#' scm(3528, 3780)
#'
#' @export
#---------------------------------------------------------------
scm <- function(x, y) {
  cpp_scm(x, y)
}
#---------------------------------------------------------------


