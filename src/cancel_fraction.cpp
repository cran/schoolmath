#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;


// Euklidischer Algorithmus
long long gcd(long long a, long long b)
{
  a = std::llabs(a);
  b = std::llabs(b);

  while (b != 0)
  {
    long long t = b;
    b = a % b;
    a = t;
  }

  return a;
}


// [[Rcpp::export]]
IntegerVector cpp_cancel_fraction(double numerator,
                                  double denominator)
{
  // NA
  if (NumericVector::is_na(numerator) ||
      NumericVector::is_na(denominator))
    return IntegerVector::create(NA_INTEGER, NA_INTEGER);

  // Nur ganze Zahlen zulassen
  if (std::floor(numerator) != numerator ||
      std::floor(denominator) != denominator)
    stop("Numerator and denominator must be whole numbers.");

  if (denominator == 0)
    stop("Denominator must not be zero.");

  long long num = static_cast<long long>(numerator);
  long long den = static_cast<long long>(denominator);

  long long d = gcd(num, den);

  num /= d;
  den /= d;

  // Minuszeichen immer im Zähler
  if (den < 0)
  {
    num = -num;
    den = -den;
  }

  return IntegerVector::create(
    Named("numerator")   = static_cast<int>(num),
    Named("denominator") = static_cast<int>(den)
  );
}
