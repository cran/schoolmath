#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;


// Prüft eine einzelne Zahl
bool isPrime(double x)
{
  // NA
  if (NumericVector::is_na(x))
    return NA_LOGICAL;

  // negative Zahlen
  if (x < 2)
    return false;

  // Dezimalzahlen
  if (std::floor(x) != x)
    return false;

  long long n = static_cast<long long>(x);

  // 2
  if (n == 2)
    return true;

  // gerade Zahlen
  if (n % 2 == 0)
    return false;

  long long limit = static_cast<long long>(std::sqrt((double)n));

  for (long long i = 3; i <= limit; i += 2)
  {
    if (n % i == 0)
      return false;
  }

  return true;
}


// [[Rcpp::export]]
LogicalVector cpp_is_prim(NumericVector x)
{
  int n = x.size();

  LogicalVector out(n);

  for (int i = 0; i < n; i++)
  {
    out[i] = isPrime(x[i]);
  }

  return out;
}
