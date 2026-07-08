#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;


// [[Rcpp::export]]
NumericVector cpp_prime_factor(double x)
{
  // NA
  if (NumericVector::is_na(x))
    return NumericVector::create(NA_REAL);

  // Nur positive ganze Zahlen zulassen
  if (x < 2 || std::floor(x) != x)
    return NumericVector();

  long long n = static_cast<long long>(x);

  std::vector<double> factors;

  // Faktor 2
  while (n % 2 == 0)
  {
    factors.push_back(2);
    n /= 2;
  }

  // ungerade Faktoren
  for (long long i = 3; i * i <= n; i += 2)
  {
    while (n % i == 0)
    {
      factors.push_back(i);
      n /= i;
    }
  }

  // Rest ist Primzahl
  if (n > 1)
    factors.push_back(n);

  return wrap(factors);
}
