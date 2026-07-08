#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;


// Hilfsfunktion zur Primzahlprüfung
bool isPrime(long long n)
{
  if (n < 2)
    return false;

  if (n == 2)
    return true;

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
NumericVector cpp_primes(int start, int end, bool live)
{
  if (start > end)
    std::swap(start, end);

  std::vector<double> out;
  bool first = true;

  for (int n = start; n <= end; ++n)
  {
    if (isPrime(n))
    {
      out.push_back(n);

      if (live)
      {
        if (!first)
          Rcpp::Rcout << ", ";

        Rcpp::Rcout << n;
        first = false;
      }
    }
  }

  if (live)
    Rcpp::Rcout << std::endl;

  return Rcpp::wrap(out);
}
