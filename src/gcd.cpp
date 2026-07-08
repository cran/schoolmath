#include <Rcpp.h>

using namespace Rcpp;


// [[Rcpp::export]]
NumericVector cpp_gcd(double x, double y)
{
  if (NumericVector::is_na(x) ||
      NumericVector::is_na(y))
  {
    return NumericVector::create(NA_REAL);
  }


  if (std::floor(x) != x ||
      std::floor(y) != y)
  {
    stop("x and y must be whole numbers.");
  }


  long long a = static_cast<long long>(std::abs(x));
  long long b = static_cast<long long>(std::abs(y));


  while (b != 0)
  {
    long long temp = b;
    b = a % b;
    a = temp;
  }


  return NumericVector::create(
    static_cast<double>(a)
  );
}
