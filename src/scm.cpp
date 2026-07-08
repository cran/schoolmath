#include <Rcpp.h>

using namespace Rcpp;


// [[Rcpp::export]]
NumericVector cpp_scm(double x, double y)
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


  if (a == 0 || b == 0)
  {
    return NumericVector::create(0);
  }


  // ggT berechnen
  long long aa = a;
  long long bb = b;

  while (bb != 0)
  {
    long long temp = bb;
    bb = aa % bb;
    aa = temp;
  }


  long long result = (a / aa) * b;


  return NumericVector::create(
    static_cast<double>(result)
  );
}
