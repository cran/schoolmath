#include <Rcpp.h>
#include <string>

using namespace Rcpp;


// ggT
long long gcd2(long long a, long long b)
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
IntegerVector cpp_decimal2fraction(std::string decimal, int period = -1)
{
  bool negative = false;

  if (decimal[0] == '-')
  {
    negative = true;
    decimal.erase(0, 1);
  }


  auto dot = decimal.find('.');


  std::string integer_part;
  std::string decimal_part;


  if (dot == std::string::npos)
  {
    integer_part = decimal;
    decimal_part = "";
  }
  else
  {
    integer_part = decimal.substr(0, dot);
    decimal_part = decimal.substr(dot + 1);
  }


  long long numerator;
  long long denominator;


  // keine Periode
  if (period < 0)
  {
    long long factor = 1;

    for (size_t i = 0; i < decimal_part.size(); i++)
      factor *= 10;


    numerator =
      std::stoll(integer_part) * factor;

    if (!decimal_part.empty())
      numerator += std::stoll(decimal_part);

    denominator = factor;
  }


  // eine periodische Ziffer
  else
  {
    std::string repeated = std::to_string(period);


    long long non_period =
      std::stoll(decimal_part);


    long long all =
      std::stoll(integer_part + decimal_part + repeated);


    long long before =
      std::stoll(integer_part + decimal_part);


    long long factor_non =
      1;

    for (size_t i = 0; i < decimal_part.size(); i++)
      factor_non *= 10;


    numerator =
      all - before;


    denominator =
      factor_non * 9;
  }


  long long divisor = gcd2(numerator, denominator);

  numerator /= divisor;
  denominator /= divisor;


  if (negative)
    numerator = -numerator;


  return IntegerVector::create(
    Named("numerator") = numerator,
    Named("denominator") = denominator
  );
}
