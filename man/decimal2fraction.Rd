\name{decimal2fraction}
\alias{decimal2fraction}
\title{ convert a decimal-number into fraction }
\description{
  This function converts a decimal number into a fraction
}
\usage{
decimal2fraction(decimal, period = 0)
}
\arguments{
  \item{decimal}{ the decimal number to be converted, given without an repeating ending }
  \item{period}{ if the decimal places have an repeating ending (period), set the period here. See examples. }
}

\author{ Joerg Schlarmann }

\examples{
## converting 23.4323
decimal2fraction(23.4323)

## converting a number with decimal period, e.g. 12.12344444444444444444
decimal2fraction(12.123, 4)

}

\keyword{ math }

