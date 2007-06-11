\name{cancel.fraction}
\alias{cancel.fraction}
\title{ cancel fractions to their simplest form}
\description{
 This function cancels a fraction to its simplest form, using greatest common divisor
}
\usage{
cancel.fraction(numerator, denominator)
}
\arguments{
  \item{numerator}{ fraction's numerator}
  \item{denominator}{ fraction's denominator }
}

\author{ Joerg Schlarmann }

\seealso{ 
	\code{\link{gcd}}
}
\examples{
## to cancel fraction 42/56 type:
cancel.fraction(42, 56)

}

\keyword{ math }

