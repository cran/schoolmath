\name{is.prim}
\alias{is.prim}
\title{ check whether a vector contains prime-numbers }
\description{
 This function checks, whether a vector contains prime-numbers. It returns TRUE or FALSE
}
\usage{
is.prim(y)
}
\arguments{
  \item{y}{ a number or vector to be checked }
}

\author{ Joerg Schlarmann }

\seealso{
	\code{\link{primes}}}
\examples{

is.prim(8)  # this will return FALSE
is.prim(11) # this will return TRUE

x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
is.prim(x)

}

\keyword{ logic }

