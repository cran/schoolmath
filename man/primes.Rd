\name{primes}
\alias{primes}
\title{ generate prime-numbers }
\description{
 This function generates prime-numbers, which can be found between a start- 
 and an end-number. 
}
\usage{
primes(start = 12, end = 9999)
}
\arguments{
  \item{start}{ start-number }
  \item{end}{ end-number }
}

\author{ Joerg Schlarmann }

\seealso{ 
	\code{\link{is.prim}}}
\examples{

primes(12,150) # list prime-numbers between 12 and 150

}

\keyword{ math }