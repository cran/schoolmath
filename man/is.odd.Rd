\name{is.odd}
\alias{is.odd}
\title{ check whether numbers of a vector are odd }
\description{
  This function checks whether the numbers of a vector are odd. It returns TRUE/FALSE
}
\usage{
is.odd(x)
}

\arguments{
  \item{x}{ A number or vector to be checked }
}

\author{ Joerg Schlarmann }

\seealso{
	\code{\link{is.even}}}
\examples{

is.odd(2)  # this will return FALSE
is.odd(3)  # this will return TRUE

x <- c(1,2,3,4,5, 6, 7)
is.odd(x)

}

\keyword{ logic }
