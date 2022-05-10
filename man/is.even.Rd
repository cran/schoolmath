\name{is.even}
\alias{is.even}
\title{ check whether numbers of a vector are even }
\description{
  This function checks whether the numbers of a vector are even. It returns TRUE/FALSE
}
\usage{
is.even(x)
}

\arguments{
  \item{x}{ A number or vector to be checked }
}

\author{ Joerg Schlarmann }

\seealso{
	\code{\link{is.odd}}}
\examples{

is.even(3)  # this will return FALSE
is.even(2)  # this will return TRUE

x <- c(1,2,3,4,5, 6, 7)
is.even(x)

}

\keyword{ logic }

