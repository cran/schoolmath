\name{is.positive}
\alias{is.positive}
\title{ check whether numbers of a vector are positive }
\description{
  This function checks whether the numbers of a vector are positive. It returns TRUE/FALSE.
}
\usage{
is.positive(x)
}

\arguments{
  \item{x}{ A number or vector to be checked }
}

\author{ Joerg Schlarmann }

\seealso{
	\code{\link{is.negative}}
	\code{\link{is.real.positive}}
}
\examples{

is.positive(-3)   # this will return FALSE
is.positive(2)  # this will return TRUE

x <- c(-1, -2, 3.02, 4, -5.2, 6, -7)
is.positive(x)

}

\keyword{ logic }
