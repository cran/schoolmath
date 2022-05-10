\name{is.negative}
\alias{is.negative}
\title{ check whether numbers of a vector are negative }
\description{
  This function checks whether the numbers of a vector are negative. It returns TRUE/FALSE
}
\usage{
is.negative(x)
}

\arguments{
  \item{x}{ A number or vector to be checked }
}

\author{ Joerg Schlarmann }

\seealso{
	\code{\link{is.positive}}
	\code{\link{is.real.positive}}
}
\examples{

is.negative(3)  # this will return FALSE
is.negative(-2)  # this will return TRUE

x <- c(-1, -2, 3.02, 4, -5.2, 6, -7)
is.negative(x)

}

\keyword{ logic }
