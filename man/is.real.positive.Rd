\name{is.real.positive}
\alias{is.real.positive}
\title{ check wether numbers of a vector are real positive }
\description{
  This function checks wether the numbers of a vector are positive. It returns TRUE/FALSE. 
  Real positive means, that zero is included as a positive number.
}
\usage{
is.real.positive(x)
}

\arguments{
  \item{x}{ A number or vector to be checked }
}

\author{ Joerg Schlarmann }

\seealso{ 
	\code{\link{is.negative}}
	\code{\link{is.positive}}
}
\examples{

is.real.positive(-3)   # this will return FALSE
is.real.positive(0)    # this will return TRUE

x <- c(0, -1, -2, 3.02, 4, -5.2, 6, -7)
is.real.positive(x)

}

\keyword{ logic }