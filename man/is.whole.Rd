\name{is.whole}
\alias{is.whole}
\title{ check whether a vector contains numbers with decimal places}
\description{
 This function checks, whether a vector contains whole numbers without decimal places.
 It returns TRUE or FALSE
}
\usage{
is.whole(x)
}
\arguments{
  \item{x}{ a number or vector to be checked }
}

\author{ Joerg Schlarmann }

\seealso{
	\code{\link{is.decimal}}}
\examples{

is.whole(3.12)  # this will return FALSE
is.whole(2)     # this will return TRUE

x <- c(1, 2, 3, 4, 5.5, 6.03, 23.07)
is.whole(x)

}

\keyword{ logic }

