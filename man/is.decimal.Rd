\name{is.decimal}
\alias{is.decimal}
\title{ check whether a vector contains numbers with decimal places}
\description{
 This function checks, whether a vector contains numbers with decimal places. It returns TRUE or FALSE
}
\usage{
is.decimal(x)
}
\arguments{
  \item{x}{ a number or vector to be checked }
}

\author{ Joerg Schlarmann }

\seealso{
	\code{\link{is.whole}}}
\examples{

is.decimal(3)    # this will return FALSE
is.decimal(2.01) # this will return TRUE

x <- c(1,2,3,4,5.5, 6.03, 23.07)
is.decimal(x)

}

\keyword{ logic }

