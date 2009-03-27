\name{mid}
\alias{mid}
\title{ Gets the mid point value in a sequence }
\description{
  This is similar to the median except it is unordered and operates on both 1 
  and 2 dimensional data. So rather than calculating the median per se, mid is
  retrieving the unordered mid point value within a sequence or tabular data.
}
\usage{
mid(x)
}
\arguments{
  \item{x}{ Any object that supports subsetting }
}
\value{
  The unordered mid point value in a data set.
}
\author{ Brian Lee Yung Rowe }
\seealso{ \code{\link{median}} }
\examples{
  mid(c(1,3,4,2,10))
  median(c(1,3,4,2,10))

  mid(matrix(c(1,3,4,2,5,10), ncol=2))
}
\keyword{ array }
