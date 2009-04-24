\name{peek}
\alias{peek}
\alias{mid}
\title{ Peeks inside a matrix or vector }
\description{
  Peek is a simple utility to conveniently look at a portion of a matrix. This
  is similar to head and tail but provides a 2-dimensional slice instead of a
  complete row.

  This is similar to the median except it is unordered and operates on both 1 
  and 2 dimensional data. So rather than calculating the median per se, mid is
  retrieving the unordered mid point value within a sequence or tabular data.
}
\usage{
mid(x)
peek(x, upper = 5, lower = 1)
}
\arguments{
  \item{x}{ Any object that supports subsetting }
  \item{upper}{ The upper bound in the subsetting }
  \item{lower}{ The lower bound in the subsetting }
}
\value{
  The unordered mid point value in a data set.

  Peek returns a subset of the original matrix, data.frame, etc.
}
\author{ Brian Lee Yung Rowe }
\seealso{ \code{\link{median}} }
\examples{
  mid(c(1,3,4,2,10))
  median(c(1,3,4,2,10))

  mid(matrix(c(1,3,4,2,5,10), ncol=2))

  peek(matrix(c(1,3,4,2, 5,10,11,2, 3,42,8,22, 23,15,3,8), ncol=4), 2)
}
\keyword{ array }
