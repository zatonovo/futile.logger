\name{peek}
\alias{peek}
\alias{a<-}
\alias{z<-}
\alias{mid}
\title{ Peeks inside a matrix or vector }
\description{
  Peek is a simple utility to conveniently look at a portion of a matrix. This
  is similar to head and tail but provides a 2-dimensional slice instead of a
  complete row.

  The a and z functions are conveniences for prepending and appending values to
  a vector. These are currently stubs but intended to support additional types
  later on.

  Mid is similar to the median except it is unordered and operates on both 1 
  and 2 dimensional data. So rather than calculating the median per se, mid is
  retrieving the unordered mid point value within a sequence or tabular data.
}
\usage{
peek(x, upper = 5, lower = 1)
a(x) <- value
z(x) <- value
mid(x)
}
\arguments{
  \item{x}{ Any object that supports subsetting }
  \item{upper}{ The upper bound in the subsetting }
  \item{lower}{ The lower bound in the subsetting }
  \item{value}{ A value to add to a vector }
}
\value{
  Peek returns a subset of the original matrix, data.frame, etc.

  The unordered mid point value in a data set.
}
\author{ Brian Lee Yung Rowe }
\seealso{ \code{\link{median}} }
\examples{
  peek(matrix(c(1,3,4,2, 5,10,11,2, 3,42,8,22, 23,15,3,8), ncol=4), 2)

  x <- c(2,3,4)
  a(x) <- 1
  z(x) <- 5

  mid(c(1,3,4,2,10))
  median(c(1,3,4,2,10))

  mid(matrix(c(1,3,4,2,5,10), ncol=2))
}
\keyword{ array }
