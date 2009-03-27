\name{anylength}
\alias{anylength}
\title{ Get the generic length of an object }
\description{
  This function consolidates size dimensions for one and two dimensional data
  structures. The idea is that many operations require knowing either how long
  a vector is or how many rows are in a matrix. So rather than switching 
  between length and nrow, anylength provides the appropriate polymorphism to 
  return the proper value.
}
\usage{
anylength(data)
}
\arguments{
  \item{data}{ Any object that recognizes length or nrow }
}
\details{
  When working with libraries, it is easy to forget the return type of a
  function, particularly when there are a lot of switches between vectors,
  matrices, and other data structures. This function along with its
  \code{\link{anynames}} counterpart provides a single interface for accessing
  this information across objects.

  The core assumption is that in most cases length is semantically synonomous
  with nrow such that the number of columns in two-dimensional structures is
  less consequential than the number of rows. This is particularly true of
  time-based objects, such as zoo or xts where the number of observations is
  equal to the number of rows in the structure.
}
\value{
  The length or nrow of the object
}
\author{ Brian Lee Yung Rowe }
\seealso{ \code{\link{anynames}} }
\examples{
  m <- matrix(c(1,2,3,4,5,6), ncol=2)
  anylength(m)

  v <- c(1,2,3,4,5)
  anylength(v)
}
\keyword{ attribute }
