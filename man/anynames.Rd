\name{anynames}
\alias{anynames}
\alias{anynames<-}
\title{ Get the useful names of a data structure }
\description{
  This function consolidataes data sets within lists and two dimensional data.
  Like \code{\link{anylength}} the idea is to unify the accessors for various
  data structures with a single common interface.
}
\usage{
anynames(data)
}
\arguments{
  \item{data}{ Any object the recognizes names or colnames }
}
\details{
  Depending on the type of structure utilized in code, one needs to call either
  names or colnames to get information related to the data sets within the 
  structure. The use of two separate functions can cause errors and slows
  development time as data structures passed from intermediate functions may
  change over time, resulting in a broken interface.

  By providing a thin layer over underlying accessors, this function attempts to
  expedite development and add a bit of polymorphism to the semantics of names.
  The explicit assumption is that data sets in two dimensional structures are
  organized by column, as this is compatible with time-series objects such as
  zoo and xts.
}
\value{
  The names or colnames of an object.
}
\author{ Brian Lee Yung Rowe }
\seealso{ \code{\link{anylength}} }
\examples{
  m <- matrix(c(1,2,3,4,5,6), ncol=2)
  anynames(m) <- c('a','b')
  anynames(m)

  v <- c(a=1,b=2,c=3,d=4,e=5)
  anynames(v)

  l <- list(a=1,b=2,c=3,d=4,e=5)
  anynames(l)
}
\keyword{ attribute  }
