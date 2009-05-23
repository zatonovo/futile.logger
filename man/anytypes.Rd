\name{anytypes}
\alias{anytypes}
\title{ Get the useful types of a data structure }
\description{
  This function consolidataes data sets within lists and two dimensional data.
  Like \code{\link{anylength}} the idea is to unify the accessors for various
  data structures with a single common interface.
}
\usage{
anytypes(data, fun = class)
}
\arguments{
  \item{data}{ Any object the recognizes names or colnames }
  \item{fun}{ The function to use to get the types. Defaults to class, although
    type or mode, etc. could be used }
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
  The types or classes of a data structure
}
\author{ Brian Lee Yung Rowe }
\seealso{ \code{\link{anynames}} }
\examples{
  d <- data.frame(ints=c(1,2,3), chars=c('a','b','c'), nums=c(.1,.2,.3))
  anytypes(d)
}
\keyword{ attribute  }
