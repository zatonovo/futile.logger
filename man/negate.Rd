\name{negate}
\alias{negate}
\title{ Selectively negate columns in tabular data }
\description{
  Negate is a convenience method to selectively negate columns of logical values
  in tabular data.
}
\usage{
negate(x, slots)
}
\arguments{
  \item{x}{ Any tabular or 2-dimensional data set }
  \item{slots}{ A vector or list of column indices }
}
\details{
  When working with logical data sets, sometimes it is necessary to negate 
  certain sets of data. If this data appears in a tabular form, where each
  column is its own set, there are times when multiple columns in the table
  need to be negated. This function provides a convenient way to selectively
  negate those columns.
}
\value{
  The data with the given columns of data negated.
}
\author{ Brian Lee Yung Rowe }
\examples{
  m <- matrix(c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE), ncol=3)
  negate(m, slots=c(2,3))
}
\keyword{ logic }
\keyword{ array }
