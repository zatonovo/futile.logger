\name{futile-package}
\alias{futile-package}
\alias{futile}
\docType{package}
\title{
  A lightweight package of function utilities
}
\description{
  This package is a collection of functions to expedite development in R and
  provie a few conveniences. The main highlights are the any* functions that
  attempt to consolidate attribute access of lists, vectors, matrices, arrays,
  and other data structures. In addition to these functions, there are functions
  related to logic and time series plus debugging interfaces to ease 
  development.
}
\details{
\tabular{ll}{
Package: \tab futile\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2009-03-22\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
  The anylength and anynames functions consolidate attribute access across many
  data structures providing a bit of convenience via polymorphism.

  The logLevel and usePlots provide global control over logging and plotting
  across an application.
}
\author{
Brian Lee Yung Rowe <r@nurometic.com>
}
\keyword{ package }
\keyword{ attribute }
\keyword{ logic }
\seealso{ \code{\link{inlineapply}}, \code{\link{anylength}}, \code{\link{anylength}}, \code{\link{negate}}, \code{\link{mid}}, \code{\link{logLevel}} }
\examples{
  inlineapply(c(1,1,2,3,5), 2, sum)
  inlineapply(c(1,2,1,3,4), 2, min)

  m <- matrix(c(1,2,3,4,5,6), ncol=2)
  anylength(m)

  v <- c(1,2,3,4,5)
  anylength(v)

  m <- matrix(c(1,2,3,4,5,6), ncol=2)
  anynames(m) <- c('d','e')
  anynames(m)

  v <- c(a=1,b=2,c=3,d=4,e=5)
  anynames(v)

  l <- list(a=1,b=2,c=3,d=4,e=5)
  anynames(l)
}
