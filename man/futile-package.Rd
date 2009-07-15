\name{futile-package}
\alias{futile-package}
\alias{futile}
\docType{package}
\title{
  A lightweight package of function utilities
}
\description{
  This package is a collection of functions to expedite development in R and
  provide a few conveniences. The main highlights are the any* functions that
  attempt to consolidate attribute access of lists, vectors, matrices, arrays,
  and other data structures. In addition to these functions, there are functions
  related to logic and time series plus debugging interfaces to ease 
  development.
}
\details{
\tabular{ll}{
Package: \tab futile\cr
Type: \tab Package\cr
Version: \tab {version}\cr
Date: \tab {date}\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
  The anylength and anynames functions consolidate attribute access across many
  data structures providing a bit of convenience via polymorphism. The anytypes
  function provides the classes or types of a data.frame-like object. This is
  useful when parsing data and it is not always clear how values will be 
  parsed.

  Peek falls into the same lineage as head and tail but supports easy-to-read
  views for 2-dimensional data structures. The biggest failing of head an tail
  (to me) is that these functions are only readable when a small number of 
  columns exist. In matrices with many colulmns, the output is difficult to
  read. Peek solves this problem.

  The logLevel and usePlots provide global control over logging and plotting
  across an application. Note that logLevel is now obsolete and has been
  replaced by the futile logging subsystem.
}
\author{
Brian Lee Yung Rowe <r@nurometic.com>
}
\keyword{ package }
\keyword{ attribute }
\keyword{ logic }
\seealso{ \code{\link{inlineapply}}, \code{\link{anylength}}, \code{\link{anylength}}, \code{\link{peek}}, \code{\link{negate}}, \code{\link{mid}}, \code{\link{logger.options}}, \code{\link{options.manager}} }
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

  peek(matrix(rnorm(225), ncol=15))
}
