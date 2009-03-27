\name{inlineapply}
\alias{inlineapply}
\alias{inlineapply.normal}
\alias{inlineapply.col}
\title{ Apply a function on a window over a data structure, but use newly minted
values in nextiteration of the function }
\description{
  This is similar to rollapply in the package zoo, except that the results of
  each iteration of inlineapply are used in the next call.
}
\usage{
inlineapply(data, width, fun, ..., col = NA, include.idx = FALSE)
}
\arguments{
  \item{data}{ The data to apply a function on }
  \item{width}{ The width of the window. Must be less than the anylength of the data }
  \item{fun}{ The function to call on each window of data }
  \item{\dots}{ Additional arguments to fun }
  \item{col}{ Apply only for the given column }
  \item{include.idx}{ Include the index as a parameter to fun }
}
\details{
  Using this method will apply a function over a window of data and make the
  changes inline w.r.t. the original data set. When using the normal inlineapply
  it is expecte that the output set will have the same dimensions as the input
  set and processing explicit columns of data is left to the implementor of the
  passed function. When setting col, only a specific column within a tabular
  data set will be processed. All other columns will be untouched.

  The idea of an inline apply is useful when prior transformations should be
  used in future calculations. This is particularly true when transforming
  time series data, such as calculating the rolling minimum over a window of
  values.
  
  Note that improper uses of inline apply could result in infinite loops. Hence,
  it is important not to modify the size of the window in the result. This 
  requirement may be enforced in a future release.
}
\value{
  A data structure with the same dimensions as data but with modified values
  from the application of the function
}
\author{ Brian Lee Yung Rowe }
\examples{
  inlineapply(c(1,1,2,3,5), 2, sum)
  inlineapply(c(1,2,1,3,4), 2, min)
}
\keyword{ ts }
\keyword{ array }
