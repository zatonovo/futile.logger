\name{hlc}
\alias{hlc}
\title{ Get a matrix containing the Hi-Low-Close of a zoo/xts object }
\description{
  This is a convenience method to convert an xts object containing OHLC time 
  series data to a TTR-compatible matrix.

}
\usage{
hlc(market)
}
\arguments{
  \item{market}{ An xts object created by calling (quantmod) getSymbols. }
}
\value{
  An object containing just the Hi, Low, Close values
}
\references{ Quantmod, TTR }
\author{ Brian Lee Yung Rowe }
\note{
  See quantmod and TTR for more information on how data is constructed and used.
}
\examples{
  require(quantmod)
  getSymbols('IBM')
  ibm <- hlc(IBM)
}
\keyword{ ts }
\keyword{ array }
