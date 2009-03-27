# Get a HLC matrix from a quantmod OHLC (or for any matter any other matrix
# that has superfluous columns)
hlc <- function(market)
{
  require(xts, quietly=TRUE)
  cbind(Hi(market), Lo(market), Cl(market))
}


