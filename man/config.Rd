\name{configLogger}
\alias{configLogger}
\title{ Create and manage loggers within 'futile.logger' }
\description{
  A number of pre-defined loggers are provided within 'futile.logger'. The
  logger configurations illustrate basic usage in addition to more advanced
  features such as logger inheritance, multiple appenders for a logger, and
  appender threshold overrides.
}
\usage{
configLogger(type, ..., threshold = INFO, defaultLayout = simpleLayout)
}
\arguments{
  \item{type}{ The type of logger to configure }
  \item{threshold}{ The logger threshold }
  \item{defaultLayout}{ The default layout used in the appenders }
  \item{\dots}{ Additional arguments to specific configurations }
}
\details{
  This function is the entry point for configuring 'futile.logger' with built-in
  configurations. The available types and associated arguments are described
  below.

\tabular{ll}{
'console'\cr
\tab 

'file'\cr
\tab file - The path to the log file

'error'\cr
\tab err.file - The path to the error file

c('console','file')\cr
\tab file - The path to the log file

c('error','file')\cr
\tab log.file - The path to the normal log file
\tab err.file - The path to the error file

}
}
\value{
}
\author{ Brian Lee Yung Rowe }
\examples{
}
\keyword{ data }
