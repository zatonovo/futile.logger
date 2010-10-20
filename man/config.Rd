\name{config_logger}
\alias{config_logger}
\alias{config_logger.default}
\alias{config_logger.source}
\alias{config_logger.console}
\alias{config_logger.error}
\alias{config_logger.file}
\alias{config_logger.fileAndConsole}
\alias{configLogger}
\title{ Create and manage loggers within 'futile.logger' }
\description{
  A number of pre-defined loggers are provided within 'futile.logger'. The
  logger configurations illustrate basic usage in addition to more advanced
  features such as logger inheritance, multiple appenders for a logger, and
  appender threshold overrides.
}
\usage{
config_logger(..., threshold = INFO, defaultLayout = simpleLayout)

configLogger(..., threshold = INFO, defaultLayout = simpleLayout)
}
\arguments{
  \item{threshold}{ The logger threshold }
  \item{defaultLayout}{ The default layout used in the appenders }
  \item{\dots}{ Additional arguments to specific configurations }
}
\details{
  As of version 1.1.0, the configuration system leverages futile.paradigm
  meaning that only one function, config_logger is defined. This makes 
  configuration simpler and less error prone by exposing a single function
  that dispatches to different functions depending on the arguments provided.
  
  config_logger()
  ROOT logger defaults to console with INFO threshold

  config_logger(config.file)
  Reads configuration R file and calls function 'customLogger' with INFO
  threshold

  config_logger(threshold, defaultLayout)
  ROOT logger prints to console with threshold threshold and layout 
  defaultLayout

  config_logger(file, threshold, defaultLayout)
  ROOT logger prints to file with threshold threshold and layout defaultLayout

  config_logger(error.file, threshold, defaultLayout)
  ROOT logger prints to error.file with threshold WARN and layout defaultLayout
  while also printing to console with threshold threshold. This is mostly for
  illustrative purposes
  Note: To use this configuration, error.file must be a named argument

  config_logger(file, file.threshold, threshold, defaultLayout)
  ROOT logger prints to console with threshold threshold and to file with
  file.threshold. Both appenders use the defaultLayout

}
\value{
  Nothing is returned, although the logging system is configured properly. To
  see the results, run loggerAppender(name) to view the appenders for the 
  logger and loggerThreshold(name) to view the logger threshold.
}
\note{ The function 'configLogger' is deprecated. Use 'config_logger' instead. }
\author{ Brian Lee Yung Rowe }
\examples{
  reset.options(logger.options)
  config_logger()
  lg <- getLogger()
  lg(DEBUG, "This won't print")
  lg(WARN, "But this will")
}
\keyword{ data }
