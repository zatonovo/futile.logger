\name{configLogger}
\alias{configLogger}
\alias{configLogger.default}
\alias{configLogger.source}
\alias{configLogger.console}
\alias{configLogger.error}
\alias{configLogger.file}
\alias{configLogger.fileAndConsole}
% The below functions are deprecated
\alias{configAsConsole}
\alias{configAsFile}
\alias{configAsConsoleAndFile}
\alias{configAsFileAndConsole}
\alias{configAsErrorAndFile}
\alias{configAsFileAndError}
\alias{configAsError}
\title{ Create and manage loggers within 'futile.logger' }
\description{
  A number of pre-defined loggers are provided within 'futile.logger'. The
  logger configurations illustrate basic usage in addition to more advanced
  features such as logger inheritance, multiple appenders for a logger, and
  appender threshold overrides.
}
\usage{
configLogger(..., threshold = INFO, defaultLayout = simpleLayout)
}
\arguments{
  \item{threshold}{ The logger threshold }
  \item{defaultLayout}{ The default layout used in the appenders }
  \item{\dots}{ Additional arguments to specific configurations }
}
\details{
  As of version 1.1.0, the configuration system leverages futile.paradigm
  meaning that only one function, configLogger is defined. This makes 
  configuration simpler and less error prone by exposing a single function
  that dispatches to different functions depending on the arguments provided.
  
  configLogger()
  ROOT logger defaults to console with INFO threshold

  configLogger(config.file)
  Reads configuration R file and calls function 'customLogger' with INFO
  threshold

  configLogger(threshold, defaultLayout)
  ROOT logger prints to console with threshold threshold and layout 
  defaultLayout

  configLogger(file, threshold, defaultLayout)
  ROOT logger prints to file with threshold threshold and layout defaultLayout

  configLogger(error.file, threshold, defaultLayout)
  ROOT logger prints to error.file with threshold WARN and layout defaultLayout
  while also printing to console with threshold threshold. This is mostly for
  illustrative purposes
  Note: To use this configuration, error.file must be a named argument

  configLogger(file, file.threshold, threshold, defaultLayout)
  ROOT logger prints to console with threshold threshold and to file with
  file.threshold. Both appenders use the defaultLayout

}
\value{
  Nothing is returned, although the logging system is configured properly. To
  see the results, run loggerAppender(name) to view the appenders for the 
  logger and loggerThreshold(name) to view the logger threshold.
}
\author{ Brian Lee Yung Rowe }
\examples{
  reset.options(logger.options)
  configLogger()
  lg <- getLogger()
  lg(DEBUG, "This won't print")
  lg(WARN, "But this will")
}
\keyword{ data }
