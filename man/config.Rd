\name{configLogger}
\alias{configLogger}
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
  below. All configurations described below use INFO as the default log level
  and simpleLayout as the default layout. These can be overridden by passing
  the appropriate argument to 'configLogger'.

  'console' \cr
  Writes all log messages to the console. Useful for interactive development

  'file' \cr
   file - The path to the log file \cr
  Writes all log messages to the specified file

  'error' \cr
  err.file - The path to the error file \cr
  Writes all log messages to the console while messages at WARN or worse
  are also written to an error file

  c('console','file') \cr
  file - The path to the log file \cr
  Writes all log messages to a file with messages at WARN or worse or also
  routed to the console

  c('error','file') \cr
  log.file - The path to the normal log file \cr
  err.file - The path to the error file \cr
  Writes messages to a general log file and to the console for messages at
  WARN or worse. A secondary error logger will log error messages at WARN or
  worse.

  Once a configuration is called, it is necessary to reset the logger options
  prior to using a different configuration. This is done by calling the
  'futile.options' function reset.options(logger.options).
}
\value{
  Nothing is returned, although the logging system is configured properly. To
  see the results, run loggerAppender(name) to view the appenders for the 
  logger and loggerThreshold(name) to view the logger threshold.
}
\author{ Brian Lee Yung Rowe }
\examples{
  reset.options(logger.options)
  configLogger('console')
  lg <- getLogger()
  lg(DEBUG, "This won't print")
  lg(WARN, "But this will")
}
\keyword{ data }
