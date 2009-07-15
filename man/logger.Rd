\name{logger.options}
\alias{logger.options}
\alias{scat}
\alias{logger.message}
\alias{logger.debug1}
\alias{logger.debug}
\alias{logger.info1}
\alias{logger.info}
\alias{logger.warn}
\alias{logger.error}
\alias{addLogger}
\alias{getLogger}
\alias{setLogger}
\alias{logger.stdout}
\alias{logger.file}
\alias{usePlots}
\alias{logLevel}
\title{ Manage the logging subsystem }
\description{
  Futile provides a logging system that mimics log4j. It can be used immediately
  with zero configuration by calling the various logger.* functions. By default
  only statements greater than info are printed i.e. all debug statements are
  hidden. These statements will print to stdout, although with a simple
  configuration change, it is possible to modify the ROOT logger to log to a 
  file.

  Gets or sets the current plotting state. When FALSE, applications should not
  plot any data.
}
\usage{
scat(format, ..., use.newline = TRUE)

logger.debug1(msg, ..., logger = 'ROOT')

logger.debug(msg, ..., logger = 'ROOT')

logger.info1(msg, ..., logger = 'ROOT')

logger.info(msg, ..., logger = 'ROOT')

logger.warn(msg, ..., logger = 'ROOT')

logger.error(msg, ..., logger = 'ROOT')

addLogger(name, level, fun, ...)

getLogger(name)

setLogger(name, level = NULL, fun = NULL, ...)

usePlots(new.val = NULL)

logLevel(new.level = NULL)

logger.message(msg, ..., logger, level, label)
}
\arguments{
  \item{format}{ }
  \item{use.newline}{ }
  \item{msg}{ }
  \item{logger}{ }
  \item{name}{ }
  \item{level}{ }
  \item{fun}{ }
  \item{label}{ }
  \item{new.val}{ The value to replace the current value with. If omitted, this
  will display the current value. }
  \item{\dots}{ Additional parameters to either the logger or formatter}
  \item{new.level}{ Obsolete }
}
\details{
  The logging subsystem mimics the well-known log4j logging system in Java. The 
  basic idea is that you have different loggers that control different logging
  behavior based on the logger being used. In log4j, the standard way of doing
  this was by assigning a logger based on the fully qualified class name. Since
  R is more inclined to functional programming, an explicit logger is passed
  to the log function instead. Hence, logging operations can be configured to
  log to stdout, to a file, to a database, etc.

  The other aspect of the log4j paradigm is that an explicit verbosity 
  threshold, or level, is specified that all loggers honor. Hence, if the 
  current log level is WARN, then all INFO and DEBUG statements will be skipped.
  The implication of this method is that logging statements can be defined at
  development time and logging verbosity can be managed at run-time, which
  improves code stability in production systems.

  Using the futile logger subsystem also provides standard information regarding
  the logging operation, including the log level, the time stamp, plus whatever
  message is passed into the function. Note that the sprintf message format is
  integrated into the functions, so message strings can be constructed as a
  format string.

  With this release, two main loggers are provided:
    . logger.stdout - writes to standard out
    . logger.file - writes to a file

  In a parallel cluster with multiple R nodes, logging to separate files can
  improve the readability of the logging process as opposed to redirecting all
  stdouts to the master. Note that for best mileage, it's best that each node
  writes to its own log file.

  As this is the initial release of the futile logging subsystem, a number of
  standard features in log4j are not yet supported. This includes the following
  features:
    . Logger hierarchies
    . Additional loggers (URL, DB, etc.)
    . Configurability of the log message template
    . Configuration file support

  Most of the above features will be added to the library based on the adoption
  of the package by the community.

  Note that due to the single threaded nature of R, excessive logging will 
  degrade performance significantly more than in Java.

  NOTE: Use of logLevel is discouraged as this is deprecated and will be removed
  in a subsequent release.
}
\value{
  For the logging subsystem, no value will be returned, although a logging 
  operation will likely be performed.

  The logger.options function will provide the values of any requested options
  or set them if a named argument is passed into the function.

  For the atomic options, accessing these values will return the value.
}
\author{ Brian Lee Yung Rowe }
\examples{
  # Writes to default ROOT logger
  logger.info("Hello, world")

  # Create some new loggers
  addLogger('a.logger', 'WARN', logger.stdout)
  addLogger('b.logger', 'INFO', logger.file, file='temp.log')

  object <- 1
  logger.debug("This is a \%s", class(object), logger='a.logger')
  logger.warn("This is a \%s", class(object), logger='a.logger')

  # Change configuration of ROOT logger
  setLogger('ROOT', level='DEBUG')
  logger.debug("Hello, world")

  usePlots()
}
\keyword{ data }
