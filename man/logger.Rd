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
\value{
  The current value of the setting.
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
