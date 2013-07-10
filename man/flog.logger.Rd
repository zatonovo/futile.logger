\name{flog.logger}
\alias{flog.carp}
\alias{flog.debug}
\alias{flog.error}
\alias{flog.fatal}
\alias{flog.info}
\alias{flog.logger}
\alias{flog.remove}
\alias{flog.threshold}
\alias{flog.trace}
\alias{flog.warn}
\alias{ftry}
\title{Manage appenders within the 'futile.logger' sub-system}
\arguments{
  \item{msg}{The message to log}

  \item{name}{The logger name to use}

  \item{capture}{Capture print output of variables instead
  of interpolate}

  \item{\dots}{Optional arguments to populate the format
  string}

  \item{expr}{An expression to evaluate}

  \item{finally}{An optional expression to evaluate at the
  end}
}
\value{
  Most of these functions exist for their side effects, so
  there are few useful return values.
}
\description{
  Provides functions for writing log messages and managing
  loggers. Typically only the
  flog.[trace|debug|info|warn|error|fatal] functions need
  to be used in conjunction with flog.threshold to
  interactively change the log level.
}
\details{
  These functions represent the high level interface to
  futile.logger.

  The primary use case for futile.logger is to write out
  log messages. There are log writers associated with all
  the predefined log levels: TRACE, DEBUG, INFO, WARN,
  ERROR, FATAL. Log messages will only be written if the
  log level is equal to or more urgent than the current
  threshold. By default the ROOT logger is set to INFO.

  > flog.debug("This won't print")

  > flog.info("But this \%s", 'will')

  > flog.warn("As will \%s", 'this')

  Typically, the built in log level constants are used in
  the call, which conform to the log4j levels (from least
  severe to most severe): TRACE, DEBUG, INFO, WARN, ERROR,
  FATAL. It is not a strict requirement to use these
  constants (any numeric value will work), though most
  users should find this level of granularity sufficient.

  Loggers are hierarchical in the sense that any requested
  logger that is undefined will fall back to its most
  immediate defined parent logger. The absolute parent is
  ROOT, which is guaranteed to be defined for the system
  and cannot be deleted. This means that you can specify a
  new logger directly.

  > flog.info("This will fall back to 'my', then 'ROOT'",
  name='my.logger')

  You can also change the threshold or any other setting
  associated with a logger. This will create an explicit
  logger where any unspecified options are copied from the
  parent logger.

  > flog.appender(appender.file("foo.log"), name='my')

  > flog.threshold(ERROR, name='my.logger')

  > flog.info("This won't print", name='my.logger')

  > flog.error("This %s print to a file", 'will',
  name='my.logger')

  If you define a logger that you later want to remove, use
  flog.remove.

  The option 'capture' allows you to print out more
  complicated data structures without a lot of ceremony.
  This variant doesn't accept format strings and instead
  appends the value to the next line of output. Consider

  > m <- matrix(rnorm(12), nrow=3)

  > flog.info("Matrix:",m, capture=TRUE)

  which preserves the formatting, whereas using
  capture=FALSE will have a cluttered output due to
  recycling.
}
\examples{
flog.threshold(DEBUG)
flog.debug("This debug message will print")

flog.threshold(WARN)
flog.debug("This one won't")

m <- matrix(rnorm(12), nrow=3)
flog.info("Matrix:",m, capture=TRUE)

ftry(log(-1))
}
\author{
  Brian Lee Yung Rowe
}
\keyword{data}

