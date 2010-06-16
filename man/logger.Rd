\name{addLogger}
\alias{addLogger}
\alias{getLogger}
\alias{loggerAppender}
\alias{loggerThreshold}
\title{ Manage appenders within the 'futile.logger' sub-system }
\description{
  Provides functions for managing loggers. The 'getLogger' function is the
  primary entry point into the package once the configuration is complete.
}
\usage{
addLogger(name, threshold, appender, layout)
getLogger(name = 'ROOT', failfast = FALSE)
loggerAppender(name)
loggerThreshold(name)
}
\arguments{
  \item{name}{ The name of the logger }
  \item{threshold}{ The logger threshold, which can be partially overridden by
    appenders. Use one of the constants defined (e.g. DEBUG, INFO, WARN, etc) }
  \item{appender}{ An appender name or a vector of appender names }
  \item{layout}{ A layout name or a vector of layout names }
  \item{failfast}{ Used internally to prevent inheritance of loggers }
}
\details{
  'addLogger' adds a logger configuration to the logger.options

  'getLogger' constructs a logger function based on the registered configuration

  'loggerAppender' provides introspection on the appenders attached to a logger

  'loggerThreshold' provides the threshold of the logger
}
\value{
  'getLogger' returns a \_function\_ that is called to log messages. The resulting
  function has two arguments: the log level for the message and the message
  itself. The below example illustrates the simplest usage pattern:

     > lg <- getLogger()
     > lg(DEBUG, "A debug message")

  Typically, the built in log level constants are used in the call, which
  conform to the log4j levels (from least severe to most severe): TRACE, DEBUG,
  INFO, WARN, ERROR, FATAL. It is not a strict requirement to use these
  constants (any numeric value will work), though most users should find this
  level of granularity sufficient.

  Loggers are hierarchical in the sense that any requested logger that is 
  undefined will fall back to its most immediate defined parent logger. The
  absolute parent is ROOT, which \_must\_ be defined for the system to work.
}
\author{ Brian Lee Yung Rowe }
\examples{
  lg <- getLogger("some.package")
  lg(DEBUG, "A debug message")
}
\keyword{ data }
