\name{futile.logger-package}
\alias{futile.logger-package}
\alias{futile.logger}
\docType{package}
\title{
  A logging sub-system for futile
}
\description{
  This package implements a logging system inspired by log4j and tries to maintain conceptual
  consistency with the package. This also extends to the similarly derivative work log4cpp.
  'futile.logger' implements the core functionality available in the log4j feature suite,
  including hierarchical loggers, custom appenders, custom layouts, multiple appenders per
  logger, and configurable thresholds. The interface tries also to maintain R idioms and thus
  is not an exact clone.
}
\details{
\tabular{ll}{
Package: \tab futile.logger\cr
Type: \tab Package\cr
Version: \tab {version}\cr
Date: \tab {date}\cr
License: \tab LGPL-3\cr
LazyLoad: \tab yes\cr
}
  The basic concepts connected to 'futile.logger' include loggers, appenders, and layouts.
  Loggers are the high-level interface for calling logging operations. These loggers are
  scoped to a particular namespace, which in more traditional object-oriented languages
  typically mapped to the package namespace. In R, a similar convention can be employed, such
  that different packages scope their logging calls to enable other code to easily switch
  on and off the log messages. Loggers have a threshold associated to them, such that only
  messages at the same level or of greater severity are printed. Multiple appenders can be
  attached to a given logger, giving flexibility in how to route log messages.
  
  Appenders do the actual work of writing log messages to a writeable target, whether that is
  a console, a file, a URL, database, etc. When creating an appender, the implementation-
  specific options are passed to the appender at instantiation. Once an appender is added
  to the 'futile.logger' options space, the appender can be attached to any logger. Appenders
  can override the threshold defined in the logger, which provides more granular control to
  the logging process.

  Layouts are responsible for formatting messages. This operation usually consists of adding
  the log level, a timestamp, plus some pretty-printing to make the log messages easy on the
  eyes.

  An important part of loggers is that they are hierarchical in nature based on the namespace
  given to the logger, such that loggers will inherit properties of parent loggers up to the
  ROOT if no definition is found. In practice packages can define a logger hierarchy that 
  will work in user libraries, assuming the ROOT logger is configured properly.

  To use 'futile.logger', at a minimum, one layout, appender, and logger (ROOT) must be 
  defined. A number of default configurations are provided in the package to get users started
  quickly, while also acting as examples for users needing more sophisticated logging.
}
\author{
Brian Lee Yung Rowe <r@nurometic.com>
}
\keyword{ package }
\keyword{ attribute }
\keyword{ logic }
\seealso{ \code{\link{addAppender}}, \code{\link{addLayout}}, \code{\link{addLogger}}, \code{\link{getLogger}} }
\examples{
  configLogger('console')
  mylogger <- getLogger()
  mylogger(DEBUG, "This won't print")
  mylogger(WARN, "This will print")
  
  logger.b <- getLogger('b')
  logger.b(WARN, "This inherits from the ROOT logger")
}

