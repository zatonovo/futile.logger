\name{addAppender}
\alias{addAppender}
\alias{addAppender.default}
\alias{addAppender.character}
\alias{getAppender}
\alias{consoleAppender}
\alias{fileAppender}
\title{ Manage appenders within the 'futile.logger' sub-system }
\description{
  Provides functions for managing appenders. Typically only addAppender is used
  when manually creating a logging configuration.
}
\usage{
getAppender(name)
\method{addAppender}{default}(name, ..., threshold = NULL)
\method{addAppender}{character}(name, fun, ..., threshold = NULL)
}
\arguments{
  \item{name}{ The name of the appender or the appender itself. If the appender
    function is passed as the name, the symbol will be used as the name }
  \item{threshold}{ The threshold at which the appender is activated }
  \item{fun}{ The appender function, which must be passed explicitly if the 
    name of the appender is different from the appender function's symbol }
  \item{\dots}{ Further arguments to pass to the appender function when it is
    activated }
}
\details{
  Appenders do the actual work of writing log messages to some target. To use
  an appender in a logger, it must first be added to the system via the
  'addAppender' call. Once the appender has been registered, loggers can 
  reference the appender and call it to log messages.

  The 'getAppender' function is used internally to get the registered appender 
  function. It is kept visible so user-level introspection is available.

  'consoleAppender' is a function that writes to the console. No additional 
  arguments are necessary when registering the appender via addAppender.

  'fileAppender is an appender that writes to a file. An additional file
  argument must be passed to addAppender when registering a fileAppender.
}
\value{
  'getAppender' returns an appender function, which is wrapped in a parent
  function to enforce a consistent calling API.
}
\author{ Brian Lee Yung Rowe }
\examples{
  addAppender('file1', fileAppender, file='file1.log')
  addAppender('file2', fileAppender, file='file2.log', threshold=WARN)
  addAppender(consoleAppender, threshold=INFO)
}
\keyword{ data }
