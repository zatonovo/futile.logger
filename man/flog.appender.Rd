\name{flog.appender}
\alias{appender.console}
\alias{appender.file}
\alias{flog.appender}
\title{Manage appenders within the 'futile.logger' sub-system}
\arguments{
  \item{\dots}{Used internally by lambda.r}
}
\value{
  When getting the appender, 'flog.appender' returns the
  appender function.  When setting an appender,
  'flog.appender' has no return value.
}
\description{
  Provides functions for managing appenders. Typically only
  addAppender is used when manually creating a logging
  configuration.
}
\details{
  Appenders do the actual work of writing log messages to
  some target. To use an appender in a logger, you must
  register it to a given logger. Use the 'flog.appender'
  function to both access and set appenders.

  flog.appender(name) \%::\% character : Function

  flog.appender(name='ROOT')

  flog.appender(fn, name) \%::\% Function : character :
  Null

  flog.appender(fn, name='ROOT')

  'appender.console' is a function that writes to the
  console. No additional arguments are necessary when
  registering the appender via flog.appender.

  flog.appender(appender.console(), name='my.logger')

  'appender.file' writes to a file, so you must pass an
  additional file argument to the function.

  flog.appender(appender.file('output.log'),
  name='my.logger')

  To use your own appender create a function that takes a
  single argument, which represents the log message. You
  need to pass a function reference to flog.appender.
}
\examples{
# Set an appender to the logger named 'my.package'. Any log operations from
# this package will now use this appender.
flog.appender(appender.file('my.package.out'), 'my.package')
}
\author{
  Brian Lee Yung Rowe
}
\keyword{data}

