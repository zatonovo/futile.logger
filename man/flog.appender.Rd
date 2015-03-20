\name{flog.appender}
\alias{appender.console}
\alias{appender.file}
\alias{flog.appender}
\title{Manage appenders for loggers}
\arguments{
  \item{\dots}{Used internally by lambda.r}
}
\description{
  Provides functions for adding and removing appenders.
}
\section{Usage}{
  # Get the appender for the given logger\cr
  flog.appender(name) \%::\% character : Function\cr
  flog.appender(name='ROOT')

  # Set the appender for the given logger\cr
  flog.appender(fn, name='ROOT')

  # Print log messages to the console\cr appender.console()

  # Write log messages to a file\cr appender.file(file)
}

\section{Details}{
  Appenders do the actual work of writing log messages to
  some target. To use an appender in a logger, you must
  register it to a given logger. Use \code{flog.appender}
  to both access and set appenders.

  The ROOT logger by default uses \code{appender.console}.

  \code{appender.console} is a function that writes to the
  console. No additional arguments are necessary when
  registering the appender via flog.appender.

  \code{appender.file} writes to a file, so you must pass
  an additional file argument to the function. To change
  the file name, just call
  \code{flog.appender(appender.file(file))} again with a
  new file name.

  To use your own appender create a function that takes a
  single argument, which represents the log message. You
  need to pass a function reference to
  \code{flog.appender}.
}

\section{Value}{
  When getting the appender, \code{flog.appender} returns
  the appender function.  When setting an appender,
  \code{flog.appender} has no return value.
}
\examples{
\dontrun{
flog.appender(appender.console(), name='my.logger')

# Set an appender to the logger named 'my.package'. Any log operations from
# this package will now use this appender.
flog.appender(appender.file('my.package.out'), 'my.package')
}
}
\author{
  Brian Lee Yung Rowe
}
\seealso{
  \code{\link{flog.logger}} \code{\link{flog.layout}}
}
\keyword{data}

