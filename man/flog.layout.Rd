\name{flog.layout}
\alias{flog.layout}
\alias{layout.format}
\alias{layout.simple}
\alias{layout.tracearg}
\title{Manage layouts within the 'futile.logger' sub-system}
\arguments{
  \item{\dots}{Used internally by lambda.r}
}
\description{
  Provides functions for managing layouts. Typically
  'flog.layout' is only used when manually creating a
  logging configuration.
}
\section{Usage}{
  # Get the layout function for the given logger\cr
  flog.layout(name) \%::\% character : Function\cr
  flog.layout(name='ROOT')

  # Set the layout function for the given logger\cr
  flog.layout(fn, name='ROOT')

  # Decorate log messages with a standard format\cr
  layout.simple(level, msg, ...)

  # Decorate log messages using a custom format\cr
  layout.format(format, datetime.fmt="%Y-%m-%d %H:%M:%S")

  # Show the value of a single variable
  layout.tracearg(level, msg, ...)
}

\section{Details}{
  Layouts are responsible for formatting messages so they
  are human-readable. Similar to an appender, a layout is
  assigned to a logger by calling \code{flog.layout}. The
  \code{flog.layout} function is used internally to get the
  registered layout function. It is kept visible so
  user-level introspection is possible.

  \code{layout.simple} is a pre-defined layout function
  that prints messages in the following format:\cr LEVEL
  [timestamp] message

  This is the default layout for the ROOT logger.

  \code{layout.format} allows you to specify the format
  string to use in printing a message. The following tokens
  are available. \describe{ \item{~l}{Log level}
  \item{~t}{Timestamp} \item{~n}{Namespace} \item{~f}{The
  calling function} \item{~m}{The message} }

  \code{layout.tracearg} is a special layout that takes a
  variable and prints its name and contents.
}
\examples{
# Set the layout for 'my.package'
flog.layout(layout.simple, name='my.package')

# Update the ROOT logger to use a custom layout
layout <- layout.format('[~l] [~t] [~n.~f] ~m')
flog.layout(layout)

# Create a custom logger to trace variables
flog.layout(layout.tracearg, name='tracer')
x <- 5
flog.info(x, name='tracer')
}
\author{
  Brian Lee Yung Rowe
}
\seealso{
  \code{\link{flog.logger}} \code{\link{flog.appender}}
}
\keyword{data}

