\name{flog.layout}
\alias{flog.layout}
\alias{layout.format}
\alias{layout.simple}
\title{Manage layouts within the 'futile.logger' sub-system}
\arguments{
  \item{\dots}{Used internally by lambda.r}
}
\value{
  'flog.layout' returns a layout function, which is wrapped
  in a parent function to enforce a consistent calling API.
}
\description{
  Provides functions for managing layouts. Typically
  'flog.layout' is only used when manually creating a
  logging configuration.
}
\details{
  Layouts are responsible for formatting messages so they
  are human-readable. Like an appender, a layout is
  assigned to a logger by calling 'flog.layout'. The
  'flog.layout' function is used internally to get the
  registered layout function. It is kept visible so
  user-level introspection is available.

  flog.layout(name) \%::\% character : Function
  flog.layout(name='ROOT')

  flog.layout(fn, name) \%::\% Function : character : Null

  flog.layout(fn, name='ROOT')

  'layout.simple' is a pre-defined layout function that
  prints messages in the following format: LEVEL
  [timestamp] Message. This is the default layout for the
  ROOT logger.

  'layout.format' allows you to specify the format string
  to use in printing a message. It is mostly included to
  provide an example of writing your own layout function.
}
\examples{
# Set the layout for 'my.package'
flog.layout(layout.simple, name='my.package')
}
\author{
  Brian Lee Yung Rowe
}
\keyword{data}

