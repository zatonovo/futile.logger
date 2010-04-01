\name{addLayout}
\alias{addLayout}
\alias{addLayout.default}
\alias{addLayout.character}
\alias{getLayout}
\alias{simpleLayout}
\alias{formatLayout}
\title{ Manage layouts within the 'futile.logger' sub-system }
\description{
  Provides functions for managing layouts. Typically only addLayout is used
  when manually creating a logging configuration.
}
\usage{
getLayout(name)
\method{addLayout}{default}(name, ...)
\method{addLayout}{character}(name, fun, ...)
}
\arguments{
  \item{name}{ The name of the layout or the layout itself. If the layout
    function is passed as the name, the symbol will be used as the name }
  \item{fun}{ The layout function, which must be passed explicitly if the 
    name of the layout is different from the layout function's symbol }
  \item{\dots}{ Further arguments to pass to the layout function when it is
    activated }
}
\details{
  Layouts are responsible for formatting messages so they are human-readable.
  Like an appender, a layout must first be registered in the system using
  'addLayout' prior to referencing it in a logger configuration. Once the layout
  is registered, it can be referenced in any logger definition.

  The 'getLayout' function is used internally to get the registered layout
  function. It is kept visible so user-level introspection is available.

  'simpleLayout' is a pre-defined layout function that prints messages in the
  following format: LEVEL [timestamp] Message.

  'formatLayout' is a pre-defined layout function that prints tokens based on 
  a format string. This is currently not implemented.
}
\value{
  'getLayout' returns a layout function, which is wrapped in a parent
  function to enforce a consistent calling API.
}
\author{ Brian Lee Yung Rowe }
\examples{
  addLayout('default', simpleLayout)
  addLayout(simpleLayout)
}
\keyword{ data }
