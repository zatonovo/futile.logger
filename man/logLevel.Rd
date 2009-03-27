\name{logLevel}
\alias{logLevel}
\alias{usePlots}
\title{ Set the current log level for scripts }
\description{
  Gets or sets the current log level. Applications can use this to determine how
  much logging should be output.

  Gets or sets the current plotting state. When FALSE, applications should not
  plot any data.
}
\usage{
logLevel(new.level = NULL)
usePlots(new.val = NULL)
}
\arguments{
  \item{new.level}{ The new log level. Typically a value between 0 (none) and 9 (most verbose) is used. If omitted, this will display the current log level. }
  \item{new.val}{ The value to replace the current value with. If omitted, this
  will display the current value. }
}
\value{
  The current value of the setting.
}
\author{ Brian Lee Yung Rowe }
\examples{
  logLevel(3)
  logLevel()
  usePlots()
}
\keyword{ data }
