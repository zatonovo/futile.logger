\name{scat}
\alias{scat}
\title{Print formatted messages}
\usage{
  scat(format, ..., use.newline = TRUE)
}
\arguments{
  \item{format}{A format string passed to sprintf}

  \item{use.newline}{Whether to append a new line at the
  end}

  \item{\dots}{Arguments to pass to sprintf for
  dereferencing}
}
\value{
  A formatted string printed to the console
}
\description{
  A replacement for cat that has built-in sprintf
  formatting
}
\details{
  Like cat but you can use format strings.
}
\examples{
apply(array(2:5),1, function(x) scat('This has happened \%s times', x) )
}
\author{
  Brian Lee Yung Rowe
}
\keyword{data}

