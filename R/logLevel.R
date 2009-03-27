# Returns current log level of package
logLevel <- function(new.level=NULL)
{
  if (! is.null(new.level)) { options('log.level'=new.level) }

  if (is.null(getOption('log.level'))) { return(0) }
  return(getOption('log.level'))
}

# Set with options('use.plots'=FALSE)
# Defaults to TRUE
usePlots <- function(new.val=NULL)
{
  if (! is.null(new.val)) { options('use.plots'=new.val) }

  if (is.null(getOption('use.plots'))) { return(TRUE) }
  return(getOption('use.plots'))
}

