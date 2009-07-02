# Write to stdout using a format string
scat <- function(format, ..., use.newline=TRUE)
{
  if (use.newline) newline = '\n'
  else newline = ''

  cat(paste(sprintf(format, ...), newline, sep=''))
}

log.debug <- function(msg, ..., logger=root)
{
  if(! logLevel() > 4) return

}

log.debug1 <- function(msg, ..., logger=root)
{
  if(! logLevel() > 6) return

}

log.info <- function(msg, ..., logger=root)
{
  if(! logLevel() > 1) return

}

log.info1 <- function(msg, ..., logger=root)
{
  if(! logLevel() > 2) return

}

log.warn <- function(msg, ..., logger=root)
{
  if(! logLevel() > 0) return

}

log.error <- function(msg, ..., logger=root)
{

}

# Get and set different loggers
# The only predefined logger is ROOT, which is set to null by default. The
# configuration can be loaded from the futile config file, which is located by
# default in ~/.futile/log.config
# The config file can be overwritten in code or by the environment variable
# FUTILE_HOME
logger <- function()
{
}

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

# Generates a function to retrieve options for a given name
options.manager <- function(option.name)
{
  function(...)
  {
    args <- list(...)

    os <- getOption(option.name)
    if (is.null(os)) os <- list()

    # Getter
    if (any(is.null(names(args))))
    {
      ns <- sapply(args, '[')
      ns <- ns[ns %in% names(os)]
      if (length(ns) == 0) return(NULL)
      return(sapply(os[ns], '['))
    }

    # Setter
    for (x in names(args)) os[[x]] <- args[[x]]
    options(argyle.options=os)

    invisible()
  }
}
