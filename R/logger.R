# Write to stdout using a format string
scat <- function(format, ..., use.newline=TRUE)
{
  if (use.newline) newline = '\n'
  else newline = ''

  cat(paste(sprintf(format, ...), newline, sep=''))
}

# This needs to be hidden
logger.message <- function(msg, ..., logger, level, label)
{
  # TODO: Put this constant somewhere else
  string.levels <- c(9,8,6,4,3,1)
  names(string.levels) <- c('DEBUG1','DEBUG','INFO1','INFO','WARN','ERROR')

  config <- getLogger(logger)
  # TODO: Need a proper logging initialization rather than putting lazy-init 
  # here
  if (is.null(config) & logger == 'ROOT')
  {
    addLogger('ROOT', level=4, fun=logger.stdout)
    config <- getLogger('ROOT')
  }

  if (is.character(config$level))
    config.level <- string.levels[config$level]
  else
    config.level <- config$level
  if (config.level < level) return()

  err <- "Logger %s needs a function"
  if (is.null(config$fun))
  {
    scat(err,logger)
    return()
  }

  if (length(list(...)) > 0) msg <- sprintf(msg, ...)
  line <- sprintf("%s [%s] %s\n", Sys.time(), label, msg)

  fn <- get(config$fun)
  fn(line, config)
  invisible()
}

logger.debug1 <- function(msg, ..., logger='ROOT')
{
  logger.message(msg, ..., logger=logger, level=9, label='DEBUG1')
  invisible()
}

logger.debug <- function(msg, ..., logger='ROOT')
{
  logger.message(msg, ..., logger=logger, level=8, label='DEBUG')
  invisible()
}

logger.info1 <- function(msg, ..., logger='ROOT')
{
  logger.message(msg, ..., logger=logger, level=6, label='INFO1')
  invisible()
}

logger.info <- function(msg, ..., logger='ROOT')
{
  logger.message(msg, ..., logger=logger, level=4, label='INFO')
  invisible()
}

logger.warn <- function(msg, ..., logger='ROOT')
{
  logger.message(msg, ..., logger=logger, level=3, label='WARN')
  invisible()
}

logger.error <- function(msg, ..., logger='ROOT')
{
  logger.message(msg, ..., logger=logger, level=1, label='ERROR')
  invisible()
}

# Add a new logger to the options config
# The following values need to be provided:
#   name - the name of the logger
#   level - log level for given logger
#   fun - the implementation for the logger. Either a function or a name of
#     a function
#   ... options to pass to logger implementation
addLogger <- function(name, level, fun, ...)
{
  config <- list(...)
  config$level <- level
  if ('function' %in% class(fun)) fun <- deparse(substitute(fun))
  config$fun <- fun
  exp <- parse(text=paste('logger.options(',name,'=config)', sep=''))
  eval(exp)
  invisible()
}

# Get a specific logger configuration
getLogger <- function(name)
{
  config <- logger.options()
  config[[name]]
}

setLogger <- function(name, level=NULL, fun=NULL, ...)
{
  config <- getLogger(name)
  if (! is.null(level)) config$level <- level
  if (! is.null(fun))
  {
    if ('function' %in% class(fun))
      config$fun <- deparse(substitute(fun))
    else
      config$fun <- fun
  }

  args <- list(...)
  for (n in names(args)) config[[n]] <- args[[n]]

  exp <- parse(text=paste('logger.options(',name,'=config)', sep=''))
  eval(exp)
  invisible()
}


# Set with options('use.plots'=FALSE)
# Defaults to TRUE
usePlots <- function(new.val=NULL)
{
  if (! is.null(new.val)) { options('use.plots'=new.val) }

  if (is.null(getOption('use.plots'))) { return(TRUE) }
  return(getOption('use.plots'))
}


# Get and set different loggers
# The only predefined logger is ROOT, which is set to null by default. The
# configuration can be loaded from the futile config file, which is located by
# default in ~/.futile/logger.config
# The config file can be overwritten in code or by the environment variable
# FUTILE_HOME
logger.stdout <- function(msg, config)
{
  cat(msg)
}

logger.file <- function(msg, config)
{
  if (! 'file' %in% names(config))
  {
    cat("Required argument 'file' is missing.\n")
    return()
  }
  cat(msg, file=config$file, append=TRUE)
}

# The logger options manager
logger.options <- options.manager('logger.options')


# OBSOLETE. Only here for backwards compatibility
logLevel <- function(new.level=NULL) 
{
  if (!is.null(new.level)) {
    options(log.level = new.level)
  }
  if (is.null(getOption("log.level"))) {
    return(0)
  }
  return(getOption("log.level"))
}

