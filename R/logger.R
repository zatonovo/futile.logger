# Generic boiler plate
# simple.formatter <- function(level, msg, ...) sprintf("[%s] %s", level,msg)
# addFormatter(simple.formatter)
# addHandler(console.handler, formatter=simple.formatter)
# addLogger(, threshold=DEBUG, handler=simple.formatter)
#
# To use the logger,
# my.log <- getLogger()
# my.log(DEBUG, "This is a log message")
ERROR <- 1
WARN <- 3
INFO <- 5
DEBUG <- 7
FINE <- 8
FINER <- 9
FINEST <- 10

# Get handlers associated with the given logger
log.handler <- function(name)
{
  key <- paste("logger", name, sep='.')
  logger <- logger.options(key)
  logger$handler
}

# Append or replace handlers for this logger
"log.handler<-" <- function(name, append=TRUE, value)
{
  key <- paste("logger", name, sep='.')
  logger <- logger.options(key)
  if (append) { logger$handler <- c(logger$handler, value) }
  else { logger$handler <- value }
  update.options(logger.options, key, logger)
}

# Get the threshold for the given logger
log.threshold <- function(name)
{
  key <- paste("logger", name, sep='.')
  logger <- logger.options(key)
  logger$threshold
}

# Set the threshold for the given logger
"log.threshold<-" <- function(name, value)
{
  key <- paste("logger", name, sep='.')
  logger <- logger.options(key)
  logger$threshold <- value
  update.options(logger.options, key, logger)
}

# Create a logger based on the config passed in from the options.manager
# config$handler <- c('format.1','format.2')
# names(config$handler) <- c('handler.1','handler.2')
.log.function <- function(config)
{
  logger <- function(level, msg, ...)
  {
    # Check level
    if (level > config$threshold) { return() }
    # Get formatter
    formatters <- config$handler
    # Get handler
    handlers <- names(config$handler)
    # Call handler
    # TODO: Finish this
    apply(handlers, 1, function(h,f) h(level, msg, ...), formatters)
  }
  logger
}


# Get a formatter registered in the system. Formatters are called by handlers
# to format messages.
getFormatter <- function(name)
{
  key <- paste("formatter", name, sep='.')
  logger.options(key)
}

# Add a formatter to the system.
addFormatter <- function(name, ...) UseMethod('addFormatter')
addFormatter.default <- function(name, ...)
  addFormatter.character(deparse(substitute(name)), name, ...)
addFormatter.character <- function(name, fun, ...)
{
  key <- paste("formatter", name, sep='.')
  fn <- function(level, msg) fun(level,msg, ...)
  logger.options(update=list(key,fn))
}

# Get a handler registered in the system.
getHandler <- function(name)
{
  key <- paste("handler", name, sep='.')
  logger.options(key)
}

# Add a handler to the system
addHandler <- function(name, ...) UseMethod('addHandler')
addHandler.default <- function(name, ...)
  addHandler.character(deparse(substitute(name)), name, ...)
addHandler.character <- function(name, fun, ...)
{
  key <- paste("handler", name, sep='.')
  fn <- function(level, msg, formatter) fun(level,msg, ..., formatter=formatter)
  logger.options(update=list(key,fn))
}

# Get a logger. If 'name' has not been registered, the inheritance hierarchy
# will be followed to find an appropriate logger. The logger is actually a
# function that can be called using the following syntax:
#   my.log <- getLogger('my.log')
#   my.log(DEBUG, "This is a log message")
getLogger <- function(name)
{
  key <- paste("logger", name, sep='.')
  # TODO: Search hierarchy
  os <- logger.options(key)
  .log.function(os)
}

# Regsiter a logger in the system with the given threshold and handlers
addLogger <- function(name, threshold, handler)
{
  if (is.null(name)) { name <- 'ROOT' }

  key <- paste("logger", name, sep='.')
  my.logger <- list(name=name, threshold=threshold, handler=handler)
  logger.options(update=list(key, my.logger))
  invisible()
}

# The logger options manager
logger.options <- options.manager('logger.options')


