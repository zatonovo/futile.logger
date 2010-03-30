# Generic boiler plate
# simple.formatter <- function(level, msg, ...) sprintf("[%s] %s", level,msg)
# addFormatter(simple.formatter)
# addHandler(console.handler, formatter=simple.formatter)
# addLogger(, threshold=DEBUG, handler=simple.formatter)
#
# To use the logger,
# my.log <- getLogger()
# my.log(DEBUG, "This is a log message")

# Get handlers associated with the given logger
loggerHandler <- function(name)
{
  key <- paste("logger", name, sep='.')
  logger <- logger.options(key)
  logger$handler
}

# Append or replace handlers for this logger
# TODO: INCOMPLETE
#"loggerHandler<-" <- function(name, append=TRUE, value)
#{
#  key <- paste("logger", name, sep='.')
#  logger <- logger.options(key)
#  if (append) { logger$handler <- c(logger$handler, value) }
#  else { logger$handler <- value }
#  updateOptions(logger.options, key, logger)
#}

# Get the threshold for the given logger
loggerThreshold <- function(name)
{
  key <- paste("logger", name, sep='.')
  logger <- logger.options(key)
  logger$threshold
}

# Set the threshold for the given logger
# TODO: INCOMPLETE
#"loggerThreshold<-" <- function(name, value)
#{
#  key <- paste("logger", name, sep='.')
#  logger <- logger.options(key)
#  logger$threshold <- value
#  updateOptions(logger.options, key, logger)
#  invisible()
#}

# Create a logger based on the config passed in from the options.manager
.LogFunction <- function(config)
{
  m <- cbind(config$handler, config$formatter)
  function(level, msg)
  {
    if (level > config$threshold) { return(invisible()) }
    
    apply(m, 1, function(x) {
      h <- getHandler(x[1])
      f <- getFormatter(x[2])
      h(level, msg, formatter=f)
    })
    invisible()
  }
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
  invisible()
}

# Get a handler registered in the system.
getHandler <- function(name)
{
  key <- paste("handler", name, sep='.')
  logger.options(key)
}

# Add a handler to the system
addHandler <- function(name, ..., threshold=NULL) UseMethod('addHandler')
addHandler.default <- function(name, ..., threshold=NULL)
  addHandler.character(deparse(substitute(name)), name, ..., threshold=threshold)
addHandler.character <- function(name, fun, ..., threshold=NULL)
{
  key <- paste("handler", name, sep='.')
  fn <- function(level, msg, formatter)
    fun(level,msg, ..., threshold=threshold, formatter=formatter)
  logger.options(update=list(key,fn))
  invisible()
}

# Get a logger. If 'name' has not been registered, the inheritance hierarchy
# will be followed to find an appropriate logger. The logger is actually a
# function that can be called using the following syntax:
#   my.log <- getLogger('my.log')
#   my.log(DEBUG, "This is a log message")
getLogger <- function(name)
{
  if (nchar(name) < 1) name <- 'ROOT'
  #cat(sprintf("Searching for logger %s\n", name))

  key <- paste("logger", name, sep='.')
  # TODO: Search hierarchy
  os <- logger.options(key)
  if (! is.null(os)) return(.LogFunction(os))
  if (name == 'ROOT') stop("ROOT logger not configured properly")

  parts <- strsplit(name, '.', fixed=TRUE)[[1]]
  parent <- paste(parts[1:length(parts)-1], collapse='.')
  getLogger(parent)
}

# Register a logger in the system with the given threshold and handlers
addLogger <- function(name, threshold, handler, formatter)
{
  if (is.null(name)) { name <- 'ROOT' }

  key <- paste("logger", name, sep='.')
  my.logger <- list(name=name, threshold=threshold,
    handler=handler, formatter=formatter)
  logger.options(update=list(key, my.logger))
  invisible()
}



