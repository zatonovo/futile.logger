# Generic boiler plate
# simple.layout <- function(level, msg, ...) sprintf("[%s] %s", level,msg)
# addLayout(simple.layout)
# addAppender(console.appender, layout=simple.layout)
# addLogger(, threshold=DEBUG, appender=simple.layout)
#
# To use the logger,
# my.log <- getLogger()
# my.log(DEBUG, "This is a log message")

# Get appenders associated with the given logger
loggerAppender <- function(name)
{
  key <- paste("logger", name, sep='.')
  logger <- logger.options(key)
  logger$appender
}

# Append or replace appenders for this logger
# TODO: INCOMPLETE
#"loggerAppender<-" <- function(name, append=TRUE, value)
#{
#  key <- paste("logger", name, sep='.')
#  logger <- logger.options(key)
#  if (append) { logger$appender <- c(logger$appender, value) }
#  else { logger$appender <- value }
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
  m <- cbind(config$appender, config$layout)
  function(level, msg)
  {
    if (level > config$threshold) { return(invisible()) }
    
    apply(m, 1, function(x) {
      h <- getAppender(x[1])
      f <- getLayout(x[2])
      h(level, msg, layout=f)
    })
    invisible()
  }
}


# Get a layout registered in the system. Layouts are called by appenders
# to format messages.
getLayout <- function(name)
{
  key <- paste("layout", name, sep='.')
  logger.options(key)
}

# Add a layout to the system.
addLayout <- function(name, ...) UseMethod('addLayout')
addLayout.default <- function(name, ...)
  addLayout.character(deparse(substitute(name)), name, ...)
addLayout.character <- function(name, fun, ...)
{
  key <- paste("layout", name, sep='.')
  fn <- function(level, msg) fun(level,msg, ...)
  logger.options(update=list(key,fn))
  invisible()
}

# Get a appender registered in the system.
getAppender <- function(name)
{
  key <- paste("appender", name, sep='.')
  logger.options(key)
}

# Add a appender to the system
addAppender <- function(name, ..., threshold=NULL) UseMethod('addAppender')
addAppender.default <- function(name, ..., threshold=NULL)
  addAppender.character(deparse(substitute(name)), name, ..., threshold=threshold)
addAppender.character <- function(name, fun, ..., threshold=NULL)
{
  key <- paste("appender", name, sep='.')
  fn <- function(level, msg, layout)
    fun(level,msg, ..., threshold=threshold, layout=layout)
  logger.options(update=list(key,fn))
  invisible()
}

# Get a logger. If 'name' has not been registered, the inheritance hierarchy
# will be followed to find an appropriate logger. The logger is actually a
# function that can be called using the following syntax:
#   my.log <- getLogger('my.log')
#   my.log(DEBUG, "This is a log message")
getLogger <- function(name='ROOT')
{
  if (nchar(name) < 1) name <- 'ROOT'
  #cat(sprintf("Searching for logger %s\n", name))

  key <- paste("logger", name, sep='.')
  # TODO: Search hierarchy
  os <- logger.options(key)
  if (! is.null(os)) return(.LogFunction(os))
  if (name == 'ROOT') 
  {
    scat("ROOT logger not configured properly. This logger is disabled")
    fn <- function(...) { invisible() }
    return(fn)
  }

  parts <- strsplit(name, '.', fixed=TRUE)[[1]]
  parent <- paste(parts[1:length(parts)-1], collapse='.')
  getLogger(parent)
}

# Register a logger in the system with the given threshold and appenders
addLogger <- function(name, threshold, appender, layout)
{
  if (is.null(name)) { name <- 'ROOT' }

  key <- paste("logger", name, sep='.')
  my.logger <- list(name=name, threshold=threshold,
    appender=appender, layout=layout)
  logger.options(update=list(key, my.logger))
  invisible()
}



