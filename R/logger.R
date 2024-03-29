#' Manage loggers
#' 
#' Provides functions for writing log messages and managing loggers. Typically
#' only the flog.[trace|debug|info|warn|error|fatal] functions need to be used
#' in conjunction with flog.threshold to interactively change the log level.
#' 
#' @section Usage:
#' # Conditionally print a log statement at TRACE log level\cr
#' flog.trace(msg, ..., name=flog.namespace(), logger=NULL, capture=FALSE)
#'
#' # Conditionally print a log statement at DEBUG log level\cr
#' flog.debug(msg, ..., name=flog.namespace(), logger=NULL, capture=FALSE)
#'
#' # Conditionally print a log statement at INFO log level\cr
#' flog.info(msg, ..., name=flog.namespace(), logger=NULL, capture=FALSE)
#'
#' # Conditionally print a log statement at WARN log level\cr
#' flog.warn(msg, ..., name=flog.namespace(), logger=NULL, capture=FALSE)
#'
#' # Conditionally print a log statement at ERROR log level\cr
#' flog.error(msg, ..., name=flog.namespace(), logger=NULL, capture=FALSE)
#'
#' # Print a log statement at FATAL log level\cr
#' flog.fatal(msg, ..., name=flog.namespace(), logger=NULL, capture=FALSE)
#'
#' # Execute an expression and capture any warnings or errors
#' ftry(expr, error=stop, silent=FALSE, finally=NULL, details='')
#'
#' @section Additional Usage:
#' These functions generally do not need to be called by an end user.
#'
#' # Get the ROOT logger\cr
#' flog.logger()
#' 
#' # Get the logger with the specified name\cr
#' flog.logger(name)
#'
#' # Set options for the given logger\cr
#' flog.logger(name, threshold=NULL, appender=NULL, layout=NULL, carp=NULL)
#' 
#' @section Details:
#' These functions represent the high level interface to futile.logger.
#' 
#' The primary use case for futile.logger is to write out log messages. There
#' are log writers associated with all the predefined log levels: TRACE, DEBUG,
#' INFO, WARN, ERROR, FATAL. Log messages will only be written if the log level
#' is equal to or more urgent than the current threshold. By default the ROOT
#' logger is set to INFO.
#' 
#' > flog.debug("This won't print") \cr
#' > flog.info("But this \%s", 'will') \cr
#' > flog.warn("As will \%s", 'this')
#' 
#' Typically, the built in log level constants are used in the call, which
#' conform to the log4j levels (from least severe to most severe): TRACE,
#' DEBUG, INFO, WARN, ERROR, FATAL. It is not a strict requirement to use these
#' constants (any numeric value will work), though most users should find this
#' level of granularity sufficient.
#' 
#' Loggers are hierarchical in the sense that any requested logger that is
#' undefined will fall back to its most immediate defined parent logger. The
#' absolute parent is ROOT, which is guaranteed to be defined for the system
#' and cannot be deleted. This means that you can specify a new logger
#' directly.
#' 
#' > flog.info("This will fall back to 'my', then 'ROOT'", name='my.logger')
#' 
#' You can also change the threshold or any other setting associated with a
#' logger. This will create an explicit logger where any unspecified options
#' are copied from the parent logger.
#' 
#' > flog.appender(appender.file("foo.log"), name='my') \cr
#' > flog.threshold(ERROR, name='my.logger') \cr
#' > flog.info("This won't print", name='my.logger') \cr
#' > flog.error("This %s print to a file", 'will', name='my.logger') \cr
#' 
#' If you have a function which gets called many times, it is a good strategy 
#' to pass the logger directly instead of its name.
#' 
#' Instead of this:
#' > simulation_fun <- function(i) {
#' >   flog.trace("We are in loop %d", i, name='my.logger')
#' >   i
#' > }
#' 
#' ... you can do this::
#' > my_logger <- flog.logger("my.logger")
#' > simulation_fun2 <- function(i) {
#' >   flog.trace("We are in loop %d", i, logger=my_logger)
#' >   i
#' > }
#' 
#' > system.time(for (i in 1:1000) simulation_fun(i))
#' > system.time(for (i in 1:1000) simulation_fun2(i))
#' 
#' If you define a logger that you later want to remove, use flog.remove.
#' 
#' The option 'capture' allows you to print out more complicated data
#' structures without a lot of ceremony. This variant doesn't accept format
#' strings and instead appends the value to the next line of output. Consider 
#'
#' > m <- matrix(rnorm(12), nrow=3) \cr
#' > flog.info("Matrix:",m, capture=TRUE)
#'
#' which preserves the formatting, whereas using capture=FALSE will have 
#' a cluttered output due to recycling.
#' 
#' @name flog.logger
#' @aliases flog.trace flog.debug flog.info flog.warn flog.error flog.fatal
#' @param msg The message to log
#' @param name The logger name to use
#' @param capture Capture print output of variables instead of interpolate
#' @param logger The logger to use. If \code{NULL} (the default), it is 
#' looked up based on \code{name}. Provide \code{logger} explicitely if 
#' the speed of the evaluation of log level is of concern (e.g., a 
#' \code{flog.trace} call in your function which has to be run many times).
#' @param \dots Optional arguments to populate the format string
#' @param expr An expression to evaluate
#' @param finally An optional expression to evaluate at the end
#' @author Brian Lee Yung Rowe
#' @seealso \code{\link{flog.threshold}} \code{\link{flog.remove}}
#' \code{\link{flog.carp}} \code{\link{flog.appender}} \code{\link{flog.layout}}
#' @keywords data
#' @examples
#' 
#' flog.threshold(DEBUG)
#' flog.debug("This debug message will print")
#' 
#' flog.threshold(WARN)
#' flog.debug("This one won't")
#' 
#' m <- matrix(rnorm(12), nrow=3)
#' flog.info("Matrix:",m, capture=TRUE)
#' 
#' ftry(log(-1))
#' 
#' \dontrun{
#' s <- c('FCX','AAPL','JPM','AMZN')
#' p <- TawnyPortfolio(s)
#'
#' flog.threshold(TRACE,'tawny')
#' ws <- optimizePortfolio(p, RandomMatrixDenoiser())
#' z <- getIndexComposition()
#'
#' flog.threshold(WARN,'tawny')
#' ws <- optimizePortfolio(p, RandomMatrixDenoiser())
#' z <- getIndexComposition()
#' }
#'
#' \dontrun{
#' flog.appender(appender.modulo(1000), name='counter')
#' lapply(1:10000, function(i) flog.info("value is %s",i, name='counter'))
#' }
NULL

.log_level <- function(msg, ..., level, name, capture, logger = NULL)
{
  if (is.null(logger)) logger <- flog.logger(name)
  if (level > logger$threshold && (is.null(logger$carp) || !logger$carp)) {
    return(invisible())
  }

  appender <- logger$appender
  layout <- logger$layout
  if (capture) {
    values <- paste(capture.output(print(...)), collapse='\n')
    message <- c(layout(level, msg, name), "\n", values, "\n")
  } else {
    message <- layout(level, msg, name, ...)
  }
  if (level <= logger$threshold) appender(message)
  invisible(message)
}

# Get the namespace that a function resides in. If no namespace exists, then
# return NULL.
# @param .where: where in the call stack should be check. 
#             0: current function (always
#            -1: parents of this function.
#            -3: when used within flog.*, 
#                it refers to the original caller of flog.*
#            -4: when used from flogger.name within a .log_level
#
# <environment: namespace:lambda.r>
flog.namespace <- function(.where=-4)
{
  sf <- sys.function(.where - 1)
  s <- format(topenv(environment(sf)))
  if (length(grep('lambda.r',s)) > 0)
    s <- attr(sys.function(-5), 'topenv')

  if (length(grep('namespace', s)) < 1) return('ROOT')

  ns <- sub('.*namespace:([^>]+)>.*','\\1', s)
  ifelse(is.null(ns), 'ROOT', ns)
}

flog.trace <- function(msg, ..., name=flog.namespace(), capture=FALSE, logger=NULL) {
  .log_level(msg, ..., level=TRACE,name=name, capture=capture, logger=logger)
}

flog.debug <- function(msg, ..., name=flog.namespace(), capture=FALSE, logger=NULL) {
  .log_level(msg, ..., level=DEBUG,name=name, capture=capture, logger=logger)
}

flog.info <- function(msg, ..., name=flog.namespace(), capture=FALSE, logger=NULL) {
  .log_level(msg, ..., level=INFO,name=name, capture=capture, logger=logger)
}

flog.warn <- function(msg, ..., name=flog.namespace(), capture=FALSE, logger=NULL) {
  .log_level(msg, ..., level=WARN,name=name, capture=capture, logger=logger)
}

flog.error <- function(msg, ..., name=flog.namespace(), capture=FALSE, logger=NULL) {
  .log_level(msg, ..., level=ERROR,name=name, capture=capture, logger=logger)
}

flog.fatal <- function(msg, ..., name=flog.namespace(), capture=FALSE, logger=NULL) {
  .log_level(msg, ..., level=FATAL,name=name, capture=capture, logger=logger)
}

#' Wrap a try block in futile.logger
#'
#' This function integrates futile.logger with the error and warning system
#' so problems can be caught both in the standard R warning system, while
#' also being emitted via futile.logger.
#'
#' @name ftry
#' @param expr The expression to evaluate in a try block
#' @param error An error handler
#' @param finally Pass-through to tryCatch finally
#' @param silent Boolean - should errors be rethrown? The same as the silent option on `try`.
#' If a custom error handler is being used that takes control over this option. Note you should
#' test the return value if you are dependent on it.
#' @param details An extra string to print when there's a warning message
#' @author Brian Lee Yung Rowe
#' @keywords data
#' @examples
#' \dontrun{
#' ftry(log("a")) # Logs the warning (but the warning still bubbles)
#'
#' x <- 'a'
#' y <- 2 # Some ID associated with x value
#' ftry(log("a"), details=sprintf("y = %s",y))
#'
#' ftry(log(-1)) # Logs the error and rethrows it
#' }
#' ftry(log(-1),silent=TRUE) # logs the error and silently continues
ftry <- function(expr, error=stop, finally=NULL, silent=FALSE, details='') {
  w.handler <- function(e) {
    flog.warn(paste("(",details,") %s", sep=''), e)
  }
  e.handler <- function(e) {
    flog.error(paste("(",details,") %s", sep=''), e)
    if (!silent | !isTRUE(all.equal(error, stop))) { error(e) }
  }
  tryCatch(
    withCallingHandlers(
      expr,
      warning = w.handler
    ),
    error = e.handler,
    finally
  )
}

# By default, use the package namespace or use the 'ROOT' logger.
flog.logger() %as%
{
  flog.logger(flog.namespace())
}

flog.logger(name) %as%
{
  if (nchar(name) < 1) name <- 'ROOT'
  #cat(sprintf("Searching for logger %s\n", name))

  key <- paste("logger", name, sep='.')
  # TODO: Search hierarchy
  os <- logger.options(key)
  if (! is.null(os)) return(os)
  if (name == 'ROOT') {
    logger <- list(name=name,
      threshold=INFO,
      appender=appender.console(),
      layout=layout.simple)
    logger.options(update=list(key, logger))
    return(logger)
  }

  parts <- strsplit(name, '.', fixed=TRUE)[[1]]
  parent <- paste(parts[1:length(parts)-1], collapse='.')
  flog.logger(parent)
}

flog.logger(name, threshold=NULL, appender=NULL, layout=NULL, carp=NULL) %as%
{
  logger <- flog.logger(name)
  if (!is.null(threshold)) logger$threshold <- threshold
  if (!is.null(appender)) logger$appender <- appender
  if (!is.null(layout)) logger$layout <- layout
  if (!is.null(carp)) logger$carp <- carp

  key <- paste("logger", name, sep='.')
  logger.options(update=list(key, logger))
  invisible()
}


#' Remove a logger
#' 
#' In the event that you no longer wish to have a logger registered,
#' use this function to remove it. Then any references to this
#' logger will inherit the next available logger in the hierarchy.
#'
#' @section Usage:
#' # Remove a logger\cr
#' flog.remove(name)
#' 
#' @name flog.remove
#' @param name The logger name to use
#' @author Brian Lee Yung Rowe
#' @keywords data
#' @examples
#' flog.threshold(ERROR, name='my.logger')
#' flog.info("Won't print", name='my.logger')
#' flog.remove('my.logger')
#' flog.info("Will print", name='my.logger')
flog.remove('ROOT') %as% { invisible() }
flog.remove(name) %as%
{
  key <- paste("logger", name, sep='.')
  logger.options(update=list(key, NULL))
  invisible()
}

#' Get and set the threshold for a logger
#'
#' The threshold affects the visibility of a given logger. When a log
#' statement is called, e.g. \code{flog.debug('foo')}, futile.logger
#' compares the threshold of the logger with the level implied in the
#' log command (in this case DEBUG). If the log level is at or higher
#' in priority than the logger threshold, a message will print.
#' Otherwise the command will silently return.
#'
#' @section Usage:
#' # Get the threshold for the given logger\cr
#' flog.threshold(name) \%::\% character : character \cr
#' flog.threshold(name=ROOT)
#'
#' # Set the threshold for the given logger\cr
#' flog.threshold(threshold, name=ROOT)
#' 
#' @name flog.threshold
#' @param threshold integer The new threshold for the given logger
#' @param name character The name of the logger
#' @author Brian Lee Yung Rowe
#' @keywords data
#' @examples
#' flog.threshold(ERROR)
#' flog.info("Won't print")
#' flog.threshold(INFO)
#' flog.info("Will print")
# Set the threshold
flog.threshold('TRACE', name='ROOT') %as% flog.threshold(TRACE, name)
flog.threshold('trace', name='ROOT') %as% flog.threshold(TRACE, name)
flog.threshold('DEBUG', name='ROOT') %as% flog.threshold(DEBUG, name)
flog.threshold('debug', name='ROOT') %as% flog.threshold(DEBUG, name)
flog.threshold('INFO', name='ROOT') %as% flog.threshold(INFO, name)
flog.threshold('info', name='ROOT') %as% flog.threshold(INFO, name)
flog.threshold('WARN', name='ROOT') %as% flog.threshold(WARN, name)
flog.threshold('warn', name='ROOT') %as% flog.threshold(WARN, name)
flog.threshold('ERROR', name='ROOT') %as% flog.threshold(ERROR, name)
flog.threshold('error', name='ROOT') %as% flog.threshold(ERROR, name)
flog.threshold('FATAL', name='ROOT') %as% flog.threshold(FATAL, name)
flog.threshold('fatal', name='ROOT') %as% flog.threshold(FATAL, name)


flog.threshold(threshold, name) %::% numeric : character : .
flog.threshold(threshold, name='ROOT') %as%
{
  flog.logger(name, threshold=threshold)
  invisible()
}

# Get the threshold
flog.threshold(name) %::% character : character
flog.threshold(name='ROOT') %as%
{
  logger <- flog.logger(name)
  names(logger$threshold)
}


#' Always return the log message
#'
#' Indicate whether the logger will always return the log message
#' despite the threshold.
#' 
#' This is a special option to allow the return value of the flog.*
#' logging functions to return the generated log message even if
#' the log level does not exceed the threshold. Note that this 
#' minorly impacts performance when enabled. This functionality
#' is separate from the appender, which is still bound to the 
#' value of the logger threshold.
#'
#' @section Usage:
#' # Indicate whether the given logger should carp\cr
#' flog.carp(name=ROOT)
#'
#' # Set whether the given logger should carp\cr
#' flog.carp(carp, name=ROOT)
#'
#' @name flog.carp
#' @param carp logical Whether to carp output or not
#' @param name character The name of the logger
#' @author Brian Lee Yung Rowe
#' @keywords data
#' @examples
#' flog.carp(TRUE)
#' x <- flog.debug("Returns this message but won't print")
#' flog.carp(FALSE)
#' y <- flog.debug("Returns nothing and prints nothing")
flog.carp(name) %::% character : logical
flog.carp(name='ROOT') %as%
{
  logger <- flog.logger(name)
  if (is.null(logger$carp)) FALSE
  else logger$carp
}

# Set whether to carp
flog.carp(carp, name='ROOT') %as%
{
  flog.logger(name, carp=carp)
  invisible()
}



