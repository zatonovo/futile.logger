#' Manage appenders within the 'futile.logger' sub-system
#' 
#' Provides functions for writing log messages and managing loggers. Typically
#' only the flog.[trace|debug|info|warn|error|fatal] functions need to be used
#' in conjunction with flog.threshold to interactively change the log level.
#' 
#' These functions represent the high level interface to futile.logger.
#' 
#' The primary use case for futile.logger is to write out log messages. There
#' are log writers associated with all the predefined log levels: TRACE, DEBUG,
#' INFO, WARN, ERROR, FATAL. Log messages will only be written if the log level
#' is equal to or more urgent than the current threshold. By default the ROOT
#' logger is set to INFO.
#' 
#' > flog.debug("This won't print") 
#' 
#' > flog.info("But this \%s", 'will') 
#' 
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
#' > flog.appender(appender.file("foo.log"), name='my')
#' 
#' > flog.threshold(ERROR, name='my.logger')
#' 
#' > flog.info("This won't print", name='my.logger') 
#' 
#' > flog.error("This %s print to a file", 'will', name='my.logger')
#' 
#' If you define a logger that you later want to remove, use flog.remove.
#' 
#' The option 'capture' allows you to print out more complicated data
#' structures without a lot of ceremony. This variant doesn't accept format
#' strings and instead appends the value to the next line of output. Consider 
#'
#' > m <- matrix(rnorm(12), nrow=3)
#'
#' > flog.info("Matrix:",m, capture=TRUE)
#'
#' which preserves the formatting, whereas using capture=FALSE will have 
#' a cluttered output due to recycling.
#' 
#' @name flog.logger
#' @aliases flog.remove flog.threshold flog.carp flog.trace
#' flog.debug flog.info flog.warn flog.error flog.fatal ftry
#' @param msg The message to log
#' @param name The logger name to use
#' @param capture Capture print output of variables instead of interpolate
#' @param \dots Optional arguments to populate the format string
#' @param expr An expression to evaluate
#' @param finally An optional expression to evaluate at the end
#' @return Most of these functions exist for their side effects, so there are
#' few useful return values.
#' @author Brian Lee Yung Rowe
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
NULL

.log_level <- function(msg, ..., level, name, capture)
{
  logger <- flog.logger(name)
  if (level > logger$threshold && (is.null(logger$carp) || !logger$carp)) {
    return(invisible())
  }

  appender <- flog.appender(name)
  layout <- flog.layout(name)
  if (capture) {
    values <- capture.output(print(...))
    message <- c(layout(level, msg), values)
  } else {
    message <- layout(level, msg, ...)
  }
  if (level <= logger$threshold) appender(message)
  invisible(message)
}

# Get the namespace that a function resides in. If no namespace exists, then
# return NULL.
# <environment: namespace:lambda.r>
get_namespace(where=1) %as% 
{
  s <- capture.output(str(environment(sys.function(where)), give.attr=FALSE))
  if (length(grep('namespace', s)) < 1) return('ROOT')

  ns <- sub('.*namespace:([^>]+)>.*','\\1', s)
  ifelse(is.null(ns), 'ROOT', ns)
}


flog.trace <- function(msg, ..., name=get_namespace(), capture=FALSE) {
  .log_level(msg, ..., level=TRACE,name=name, capture=capture)
}

flog.debug <- function(msg, ..., name=get_namespace(), capture=FALSE) {
  .log_level(msg, ..., level=DEBUG,name=name, capture=capture)
}

flog.info <- function(msg, ..., name=get_namespace(), capture=FALSE) {
  .log_level(msg, ..., level=INFO,name=name, capture=capture)
}

flog.warn <- function(msg, ..., name=get_namespace(), capture=FALSE) {
  .log_level(msg, ..., level=WARN,name=name, capture=capture)
}

flog.error <- function(msg, ..., name=get_namespace(), capture=FALSE) {
  .log_level(msg, ..., level=ERROR,name=name, capture=capture)
}

flog.fatal <- function(msg, ..., name=get_namespace(), capture=FALSE) {
  .log_level(msg, ..., level=FATAL,name=name, capture=capture)
}

ftry <- function(expr, error=stop, finally=NULL) {
  w.handler <- function(e) flog.warn("%s", e)
  e.handler <- function(e) { flog.error("%s", e); error(e) }
  tryCatch(expr, warning=w.handler, error=e.handler, finally)
}

# Get a logger. By default, use the package namespace or use the 'ROOT' logger.
flog.logger() %as%
{
  flog.logger(get_namespace())
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


flog.remove('ROOT') %as% { invisible() }
flog.remove(name) %as% 
{
  key <- paste("logger", name, sep='.')
  logger.options(update=list(key, NULL))
  invisible()
}

# Get the threshold for the given logger
flog.threshold(name) %::% character : character
flog.threshold(name='ROOT') %as%
{
  logger <- flog.logger(name)
  names(logger$threshold)
}

# Set the threshold
flog.threshold(threshold, name='ROOT') %as%
{
  flog.logger(name, threshold=threshold)
  invisible()
}

# Indicate whether the logger will always output a message
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



