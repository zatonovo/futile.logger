#' Manage layouts within the 'futile.logger' sub-system
#' 
#' Provides functions for managing layouts. Typically 'flog.layout' is only
#' used when manually creating a logging configuration.
#' 
#' Layouts are responsible for formatting messages so they are human-readable.
#' Like an appender, a layout is assigned to a logger by calling 'flog.layout'.
#' The 'flog.layout' function is used internally to get the registered layout
#' function. It is kept visible so user-level introspection is available.
#' 
#' flog.layout(name) \%::\% character : Function flog.layout(name='ROOT')
#' 
#' flog.layout(fn, name) \%::\% Function : character : Null
#'
#' flog.layout(fn, name='ROOT')
#' 
#' 'layout.simple' is a pre-defined layout function that prints messages in the
#' following format: LEVEL [timestamp] Message. This is the default layout for
#' the ROOT logger.
#' 
#' 'layout.format' allows you to specify the format string to use in printing a
#' message. It is mostly included to provide an example of writing your own
#' layout function.
#' 
#' @name flog.layout
#' @aliases layout.simple layout.format
#' @param \dots Used internally by lambda.r
#' @return 'flog.layout' returns a layout function, which is wrapped in a
#' parent function to enforce a consistent calling API.
#' @author Brian Lee Yung Rowe
#' @keywords data
#' @examples
#' 
#' # Set the layout for 'my.package'
#' flog.layout(layout.simple, name='my.package')
#' 
NULL

# Get the layout for the given logger
flog.layout(name) %::% character : Function
flog.layout(name='ROOT') %as%
{
  logger <- flog.logger(name)
  logger$layout
}

# Set the layout
flog.layout(fn, name='ROOT') %as%
{
  flog.logger(name, layout=fn)
  invisible()
}

# This file provides some standard formatters
# This prints out a string in the following format:
#   LEVEL [timestamp] message
layout.simple <- function(level, msg, ...)
{
  the.time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (! is.null(substitute(...))) msg <- sprintf(msg, ...)
  sprintf("%s [%s] %s\n", names(level),the.time, msg)
}


# This parses and prints a user-defined format string. Available tokens are
# ~l - Log level
# ~t - Timestamp
# ~n - Namespace
# ~f - Calling function
# ~m - Message
#
# layout <- layout.format('[~l] [~t] [~n.~f] ~m')
# flog.layout(layout)
layout.format <- function(format, datetime.fmt="%Y-%m-%d %H:%M:%S")
{
  where <- 1
  function(level, msg, ...) {
    if (! is.null(substitute(...))) msg <- sprintf(msg, ...)
    the.level <- names(level)
    the.time <- format(Sys.time(), datetime.fmt)
    the.namespace <- get_namespace()
    #print(sys.calls())
    the.function <- tryCatch(sys.call(where)[[1]], error=function(e) "(shell)")
    #pattern <- c('~l','~t','~n','~f','~m')
    #replace <- c(the.level, the.time, the.namespace, the.function, msg)
    message <- gsub('~l',the.level, format, fixed=TRUE)
    message <- gsub('~t',the.time, message, fixed=TRUE)
    message <- gsub('~n',the.namespace, message, fixed=TRUE)
    message <- gsub('~f',the.function, message, fixed=TRUE)
    message <- gsub('~m',msg, message, fixed=TRUE)
    sprintf("%s\n", message)
  }
}

