#' Manage layouts within the 'futile.logger' sub-system
#' 
#' Provides functions for managing layouts. Typically 'flog.layout' is only
#' used when manually creating a logging configuration.
#' 
#' @section Usage:
#' # Get the layout function for the given logger\cr
#' flog.layout(name) \%::\% character : Function\cr
#' flog.layout(name='ROOT')
#' 
#' # Set the layout function for the given logger\cr
#' flog.layout(fn, name='ROOT')
#' 
#' # Decorate log messages with a standard format\cr
#' layout.simple(level, msg, ...)
#' 
#' # Decorate log messages with a standard format and a pid\cr
#' layout.simple.parallel(level, msg, ...)
#'
#' # Generate log messages as JSON\cr
#' layout.json(level, msg, ...)
#'
#' # Decorate log messages using a custom format\cr
#' layout.format(format, datetime.fmt="%Y-%m-%d %H:%M:%S")
#'
#' # Show the value of a single variable
#' layout.tracearg(level, msg, ...)
#' 
#' # Generate log messages in a Graylog2 HTTP GELF accetable format
#' layout.graylog(common.fields)
#' 
#' @section Details:
#' Layouts are responsible for formatting messages so they are human-readable.
#' Similar to an appender, a layout is assigned to a logger by calling 
#' \code{flog.layout}. The \code{flog.layout} function is used internally
#' to get the registered layout function. It is kept visible so 
#' user-level introspection is possible.
#' 
#' \code{layout.simple} is a pre-defined layout function that 
#' prints messages in the following format:\cr
#'   LEVEL [timestamp] message
#'
#' This is the default layout for the ROOT logger.
#' 
#' \code{layout.format} allows you to specify the format string to use 
#' in printing a message. The following tokens are available.
#' \describe{
#' \item{~l}{Log level}
#' \item{~t}{Timestamp}
#' \item{~n}{Namespace}
#' \item{~f}{The calling function}
#' \item{~m}{The message}
#' \item{~p}{The process PID}
#' }
#'
#' \code{layout.json} converts the message and any additional objects provided
#' to a JSON structure. E.g.:
#' 
#' flog.info("Hello, world", cat='asdf')
#'  
#' yields something like
#' 
#' \{"level":"INFO","timestamp":"2015-03-06 19:16:02 EST","message":"Hello, world","func":"(shell)","cat":["asdf"]\}
#' 
#' \code{layout.tracearg} is a special layout that takes a variable
#' and prints its name and contents.
#' 
#' \code{layout.graylog} is a special layout for use with the appender.graylog to
#' generate json acceptable to a Graylog2 HTTP GELF endpoint. Standard fields to
#' be included with every message can be included by setting the common.fields
#' to a list of properties. E.g.: 
#' 
#' flog.layout(layout.graylog(common.fields = list(host_ip = "10.10.11.23", 
#'                                                 env = "production")))
#' 
#' @name flog.layout
#' @aliases layout.simple layout.simple.parallel layout.format layout.tracearg layout.json layout.graylog
#' @param \dots Used internally by lambda.r
#' @author Brian Lee Yung Rowe
#' @seealso \code{\link{flog.logger}} \code{\link{flog.appender}}
#' @keywords data
#' @examples
#' # Set the layout for 'my.package'
#' flog.layout(layout.simple, name='my.package')
#'
#' # Update the ROOT logger to use a custom layout
#' layout <- layout.format('[~l] [~t] [~n.~f] ~m')
#' flog.layout(layout)
#' 
#' # Create a custom logger to trace variables
#' flog.layout(layout.tracearg, name='tracer')
#' x <- 5
#' flog.info(x, name='tracer')
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
  if (length(list(...)) > 0) {
    parsed <- lapply(list(...), function(x) if(is.null(x)) 'NULL' else x )
    msg <- do.call(sprintf, c(msg, parsed))
  }
  sprintf("%s [%s] %s\n", names(level),the.time, msg)
}

layout.simple.parallel <- function(level, msg, ...)
{
  the.time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  the.pid  <- Sys.getpid()
  if (length(list(...)) > 0) {
    parsed <- lapply(list(...), function(x) if(is.null(x)) 'NULL' else x)
    msg <- do.call(sprintf, c(msg, parsed))
  }
  sprintf("%s [%s %s] %s\n", names(level), the.time, the.pid, msg)
}

# Get name of a parent function in call stack
# @param .where: where in the call stack. -1 means parent of the caller.
.get.parent.func.name <- function(.where) {
  the.function <- tryCatch(deparse(sys.call(.where - 1)[[1]]), 
        error=function(e) "(shell)")
  the.function <- ifelse(
    length(grep('flog\\.',the.function)) == 0, the.function, '(shell)')

  the.function
}

# Generates a list object, then converts it to JSON and outputs it
layout.json <- function(level, msg, ...) {
  if (!requireNamespace("jsonlite", quietly=TRUE))
    stop("layout.json requires jsonlite. Please install it.", call.=FALSE)
  
  the.function <- .get.parent.func.name(-3) # get name of the function 
                                            # 3 deep in the call stack
  
  output_list <- list(
    level=jsonlite::unbox(names(level)),
    timestamp=jsonlite::unbox(format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
    message=jsonlite::unbox(msg),
    func=jsonlite::unbox(the.function),
    additional=...
  )
  paste0(jsonlite::toJSON(output_list, simplifyVector=TRUE), '\n')
}

# This parses and prints a user-defined format string. Available tokens are
# ~l - Log level
# ~t - Timestamp
# ~n - Namespace
# ~f - Calling function
# ~m - Message
# ~p - PID
#
# layout <- layout.format('[~l] [~t] [~n.~f] ~m')
# flog.layout(layout)
layout.format <- function(format, datetime.fmt="%Y-%m-%d %H:%M:%S")
{
  .where = -3 # get name of the function 3 deep in the call stack
              # that is, the function that has called flog.*
  function(level, msg, ...) {
    if (! is.null(substitute(...))) msg <- sprintf(msg, ...)
    the.level <- names(level)
    the.time <- format(Sys.time(), datetime.fmt)
    the.namespace <- flog.namespace(.where)
    the.namespace <- ifelse(the.namespace == 'futile.logger', 'ROOT', the.namespace)
    the.function <- .get.parent.func.name(.where)
    the.pid <- Sys.getpid()
    #pattern <- c('~l','~t','~n','~f','~m')
    #replace <- c(the.level, the.time, the.namespace, the.function, msg)
    message <- gsub('~l',the.level, format, fixed=TRUE)
    message <- gsub('~t',the.time, message, fixed=TRUE)
    message <- gsub('~n',the.namespace, message, fixed=TRUE)
    message <- gsub('~f',the.function, message, fixed=TRUE)
    message <- gsub('~m',msg, message, fixed=TRUE)
    message <- gsub('~p',the.pid, message, fixed=TRUE)
    sprintf("%s\n", message)
  }
}

layout.tracearg <- function(level, msg, ...)
{
  the.time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (is.character(msg)) {
    if (! is.null(substitute(...))) msg <- sprintf(msg, ...)
  } else {
    external.call <- sys.call(-2)
    external.fn <- eval(external.call[[1]])
    matched.call <- match.call(external.fn, external.call)
    matched.call <- matched.call[-1]
    matched.call.names <- names(matched.call)

    ## We are interested only in the msg and ... parameters,
    ## i.e. in msg and all parameters not explicitly declared
    ## with the function
    is.output.param <- matched.call.names == "msg" |
      !(matched.call.names %in% c(setdiff(names(formals(external.fn)), "...")))

    label <- lapply(matched.call[is.output.param], deparse)
    msg <- sprintf("%s: %s", label, c(msg, list(...)))
  }
  sprintf("%s [%s] %s\n", names(level),the.time, msg)
}


# This creates a json string that will work with the appender.graylog
layout.graylog <- function(common.fields, datetime.fmt="%Y-%m-%d %H:%M:%S")
{
  .where = -3 # get name of the function 3 deep in the call stack
  # that is, the function that has called flog.*
  
  missing.common.fields <- missing(common.fields)
  
  function(level, msg, ...) {
    
    if (! is.null(substitute(...))) msg <- sprintf(msg, ...)
    
    the.namespace <- flog.namespace(.where)
    
    output_list <- list(
      flogger_level = names(level),
      time = format(Sys.time(), datetime.fmt), 
      namespace = ifelse(the.namespace == 'futile.logger', 'ROOT', the.namespace),
      func = .get.parent.func.name(.where), 
      pid = Sys.getpid(),
      message = msg
    )
    
    if (!missing.common.fields)
      output_list <- c(output_list, common.fields)
    
    jsonlite::toJSON(output_list, auto_unbox = TRUE)
    
  }
}  