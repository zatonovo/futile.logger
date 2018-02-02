#' Manage appenders for loggers
#' 
#' Provides functions for adding and removing appenders.
#' 
#' @section Usage:
#' # Get the appender for the given logger\cr
#' flog.appender(name) \%::\% character : Function\cr
#' flog.appender(name='ROOT')
#'
#' # Set the appender for the given logger\cr
#' flog.appender(fn, name='ROOT')
#'
#' # Print log messages to the console\cr
#' appender.console()
#' 
#' # Write log messages to a file\cr
#' appender.file(file)
#' 
#' # Write log messages to a dynamically-named file\cr
#' appender.file2(format)
#' 
#' # Write log messages to console and a file\cr
#' appender.tee(file)
#' 
#' # Write log messages to a Graylog2 HTTP GELF endpoint\cr
#' appender.graylog(server, port)
#' 
#' # Special meta appender that prints only when the internal counter mod n = 0\cr
#' appender.modulo(n, appender=appender.console())
#'
#' @section Details:
#' Appenders do the actual work of writing log messages to some target.
#' To use an appender in a logger, you must register it to a given logger.
#' Use \code{flog.appender} to both access and set appenders.
#' 
#' The ROOT logger by default uses \code{appender.console}.
#' 
#' \code{appender.console} is a function that writes to the console.
#' No additional arguments are necessary when registering the appender 
#' via flog.appender.
#' 
#' \code{appender.file} writes to a file, so you must pass an additional file
#' argument to the function. To change the file name, just call
#' \code{flog.appender(appender.file(file))} again with a new file name.
#'
#' \code{appender.file2} is similar, but the filename is dynamically
#' determined at runtime. It may include most of the same tokens as
#' \code{layout.format} (all except \code{"~m"}, the message
#' itself). This allows, for instance, having separate logfiles for
#' each log level.
#' 
#' To use your own appender create a function that takes a single argument,
#' which represents the log message. You need to pass a function reference to
#' \code{flog.appender}.
#' 
#' \code{appender.tee} writes to both the console and file.
#' 
#' \code{appender.graylog} writes to a Graylog2 HTTP GELF endpoint.
#'
#' \code{appender.modulo} is a meta appender. It calls \code{appender} every \code{n} times.
#'
#' @section Value:
#' When getting the appender, \code{flog.appender} returns the appender
#' function.  When setting an appender, \code{flog.appender} has no 
#' return value.
#'
#' @name flog.appender
#' @aliases appender.console appender.file appender.file2 appender.tee appender.modulo appender.graylog
#' @param \dots Used internally by lambda.r
#' @author Brian Lee Yung Rowe
#' @seealso \code{\link{flog.logger}} \code{\link{flog.layout}}
#' @keywords data
#' @examples
#' \dontrun{
#' flog.appender(appender.console(), name='my.logger')
#'
#' # Set an appender to the logger named 'my.package'. Any log operations from
#' # this package will now use this appender.
#' flog.appender(appender.file('my.package.out'), 'my.package')
#'
#' # Set an appender to a file named using the message level and calling function.
#' # Also tee the messages to the console.
#' flog.appender(appender.file2('~l-~f.log', console = TRUE))
#' }

# Get appenders associated with the given logger
flog.appender(name) %::% character : Function
flog.appender(name='ROOT') %as%
{
  logger <- flog.logger(name)
  logger$appender
}

# Set the appender for the given logger
flog.appender(fn, name='ROOT') %as%
{
  flog.logger(name, appender=fn)
  invisible()
}

# Some default handlers for use in futile.logger. All handlers need to conform
# to the below signature: function(line)
appender.console <- function()
{
  function(line) cat(line, sep='')
}

# Write to a file.
appender.file <- function(file)
{
  function(line) cat(line, file=file, append=TRUE, sep='')
}

# Write to a file and to console 
appender.tee <- function(file){ 
  function(line) {
    cat(line, sep='') 
    cat(line, file=file, append=TRUE, sep='')
  }
}

# Write to a dynamically-named file (and optionally the console), with inheritance
appender.file2 <- function(format, console=FALSE, inherit=TRUE,
                           datetime.fmt="%Y%m%dT%H%M%S") {
  .nswhere <- -3 # get name of the function 2 deep in the call stack
                 # that is, the function that has called flog.*
  .funcwhere <- -3 # ditto for the function name
  .levelwhere <- -1 # ditto for the current "level"
  function(line) {
    if (console) cat(line, sep='')
    err <- function(e) {
      stop('Illegal function call, must call from flog.trace, flog.debug, flog.info, flog.warn, flog.error, flog.fatal, etc.')
    }
    the.level <- tryCatch(get("level", envir=sys.frame(.levelwhere)),error = err)
    the.threshold <- tryCatch(get('logger',envir=sys.frame(.levelwhere)), error=err)$threshold
    if(inherit) {
      LEVELS <- c(FATAL, ERROR, WARN, INFO, DEBUG, TRACE)
      levels <- names(LEVELS[the.level <= LEVELS & LEVELS <= the.threshold])
    } else levels <- names(the.level)
    the.time <- format(Sys.time(), datetime.fmt)
    the.namespace <- flog.namespace(.nswhere)
    the.namespace <- ifelse(the.namespace == 'futile.logger', 'ROOT', the.namespace)
    the.function <- .get.parent.func.name(.funcwhere)
    the.pid <- Sys.getpid()
    filename <- gsub('~t', the.time, format, fixed=TRUE)
    filename <- gsub('~n', the.namespace, filename, fixed=TRUE)
    filename <- gsub('~f', the.function, filename, fixed=TRUE)
    filename <- gsub('~p', the.pid, filename, fixed=TRUE)
    if(length(grep('~l', filename)) > 0) {
      sapply(levels, function(level) {
        filename <- gsub('~l', level, filename, fixed=TRUE)
        cat(line, file=filename, append=TRUE, sep='')
      })
    }else cat(line, file=filename, append=TRUE, sep='')
    invisible()
  }
}


# Special meta appender that prints only when the internal counter mod n = 0
appender.modulo <- function(n, appender=appender.console()) {
  i <- 0
  function(line) {
    i <<- i + 1
    if (i %% n == 0) appender(sprintf("[%s] %s", i,line))
    invisible()
  }
}

# Write to a Graylog2 HTTP GELF Endpoint
appender.graylog <- function(server, port, debug = FALSE) {

  if (!requireNamespace("jsonlite", quietly=TRUE))
    stop("appender.graylog requires jsonlite. Please install it.", call. = FALSE)
  if (!requireNamespace("httr", quietly=TRUE))
    stop("appender.graylog requires httr. Please install it.", call. = FALSE)  
  
  function(line) {

    ret <- httr::POST(paste0("http://", server, ":", port, "/gelf"), 
                      body = list(short_message = line),
                      encode = 'json')
    
    if (debug) print(ret)
  }
}
