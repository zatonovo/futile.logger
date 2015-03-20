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
#' # Write log messages to console and a file\cr
#' appender.tee(file)
#' 
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
#' 
#' \code{appender.file} writes to a file, so you must pass an additional file
#' argument to the function. To change the file name, just call
#' \code{flog.appender(appender.file(file))} again with a new file name.
#' 
#' To use your own appender create a function that takes a single argument,
#' which represents the log message. You need to pass a function reference to
#' \code{flog.appender}.
#' 
#' \code{appender.tee} writes to both the console and file.
#'
#' @section Value:
#' When getting the appender, \code{flog.appender} returns the appender
#' function.  When setting an appender, \code{flog.appender} has no 
#' return value.
#'
#' @name flog.appender
#' @aliases appender.console appender.file appender.tee
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
