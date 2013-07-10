#' Manage appenders within the 'futile.logger' sub-system
#' 
#' Provides functions for managing appenders. Typically only addAppender is
#' used when manually creating a logging configuration.
#' 
#' Appenders do the actual work of writing log messages to some target. To use
#' an appender in a logger, you must register it to a given logger. Use the
#' 'flog.appender' function to both access and set appenders.
#' 
#' flog.appender(name) \%::\% character : Function 
#' 
#' flog.appender(name='ROOT')
#' 
#' 
#' flog.appender(fn, name) \%::\% Function : character : Null
#' 
#' flog.appender(fn, name='ROOT')
#' 
#' 'appender.console' is a function that writes to the console. No additional
#' arguments are necessary when registering the appender via flog.appender.
#' 
#' flog.appender(appender.console(), name='my.logger')
#' 
#' 'appender.file' writes to a file, so you must pass an additional file
#' argument to the function.
#' 
#' flog.appender(appender.file('output.log'), name='my.logger')
#' 
#' To use your own appender create a function that takes a single argument,
#' which represents the log message. You need to pass a function reference to
#' flog.appender.
#' 
#' @name flog.appender
#' @aliases appender.console appender.file
#' @param \dots Used internally by lambda.r
#' @return When getting the appender, 'flog.appender' returns the appender
#' function.  When setting an appender, 'flog.appender' has no return value.
#' @author Brian Lee Yung Rowe
#' @keywords data
#' @examples
#' 
#' # Set an appender to the logger named 'my.package'. Any log operations from
#' # this package will now use this appender.
#' flog.appender(appender.file('my.package.out'), 'my.package')
#' 
NULL

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
