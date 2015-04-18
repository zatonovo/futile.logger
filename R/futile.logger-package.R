#' A Logging Utility for R
#' 
#' This package implements a logging system inspired by log4j. The basic idea
#' of layouts, appenders, and loggers is faithful to log4j, while the
#' implementation and idiom is all R. This means that support for hierarchical
#' loggers, custom appenders, custom layouts is coupled with a simple and
#' intuitive functional syntax.
#' 
#' \tabular{ll}{
#' Package: \tab futile.logger\cr
#' Type: \tab Package\cr
#' Version: \tab 1.4.1\cr
#' Date: \tab 2015-04-18\cr
#' License: \tab LGPL-3\cr
#' LazyLoad: \tab yes\cr
#' }
#' 
#' The latest version of futile.logger introduces zero-configuration semantics
#' out of the box. This means that you can use the default configuration as is.
#' It is also easy to interactively change the configuration of the ROOT
#' logger, as well as create new loggers. Since loggers form a hierarchy based
#' on their name, the ROOT logger is the starting point of the hierarchy and
#' always exists. By default the ROOT logger is defined with a simple layout,
#' printing to the console, with an INFO threshold. This means that writing to
#' any logger with a threshold of INFO or higher will write to the console.
#' 
#' All of the logging functions take a format string so it is easy to add
#' arbitrary values to log messages.
#' 
#' > flog.info("This song is just \%s words \%s", 7, "long")
#' 
#' Thresholds range from most verbose to least verbose: TRACE, DEBUG, INFO,
#' WARN, ERROR, FATAL. You can easily change the threshold of the ROOT logger
#' by calling > flog.threshold(TRACE) which changes will print all log messages
#' from every package. To suppress most logging by default but turn on all
#' debugging for a logger 'my.logger', you would execute 
#'
#' > flog.threshold(ERROR)\cr
#' > flog.threshold(TRACE, name='my.logger')
#'
#' Any arbitrary logger can be defined simply by specifying it in any
#' futile.logger write operation (futile.threshold, futile.appender,
#' futile.layout). If the logger hasn't been defined, then it will be defined
#' dynamically. Any unspecified options will be copied from the parent logger.
#' 
#' When writing log messages, futile.logger will search the hierarchy based on
#' the logger name. In our example, if 'my.logger' hasn't been defined then
#' futile.logger will look for a logger named 'my' and finally the ROOT logger.
#' 
#' Functions calling futile.logger from a package are automatically assigned a
#' logger that has the name of the package. Suppose we have log messages in a
#' package called 'my.package'. Then any function that calls futile.logger from
#' within the package will automatically be assigned a default logger of
#' 'my.package' instead of ROOT. This means that it is easy to change the log
#' setting of any package that uses futile.logger for logging by just updating
#' the logger for the given package. For instance suppose you want to output
#' log message for my.package to a file instead.
#' 
#' > flog.appender(appender.file('my.package.log'), name='my.package')
#' 
#' Now all log statements in the package my.package will be written to a file
#' instead of the console. All other log messages will continue to be written
#' to the console.
#' 
#' Appenders do the actual work of writing log messages to a writeable target,
#' whether that is a console, a file, a URL, database, etc. When creating an
#' appender, the implementation-specific options are passed to the appender at
#' instantiation. The package defines two appender generator functions:
#' 
#' \describe{
#'   \item{appender.file}{Write to a file}
#'   \item{appender.console}{Write to the console}
#' }
#' 
#' Each of these functions returns the actual appender function, so be sure to
#' actually call the function!
#' 
#' Layouts are responsible for formatting messages. This operation usually
#' consists of adding the log level, a timestamp, plus some pretty-printing to
#' make the log messages easy on the eyes. The package supplies several layouts:
#' 
#' \describe{
#'   \item{layout.simple}{Writes messages with a default format}
#'   \item{layout.json}{Generates messages in a JSON format}
#'   \item{layout.format}{Define your own format}
#'   \item{layout.tracearg}{Print a variable name along with its value}
#' }
#' 
#' @name futile.logger-package
#' @aliases futile.logger-package futile.logger flog.namespace
#' @docType package
#' @exportPattern "^[^\\.]"
#' @import lambda.r futile.options
#' @author Brian Lee Yung Rowe <r@@zatonovo.com>
#' @seealso \code{\link{flog.logger}}, \code{\link{flog.threshold}},
#' \code{\link{flog.layout}}, \code{\link{flog.appender}}
#' @keywords package attribute logic
#' @examples
#' 
#' flog.debug("This %s print", "won't")
#' flog.warn("This %s print", "will")
#'   
#' flog.info("This inherits from the ROOT logger", name='logger.a')
#' flog.threshold(DEBUG, name='logger.a')
#' flog.debug("logger.a has now been set to DEBUG", name='logger.a')
#' flog.debug("But the ROOT logger is still at INFO (so this won't print)")
#' 
#' \dontrun{
#' flog.appender(appender.file("other.log"), name='logger.b')
#' flog.info("This writes to a %s", "file", name='logger.b')
#' }
#' 
NULL
