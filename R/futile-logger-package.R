#' An easy to use logging package for R.
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
#' Version: \tab 1.3.3\cr
#' Date: \tab 2013-07-09\cr
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
#' debugging for a logger 'my.logger', you would execute >
#' flog.threshold(ERROR) > flog.threshold(TRACE, name='my.logger')
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
#' flog.appender(appender.file('my.package.log'), name='my.package')
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
#' . appender.file - Write to a file . appender.console - Write to the console
#' 
#' Each of these functions returns the actual appender function, so be sure to
#' actually call the function!
#' 
#' Layouts are responsible for formatting messages. This operation usually
#' consists of adding the log level, a timestamp, plus some pretty-printing to
#' make the log messages easy on the eyes. The package supplies two layouts:
#' 
#' . layout.simple - Writes messages with a default format . layout.format -
#' Define your own format
#' 
#' @name futile.logger-package
#' @aliases futile.logger-package futile.logger get_namespace
#' @docType package
#' @exportPattern "^[^\\.]"
#' @author Brian Lee Yung Rowe <r@@zatonovo.com>
#' @seealso \code{\link{flog.logger}}, \code{\link{flog.threshold}},
#' \code{\link{flog.layout}}, \code{\link{flog.appender}}
#' @keywords package attribute logic
#' @examples
#' 
#'   flog.debug("This %s print", "won't")
#'   flog.warn("This %s print", "will")
#'   
#'   flog.info("This inherits from the ROOT logger", name='logger.a')
#'   flog.threshold(DEBUG, name='logger.a')
#'   flog.debug("logger.a has now been set to DEBUG", name='logger.a')
#'   flog.debug("But the ROOT logger is still at INFO (so this won't print)")
#' 
#'   flog.appender(appender.file("other.log"), name='logger.b')
#'   flog.info("This writes to a %s", "file", name='logger.b')
#' 
NULL





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
#' > flog.debug("This won't print") > flog.info("But this %s", 'will') >
#' flog.warn("As will %s", 'this')
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
#' > flog.appender(appender.file("foo.log"), name='my') > flog.threshold(ERROR,
#' name='my.logger') > flog.info("This won't print", name='my.logger') >
#' flog.error("This %s print to a file", 'will', name='my.logger')
#' 
#' If you define a logger that you later want to remove, use flog.remove.
#' 
#' The option 'capture' allows you to print out more complicated data
#' structures without a lot of ceremony. This variant doesn't accept format
#' strings and instead appends the value to the next line of output. Consider >
#' m <- matrix(rnorm(12), nrow=3) > flog.info("Matrix:",m, capture=TRUE) which
#' preserves the formatting, whereas using capture=FALSE will have a cluttered
#' output due to recycling.
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
#'   flog.threshold(DEBUG)
#'   flog.debug("This debug message will print")
#' 
#'   flog.threshold(WARN)
#'   flog.debug("This one won't")
#' 
#'   m <- matrix(rnorm(12), nrow=3)
#'   flog.info("Matrix:",m, capture=TRUE)
#' 
#'   ftry(log(-1))
#' 
NULL



