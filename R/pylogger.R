#' @keywords data
#' @name logger.setup
#' @title Set Up Python-Style Logging Namespaces
#' @author Jonathan Callahan
#' @param traceLog file name or full path where \code{logger.trace()} messages will be sent
#' @param debugLog file name or full path where \code{logger.debug()} messages will be sent
#' @param infoLog file name or full path where \code{logger.info()} messages will be sent
#' @param warnLog file name or full path where \code{logger.warn()} messages will be sent
#' @param errorLog file name or full path where \code{logger.error()} messages will be sent
#' @param fatalLog file name or full path where \code{logger.fatal()} messages will be sent
#' @return No return value.
#' @description Python style logging allows developers to create log files at different levels
#' so that an \code{errorLog} will contain only log messages at the \code{ERROR} level while
#' a \code{debugLog} will contain log messages at the \code{DEBUG} level as well as all higher
#' levels.
#' 
#' Python-style log files are set up with \code{logger.setup()}. Logs can be set up for any
#' combination of log levels. Accepting the default \code{NULL} setting for any log file
#' simply means that log file will not be created.
#' 
#' Python-style logging requires the use of \code{logger.debug()} style logging statements as seen
#' in the example below.
#' @examples
#' \dontrun{
#' logger.setup(debugLog='debug.log', infoLog='info.log', errorLog='error.log')
#' logger.trace('trace statement')
#' logger.debug('debug statement')
#' logger.info('info statement')
#' logger.warn('warn statement')
#' logger.error('error statement')
#' logger.fatal('fatal statement')
#' }
#' @seealso \code{\link{logger.trace}} \code{\link{logger.debug}}  \code{\link{logger.info}}
#' \code{\link{logger.warn}} \code{\link{logger.error}} \code{\link{logger.fatal}}

# Set up logging namespaces
logger.setup <- function(traceLog=NULL,
                           debugLog=NULL,
                           infoLog=NULL,
                           warnLog=NULL,
                           errorLog=NULL,
                           fatalLog=NULL) {
  
  # By default, the console receives only FATAL messages.
  flog.threshold(FATAL)
  
  # Set up TRACE logging
  if ( is.null(traceLog) ) {
    invisible( flog.logger("trace", TRACE, appender.null()) )
  } else {
    if ( file.exists(traceLog) ) result <- file.remove(traceLog)
    invisible( flog.logger("trace", TRACE, appender.file(traceLog)) )
  }
  
  # Set up DEBUG logging
  if ( is.null(debugLog) ) {
    invisible( flog.logger("debug", DEBUG, appender.null()) )
  } else {
    if ( file.exists(debugLog) ) result <- file.remove(debugLog)
    invisible( flog.logger("debug", DEBUG, appender.file(debugLog)) )
  }
  
  # Set up INFO logging
  if ( is.null(infoLog) ) {
    invisible( flog.logger("info", INFO, appender.null()) )
  } else {
    if ( file.exists(infoLog) ) result <- file.remove(infoLog)
    invisible( flog.logger("info", INFO, appender.file(infoLog)) )
  }
  
  # Set up WARN logging
  if ( is.null(warnLog) ) {
    invisible( flog.logger("warn", WARN, appender.null()) )
  } else {
    if ( file.exists(warnLog) ) result <- file.remove(warnLog)
    invisible( flog.logger("warn", WARN, appender.file(warnLog)) )
  }
  
  # Set up ERROR logging
  if ( is.null(errorLog) ) {
    invisible( flog.logger("error", ERROR, appender.null()) )
  } else {
    if ( file.exists(errorLog) ) result <- file.remove(errorLog)
    invisible( flog.logger("error", ERROR, appender.file(errorLog)) )
  }
  
  # Set up FATAL logging
  if ( is.null(fatalLog) ) {
    invisible( flog.appender(appender.console(), name='ROOT') )
  } else {
    if ( file.exists(fatalLog) ) result <- file.remove(fatalLog)
    invisible( flog.appender(appender.tee(fatalLog)) )
  }
  
}

#' @keywords data
#' @name logger.trace
#' @aliases logger.debug logger.info logger.warn logger.error logger.fatal
#' @title Python-Style Logging STatements
#' @author Jonathan Callahan
#' @param \dots arguments to \code{flog.debug(...)} style log statement
#' @param name ignored -- included to match \code{flog.debug(...)} signature
#' @return No return value.
#' @description After initializing the level-specific namespaces with \code{logger.setup(...)}
#' these statements can be used in exactly the same way as the equivalent \code{flog.debug(...)} style statements.
#' 
#' Log statements will end up in all appropriate log files, emulating Python-style logging.
#' @examples
#' \dontrun{
#' logger.setup(debugLog='debug.log', infoLog='info.log', errorLog='error.log')
#' logger.trace('trace statement')
#' logger.debug('debug statement')
#' logger.info('info statement')
#' logger.warn('warn statement')
#' logger.error('error statement')
#' logger.fatal('fatal statement')
#' }
#' @seealso \code{\link{logger.setup}}

# Log at the TRACE level
logger.trace <- function(..., name='ROOT') {
  flog.trace(..., name='trace')
}

# Log at the DEBUG level
logger.debug <- function(..., name='ROOT') {
  flog.debug(..., name='trace')
  flog.debug(..., name='debug')
}

# Log at the INFO level
logger.info <- function(..., name='ROOT') {
  flog.info(..., name='trace')
  flog.info(..., name='debug')
  flog.info(..., name='info')
}

# Log at the WARN level
logger.warn <- function(..., name='ROOT') {
  flog.warn(..., name='trace')
  flog.warn(..., name='debug')
  flog.warn(..., name='info')
  flog.warn(..., name='warn')
}

# Log at the ERROR level
logger.error <- function(..., name='ROOT') {
  flog.error(..., name='trace')
  flog.error(..., name='debug')
  flog.error(..., name='info')
  flog.error(..., name='warn')
  flog.error(..., name='error')
}

# Log at the fatal level
logger.fatal <- function(..., name='ROOT') {
  flog.fatal(..., name='trace')
  flog.fatal(..., name='debug')
  flog.fatal(..., name='info')
  flog.fatal(..., name='warn')
  flog.fatal(..., name='error')
  flog.fatal(...)
}
