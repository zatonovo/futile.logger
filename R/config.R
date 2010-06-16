# The logger options manager
logger.options <- OptionsManager('logger.options')

configLogger <- function(..., threshold=INFO, defaultLayout=simpleLayout)
{
  if (length(list(...)) < 1)
    UseFunction('configLogger', threshold=threshold, defaultLayout=defaultLayout)
  else
    UseFunction('configLogger', ..., threshold=threshold, defaultLayout=defaultLayout)
}

register('configLogger',environment())

#cat("Current environment (B):", environmentName(environment()), '\n')

# The zero-argument default attempts to read a config file in the current
# directory
guard(configLogger.default, function(config.file) is.character(config.file))
configLogger.default <- function(config.file)
{
  cat("WARN: Calling configLogger(type) is deprecated")
  configLogger.source(config.file,
    'configLogger', threshold=INFO, defaultLayout=simpleLayout)
}

# The requirement for a function name makes this slightly idiot-proof so as to
# not leave any gaping vulnerabilities.
guard(configLogger.source, c(
  function(f,fn, t,dl) is.character(f) && is.character(fn.name),
  function(f,fn, t,dl) length(grep('\\.R$',f)) > 0
))
configLogger.source <- function(file, fn.name, threshold, defaultLayout)
{
  source(file)
  do.call(fn.name, list(threshold=threshold, defaultLayout=defaultLayout))
}

# A predefined logging config to write to the console. All examples use the
# simpleLayout by default.
#configAsConsole <- function(threshold, defaultLayout)
guard(configLogger.console, function(threshold, defaultLayout) is.numeric(threshold))
configLogger.console <- function(threshold, defaultLayout)
{
  if (! is.numeric(threshold)) stop("Invalid threshold specified")
  addLayout(defaultLayout)
  addAppender(consoleAppender)
  addLogger('ROOT',threshold, appender='consoleAppender',layout='defaultLayout')
}

# A predefined logging config to write to a file. This example shows how names
# can be set arbitrarily rather than parsed from the function name.
#configAsFile <- function(file, threshold, defaultLayout)
guard(configLogger.file, function(f,t,d) is.character(f) && is.numeric(t))
configLogger.file <- function(file, threshold, defaultLayout)
{
  if (is.null(file)) stop("Missing parameter 'file'")
  addLayout('default', defaultLayout)
  addAppender('file', fileAppender, file=file)
  addLogger('ROOT',threshold, appender='file', layout='default')
}

# Provides a config to write to both console and file but only to the console
# if the log level is WARN or ERROR
guard(configLogger.fileAndConsole, c(
  function(f,ft,t,dl) is.character(file) && is.character(file.threshold),
  function(f,ft,t,dl) is.numeric(t) && is.function(dl)
))
configLogger.fileAndConsole <- function(file, file.threshold, threshold, defaultLayout)
{
  addLayout('default', defaultLayout)
  addAppender('console', consoleAppender, threshold=WARN)
  addAppender('file', fileAppender, file=file)
  addLogger('ROOT',threshold, appender=c('console','file'), layout='default')
}

#configAsFileAndConsole <- function(file=NULL, threshold, defaultLayout)
#{
#  configAsConsoleAndFile(file, threshold, defaultLayout)
#}
configAsFileAndConsole <- function(file=NULL, threshold, defaultLayout)
{
  cat("WARN: This function is deprecated. Use configLogger\n")
  configLogger(file, threshold, defaultLayout)
}

# Provides a config to write to the console and a special log file for error
# messages routed to an error logger. All others go to a file.
configAsErrorAndFile <- function(log.file=NULL, err.file=NULL,
  threshold, defaultLayout)
{
  cat("WARN: This config is no longer supported. Use configLogger instead\n")
  invisible()
}
#guard(config.errorAndFile, 
#  function(lf,ef,t,d) is.character(lf) && is.character(ef) && is.character(t)
#)
#config.errorAndFile <- function(log.file, err.file, threshold, defaultLayout)
#{
#  addLayout('default', defaultLayout)
#  addAppender('console', consoleAppender, threshold=WARN)
#  addAppender('log.file', fileAppender, file=log.file)
#  addAppender('err.file', fileAppender, file=err.file)
#  addLogger('ROOT',threshold, appender=c('log.file','console'),
#    layout='default')
#  addLogger('error',WARN, appender='err.file', layout='default')
#}

configAsFileAndError <- function(log.file=NULL, err.file=NULL,
  threshold, defaultLayout)
{
  cat("WARN: This config is no longer supported. Use configLogger instead\n")
  invisible()
}
#{
#  configAsErrorAndFile(log.file, err.file, threshold, defaultLayout)
#}

# Only output errors to file. Everything goes to console
configAsError <- function(err.file=NULL, threshold, defaultLayout)
{
  cat("WARN: This function is deprecated. Use configLogger\n")
  configLogger(file, WARN, threshold, defaultLayout)
}

#configAsError <- function(err.file=NULL, threshold, defaultLayout)
#{
#  if (is.null(err.file)) stop("Missing parameter 'err.file'")
#
#  addLayout('default', defaultLayout)
#  addAppender('console', consoleAppender)
#  addAppender('err.file', fileAppender, file=err.file, threshold=WARN)
#  addLogger('ROOT',threshold, appender=c('err.file','console'),
#    layout='default')
#}


# Entry point to the pre-defined loggers
#configLogger <- function(type='console', ..., threshold=INFO, defaultLayout=simpleLayout)
#{
#  fn.name <- sprintf('configAs%s', paste(.LOGGERS[type], collapse='And'))
#  if (is.null(fn.name)) stop("Invalid config type specified")
#
#  do.call(fn.name, list(..., threshold=threshold, defaultLayout=defaultLayout))
#  invisible()
#}

