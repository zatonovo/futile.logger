# The logger options manager
logger.options <- OptionsManager('logger.options')

configLogger <- function(..., threshold=INFO, defaultLayout=simpleLayout)
{
  if (length(list(...)) < 1)
    UseFunction('configLogger', threshold=threshold, defaultLayout=defaultLayout)
  else
    UseFunction('configLogger', ..., threshold=threshold, defaultLayout=defaultLayout)
}

# This is obsolete
#register('configLogger',environment())

# The zero-argument default attempts to read a config file in the current
# directory
guard(configLogger.default, is.character(config.file))
configLogger.default <- function(config.file)
{
  cat("WARN: Calling configLogger(type) is deprecated")
  configLogger.source(config.file,
    'customLogger', threshold=INFO, defaultLayout=simpleLayout)
}

# The requirement for a function name makes this slightly idiot-proof so as to
# not leave any gaping vulnerabilities.
guard(configLogger.source,
  is.character(config.file) & length(grep('\\.R$',config.file)) > 0 &
  is.character(fn.name)
)
configLogger.source <- function(config.file, fn.name, threshold, defaultLayout)
{
  source(config.file)
  do.call(fn.name, list(threshold=threshold, defaultLayout=defaultLayout))
}

# A predefined logging config to write to the console. All examples use the
# simpleLayout by default.
#configAsConsole <- function(threshold, defaultLayout)
guard(configLogger.console, is.numeric(threshold))
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
guard(configLogger.file, is.character(file) & is.numeric(threshold))
configLogger.file <- function(file, threshold, defaultLayout)
{
  if (is.null(file)) stop("Missing parameter 'file'")
  addLayout('default', defaultLayout)
  addAppender('file', fileAppender, file=file)
  addLogger('ROOT',threshold, appender='file', layout='default')
}

# Everything goes to console but only errors go to error.file
guard(configLogger.error, is.character(error.file) & is.numeric(threshold))
configLogger.error <- function(error.file, threshold, defaultLayout)
{
  addLayout('default', defaultLayout)
  addAppender('console', consoleAppender)
  addAppender('error.file', fileAppender, file=error.file, threshold=WARN)
  addLogger('ROOT',threshold, appender=c('console','error.file'), layout='default')
}

# Provides a config to write to both console and file but only to the console
# if the log level is WARN or ERROR
guard(configLogger.fileAndConsole, 
  is.character(file) & is.character(file.threshold) &
  is.numeric(threshold) & is.function(defaultLayout)
)
configLogger.fileAndConsole <- function(file, file.threshold, threshold, defaultLayout)
{
  addLayout('default', defaultLayout)
  addAppender('console', consoleAppender, threshold)
  addAppender('file', fileAppender, file=file)
  addLogger('ROOT',file.threshold, appender=c('console','file'), layout='default')
}

########################## DEPRECATED FUNCTIONS ###############################

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

configAsFileAndError <- function(log.file=NULL, err.file=NULL,
  threshold, defaultLayout)
{
  cat("WARN: This config is no longer supported. Use configLogger instead\n")
  invisible()
}

# Only output errors to file. Everything goes to console
configAsError <- function(err.file=NULL, threshold, defaultLayout)
{
  cat("WARN: This function is deprecated. Use configLogger\n")
  configLogger(file, WARN, threshold, defaultLayout)
}

