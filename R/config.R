# The logger options manager
logger.options <- OptionsManager('logger.options')

config_logger <- function(..., threshold=INFO, defaultLayout=simpleLayout)
{
  if (length(list(...)) < 1)
    UseFunction('config_logger', threshold=threshold, defaultLayout=defaultLayout)
  else
    UseFunction('config_logger', ..., threshold=threshold, defaultLayout=defaultLayout)
}

# This is obsolete
#register('config_logger',environment())

# The zero-argument default attempts to read a config file in the current
# directory
config_logger.default %when% (is.character(config.file))
config_logger.default <- function(config.file)
{
  cat("WARN: Calling config_logger(type) is deprecated")
  config_logger.source(config.file,
    'customLogger', threshold=INFO, defaultLayout=simpleLayout)
}

# The requirement for a function name makes this slightly idiot-proof so as to
# not leave any gaping vulnerabilities.
config_logger.source %when% (is.character(config.file) &
  length(grep('\\.R$',config.file)) > 0 & is.character(fn.name))
config_logger.source <- function(config.file, fn.name, threshold, defaultLayout)
{
  source(config.file)
  do.call(fn.name, list(threshold=threshold, defaultLayout=defaultLayout))
}

# A predefined logging config to write to the console. All examples use the
# simpleLayout by default.
#configAsConsole <- function(threshold, defaultLayout)
config_logger.console %when% (is.numeric(threshold))
config_logger.console <- function(threshold, defaultLayout)
{
  if (! is.numeric(threshold)) stop("Invalid threshold specified")
  addLayout(defaultLayout)
  addAppender(consoleAppender)
  addLogger('ROOT',threshold, appender='consoleAppender',layout='defaultLayout')
}

# A predefined logging config to write to a file. This example shows how names
# can be set arbitrarily rather than parsed from the function name.
#configAsFile <- function(file, threshold, defaultLayout)
config_logger.file %when% (is.character(file) & is.numeric(threshold))
config_logger.file <- function(file, threshold, defaultLayout)
{
  if (is.null(file)) stop("Missing parameter 'file'")
  addLayout('default', defaultLayout)
  addAppender('file', fileAppender, file=file)
  addLogger('ROOT',threshold, appender='file', layout='default')
}

# Everything goes to console but only errors go to error.file
config_logger.error %when% (is.character(error.file) & is.numeric(threshold))
config_logger.error <- function(error.file, threshold, defaultLayout)
{
  addLayout('default', defaultLayout)
  addAppender('console', consoleAppender)
  addAppender('error.file', fileAppender, file=error.file, threshold=WARN)
  addLogger('ROOT',threshold, appender=c('console','error.file'), layout='default')
}

# Provides a config to write to both console and file but only to the console
# if the log level is WARN or ERROR
config_logger.fileAndConsole %when% (is.character(file) &
  is.character(file.threshold) & is.numeric(threshold) &
  is.function(defaultLayout))
config_logger.fileAndConsole <- function(file, file.threshold, threshold, defaultLayout)
{
  addLayout('default', defaultLayout)
  addAppender('console', consoleAppender, threshold)
  addAppender('file', fileAppender, file=file)
  addLogger('ROOT',file.threshold, appender=c('console','file'), layout='default')
}

# This is for backwards compatibility
configLogger <- config_logger
