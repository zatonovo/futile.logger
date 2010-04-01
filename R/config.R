# The logger options manager
logger.options <- OptionsManager('logger.options')
.LOGGERS <- c('Console','File','Error')
names(.LOGGERS) <- c('console','file','error')

# A predefined logging config to write to the console. All examples use the
# simpleLayout by default.
configAsConsole <- function(threshold, defaultLayout)
{
  if (! is.numeric(threshold)) stop("Invalid threshold specified")
  addLayout(defaultLayout)
  addAppender(consoleAppender)
  addLogger('ROOT',threshold, appender='consoleAppender', layout='defaultLayout')
}

# A predefined logging config to write to a file. This example shows how names
# can be set arbitrarily rather than parsed from the function name.
configAsFile <- function(file, threshold, defaultLayout)
{
  if (is.null(file)) stop("Missing parameter 'file'")
  addLayout('default', defaultLayout)
  addAppender('file', fileAppender, file=file)
  addLogger('ROOT',threshold, appender='file', layout='default')
}

# Provides a config to write to both console and file but only to the console
# if the log level is WARN or ERROR
configAsConsoleAndFile <- function(file=NULL, threshold, defaultLayout)
{
  if (is.null(file)) stop("Missing parameter 'file'")
  addLayout('default', defaultLayout)
  addAppender('console', consoleAppender, threshold=WARN)
  addAppender('file', fileAppender, file=file)
  addLogger('ROOT',threshold, appender=c('console','file'), layout='default')
}

configAsFileAndConsole <- function(file=NULL, threshold, defaultLayout)
{
  configAsConsoleAndFile(file, threshold, defaultLayout)
}

# Provides a config to write to the console and a special log file for error
# messages routed to an error logger. All others go to a file.
configAsErrorAndFile <- function(log.file=NULL, err.file=NULL,
  threshold, defaultLayout)
{
  if (is.null(log.file)) stop("Missing parameter 'log.file'")
  if (is.null(err.file)) stop("Missing parameter 'err.file'")

  addLayout('default', defaultLayout)
  addAppender('console', consoleAppender, threshold=WARN)
  addAppender('log.file', fileAppender, file=log.file)
  addAppender('err.file', fileAppender, file=err.file)
  addLogger('ROOT',threshold, appender=c('log.file','console'),
    layout='default')
  addLogger('error',WARN, appender='err.file', layout='default')
}

configAsFileAndError <- function(log.file=NULL, err.file=NULL,
  threshold, defaultLayout)
{
  configAsErrorAndFile(log.file, err.file, threshold, defaultLayout)
}

# Only output errors to file. Everything goes to console
configAsError <- function(err.file=NULL, threshold, defaultLayout)
{
  if (is.null(err.file)) stop("Missing parameter 'err.file'")

  addLayout('default', defaultLayout)
  addAppender('console', consoleAppender)
  addAppender('err.file', fileAppender, file=err.file, threshold=WARN)
  addLogger('ROOT',threshold, appender=c('err.file','console'),
    layout='default')
}


# Entry point to the pre-defined loggers
configLogger <- function(type='console', ..., threshold=INFO, defaultLayout=simpleLayout)
{
  fn.name <- sprintf('configAs%s', paste(.LOGGERS[type], collapse='And'))
  if (is.null(fn.name)) stop("Invalid config type specified")

  do.call(fn.name, list(..., threshold=threshold, defaultLayout=defaultLayout))
  invisible()
}

