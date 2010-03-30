# The logger options manager
logger.options <- OptionsManager('logger.options')

# A predefined logging config to write to the console. All examples use the
# simpleLayout by default.
configAsConsole <- function(threshold=INFO, defaultLayout=simpleLayout)
{
  addLayout(defaultLayout)
  addAppender(consoleAppender)
  addLogger('ROOT',threshold, appender='consoleAppender', layout='defaultLayout')
}

# A predefined logging config to write to a file. This example shows how names
# can be set arbitrarily rather than parsed from the function name.
configAsFile <- function(file, threshold=INFO, defaultLayout=simpleLayout)
{
  addLayout('default', defaultLayout)
  addAppender('file', fileAppender, file=file)
  addLogger('ROOT',threshold, appender='file', layout='default')
}

# Provides a config to write to both console and file but only to the console
# if the log level is WARN or ERROR
configAsConsoleAndFile <- function(file, threshold=INFO, defaultLayout=simpleLayout)
{
  addLayout('default', defaultLayout)
  addAppender('console', consoleAppender, threshold=WARN)
  addAppender('file', fileAppender, file=file)
  addLogger('ROOT',threshold, appender=c('console','file'), layout='default')
}

# Provides a config to write to the console and a special log file for error
# messages routed to an error logger. All others go to a file.
configAsErrorConsole <- function(log.file, err.file, 
  threshold=INFO, defaultLayout=simpleLayout)
{
  addLayout('default', defaultLayout)
  addAppender('console', consoleAppender)
  addAppender('log.file', fileAppender, file=log.file)
  addAppender('err.file', fileAppender, file=err.file)
  addLogger('ROOT',threshold, appender='log.file', layout='default')
  addLogger('error',WARN, appender='err.file', layout='default')
}


