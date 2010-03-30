# The logger options manager
logger.options <- OptionsManager('logger.options')

# A predefined logging config to write to the console. All examples use the
# simpleFormatter by default.
configAsConsole <- function(threshold=INFO, defaultFormatter=simpleFormatter)
{
  addFormatter(defaultFormatter)
  addHandler(consoleHandler)
  addLogger('ROOT',threshold, handler='consoleHandler', formatter='defaultFormatter')
}

# A predefined logging config to write to a file. This example shows how names
# can be set arbitrarily rather than parsed from the function name.
configAsFile <- function(file, threshold=INFO, defaultFormatter=simpleFormatter)
{
  addFormatter('default', defaultFormatter)
  addHandler('file', fileHandler, file=file)
  addLogger('ROOT',threshold, handler='file', formatter='default')
}

# Provides a config to write to both console and file but only to the console
# if the log level is WARN or ERROR
configAsConsoleAndFile <- function(file, threshold=INFO, defaultFormatter=simpleFormatter)
{
  addFormatter('default', defaultFormatter)
  addHandler('console', consoleHandler, threshold=WARN)
  addHandler('file', fileHandler, file=file)
  addLogger('ROOT',threshold, handler=c('console','file'), formatter='default')
}

# Provides a config to write to the console and a special log file for error
# messages routed to an error logger. All others go to a file.
configAsErrorConsole <- function(log.file, err.file, 
  threshold=INFO, defaultFormatter=simpleFormatter)
{
  addFormatter('default', defaultFormatter)
  addHandler('console', consoleHandler)
  addHandler('log.file', fileHandler, file=log.file)
  addHandler('err.file', fileHandler, file=err.file)
  addLogger('ROOT',threshold, handler='log.file', formatter='default')
  addLogger('error',WARN, handler='err.file', formatter='default')
}


