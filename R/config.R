# The logger options manager
logger.options <- OptionsManager('logger.options')

#config_logger %as% function(..., threshold=INFO, defaultLayout=simpleLayout)
#{
#  if (length(list(...)) < 1)
#    UseFunction('config_logger', threshold=threshold, defaultLayout=defaultLayout)
#  else
#    UseFunction('config_logger', ..., threshold=threshold, defaultLayout=defaultLayout)
#}


# The zero-argument default attempts to read a config file in the current
# directory
config_logger %when% TRUE
config_logger %as% function()
{
  config_logger(INFO, simpleLayout)
}

# A predefined logging config to write to the console. All examples use the
# simpleLayout by default.
config_logger %when% (is.numeric(threshold))
config_logger %as% function(threshold)
{
  config_logger(threshold, simpleLayout)
}

# A predefined logging config to write to the console. All examples use the
# simpleLayout by default.
config_logger %when% (is.numeric(threshold))
config_logger %as% function(threshold, defaultLayout)
{
  addLayout(defaultLayout)
  addAppender(consoleAppender)
  addLogger('ROOT',threshold, appender='consoleAppender',layout='defaultLayout')
}

config_logger %when% (is.character(config.file))
config_logger %as% function(config.file)
{
  config_logger(config.file,
    'customLogger', threshold=INFO, defaultLayout=simpleLayout)
}

# The requirement for a function name makes this slightly idiot-proof so as to
# not leave any gaping vulnerabilities.
config_logger %when% is.character(config.file)
config_logger %also% (length(grep('\\.R$',config.file)) > 0)
config_logger %also% is.character(fn.name)
config_logger %as% function(config.file, fn.name, threshold, defaultLayout)
{
  source(config.file)
  do.call(fn.name, list(threshold=threshold, defaultLayout=defaultLayout))
}

# A predefined logging config to write to a file. This example shows how names
# can be set arbitrarily rather than parsed from the function name.
#configAsFile <- function(file, threshold, defaultLayout)
config_logger %when% (is.character(file) & is.numeric(threshold))
config_logger %as% function(file, threshold, defaultLayout)
{
  if (is.null(file)) stop("Missing parameter 'file'")
  addLayout('default', defaultLayout)
  addAppender('file', fileAppender, file=file)
  addLogger('ROOT',threshold, appender='file', layout='default')
}

# Everything goes to console but only errors go to error.file
config_logger %when% (is.character(error.file) & is.numeric(threshold))
config_logger %as% function(error.file, threshold, defaultLayout)
{
  addLayout('default', defaultLayout)
  addAppender('console', consoleAppender)
  addAppender('error.file', fileAppender, file=error.file, threshold=WARN)
  addLogger('ROOT',threshold, appender=c('console','error.file'), layout='default')
}

# Provides a config to write to both console and file but only to the console
# if the log level is WARN or ERROR
config_logger %when% is.character(file)
config_logger %also% is.character(file.threshold)
config_logger %also% is.numeric(threshold)
config_logger %also% is.function(defaultLayout)
config_logger %as% function(file, file.threshold, threshold, defaultLayout)
{
  addLayout('default', defaultLayout)
  addAppender('console', consoleAppender, threshold)
  addAppender('file', fileAppender, file=file)
  addLogger('ROOT',file.threshold, appender=c('console','file'), layout='default')
}

