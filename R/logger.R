.log.level <- function(msg, ..., level, name)
{
  logger <- log.logger(name)
  if (level > logger$threshold) { return(invisible()) }

  appender <- log.appender(name)
  layout <- log.layout(name)
  appender(layout(level, msg, ...))
  invisible()
}

log.trace <- function(msg, ..., name='ROOT')
  .log.level(msg, ..., level=TRACE,name=name)
log.debug <- function(msg, ..., name='ROOT')
  .log.level(msg, ..., level=DEBUG,name=name)
log.info <- function(msg, ..., name='ROOT')
  .log.level(msg, ..., level=INFO,name=name)
log.warn <- function(msg, ..., name='ROOT')
  .log.level(msg, ..., level=WARN,name=name)
log.error <- function(msg, ..., name='ROOT')
  .log.level(msg, ..., level=ERROR,name=name)
log.fatal <- function(msg, ..., name='ROOT')
  .log.level(msg, ..., level=FATAL,name=name)

# Get a logger
log.logger(name='ROOT') %as%
{
  if (nchar(name) < 1) name <- 'ROOT'
  #cat(sprintf("Searching for logger %s\n", name))

  key <- paste("logger", name, sep='.')
  # TODO: Search hierarchy
  os <- logger.options(key)
  if (! is.null(os)) return(os)
  if (name == 'ROOT') {
    logger <- list(name=name,
      threshold=INFO, 
      appender=appender.console(),
      layout=layout.simple)
    logger.options(update=list(key, logger))
    return(logger)
  }

  parts <- strsplit(name, '.', fixed=TRUE)[[1]]
  parent <- paste(parts[1:length(parts)-1], collapse='.')
  log.logger(parent)
}

log.logger(name, threshold=NULL, appender=NULL, layout=NULL) %as%
{
  logger <- log.logger(name)
  if (!is.null(threshold)) logger$threshold <- threshold
  if (!is.null(appender)) logger$appender <- appender
  if (!is.null(layout)) logger$layout <- layout
  
  key <- paste("logger", name, sep='.')
  logger.options(update=list(key, logger))
  invisible()
}


log.remove('ROOT') %as% { invisible() }
log.remove(name) %as% 
{
  logger.options(update=list(key, NULL))
  invisible()
}
seal(log.logger)
seal(log.remove)

# Get the threshold for the given logger
log.threshold(name) %::% character : numeric
log.threshold(name='ROOT') %as%
{
  logger <- log.logger(name)
  logger$threshold
}

# Set the threshold
log.threshold(threshold, name='ROOT') %as%
{
  log.logger(name, threshold=threshold)
  invisible()
}

# Get appenders associated with the given logger
#log.appender(name) %::% character : Function
log.appender(name='ROOT') %as%
{
  logger <- log.logger(name)
  logger$appender
}

# Set the appender for the given logger
log.appender(fn, name='ROOT') %as%
{
  log.logger(name, appender=fn)
  invisible()
}

# Get the layout for the given logger
#log.layout(name) %::% character : Function
log.layout(name='ROOT') %as%
{
  logger <- log.logger(name)
  logger$layout
}

# Set the layout
log.layout(fn, name='ROOT') %as%
{
  log.logger(name, layout=fn)
  invisible()
}

seal(log.threshold)
seal(log.appender)
seal(log.layout)

