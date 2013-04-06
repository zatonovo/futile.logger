.log_level <- function(msg, ..., level, name, capture)
{
  logger <- flog.logger(name)
  if (level > logger$threshold) { return(invisible()) }

  appender <- flog.appender(name)
  layout <- flog.layout(name)
  if (capture) {
    values <- capture.output(print(...))
    appender(c(layout(level, msg), values))
  } else {
    appender(layout(level, msg, ...))
  }
  invisible()
}

# Get the namespace that a function resides in. If no namespace exists, then
# return NULL.
# <environment: namespace:lambda.r>
get_namespace() %as% 
{
  s <- capture.output(str(environment(sys.function(1)), give.attr=FALSE))
  if (length(grep('namespace', s)) < 1) return('ROOT')

  ns <- sub('.*namespace:([^>]+)>.*','\\1', s)
  ifelse(is.null(ns), 'ROOT', ns)
}


flog.trace <- function(msg, ..., name=get_namespace(), capture=FALSE) {
  .log_level(msg, ..., level=TRACE,name=name, capture=capture)
}

flog.debug <- function(msg, ..., name=get_namespace(), capture=FALSE) {
  .log_level(msg, ..., level=DEBUG,name=name, capture=capture)
}

flog.info <- function(msg, ..., name=get_namespace(), capture=FALSE) {
  .log_level(msg, ..., level=INFO,name=name, capture=capture)
}

flog.warn <- function(msg, ..., name=get_namespace(), capture=FALSE) {
  .log_level(msg, ..., level=WARN,name=name, capture=capture)
}

flog.error <- function(msg, ..., name=get_namespace(), capture=FALSE) {
  .log_level(msg, ..., level=ERROR,name=name, capture=capture)
}

flog.fatal <- function(msg, ..., name=get_namespace(), capture=FALSE) {
  .log_level(msg, ..., level=FATAL,name=name, capture=capture)
}

# Get a logger. By default, use the package namespace or use the 'ROOT' logger.
flog.logger() %as%
{
  flog.logger(get_namespace())
}

flog.logger(name) %as%
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
  flog.logger(parent)
}

flog.logger(name, threshold=NULL, appender=NULL, layout=NULL) %as%
{
  logger <- flog.logger(name)
  if (!is.null(threshold)) logger$threshold <- threshold
  if (!is.null(appender)) logger$appender <- appender
  if (!is.null(layout)) logger$layout <- layout
  
  key <- paste("logger", name, sep='.')
  logger.options(update=list(key, logger))
  invisible()
}


flog.remove('ROOT') %as% { invisible() }
flog.remove(name) %as% 
{
  key <- paste("logger", name, sep='.')
  logger.options(update=list(key, NULL))
  invisible()
}

# Get the threshold for the given logger
flog.threshold(name) %::% character : numeric
flog.threshold(name='ROOT') %as%
{
  logger <- flog.logger(name)
  logger$threshold
}

# Set the threshold
flog.threshold(threshold, name='ROOT') %as%
{
  flog.logger(name, threshold=threshold)
  invisible()
}

# Get appenders associated with the given logger
flog.appender(name) %::% character : Function
flog.appender(name='ROOT') %as%
{
  logger <- flog.logger(name)
  logger$appender
}

# Set the appender for the given logger
flog.appender(fn, name='ROOT') %as%
{
  flog.logger(name, appender=fn)
  invisible()
}

# Get the layout for the given logger
flog.layout(name) %::% character : Function
flog.layout(name='ROOT') %as%
{
  logger <- flog.logger(name)
  logger$layout
}

# Set the layout
flog.layout(fn, name='ROOT') %as%
{
  flog.logger(name, layout=fn)
  invisible()
}


