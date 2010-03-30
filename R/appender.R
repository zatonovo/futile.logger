# Some default handlers for use in futile.logger. All handlers need to conform
# to the below signature:
#  function(level, msg, ..., layout)
consoleAppender <- function(level, msg, ..., threshold, layout)
{
  if (! is.null(threshold) && level > threshold) { return(0) }
  cat(layout(level, msg, ...))
}

# Write to a file.
fileAppender <- function(level, msg, file, ..., threshold, layout)
{
  if (! is.null(threshold) && level > threshold) { return(0) }
  cat(layout(level, msg, ...), file=file, append=TRUE)
}
