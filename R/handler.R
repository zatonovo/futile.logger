# Some default handlers for use in futile.logger. All handlers need to conform
# to the below signature:
#  function(level, msg, ..., formatter)
consoleHandler <- function(level, msg, ..., formatter)
{
  cat(formatter(level, msg, ...))
}

# Write to a file.
fileHandler <- function(level, msg, file, formatter)
{
  cat(formatter(level, msg, ...), file=file, append=TRUE)
}
