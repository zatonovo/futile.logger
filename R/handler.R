# Some default handlers for use in futile.logger. All handlers need to conform
# to the below signature:
#  function(level, msg, ..., formatter)
console.handler <- function(level, msg, formatter)
{
  cat(formatter(level, msg, ...))
}

# Write to a file.
file.handler <- function(level, msg, file, formatter)
{
  cat(msg, file=file, append=TRUE)
}
