# Some default handlers for use in futile.logger. All handlers need to conform
# to the below signature: function(line)
appender.console <- function()
{
  function(line) cat(line)
}

# Write to a file.
appender.file <- function(file)
{
  function(line) cat(line, file=file, append=TRUE)
}
