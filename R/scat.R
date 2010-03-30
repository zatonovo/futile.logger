# Write to stdout using a format string
scat <- function(format, ..., use.newline=TRUE)
{
  if (use.newline) newline = '\n'
  else newline = ''

  cat(paste(sprintf(format, ...), newline, sep=''))
}

