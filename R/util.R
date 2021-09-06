# From
# https://github.com/zatonovo/futile.logger/issues/91
.message_to_console <- function(line) {
  msg <- paste(line, collapse="")
  cond <- simpleMessage(msg)
  class(cond) <- c("futile.logger.message", class(cond))
  message(cond)
}

.cat_to_console <- function(line) {
  cat(line, sep='')
}

.get_console_writer <- function() {
  legacy <- Sys.getenv('FUTILE_LOGGER_LEGACY',FALSE)
  if (legacy) { return(.cat_to_console) }
  .message_to_console
}


.write_to_console <- .get_console_writer()
