# This file provides some standard formatters
# This prints out a string in the following format:
#   LEVEL [timestamp] message
layout.simple <- function(level, msg, ...)
{
  the.time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (! is.null(substitute(...))) msg <- sprintf(msg, ...)
  sprintf("%s [%s] %s\n", names(level),the.time, msg)
}


# This parses and prints a user-defined format string. Available tokens are
# ~l - Log level
# ~t - Timestamp
# ~n - Namespace
# ~f - Calling function
# ~m - Message
#
# layout <- layout.format('[~l] [~t] [~n.~f] ~m')
# flog.layout(layout)
layout.format <- function(format, datetime.fmt="%Y-%m-%d %H:%M:%S")
{
  where <- 1
  function(level, msg, ...) {
    if (! is.null(substitute(...))) msg <- sprintf(msg, ...)
    the.level <- names(level)
    the.time <- format(Sys.time(), datetime.fmt)
    the.namespace <- get_namespace()
    #print(sys.calls())
    the.function <- tryCatch(sys.call(where)[[1]], error=function(e) "(shell)")
    #pattern <- c('~l','~t','~n','~f','~m')
    #replace <- c(the.level, the.time, the.namespace, the.function, msg)
    message <- gsub('~l',the.level, format, fixed=TRUE)
    message <- gsub('~t',the.time, message, fixed=TRUE)
    message <- gsub('~n',the.namespace, message, fixed=TRUE)
    message <- gsub('~f',the.function, message, fixed=TRUE)
    message <- gsub('~m',msg, message, fixed=TRUE)
    sprintf("%s\n", message)
  }
}

