
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



# This file provides some standard formatters
# This prints out a string in the following format:
#   LEVEL [timestamp] message
layout.simple <- function(level, msg, id='', ...)
{
  the.time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (length(list(...)) > 0) {
    parsed <- lapply(list(...), function(x) if(is.null(x)) 'NULL' else x )
    msg <- do.call(sprintf, c(msg, parsed))
  }
  sprintf("%s [%s] %s\n", names(level),the.time, msg)
}







# Generates a list object, then converts it to JSON and outputs it
layout.json <- function(user_id,session_id){


the.user_id <- ifelse(user_id %in% c('', 'futile.logger'), 'ROOT', user_id) 
the.session_id<- ifelse(session_id %in% c('', 'futile.logger'), 'ROOT', session_id) 


function(level, msg, id='', ...) {
  if (!requireNamespace("jsonlite", quietly=TRUE))
    stop("layout.json requires jsonlite. Please install it.", call.=FALSE)
  
  the.function <- .get.parent.func.name(-3) # get name of the function 
                                            # 3 deep in the call stack
  the.id <- ifelse(id %in% c('', 'futile.logger'), 'ROOT', id) 
  
  
  output_list <- list(
    app_name=jsonlite::unbox(the.id),
    
    user_id=jsonlite::unbox(the.user_id),
    session_id=jsonlite::unbox(the.session_id),
    
    
    level=jsonlite::unbox(names(level)),
    timestamp=jsonlite::unbox(format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
    calling_function=jsonlite::unbox(the.function),
    message=jsonlite::unbox(msg),
    additional=...
  )
  paste0(jsonlite::toJSON(output_list, simplifyVector=TRUE), '\n')
}
}





# This parses and prints a user-defined format string. Available tokens are
# ~l - Log level
# ~t - Timestamp
# ~n - Namespace
# ~f - Calling function
# ~m - Message
# ~p - PID
#
# layout <- layout.format('[~l] [~t] [~n.~f] ~m')
# flog.layout(layout)
layout.format <- function(format, datetime.fmt="%Y-%m-%d %H:%M:%S")
{
  .where = -3 # get name of the function 3 deep in the call stack
              # that is, the function that has called flog.*
  function(level, msg, id='', ...) {
    if (! is.null(substitute(...))) msg <- sprintf(msg, ...)
    the.level <- names(level)
    the.time <- format(Sys.time(), datetime.fmt)
    the.namespace <- flog.namespace(.where)
    the.namespace <- ifelse(the.namespace == 'futile.logger', 'ROOT', the.namespace)
    the.function <- .get.parent.func.name(.where)
    the.pid <- Sys.getpid()
    the.id <- ifelse(id %in% c('', 'futile.logger'), 'ROOT', id) 
    #pattern <- c('~l','~t','~n','~f','~m')
    #replace <- c(the.level, the.time, the.namespace, the.function, msg)
    message <- gsub('~l',the.level, format, fixed=TRUE)
    message <- gsub('~t',the.time, message, fixed=TRUE)
    message <- gsub('~n',the.namespace, message, fixed=TRUE)
    message <- gsub('~f',the.function, message, fixed=TRUE)
    message <- gsub('~m',msg, message, fixed=TRUE)
    message <- gsub('~p',the.pid, message, fixed=TRUE)
    message <- gsub('~i',the.id, message, fixed=TRUE)
    sprintf("%s\n", message)
  }
}


