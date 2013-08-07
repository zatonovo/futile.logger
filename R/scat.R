#' Print formatted messages
#' 
#' A replacement for \code{cat} that has built-in sprintf formatting
#' 
#' Like \code{cat} but you can use format strings.
#' 
#' @param format A format string passed to sprintf
#' @param use.newline Whether to append a new line at the end
#' @param \dots Arguments to pass to sprintf for dereferencing
#' @return A formatted string printed to the console
#' @author Brian Lee Yung Rowe
#' @keywords data
#' @examples
#' 
#'   apply(array(2:5),1, function(x) scat('This has happened %s times', x) )
#' 
scat <- function(format, ..., use.newline=TRUE)
{
  if (use.newline) newline = '\n'
  else newline = ''

  cat(paste(sprintf(format, ...), newline, sep=''))
}

