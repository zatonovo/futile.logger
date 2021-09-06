library(testthat)
library(futile.logger)

get_log_output <- function(expr) {
  o <- evaluate_promise(expr)
  o$message
}

test_check("futile.logger")
