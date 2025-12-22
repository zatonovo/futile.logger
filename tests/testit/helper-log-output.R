# This file is automatically sourced by testthat for each test file
# This is best practice for testthat and also enables testing during package development in RStudio
#
# See also:
# https://github.com/zatonovo/futile.logger/issues/99
# https://stackoverflow.com/a/41777777/

get_log_output <- function(expr) {
  o <- evaluate_promise(expr)
  o$message
}
