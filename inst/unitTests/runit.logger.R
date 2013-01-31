test.default <- function() {
  raw <- capture.output(flog.info("log message"))
  cat("\n[test.default] Raw:",raw,"\n")
  checkTrue(length(grep('INFO', raw)) > 0)
  checkTrue(length(grep('log message', raw)) > 0)
}

test.change_threshold <- function() {
  flog.threshold(ERROR)
  raw <- capture.output(flog.info("log message"))
  cat("\n[test.change_threshold] Raw:",raw,"\n")
  checkTrue("NULL" == raw)
}

