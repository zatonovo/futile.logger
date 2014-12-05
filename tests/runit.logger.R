test.default <- function() {
  raw <- capture.output(flog.info("log message"))
  cat("\n[test.default] Raw:",raw,"\n")
  checkTrue(length(grep('INFO', raw)) > 0)
  checkTrue(length(grep('log message', raw)) > 0)
}

test.trace <- function() {
  flog.threshold(DEBUG)
  raw <- capture.output(flog.trace("log message"))
  checkTrue("NULL" == raw)

  flog.threshold(TRACE)
  raw <- capture.output(flog.trace("log message"))
  checkTrue(length(grep('TRACE', raw)) > 0)
  checkTrue(length(grep('log message', raw)) > 0)
}

test.debug <- function() {
  flog.threshold(INFO)
  raw <- capture.output(flog.debug("log message"))
  checkTrue("NULL" == raw)

  flog.threshold(DEBUG)
  raw <- capture.output(flog.debug("log message"))
  checkTrue(length(grep('DEBUG', raw)) > 0)
  checkTrue(length(grep('log message', raw)) > 0)

  flog.threshold(TRACE)
  raw <- capture.output(flog.debug("log message"))
  checkTrue(length(grep('DEBUG', raw)) > 0)
  checkTrue(length(grep('log message', raw)) > 0)
}

test.info <- function() {
  flog.threshold(WARN)
  raw <- capture.output(flog.info("log message"))
  checkTrue("NULL" == raw)

  flog.threshold(INFO)
  raw <- capture.output(flog.info("log message"))
  checkTrue(length(grep('INFO', raw)) > 0)
  checkTrue(length(grep('log message', raw)) > 0)

  flog.threshold(DEBUG)
  raw <- capture.output(flog.info("log message"))
  checkTrue(length(grep('INFO', raw)) > 0)
  checkTrue(length(grep('log message', raw)) > 0)
}

test.warn <- function() {
  flog.threshold(ERROR)
  raw <- capture.output(flog.warn("log message"))
  checkTrue("NULL" == raw)

  flog.threshold(WARN)
  raw <- capture.output(flog.warn("log message"))
  checkTrue(length(grep('WARN', raw)) > 0)
  checkTrue(length(grep('log message', raw)) > 0)

  flog.threshold(INFO)
  raw <- capture.output(flog.warn("log message"))
  checkTrue(length(grep('WARN', raw)) > 0)
  checkTrue(length(grep('log message', raw)) > 0)
}

test.error <- function() {
  flog.threshold(FATAL)
  raw <- capture.output(flog.error("log message"))
  checkTrue("NULL" == raw)

  flog.threshold(ERROR)
  raw <- capture.output(flog.error("log message"))
  checkTrue(length(grep('ERROR', raw)) > 0)
  checkTrue(length(grep('log message', raw)) > 0)

  flog.threshold(WARN)
  raw <- capture.output(flog.error("log message"))
  checkTrue(length(grep('ERROR', raw)) > 0)
  checkTrue(length(grep('log message', raw)) > 0)

}

test.fatal <- function() {
  flog.threshold(FATAL)
  raw <- capture.output(flog.fatal("log message"))
  checkTrue(length(grep('FATAL', raw)) > 0)
  checkTrue(length(grep('log message', raw)) > 0)

  flog.threshold(ERROR)
  raw <- capture.output(flog.fatal("log message"))
  checkTrue(length(grep('FATAL', raw)) > 0)
  checkTrue(length(grep('log message', raw)) > 0)
}

test.ftry <- function() {
  raw <- capture.output(ftry(log(-1)))
  checkTrue(length(grep('WARN', raw)) > 0)
  checkTrue(length(grep('simpleWarning', raw)) > 0)
}

test.carp <- function() {
  checkTrue(! flog.carp())
  flog.carp(TRUE)
  flog.threshold(WARN)
  raw <- flog.debug("foo")
  checkTrue(length(grep('DEBUG', raw)) > 0)
}

test.layout.1 <- function() {
  flog.threshold(INFO)
  flog.layout(layout.format('xxx[~l]xxx'))
  raw <- capture.output(flog.info("log message"))
  checkTrue(length(grep('xxx[INFO]xxx', raw)) > 0)
  checkTrue(length(grep('log message', raw)) == 0)
}
