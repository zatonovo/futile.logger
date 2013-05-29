context("root logger")
test_that("Default settings", {
  raw <- capture.output(flog.info("log message"))
  #cat("\n[test.default] Raw:",raw,"\n")
  expect_that(length(grep('INFO', raw)) > 0, is_true())
  expect_that(length(grep('log message', raw)) > 0, is_true())
})

test_that("Change root threshold", {
  flog.threshold(ERROR)
  raw <- capture.output(flog.info("log message"))
  #cat("\n[test.change_threshold] Raw:",raw,"\n")
  expect_that(length(raw) == 0, is_true())
})

context("new logger")
test_that("Create new logger", {
  flog.threshold(ERROR)
  flog.threshold(DEBUG, name='my.package')
  raw.root <- capture.output(flog.info("log message"))
  raw.mine <- capture.output(flog.info("log message", name='my.package'))
  expect_that(length(raw.root) == 0, is_true())
  expect_that(length(grep('INFO', raw.mine)) > 0, is_true())
  expect_that(length(grep('log message', raw.mine)) > 0, is_true())
})

context("logger hierarchy")
test_that("Hierarchy is honored", {
  flog.threshold(ERROR)
  flog.threshold(TRACE, name='my')
  flog.threshold(DEBUG, name='my.package')
  raw.root <- capture.output(flog.info("log message"))
  raw.l1 <- capture.output(flog.trace("log message", name='my'))
  raw.l2 <- capture.output(flog.trace("log message", name='my.package'))
  expect_that(length(raw.root) == 0, is_true())
  expect_that(length(grep('TRACE', raw.l1)) > 0, is_true())
  expect_that(length(grep('log message', raw.l1)) > 0, is_true())
  expect_that(length(raw.l2) == 0, is_true())
})

test_that("Hierarchy inheritance", {
  flog.remove('my.package')
  flog.remove('my')
  flog.threshold(ERROR)
  flog.threshold(TRACE, name='my')
  raw.root <- capture.output(flog.info("log message"))
  raw.l1 <- capture.output(flog.trace("log message", name='my'))
  raw.l2 <- capture.output(flog.trace("log message", name='my.package'))
  expect_that(length(raw.root) == 0, is_true())
  expect_that(length(grep('TRACE', raw.l1)) > 0, is_true())
  expect_that(length(grep('log message', raw.l1)) > 0, is_true())
  expect_that(length(grep('TRACE', raw.l2)) > 0, is_true())
  expect_that(length(grep('log message', raw.l2)) > 0, is_true())
})


# Can't test this since test_that calls suppressMessages
#context("package loggers")
#test_that("ftry captures warnings", {
  #raw <- capture.output(ftry(log(-1)))
  #cat("raw =",raw,'\n')
  #expect_that(length(grep('WARN', raw)) > 0, is_true())
  #expect_that(length(grep('simpleWarning', raw)) > 0, is_true())
#})

context("format string")
test_that("Embedded format string", {
  flog.threshold(INFO)
  raw <- capture.output(flog.info("This is a %s message", "log"))
  #cat("\n[test.default] Raw:",raw,"\n")
  expect_that(length(grep('INFO', raw)) > 0, is_true())
  expect_that(length(grep('log message', raw)) > 0, is_true())
})

test_that("Custom layout dereferences level field", {
  flog.threshold(INFO)
  flog.layout(layout.format('xxx[~l]xxx'))
  raw <- capture.output(flog.info("log message"))
  flog.layout(layout.simple)
  expect_that('xxx[INFO]xxx' == raw, is_true())
  expect_that(length(grep('log message', raw)) == 0, is_true())
})

context("carp")
test_that("carp returns output", {
  expect_that(flog.carp(), is_false())
  flog.carp(TRUE)
  flog.threshold(WARN)
  raw <- flog.debug("foo")
  expect_that(length(grep('DEBUG', raw)) > 0, is_true())
})

