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


context("package loggers")


context("format string")
test_that("Embedded format string", {
  flog.threshold(INFO)
  raw <- capture.output(flog.info("This is a %s message", "log"))
  #cat("\n[test.default] Raw:",raw,"\n")
  expect_that(length(grep('INFO', raw)) > 0, is_true())
  expect_that(length(grep('log message', raw)) > 0, is_true())
})

