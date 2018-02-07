context("root logger")
test_that("Default settings", {
  flog.threshold(INFO)
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


context("capture output")
test_that("Capture works as expected", {
  flog.threshold(INFO)
  raw <- capture.output(flog.info("log message", head(cars), capture = TRUE))
  expect_that(length(raw) == 9, is_true())
  expect_that(grepl('^INFO',raw[1]), is_true())
  expect_that(nchar(raw[2]) == 0, is_true())
  expect_that(grepl('dist$',raw[3]), is_true())
  })

test_that("Get threshold names", {
  flog.threshold(ERROR)
  expect_that(flog.threshold() == "ERROR", is_true())  
  flog.threshold(DEBUG, name='my.package')
  expect_that(flog.threshold(name='my.package') == "DEBUG", is_true()) 
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

context("carp")
test_that("carp returns output", {
  expect_that(flog.carp(), is_false())
  flog.carp(TRUE)
  flog.threshold(WARN)
  raw <- flog.debug("foo")
  flog.carp(FALSE)
  expect_that(length(grep('DEBUG', raw)) > 0, is_true())
})

context("logger passed explicitely")
test_that("logger can be provided explicitely", {
  flog.threshold(DEBUG, name='my.package')
  my_logger <- flog.logger('my.package')
  with_logger <- capture.output(flog.info("log message", logger=my_logger))
  expect_that(length(grep('INFO', with_logger)) > 0, is_true())
  expect_that(length(grep('log message', with_logger)) > 0, is_true())
})
test_that("logger provided explicitely is much faster if nothing has to be logged", {
  flog.threshold(INFO, name='my.package')
  my_logger <- flog.logger('my.package')
  fun_wn <- function(i) {
    flog.debug("step %d", i, name='my.package')
    i
  }
  fun_wl <- function(i) {
    flog.debug("step %d", i, logger=my_logger)
    i
  }
  tw <- system.time(for (i in 1:10000) fun_wn(i))["elapsed"]
  tl <- system.time(for (i in 1:10000) fun_wl(i))["elapsed"]
  expect_true(tw > tl*10)
})
