context("root logger")
test_that("Default settings", {
  flog.threshold(INFO)
  raw <- get_log_output(flog.info("log message"))
  #cat("\n[test.default] Raw:",raw,"\n")
  expect_true(length(grep('INFO', raw)) > 0)
  expect_true(length(grep('log message', raw)) > 0)
})

test_that("Change root threshold", {
  flog.threshold(ERROR)
  raw <- get_log_output(flog.info("log message"))
  #cat("\n[test.change_threshold] Raw:",raw,"\n")
  expect_true(length(raw) == 0)
})


context("capture output")
test_that("Capture works as expected", {
  flog.threshold(INFO)
  raw <- get_log_output(flog.info("log message", head(cars), capture = TRUE))
  expect_true(length(raw) == 1)
  expect_true(grepl('^INFO',raw))
  expect_true(grepl('dist',raw))
  })

test_that("Get threshold names", {
  flog.threshold(ERROR)
  expect_true(flog.threshold() == "ERROR")  
  flog.threshold(DEBUG, name='my.package')
  expect_true(flog.threshold(name='my.package') == "DEBUG") 
})


context("new logger")
test_that("Create new logger", {
  flog.threshold(ERROR)
  flog.threshold(DEBUG, name='my.package')
  raw.root <- get_log_output(flog.info("log message"))
  raw.mine <- get_log_output(flog.info("log message", name='my.package'))
  expect_true(length(raw.root) == 0)
  expect_true(length(grep('INFO', raw.mine)) > 0)
  expect_true(length(grep('log message', raw.mine)) > 0)
})

context("logger hierarchy")
test_that("Hierarchy is honored", {
  flog.threshold(ERROR)
  flog.threshold(TRACE, name='my')
  flog.threshold(DEBUG, name='my.package')
  raw.root <- get_log_output(flog.info("log message"))
  raw.l1 <- get_log_output(flog.trace("log message", name='my'))
  raw.l2 <- get_log_output(flog.trace("log message", name='my.package'))
  expect_true(length(raw.root) == 0)
  expect_true(length(grep('TRACE', raw.l1)) > 0)
  expect_true(length(grep('log message', raw.l1)) > 0)
  expect_true(length(raw.l2) == 0)
})

test_that("Hierarchy inheritance", {
  flog.remove('my.package')
  flog.remove('my')
  flog.threshold(ERROR)
  flog.threshold(TRACE, name='my')
  raw.root <- get_log_output(flog.info("log message"))
  raw.l1 <- get_log_output(flog.trace("log message", name='my'))
  raw.l2 <- get_log_output(flog.trace("log message", name='my.package'))
  expect_true(length(raw.root) == 0)
  expect_true(length(grep('TRACE', raw.l1)) > 0)
  expect_true(length(grep('log message', raw.l1)) > 0)
  expect_true(length(grep('TRACE', raw.l2)) > 0)
  expect_true(length(grep('log message', raw.l2)) > 0)
})


# Can't test this since test_that calls suppressMessages
#context("package loggers")
#test_that("ftry captures warnings", {
  #raw <- get_log_output(ftry(log(-1)))
  #cat("raw =",raw,'\n')
  #expect_true(length(grep('WARN', raw)) > 0)
  #expect_true(length(grep('simpleWarning', raw)) > 0)
#})

context("carp")
test_that("carp returns output", {
  on.exit(flog.carp(FALSE))

  flog.carp(TRUE)
  expect_true(flog.carp())
  flog.threshold(WARN)
  raw <- flog.debug("foo")
  flog.carp(FALSE)
  expect_true(length(grep('DEBUG', raw)) > 0)
})

context("logger provided explicitly")
test_that("logger can be provided explicitly", {
  flog.threshold(DEBUG, name='my.package')
  my_logger <- flog.logger('my.package')
  with_logger <- get_log_output(flog.info("log message", logger=my_logger))
  expect_true(length(grep('INFO', with_logger)) > 0)
  expect_true(length(grep('log message', with_logger)) > 0)
})
test_that("logger provided explicitly is much faster if nothing has to be logged", {
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
