# :vim set ff=R
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


context("null values")
test_that("Raw null value is printed", {
  raw <- capture.output(flog.info('xxx[%s]xxx', NULL))
  expect_that(length(grep('xxx[NULL]xxx', raw)) == 0, is_true())
})

test_that("Single null value is printed", {
  opt <- list()
  raw <- capture.output(flog.info('xxx[%s]xxx', opt$noexist))
  expect_that(length(grep('xxx[NULL]xxx', raw)) == 0, is_true())
})

test_that("Null is printed amongst variables", {
  opt <- list()
  raw <- capture.output(flog.info('aaa[%s]aaa xxx[%s]xxx', 3, opt$noexist))
  expect_that(length(grep('aaa[3]aaa', raw)) == 0, is_true())
  expect_that(length(grep('xxx[NULL]xxx', raw)) == 0, is_true())
})
