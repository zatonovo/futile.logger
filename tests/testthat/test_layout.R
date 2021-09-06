# :vim set ff=R

## record default layout so that we can restore later
default.layout <- flog.layout()

context("format string")
test_that("Embedded format string", {
  flog.threshold(INFO)
  raw <- get_log_output(flog.info("This is a %s message", "log"))
  #cat("\n[test.default] Raw:",raw,"\n")
  expect_true(length(grep('INFO', raw)) > 0)
  expect_true(length(grep('log message', raw)) > 0)
})

test_that("layout.simple.parallel layout", {
  flog.threshold(INFO)
  flog.layout(layout.simple.parallel)
  raw <- get_log_output(flog.info("log message"))

  expect_true(length(paste0('INFO [[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2} ', Sys.getpid(), '] log message') ==  raw) > 0)
  expect_true(length(grep('log message', raw)) > 0)
})

test_that("~p token gets correct PID", {
  flog.threshold(INFO)
  flog.layout(layout.format('xxx[~l ~p]xxx'))
  raw <- get_log_output(flog.info("log message"))

  expect_true(paste0('xxx[INFO ',Sys.getpid(),']xxx\n') == raw)
  expect_true(length(grep('log message', raw)) == 0)
})

test_that("~i token (logger name)", {
  flog.threshold(INFO)
  flog.layout(layout.format('<~i> ~m'))
  # with no logger name 
  # (perhaps due to the behavior of flog.namespace for nested function call, 
  #  logger name becomes "base" instead of "futile.logger" 
  #  when no name is given.  this test is currently disabled)
  #out <- flog.info("log message")
  #out <- sub('[[:space:]]+$', '', out)  # remove line break at end
  #expect_equal(out, '<ROOT> log message')

  # with logger name
  out <- get_log_output(flog.info("log message", name='mylogger'))
  expect_equal(out, '<mylogger> log message\n')
})

test_that("Custom layout dereferences level field", {
  flog.threshold(INFO)
  flog.layout(layout.format('xxx[~l]xxx'))
  raw <- get_log_output(flog.info("log message"))

  expect_true('xxx[INFO]xxx\n' == raw)
  expect_true(length(grep('log message', raw)) == 0)
})

context("null values")
flog.layout(default.layout)
test_that("Raw null value is printed", {
  raw <- get_log_output(flog.info('xxx[%s]xxx', NULL))
  expect_true(length(grep('xxx[NULL]xxx', raw, fixed=TRUE)) == 1)
})

test_that("Single null value is printed", {
  opt <- list()
  raw <- get_log_output(flog.info('xxx[%s]xxx', opt$noexist))
  expect_true(length(grep('xxx[NULL]xxx', raw, fixed=TRUE)) == 1)
})

test_that("Null is printed amongst variables", {
  opt <- list()
  raw <- get_log_output(flog.info('aaa[%s]aaa xxx[%s]xxx', 3, opt$noexist))
  expect_true(length(grep('aaa[3]aaa', raw, fixed=TRUE)) == 1)
  expect_true(length(grep('xxx[NULL]xxx', raw, fixed=TRUE)) == 1)
})

context("sprintf arguments")
test_that("no extra arguments are passed", {
  expect_true(grepl('foobar\n$', get_log_output(flog.info('foobar'))))
  expect_true(grepl('foobar %s\n$', get_log_output(flog.info('foobar %s'))))
  expect_true(grepl('10%\n$', get_log_output(flog.info('10%'))))
})

test_that("some extra arguments are passed", {
  expect_true(grepl('foobar\n$', get_log_output(flog.info('foobar', pi))))
  expect_true(grepl(
    'foobar foo\n$',
    get_log_output(flog.info('foobar %s', 'foo'))))
  expect_true(grepl('100\n$', get_log_output(flog.info('10%d', 0))))
  expect_true(
    grepl('foo and bar equals to foobar\n',
          get_log_output(
            flog.info('%s and %s equals to %s', 'foo', 'bar', 'foobar'))))
})

context("Function name and namespace detection")
test_that("Function name detection inside nested functions", {
    flog.layout(layout.format("[~f] ~m"))
    a <- function() { flog.info("inside A") }
    b <- function() { a() }
    d <- function() { b() }
    e <- function() { d() }
    expect_equal('[a] inside A\n', get_log_output(e()))
})

drop_log_prefix <- function(msg) {
    sub('[A-Z]* \\[.*\\] ', '', msg)
}

context("glue layout")
flog.layout(layout.glue)
test_that("glue features work", {
  expect_equal(
    drop_log_prefix(get_log_output(flog.info('foobar'))), 'foobar\n')
  expect_equal(
    drop_log_prefix(get_log_output(flog.info('{a}{b}', a = 'foo', b = 'bar'))),
    'foobar\n')
  expect_equal(
    drop_log_prefix(get_log_output(flog.info('foo{b}', b = 'bar'))),
    'foobar\n')

  b <- 'bar'
  expect_equal(
    drop_log_prefix(get_log_output(flog.info('foo{b}'))), 'foobar\n')
  rm(b)

  expect_equal(
    drop_log_prefix(get_log_output(flog.info('foo', 'bar'))), 'foobar\n')
})

## back to the default layout
invisible(flog.layout(default.layout))
