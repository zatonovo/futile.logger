# :vim set ff=R

## record default layout so that we can restore later
default.layout <- flog.layout()

assert("Embedded format string", {
  flog.threshold(INFO)
  raw <- get_log_output(flog.info("This is a %s message", "log"))
  #cat("\n[test.default] Raw:",raw,"\n")
  (length(grep('INFO', raw)) > 0)
  (length(grep('log message', raw)) > 0)
})

assert("layout.simple.parallel layout", {
  flog.threshold(INFO)
  flog.layout(layout.simple.parallel)
  raw <- get_log_output(flog.info("log message"))

  (length(paste0('INFO [[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2} ', Sys.getpid(), '] log message') ==  raw) > 0)
  (length(grep('log message', raw)) > 0)
})

assert("~p token gets correct PID", {
  flog.threshold(INFO)
  flog.layout(layout.format('xxx[~l ~p]xxx'))
  raw <- get_log_output(flog.info("log message"))

  (paste0('xxx[INFO ',Sys.getpid(),']xxx\n') == raw)
  (length(grep('log message', raw)) == 0)
})

assert("~i token (logger name)", {
  flog.threshold(INFO)
  flog.layout(layout.format('<~i> ~m'))
  # with no logger name 
  # (perhaps due to the behavior of flog.namespace for nested function call, 
  #  logger name becomes "base" instead of "futile.logger" 
  #  when no name is given.  this test is currently disabled)
  #out <- flog.info("log message")
  #out <- sub('[[:space:]]+$', '', out)  # remove line break at end
  #(out == '<ROOT> log message')

  # with logger name
  out <- get_log_output(flog.info("log message", name='mylogger'))
  (out == '<mylogger> log message\n')
})

assert("Custom layout dereferences level field", {
  flog.threshold(INFO)
  flog.layout(layout.format('xxx[~l]xxx'))
  raw <- get_log_output(flog.info("log message"))

  ('xxx[INFO]xxx\n' == raw)
  (length(grep('log message', raw)) == 0)
})

flog.layout(default.layout)
assert("Raw null value is printed", {
  raw <- get_log_output(flog.info('xxx[%s]xxx', NULL))
  (length(grep('xxx[NULL]xxx', raw, fixed=TRUE)) == 1)
})

assert("Single null value is printed", {
  opt <- list()
  raw <- get_log_output(flog.info('xxx[%s]xxx', opt$noexist))
  (length(grep('xxx[NULL]xxx', raw, fixed=TRUE)) == 1)
})

assert("Null is printed amongst variables", {
  opt <- list()
  raw <- get_log_output(flog.info('aaa[%s]aaa xxx[%s]xxx', 3, opt$noexist))
  (length(grep('aaa[3]aaa', raw, fixed=TRUE)) == 1)
  (length(grep('xxx[NULL]xxx', raw, fixed=TRUE)) == 1)
})

assert("no extra arguments are passed", {
  (grepl('foobar\n$', get_log_output(flog.info('foobar'))))
  (grepl('foobar %s\n$', get_log_output(flog.info('foobar %s'))))
  (grepl('10%\n$', get_log_output(flog.info('10%'))))
})

assert("some extra arguments are passed", {
  (grepl('foobar\n$', get_log_output(flog.info('foobar', pi))))
  (grepl(
    'foobar foo\n$',
    get_log_output(flog.info('foobar %s', 'foo'))))
  (grepl('100\n$', get_log_output(flog.info('10%d', 0))))
  (
    grepl('foo and bar equals to foobar\n',
          get_log_output(
            flog.info('%s and %s equals to %s', 'foo', 'bar', 'foobar'))))
})

assert("Function name detection inside nested functions", {
    flog.layout(layout.format("[~f] ~m"))
    a <- function() { flog.info("inside A") }
    b <- function() { a() }
    d <- function() { b() }
    e <- function() { d() }
    ('[a] inside A\n' == get_log_output(e()))
})

drop_log_prefix <- function(msg) {
    sub('[A-Z]* \\[.*\\] ', '', msg)
}

flog.layout(layout.glue)
assert("glue features work", {
  (drop_log_prefix(get_log_output(flog.info('foobar'))) == 'foobar\n')
  (drop_log_prefix(get_log_output(flog.info('{a}{b}', a = 'foo', b = 'bar'))) ==
    'foobar\n')
  (drop_log_prefix(get_log_output(flog.info('foo{b}', b = 'bar'))) == 'foobar\n')

  b <- 'bar'
  (drop_log_prefix(get_log_output(flog.info('foo{b}'))) == 'foobar\n')
  rm(b)

  (drop_log_prefix(get_log_output(flog.info('foo', 'bar'))) == 'foobar\n')
})

## back to the default layout
invisible(flog.layout(default.layout))
