assert("Default settings", {
  flog.threshold(INFO)
  raw <- get_log_output(flog.info("log message"))
  #cat("\n[test.default] Raw:",raw,"\n")
  (length(grep('INFO', raw)) > 0)
  (length(grep('log message', raw)) > 0)
})

assert("Change root threshold", {
  flog.threshold(ERROR)
  raw <- get_log_output(flog.info("log message"))
  #cat("\n[test.change_threshold] Raw:",raw,"\n")
  (length(raw) == 0)
})


assert("Capture works as expected", {
  flog.threshold(INFO)
  raw <- get_log_output(flog.info("log message", head(cars), capture = TRUE))
  (length(raw) == 1)
  (grepl('^INFO',raw))
  (grepl('dist',raw))
  })

assert("Get threshold names", {
  flog.threshold(ERROR)
  (flog.threshold() == "ERROR")  
  flog.threshold(DEBUG, name='my.package')
  (flog.threshold(name='my.package') == "DEBUG") 
})


assert("Create new logger", {
  flog.threshold(ERROR)
  flog.threshold(DEBUG, name='my.package')
  raw.root <- get_log_output(flog.info("log message"))
  raw.mine <- get_log_output(flog.info("log message", name='my.package'))
  (length(raw.root) == 0)
  (length(grep('INFO', raw.mine)) > 0)
  (length(grep('log message', raw.mine)) > 0)
})

assert("Hierarchy is honored", {
  flog.threshold(ERROR)
  flog.threshold(TRACE, name='my')
  flog.threshold(DEBUG, name='my.package')
  raw.root <- get_log_output(flog.info("log message"))
  raw.l1 <- get_log_output(flog.trace("log message", name='my'))
  raw.l2 <- get_log_output(flog.trace("log message", name='my.package'))
  (length(raw.root) == 0)
  (length(grep('TRACE', raw.l1)) > 0)
  (length(grep('log message', raw.l1)) > 0)
  (length(raw.l2) == 0)
})

assert("Hierarchy inheritance", {
  flog.remove('my.package')
  flog.remove('my')
  flog.threshold(ERROR)
  flog.threshold(TRACE, name='my')
  raw.root <- get_log_output(flog.info("log message"))
  raw.l1 <- get_log_output(flog.trace("log message", name='my'))
  raw.l2 <- get_log_output(flog.trace("log message", name='my.package'))
  (length(raw.root) == 0)
  (length(grep('TRACE', raw.l1)) > 0)
  (length(grep('log message', raw.l1)) > 0)
  (length(grep('TRACE', raw.l2)) > 0)
  (length(grep('log message', raw.l2)) > 0)
})


# Can't test this since assert calls suppressMessages
assert("ftry captures warnings", {
  raw <- get_log_output(ftry(log(-1)))
  cat("raw =",raw,'\n')
  (length(grep('WARN', raw)) > 0)
  (length(grep('simpleWarning', raw)) > 0)
})

assert("carp returns output", {
  on.exit(flog.carp(FALSE))

  flog.carp(TRUE)
  (flog.carp())
  flog.threshold(WARN)
  raw <- flog.debug("foo")
  flog.carp(FALSE)
  (length(grep('DEBUG', raw)) > 0)
})

assert("logger can be provided explicitly", {
  flog.threshold(DEBUG, name='my.package')
  my_logger <- flog.logger('my.package')
  with_logger <- get_log_output(flog.info("log message", logger=my_logger))
  (length(grep('INFO', with_logger)) > 0)
  (length(grep('log message', with_logger)) > 0)
})
assert("logger provided explicitly is much faster if nothing has to be logged", {
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
  (tw > tl*10)
})
