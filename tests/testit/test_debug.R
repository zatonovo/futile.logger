
assert("debug level is logged", {
    flog.threshold(DEBUG)
    msg <- 'test debug'
    act <- get_log_output(flog.debug(msg))
    (grepl(msg, act))
})

assert("higher levels are logged", {
    flog.threshold(DEBUG)
    act.info <- get_log_output(flog.info('test info'))
    (grepl('test info', act.info))

    act.warn <- get_log_output(flog.warn('test warn'))
    (grepl('test warn', act.warn))

    act.error <- get_log_output(flog.error('test error'))
    (grepl('test error', act.error))

    act.fatal <- get_log_output(flog.fatal('test fatal'))
    (grepl('test fatal', act.fatal))
})

assert("lower levels are not logged", {
    flog.threshold(DEBUG)
    act <- get_log_output(flog.trace("testlog"))
    (length(act) < 1)
})
