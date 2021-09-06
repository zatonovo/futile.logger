context("debug level logging")

test_that("debug level is logged", {
    flog.threshold(DEBUG)
    msg <- 'test debug'
    act <- get_log_output(flog.debug(msg))
    expect_true(grepl(msg, act))
})

test_that("higher levels are logged", {
    flog.threshold(DEBUG)
    act.info <- get_log_output(flog.info('test info'))
    expect_true(grepl('test info', act.info))

    act.warn <- get_log_output(flog.warn('test warn'))
    expect_true(grepl('test warn', act.warn))

    act.error <- get_log_output(flog.error('test error'))
    expect_true(grepl('test error', act.error))

    act.fatal <- get_log_output(flog.fatal('test fatal'))
    expect_true(grepl('test fatal', act.fatal))
})

test_that("lower levels are not logged", {
    flog.threshold(DEBUG)
    act <- get_log_output(flog.trace("testlog"))
    expect_true(length(act) < 1)
})
