context("debug level logging")

test_that("debug level is logged", {
    flog.threshold(DEBUG)
    expect_output(flog.debug("testlog"), "testlog")
})

test_that("higher levels are logged", {
    testlog = paste0("testlog ", sample(100, 1))
    flog.threshold(DEBUG)
    expect_output(flog.info(testlog), testlog)
    expect_output(flog.warn(testlog), testlog)
    expect_output(flog.error(testlog), testlog)
    expect_output(flog.fatal(testlog), testlog)
})

test_that("lower levels are not logged", {
    flog.threshold(DEBUG)
    expect_output(flog.trace("testlog"), "")
})