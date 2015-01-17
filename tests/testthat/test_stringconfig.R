context("String configuration")

test_that("trace threshold is set via string", {
    flog.threshold("trace")
    expect_equal(flog.threshold(), "TRACE")
    expect_equivalent(flog.logger()$threshold, 9)
    flog.threshold("TRACE")
    expect_equal(flog.threshold(), "TRACE")
})

test_that("debug threshold is set via string", {
    flog.threshold("debug")
    expect_equal(flog.threshold(), "DEBUG")
    expect_equivalent(flog.logger()$threshold, 8)
    flog.threshold("DEBUG")
    expect_equal(flog.threshold(), "DEBUG")
})

test_that("info threshold is set via string", {
    flog.threshold("info")
    expect_equal(flog.threshold(), "INFO")
    expect_equivalent(flog.logger()$threshold, 6)
    flog.threshold("INFO")
    expect_equal(flog.threshold(), "INFO")
})

test_that("warn threshold is set via string", {
    flog.threshold("warn")
    expect_equal(flog.threshold(), "WARN")
    expect_equivalent(flog.logger()$threshold, 4)
    flog.threshold("WARN")
    expect_equal(flog.threshold(), "WARN")
})

test_that("error threshold is set via string", {
    flog.threshold("error")
    expect_equal(flog.threshold(), "ERROR")
    expect_equivalent(flog.logger()$threshold, 2)
    flog.threshold("ERROR")
    expect_equal(flog.threshold(), "ERROR")
})

test_that("fatal threshold is set via string", {
    flog.threshold("fatal")
    expect_equal(flog.threshold(), "FATAL")
    expect_equivalent(flog.logger()$threshold, 1)
    flog.threshold("FATAL")
    expect_equal(flog.threshold(), "FATAL")
})