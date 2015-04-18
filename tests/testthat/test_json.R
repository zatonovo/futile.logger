if (requireNamespace("jsonlite", quietly=TRUE)) {
context("JSON: typical usage")
flog.threshold(INFO)
flog.layout(layout.json)

test_that("simple string", {
  raw <- capture.output(flog.info("log message"))
  aslist <- jsonlite::fromJSON(raw)
  expect_equal(aslist$level, "INFO")
  expect_equal(aslist$message, "log message")

  ts <- strptime(aslist$timestamp, "%Y-%m-%d %H:%M:%S %z")
  expect_true('POSIXt' %in% class(ts))
})

test_that("additional objects", {
  raw <- capture.output(
    flog.info("log message", pet="hamster", weight=12, stuff=c("a", "b")))
  aslist <- jsonlite::fromJSON(raw)
  expect_equal(aslist$level, "INFO")
  expect_equal(aslist$message, "log message")
  expect_equal(aslist$pet, "hamster")
  expect_equal(aslist$weight, 12)
  expect_equal(aslist$stuff, c("a", "b"))
})

context("JSON: NULL values")

#test_that("NULL message", {
#  raw <- capture.output(flog.info(NULL))
#  aslist <- fromJSON(raw)
#  expect_equal(length(aslist$message), 0)
#})

test_that("NULL additional objects", {
  raw <- capture.output(flog.info("log message", nullthing=NULL))
  aslist <- jsonlite::fromJSON(raw)
  expect_equal(length(aslist$nullthing), 0)
})

# knockdown
flog.layout(layout.simple)

}
