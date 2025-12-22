if (requireNamespace("jsonlite", quietly=TRUE)) {
flog.threshold(INFO)
flog.layout(layout.json)

assert("simple string", {
  raw <- get_log_output(flog.info("log message"))
  aslist <- jsonlite::fromJSON(raw)
  (aslist$level == "INFO")
  (aslist$message == "log message")

  ts <- strptime(aslist$timestamp, "%Y-%m-%d %H:%M:%S %z")
  ('POSIXt' %in% class(ts))
})

assert("additional objects", {
  raw <- get_log_output(
    flog.info("log message", pet="hamster", weight=12, stuff=c("a", "b")))
  aslist <- jsonlite::fromJSON(raw)
  (aslist$level == "INFO")
  (aslist$message == "log message")
  (aslist$pet == "hamster")
  (aslist$weight == 12)
  (aslist$stuff == c("a", "b"))
})


assert("NULL additional objects", {
  raw <- get_log_output(flog.info("log message", nullthing=NULL))
  aslist <- jsonlite::fromJSON(raw)
  (length(aslist$nullthing) == 0)
})

# knockdown
flog.layout(layout.simple)

}
