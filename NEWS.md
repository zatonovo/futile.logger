**Purpose of this document: Describe the changes in new futile.logger releases...**



# futile.logger 1.4.5 (Sep 6, 2021)

## New features

* New logging appender functions:
  * `appender.file2`:   Write log messages to a dynamically-named file
  * `appender.graylog`: Write log messages to a Graylog2 HTTP GELF endpoint
  * `appender.modulo`:  Special meta appender that prints only when the internal counter mod n = 0
  * `appender.syslog`:  Write log message to Linux syslog using the `rsyslog::open_syslog()` function
  
* New `layout.*()` functions to support new logging output formats:
  * `layout.graylog()`:        Generate log messages in a Graylog2 HTTP GELF accetable format 
  * `layout.simple.parallel()` Decorate log messages with a standard format and a pid (process ID from `Sys.getpid()`)
  * `layout.colored()`:        Decorate log messages with a standard format colored by log level
  * `layout.glue()`:           Decorate log messages with a standard format using glue instead of sprintf

* New internal function `prepare_arg()` to format variables in the logging output (passed in `...` to the `flog.*()` functions

## Minor improvements and fixes

* The package change log is now maintained in the `NEWS` file
  (old release added for information purposes only without further known details)
* New argument `silent` in `ftry()` to control if errors are rethrown or not
* New argument `logger` for `flog.info()`, `flog.warn()`, `flog.error()`, `flog.fatal()`, `flog.debug()` and `flog.trace()`
  to use a predefined logger for higher through-put in heavy-logging scenarios (eg. `flog.trace()`)
* Support to write a logger name in the logging output by using a custom layout:
  The new argument `id` in `layout.*()` functions makes the logger name available in message formats via the `~i` ("identifier") token.
  Logger names are passed in the `name` argument to `flog.*()` logging functions.
  The logging format is set via the `flog.layout()` function.
  
  Example:
  
  ```
  flog.layout(layout.format('~l [~t] <~i> ~m'), name = "ROOT")   # set custom layout ROOT logger
  flog.info("my log message", name="my logger name") # the logger name is now in the logging output
  # > INFO [2021-11-04 16:23:38] <my logger name> my log message
  ```

## API breaking changes and deprecated functions

* TODO (the new `logger` argument in `flog.*()` functions look like breaking the API since it was not added at the end
  and the semantics may have changed regarding the `name` argument).
  => NO, IT ISN'T syntactically API-breaking since it follows AFTER the `...` argument and therefore always requires to use (position-independent) named arguments! Still: The semantics of the `name` argument may have changed (check this!)

# futile.logger 1.4.3 (Jul 10, 2016) - CRAN release

* https://cran.r-project.org/package=futile.logger

# futile.logger 1.4.2 (May 10, 2016)

# futile.logger 1.4.1 (Apr 19, 2015) - CRAN release

# futile.logger 1.4 (Apr 18, 2015) - CRAN release

* Note: CRAN release date was 2015-03-21 while Git tag creation date was 2015-04-18

# futile.logger 1.3.8 (Oct 9, 2014)

* No description available

# futile.logger 1.3.7 (Jan 25, 2014) - CRAN release

* No description available

# futile.logger 1.3.6 (Dec 5, 2013)

* No description available

# futile.logger 1.3.5 (Aug 15, 2013) - CRAN release

* No description available

# futile.logger 1.3.4 (Aug 9, 2013)

* No description available

# futile.logger 1.3.3 (Jul 17, 2013) - CRAN release

* No description available

# futile.logger 1.3.1 (Apr 6, 2013) - CRAN release

* No description available

# futile.logger older CRAN releases (2010 - 2012)

* See the CRAN archive: https://cran.r-project.org/src/contrib/Archive/futile.logger/
* No further descriptions available
