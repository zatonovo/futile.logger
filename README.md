[![Build Status](https://travis-ci.org/zatonovo/futile.logger.png)](https://travis-ci.org/zatonovo/futile.logger)

Overview
========
futile.logger is a logging utility for R. Originally built based on log4j, 
the latest version introduces a new API that is more consistent with R idioms.
In practice this means an interface that works equally well in the shell for
interactive use and also in scripts for system use.

The underlying concepts of log4j still exist, e.g. loggers, appenders, and
formatters. There continues to be a hierarchical system for logger. In 
addition, there is now automatic package scoping, which means that packages
are given their own logger namespace so you can interactively turn on and
off logging for specific packages.

Also included is formatting logic to log list objects (which includes 
data.frames) in a smart way.

Usage
=====
Out of the box, the default `ROOT` logger logs to the console with threshold
set to INFO.

```R
flog.info("Hello, %s", "world")

# Put pid in logging in multi-processing
flog.info('%d message', Sys.getpid()) 

# This won't print by default
flog.debug("Goodbye, %s", "world")

# Change the log level to debug and try again
flog.threshold(DEBUG)
flog.debug("Goodbye, %s", "world")

# Keep an alternate logger at WARN
flog.threshold(WARN, name='quiet')

# This won't print since it's using the logger named 'quiet'!
flog.debug("Goodbye, %s", "world", name='quiet')

```

Loggers
-------
A logger is simply a namespace bound to a threshold, an appender, and a
formatter. Loggers are configured automatically whenever they are 
referenced (for example when changing the threshold) inheriting the settings
of the root logger. To explicitly create a logger call `flog.logger()`.

```R
flog.logger("tawny", WARN, appender=appender.file('tawny.log'))
```
Please notice that you shall not set the name as any of the following keywords:   
'TRACE', 'trace', 'DEBUG', 'debug', 'INFO', 'info', 'WARN', 'warn', 'ERROR', 'error', 'FATAL', 'fatal'

To remove a logger, use `flog.remove()`. If no such logger exists,
the command is safely ignored.

```R
flog.remove("tawny")
```

Thresholds
----------
The logger threshold determines what will be logged for a given logger. Use
this function to retrieve and also change this threshold.

```R
# Get the logging threshold for the ROOT logger
flog.threshold()
```

The default logger is ROOT. To change the threshold of a different logger, 
specify the logger argument with a string that represents the logger. Note
that a log.(debug|info|warn|error) command running from a package will
automatically be associated with a logger with the name of the package. This
structure means you can change the log level for a specific package as 
necessary.

```R
# Set root logger to DEBUG level to see all log messages
flog.threshold(DEBUG)
# Suppress log messages below WARN for logger 'quiet'
flog.threshold(WARN, name="quiet")
```

Appenders
---------
An appender defines where output is directed. Typically only one appender is
used per logger, but multiple can be assigned. The package provides the 
following appenders:

+ `appender.console`
+ `appender.file`
+ `appender.tee`
+ `appender.file2`

To change the appenders assigned to a logger, use `flog.appender()`:
```R
# Change the 'quiet' logger to write to a file
flog.appender(appender.file('quiet.log'), 'quiet')
flog.warn("Goodbye, %s", "world", name='quiet')
```

You can create your own appender by defining a function that accepts a single
character argument. It is up to you to define the behavior. For example,
an appender that logs to a URL might look like the following.

```R
url_appender.gen <- function(url) {
  conn <- url(url)
  function(line) {
    conn.write(line)
  }
}
```

Logging hierarchy
-------
We can create python-style logging hierarchy using `appender.file2`. 
Following snippet will write to both `mylog-WARN.log` and `mylog-INFO.log`
```R
flog.appender(appender.file2("mylog-~l.log", console=TRUE), name='mylogger')
flog.warn('msg1', name='mylogger')
```
If we change the threshold to `DEBUG`, it will also write to `mylog-DEBUG.log`. 
```R
flog.threshold(DEBUG, 'mylogger')
flog.warn('msg2', name='mylogger')
```

If set `inherit=FALSE`, will only write to `mylog-WARN.log` 
```R
flog.appender(appender.file2("mylog-~l.log", console=TRUE, inherit=FALSE), name='mylogger')
flog.warn('msg3', name='mylogger')
```
In this scenario, if we use `flog.info`, it will only write to `mylog-INFO.log`.
```R
flog.info('msg4', name='mylogger')
```

Layouts
-------
A layout defines how a log message is printed. The default layout.simple
prints log messages using the following format:
  LEVEL [datetime] Message

The layouts included in the package are:
+ layout.simple - Use a default format
+ layout.simple.parallel - Use a default format with a process id
+ layout.format - Provide a customizable format string
+ layout.tracearg - Dump a variable with its name


What's New
==========
+ Function to wrap a try/catch with logging (ftry)
+ Capture output for print statements (for more complex objects)
+ New layout.tracearg


