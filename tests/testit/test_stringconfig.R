assert("trace threshold is set via string", {
    flog.threshold("trace")
    (flog.threshold() == "TRACE")
    (flog.logger()$threshold == 9)
    flog.threshold("TRACE")
    (flog.threshold() == "TRACE")
})

assert("debug threshold is set via string", {
    flog.threshold("debug")
    (flog.threshold() == "DEBUG")
    (flog.logger()$threshold == 8)
    flog.threshold("DEBUG")
    (flog.threshold() == "DEBUG")
})

assert("info threshold is set via string", {
    flog.threshold("info")
    (flog.threshold() == "INFO")
    (flog.logger()$threshold == 6)
    flog.threshold("INFO")
    (flog.threshold() == "INFO")
})

assert("warn threshold is set via string", {
    flog.threshold("warn")
    (flog.threshold() == "WARN")
    (flog.logger()$threshold == 4)
    flog.threshold("WARN")
    (flog.threshold() == "WARN")
})

assert("error threshold is set via string", {
    flog.threshold("error")
    (flog.threshold() == "ERROR")
    (flog.logger()$threshold == 2)
    flog.threshold("ERROR")
    (flog.threshold() == "ERROR")
})

assert("fatal threshold is set via string", {
    flog.threshold("fatal")
    (flog.threshold() == "FATAL")
    (flog.logger()$threshold == 1)
    flog.threshold("FATAL")
    (flog.threshold() == "FATAL")
})
