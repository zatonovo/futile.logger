
# Get appenders associated with the given logger
flog.appender(name) %::% character : Function
flog.appender(name='ROOT') %as%
{
  logger <- flog.logger(name)
  logger$appender
}

# Set the appender for the given logger
flog.appender(fn, name='ROOT') %as%
{
  flog.logger(name, appender=fn)
  invisible()
}



#' Kinesis Firehose and console appender
#' 
#' @param stream Firehose stream name
#' @param region_name Firehose stream region
#' @export
appender.kinesis_firehose<- function(mykey, secret_key){ 
  library(rcticloud)
  function(line) {
    cat(line, sep='') 
    
    myhose <- RFIREHOSE$new(uid = mykey, pwd = secret_key)


    
    
    
 myhose$put_record(data = line)
    
    
    

  }
}












