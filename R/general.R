# Get either names or colnames from a list or data.frame. This attempts to 
# create some polymorphism around lists, vectors, and data.frames.
anynames <- function(data)
{
  ns <- names(data)
  if (is.null(ns)) { ns <- colnames(data) }
  ns
}

"anynames<-" <- function(data, value)
{
  if (is.null(names(data))) colnames(data) <- value
  else names(data) <- value
  invisible()
}

# Gets the length of a vector or the rows of a matrix or data frame.
anylength <- function(data)
{
  len <- nrow(data)
  if (is.null(len)) { len <- length(data) }
  len
}

# Similar to rollapply but the values are inline, such that the next iteration
# can act on the newly minted values
# Example
# inlineapply(c(1,1,2,3,5), 2, sum)
# [1]  1  2  4  7 12
inlineapply <- function(data, width, fun, ..., col=NA, include.idx=FALSE)
{
  type <- 'normal'
  if (!is.na(col)) type <- 'col'
  do.call(paste('inlineapply.',type,sep=''),
    list(data=data,width=width, fun=fun, ..., col=col,include.idx=include.idx))
}

inlineapply.normal <- function(data, width, fun, ..., include.idx=FALSE)
{
  idxs <- width:anylength(data)
  if (include.idx)
  {
    for (idx in idxs)
    {
      inf <- idx - width + 1
      if (is.na(sum(data[inf:idx]))) { next }
      data[idx] <- fun(data[inf:idx], idx=idx, ...)
    }
  }
  else
  {
    for (idx in idxs)
    {
      inf <- idx - width + 1
      if (is.na(sum(data[inf:idx]))) { next }
      data[idx] <- fun(data[inf:idx], ...)
    }
  }
  data
}

inlineapply.col <- function(data, width, fun, ..., col=NA, include.idx=FALSE)
{
  idxs <- width:anylength(data)
  if (include.idx)
  {
    for (idx in idxs)
    {
      inf <- idx - width + 1
      if (is.na(sum(data[inf:idx]))) { next }
      data[idx,col] <- fun(data[inf:idx], idx=idx, ...)
    }
  }
  else
  {
    for (idx in idxs)
    {
      inf <- idx - width + 1
      if (is.na(sum(data[inf:idx]))) { next }
      data[idx,col] <- fun(data[inf:idx], ...)
    }
  }
  data
}

# Get the middle value in the series
mid <- function(x)
{
  if (is.null(nrow(x)))
  {
    len <- length(x)
    if (len %% 2 == 0) { m <- x[len/2] }
    else { m <- x[len %/% 2 +1] }
  }
  else
  {
    len <- nrow(x)
    if (len %% 2 == 0) { m <- x[len/2,] }
    else { m <- x[len %/% 2 +1,] }
  }
  m
}


# Return a portion of a matrix. This is useful for debugging.
peek <- function(x, upper=5, lower=1)
{
  if (is.null(dim(x))) return(x[lower:upper])
  return(x[lower:upper,lower:upper])
}

