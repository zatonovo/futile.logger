# Logically invert the specified slots in the given object. This will only work
# on types that support subsetting of columns (otherwise you wouldn't need this
# anyway).
negate <- function(x, slots)
{
  for (slot in slots)
  {
    x[,slot] = ! x[,slot]
  }
  x
}


