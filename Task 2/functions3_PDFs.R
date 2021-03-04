split.after.doublespace <- function(x)
{
  xnew <- NULL
  for(j in 1:NROW(x))
  {
    xx <- unlist(strsplit(x[j], split="  "))
    xnew <- c(xnew, xx[1])
  }
  return(xnew)
}

remove.empty.and.split <- function(x, spltsgn)
{
  xnew <- x %>% str_squish() %>% strsplit(split = spltsgn)
  xnew <- unlist(xnew)
  return(xnew)
}