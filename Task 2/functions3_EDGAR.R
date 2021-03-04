
# Tranform EDGAR dates to proper dates
transform.date <- function(xx)
{
  xxnew <- unlist(strsplit(xx, split=" "))
  xmonth <- xxnew[1]
  if(xmonth=="Jan."){xmonth2 <- "01"}
  if(xmonth=="Feb."){xmonth2 <- "02"}
  if(xmonth=="Mar."){xmonth2 <- "03"}
  if(xmonth=="Apr."){xmonth2 <- "04"}
  if(xmonth=="May."){xmonth2 <- "05"}
  if(xmonth=="Jun."){xmonth2 <- "06"}
  if(xmonth=="Jul."){xmonth2 <- "07"}
  if(xmonth=="Aug."){xmonth2 <- "08"}
  if(xmonth=="Sep."){xmonth2 <- "09"}
  if(xmonth=="Oct."){xmonth2 <- "10"}
  if(xmonth=="Nov."){xmonth2 <- "11"}
  if(xmonth=="Dec."){xmonth2 <- "12"}
  xday <- xxnew[2]
  xday2 <- unlist(strsplit(xday, split=","))
  xday2 <- xday2[1]
  xyear <- xxnew[3]
  xnewdate <- paste(xyear, "-", xmonth2, "-", xday2, sep="")
  return(xnewdate)
}

convert.brackets <- function(x){
  if(grepl("\\(.*\\)", x)){
    paste0("-", gsub("\\(|\\)", "", x))
  } else {
    x
  }
}