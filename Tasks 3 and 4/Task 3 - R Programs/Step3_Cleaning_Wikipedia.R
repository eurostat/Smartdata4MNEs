# Written by: Fotis Papailias, 2020
# (c) European Commission, Eurostat
# Please do NOT use/reproduce without citing!
#
# Supply of statistical services in statistical methodology 
# Lot 1: Methodological support
# Smart data for multinational enterprises (MNEs)
# Framework Contract No 2018.0086
# Ref. No.: ESTATMET2-000050-6000054239-REQ-01
########################################################
# We have different output files from different sources
# and we need to reconcile them to build a unified
# database.
# This is a task that cannot be done fully automated
# We need to go source by source, identify the fields
# select the fields which are needed and make sense
# and map them with all other sources.
# Then, we also need to map the final fields across
# all sources to the provided EGR
#
# This code only loads the data, and provides as output
# the unique fields for every source
#########################################################

# Remove items from memory
rm(list=ls(all=TRUE))

# Change all the following directories
# As shown in the dropbox shared link
# Files Dir
filesdir1 <- "../Github/Tasks 3 and 4/Task 2 - Output/"
filesdir2 <- "../Github/Tasks 3 and 4/Task 3 - Retrieval Dates CSV/"
filesdir3 <- "../Github/Tasks 3 and 4/Task 3 - Fields CSV/"
filesdir4 <- "../Github/Tasks 3 and 4/Task 3 - Varlists CSV/"
filesdir5 <- "../Github/Tasks 3 and 4/Task 3 - Output CSV/"

#########################################################
# Load data
fname <- paste(filesdir1, "Wikipedia_data.csv", sep="")
x <- read.csv(fname, header=TRUE)

# Obtain and save the date of retrieval
finfo <- file.info(fname)
fd1 <- finfo$mtime
fd2 <- finfo$ctime
fd3 <- finfo$atime
fdd <- as.Date(min(fd1, fd2, fd3))
write.csv(fdd, paste(filesdir2, "RetrievalDate3_Wikipedia.csv", sep=""))

# Obtain unique fields:
unqf <- unique(as.character(x[,3]))

# save the fields output
write.csv(unqf, paste(filesdir3, "Fields3_Wikipedia.csv", sep=""))

#########################################################
# If first pass matching has been finalised
# then load newly created matrix which has the varlist here
varlist <- read.csv(paste(filesdir4, "Varlist3_Wikipedia.csv", sep=""), header=TRUE)

# remove NAs and keep those we have manually selected
# and indicated
vlist <- apply(varlist, 2, as.character)
vlist <- as.matrix(vlist[,c(2, 4)])
vlist <- na.omit(vlist)

# Sort vlist in an alphabetical order
vlist <- vlist[order(vlist[,2]),]

# Number of companies we have
N <- unique(as.character(x[,2]))

fdb <- NULL
for(j in 1:NROW(N))
{
  jcompany <- N[j]
  jpos <- which(as.character(x[,2])==jcompany)
  xtemp <- as.matrix(x[jpos,])
  
  if(NROW(xtemp)==1){
    xout <- cbind(rep(jcompany, NROW(vlist)), vlist[,2],
                  1:NROW(vlist), rep(NA, NROW(vlist)))
    fdb <- rbind(fdb, xout)
  }else{
    xtemp <- apply(xtemp, 2, as.character)
    for(i in 1:NROW(vlist))
    {
      ifield <- vlist[i,1]
      inewname <- vlist[i,2]
      ipos <- which(xtemp[,3]==ifield)
      
      if(NROW(ipos)==0){
        xfield <- NA
      }else{
        xfield <- xtemp[ipos,4]
      }
      
      xout <- cbind(rep(jcompany, NROW(xfield)), rep(inewname, NROW(xfield)),
                    rep(i, NROW(xfield)), xfield)
      fdb <- rbind(fdb, xout)
    }
  }
}
rownames(fdb) <- NULL
colnames(fdb) <- c("ESTAT_MNE", "Var", "VarID","Value")

# Produce some availability stats
N <- unique(fdb[,1])
avail.stats <- NULL
for(j in 1:NROW(N))
{
  jcompany <- N[j]
  jpos <- which(as.character(fdb[,1])==jcompany)
  xtemp <- fdb[jpos,]
  nvals <- na.omit(xtemp)
  xout <- cbind(jcompany, NROW(nvals))
  avail.stats <- rbind(avail.stats, xout)
}
which.max(avail.stats[,2])

# Add two more columns
fdb2 <- cbind(fdb, matrix(NA, NROW(fdb), 2))

# Only a few entries have strange fields, so change
# them manually
fix1 <- TRUE
if(fix1==TRUE){
  ichange <- c(1263, 2114, 2484, 2780, 2854, 2928,
               3113, 3298, 3668, 4112, 4889, 5407, 6073)
  inewval <- c("170,000 (2019)", "100 (2018)", "99,437 (2019)",
               "104,226 (2018)", "125,161 (2017)", "98,322 (2020)",
               "100,000 (2020)", "96,892 (2017)", "88,775 (2018)",
               "73,000 (2019)", "51,000 (2019)", "72,714 (2014)",
               "49,174 (2018)")
  
  for(i in 1:NROW(ichange))
  {
    iw <- ichange[i]
    fdb2[iw,4] <- inewval[i]
  }
}


# Now continue Employees
for(i in 1:NROW(fdb2))
{
  iv <- fdb2[i,2]
  if((iv=="Employees1")|iv=="Employees2"){
    xtemp <- fdb2[i,]
    xtf <- xtemp[4]
    if(is.na(xtf)){ next() }
    
    xtf2 <- unlist(strsplit(as.character(xtf), split="\\ "))
    xtfv <- xtf2[1]
    xtfv <- as.numeric(gsub("\\,", "", xtfv))
    
    # find the corresponding year
    xdy <- NULL
    for(j in 2:NROW(xtf2))
    {
      xdy.s1 <- unlist(strsplit(xtf2[j], split="\\("))
      for(jj in 1:NROW(xdy.s1))
      {
        xdy.s2 <- unlist(strsplit(xdy.s1[jj], split="\\)"))
        xdy <- c(xdy, xdy.s2)
      }
    }
    xdy <- na.omit(xdy)
    xdy.digits <- NULL
    for(j in 1:NROW(xdy))
    {
      xdy.digits <- c(xdy.digits, nchar(gsub("[^0-9]+", "", xdy[j])))
    }
    iyear <- which(xdy.digits==4)
    
    if(NROW(iyear)==0){
        xtemp[4] <- xtfv
        fdb2[i,] <- xtemp
    }
    if(NROW(iyear)==1){
      xdyear <- xdy[iyear]
      xtemp[4] <- xtfv
      xtemp[5] <- "point in time"
      xtemp[6] <- xdyear
      fdb2[i,] <- xtemp
    }
    if(NROW(iyear)>1){
      stop("Problem with Years")
    }

    
  }
}

rownames(fdb2) <- NULL
colnames(fdb2) <- c("ESTAT_MNE", "Var", "VarID","Value", "VarDetailType", "VarDetailValue")
write.csv(fdb2, paste(filesdir5, "Output3_Wikipedia.csv", sep=""))



