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
fname <- paste(filesdir1, "Wikidata.csv", sep="")
x <- read.csv(fname, header=TRUE)

# Obtain and save the date of retrieval
finfo <- file.info(fname)
fd1 <- finfo$mtime
fd2 <- finfo$ctime
fd3 <- finfo$atime
fdd <- as.Date(min(fd1, fd2, fd3))
write.csv(fdd, paste(filesdir2, "RetrievalDate2_Wikidata.csv", sep=""))

# Obtain unique fields:
unqf <- unique(as.character(x[,3]))

# save the fields output
write.csv(unqf, paste(filesdir3, "Fields2_Wikidata.csv", sep=""))

#########################################################
# If first pass matching has been finalised
# then load newly created matrix which has the varlist here
varlist <- read.csv(paste(filesdir4, "Varlist2_Wikidata.csv", sep=""), header=TRUE)

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
                  1:NROW(vlist), rep(NA, NROW(vlist)),
                  rep(NA, NROW(vlist)), rep(NA, NROW(vlist)))
    fdb <- rbind(fdb, xout)
  }else{
    xtemp <- apply(xtemp, 2, as.character)
    for(i in 1:NROW(vlist))
    {
      ifield <- vlist[i,1]
      inewname <- vlist[i,2]
      ipos <- which(xtemp[,3]==ifield)
      
      if(NROW(ipos)==0){
        xfield <- xdetailtype <- xdetailvalu <- NA
      }else{
        xfield <- xtemp[ipos,4]
        xdetailtype <- xtemp[ipos,5]
        xdetailvalu <- xtemp[ipos,6]
      }
      
      xout <- cbind(rep(jcompany, NROW(xfield)), rep(inewname, NROW(xfield)),
                    rep(i, NROW(xfield)), xfield, xdetailtype, xdetailvalu)
      fdb <- rbind(fdb, xout)
    }
  }
}
rownames(fdb) <- NULL
colnames(fdb) <- c("ESTAT_MNE", "Var", "VarID","Value", "VarDetailType", "VarDetailValue")

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

# Make a first pass and make sure that all dates
# are transformed to a suitable format
fdb2 <- NULL
for(i in 1:NROW(fdb))
{
  xtemp <- fdb[i,]
  if(is.na(xtemp[5])){
    fdb2 <- rbind(fdb2, xtemp)
    next()
  }
  if((xtemp[5]=="announcement date")|
     (xtemp[5]=="dissolved, abolished or demolished")|
     (xtemp[5]=="end time")|
     (xtemp[5]=="point in time")|
     (xtemp[5]=="start time")){
    xdate <- substr(xtemp[6], start=1, stop=10)
    xtemp2 <- xtemp
    xtemp2[6] <- xdate
  }else{
    xtemp2 <- xtemp
  }
  fdb2 <- rbind(fdb2, xtemp2)
}
rownames(fdb2) <- NULL

# Now, that we have proper dates, make sure that
# multiple entries are ordered chronologically
fdb3 <- NULL
N <- unique(fdb2[,1])
V <- unique(fdb2[,2])
for(j in 1:NROW(N))
{
  jcompany <- N[j]
  jpos <- which(as.character(fdb2[,1])==jcompany)
  xtemp <- as.matrix(fdb2[jpos,])
  
  for(i in 1:NROW(V))
  {
    ivar <- V[i]
    ipos <- which(xtemp[,2]==ivar)
    xtemp2 <- xtemp[ipos,]
    
    if(NROW(ipos)==1){
      fdb3 <- rbind(fdb3, xtemp2)
      next()
    }
    
    # Find NAs and Non-NAs
    iNAs <- which(is.na(xtemp2[,5])==TRUE)
    inoNAs <- which(is.na(xtemp2[,5])==FALSE)
    
    if((NROW(inoNAs)==0)|(NROW(inoNAs)==1)){
      fdb3 <- rbind(fdb3, xtemp2)
      next()
    }
    
    if(NROW(iNAs)==0){ iNAs <- NULL}
    
    xtemp3 <- xtemp2[inoNAs,]
    xcheck <- which(xtemp3[,5]=="point in time")
    if((NROW(xcheck)==0)|(NROW(xcheck)==1)){
      fdb3 <- rbind(fdb3, xtemp2[iNAs,], xtemp3)
      next()
    }
    
    # which is NOT point in time?
    if(NROW(xtemp3)>NROW(xcheck)){
      iall <- 1:NROW(xtemp3)
      iNON <- iall %in% xcheck
      iNON <- iall[iNON==FALSE]
    }else{
      iNON <- NULL
    }
    
    xtemp3a <- xtemp3[iNON,]
    xtemp3b <- xtemp3[xcheck,]
    
    xtemp3dt <- order(as.Date(xtemp3b[,6]))
    xtemp3b <- xtemp3b[xtemp3dt,]
    fdb3 <- rbind(fdb3, xtemp2[iNAs,], xtemp3a, xtemp3b)
    
  }
}

rownames(fdb3) <- NULL

write.csv(fdb3, paste(filesdir5, "Output2_Wikidata.csv", sep=""))



