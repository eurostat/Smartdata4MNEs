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
fname <- paste(filesdir1, "OpenCorp_data.csv", sep="")
x <- read.csv(fname, header=TRUE)

# Obtain and save the date of retrieval
finfo <- file.info(fname)
fd1 <- finfo$mtime
fd2 <- finfo$ctime
fd3 <- finfo$atime
fdd <- as.Date(min(fd1, fd2, fd3))
write.csv(fdd, paste(filesdir2, "RetrievalDate6_OpenCorp.csv", sep=""))

# Obtain unique fields:
unqf <- unique(as.character(x[,3]))

# save the fields output
write.csv(unqf, paste(filesdir3, "Fields6_OpenCorp.csv", sep=""))

#########################################################
# If first pass matching has been finalised
# then load newly created matrix which has the varlist here
varlist <- read.csv(paste(filesdir4, "Varlist6_OpenCorp.csv", sep=""), header=TRUE)

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
  
    ifsel <- NULL
    for(i in 1:NROW(xtemp))
    {
      if((xtemp[i,3]=="officer.name")|
         (xtemp[i,3]=="officer.position")|
         (xtemp[i,3]=="officer.start_date")|
         (xtemp[i,3]=="officer.end_date")|
         (xtemp[i,3]=="officer.occupation")){
        ifsel <- c(ifsel, i)
      }
    }
    xfield <- cbind(xtemp[ifsel,3], ifsel, xtemp[ifsel,4])
    xout <- cbind(rep(jcompany, NROW(xfield)), xfield)
    fdb <- rbind(fdb, xout)
  }
}
rownames(fdb) <- NULL
colnames(fdb) <- c("ESTAT_MNE", "Var", "VarID","Value")

write.csv(fdb, paste(filesdir5, "Output6_OpenCorp.csv", sep=""))



