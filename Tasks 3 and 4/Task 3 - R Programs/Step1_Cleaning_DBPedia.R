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
# Load DBpedia
fname <- paste(filesdir1, "DBPedia_data.csv", sep="")
x <- read.csv(fname, header=TRUE)

# Obtain and save the date of retrieval
finfo <- file.info(fname)
fd1 <- finfo$mtime
fd2 <- finfo$ctime
fd3 <- finfo$atime
fdd <- as.Date(min(fd1, fd2, fd3))
write.csv(fdd, paste(filesdir2, "RetrievalDate1_DBPedia.csv", sep=""))

# Obtain unique fields:
unqf <- unique(as.character(x[,3]))

# save the fields output
write.csv(unqf, paste(filesdir3, "Fields1_DBPedia.csv", sep=""))

# Check some values using an example below
i <- "Compass Group Plc"
ipos <- which(as.character(x[,2])==i)
xdt <- apply(x[ipos,], 2, as.character)

j <- 279
jf <- unqf[j]
jpos <- which(xdt[,3]==jf)
xdch <- xdt[jpos,]
xdch

#########################################################
# If first pass matching has been finalised
# then load newly manually-created matrix which has the varlist here
varlist <- read.csv(paste(filesdir4, "Varlist1_DBpedia.csv", sep=""), header=TRUE)

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
  xtemp <- apply(x[jpos,], 2, as.character)
  
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

# Extract the company with most of the available data
# so we can check the fields and make the final cleaning
# jwhich <- which.max(avail.stats[,2])
# jcompany <- N[jwhich]
# jpos <- which(as.character(fdb[,1])==jcompany)
# xtemp <- fdb[jpos,]
# write.csv(xtemp, "Example1_DBPedia.csv")

# make the final cleaning
fdb.final <- NULL
for(i in 1:NROW(fdb))
{
  xtemp <- fdb[i,]
  xvarc <- as.numeric(xtemp[3])
  
  # step 1: this will fix entries which are URL fields
  if((xvarc==4)|(xvarc==6)|(xvarc==8)|(xvarc==9)|(xvarc==10)|
     (xvarc==13)|(xvarc==20)|(xvarc==21)|(xvarc==22)|(xvarc==26)|
     (xvarc==29)|(xvarc==30)|(xvarc==32)|(xvarc==37)|(xvarc==39)|
     (xvarc==42)|(xvarc==46)|(xvarc==52)|(xvarc==53)|(xvarc==54)|
     (xvarc==55)|(xvarc==57)|(xvarc==59)|(xvarc==62)|(xvarc==64)|
     (xvarc==70)|(xvarc==72)){
    xft <- xtemp[4]
    xft2 <- unlist(strsplit(xft, split="/"))
    xtemp[4] <- xft2[NROW(xft2)]
  }
  
  fdb.final <- rbind(fdb.final, xtemp)
}

# Step 2: make sure that double entries are removed
N <- unique(fdb.final[,1])
fdb.final2 <- NULL
for(i in 1:NROW(N))
{
  icompany <- N[i]
  ipos <- which(fdb.final[,1]==icompany)
  xtemp <- fdb.final[ipos,]
  V <- unique(fdb.final[,2])
  
  for(j in 1:NROW(V))
  {
    jfield <- V[j]
    jrows <- which(xtemp[,2]==jfield)
    jtemp <- xtemp[jrows,]
    if(NROW(jtemp)>4){
      jtemp <- jtemp[duplicated(jtemp[,4])==FALSE,]
    }
    fdb.final2 <- rbind(fdb.final2, jtemp)
  }
}

rownames(fdb.final2) <- NULL
write.csv(fdb.final2, paste(filesdir5, "Output1_DBPedia.csv", sep=""))



