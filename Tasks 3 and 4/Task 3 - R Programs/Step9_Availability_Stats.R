# Written by: Fotis Papailias, 2020
# (c) European Commission, Eurostat
# Please do NOT use/reproduce without citing!
#
# Supply of statistical services in statistical methodology 
# Lot 1: Methodological support
# Smart data for multinational enterprises (MNEs)
# Framework Contract No 2018.0086
# Ref. No.: ESTATMET2-000050-6000054239-REQ-01
#
# Once we have selected the fields and cleaned the output
# Using the codes for Steps 1 to 7
# we take all the "cleaned output" and put it under a unified
# database
#
# We also need the retrieval dates (also output from Steps 1 to 7)

# Remove items from memory
rm(list=ls(all=TRUE))

# Set the working directories
filesdir1 <- "E:/Dropbox/ESTAT-MNEs/Tasks 3 and 4 - Final/Task 3 - Output CSV/"
filesdir2 <- "E:/Dropbox/ESTAT-MNEs/Tasks 3 and 4 - Final/Task 3 - Additional Fields and Stats CSV/"

# Read the retrieval dates
x <- read.csv(paste(filesdir1, "Output8_FinalDatabase.csv", sep=""), header=TRUE)
x <- apply(x[,2:NCOL(x)], 2, as.character)
cnames <- colnames(x)
rownames(x) <- colnames(x) <- NULL
colnames(x) <- cnames

# Availability by Source
xid <- 1
id <- sort(unique(x[,xid]))
av <- NULL
for(j in 1:NROW(id))
{
  jpos <- which(x[,xid]==id[j])
  xtemp <- x[jpos,]
  noNA <- na.omit(xtemp[,5])
  av <- c(av, NROW(noNA))
}
out1 <- cbind(id, av)

# Availability by Company
xid <- 3
id <- sort(unique(x[,xid]))
av <- NULL
for(j in 1:NROW(id))
{
  jpos <- which(x[,xid]==id[j])
  xtemp <- x[jpos,]
  noNA <- na.omit(xtemp[,5])
  av <- c(av, NROW(noNA))
}
# Create some bands
av <- as.numeric(av)
avb <- NULL
for(j in 1:NROW(av))
{
  xch <- av[j]
  if((xch>=0 & xch<=50)){xcb <- "MNE_Band1(0-50)"}
  if((xch>=51 & xch<=100)){xcb <- "MNE_Band2(51-100)"}
  if((xch>=101 & xch<=200)){xcb <- "MNE_Band3(101-200)"}
  if((xch>=201 & xch<=300)){xcb <- "MNE_Band4(201-300)"}
  if((xch>=301)){xcb <- "MNE_Band5(301+)"}
  avb <- c(avb, xcb)
}
out2 <- cbind(id, av, avb)


# Availability by Variable
xid <- 4
id <- sort(unique(x[,xid]))
av <- NULL
for(j in 1:NROW(id))
{
  jpos <- which(x[,xid]==id[j])
  xtemp <- x[jpos,]
  noNA <- na.omit(xtemp[,5])
  av <- c(av, NROW(noNA))
}

# Create some bands
av <- as.numeric(av)
avb <- NULL
for(j in 1:NROW(av))
{
  xch <- av[j]
  if((xch>=0 & xch<=25)){xcb <- "Var_Band1(0-25)"}
  if((xch>=26 & xch<=50)){xcb <- "Var_Band2(26-50)"}
  if((xch>=51 & xch<=100)){xcb <- "Var_Band3(51-100)"}
  if((xch>=101 & xch<=150)){xcb <- "Var_Band4(101-150)"}
  if((xch>=151 & xch<=200)){xcb <- "Var_Band5(151-200)"}
  if((xch>=201 & xch<=300)){xcb <- "Var_Band6(201-300)"}
  if((xch>=301)){xcb <- "Var_Band7(300+)"}
  avb <- c(avb, xcb)
}
out3 <- cbind(id, av, avb)

write.csv(out1, paste(filesdir2, "Avail1_Src.csv", sep=""))
write.csv(out2, paste(filesdir2, "Avail2_MNE.csv", sep=""))
write.csv(out3, paste(filesdir2, "Avail3_Var.csv", sep=""))

# Finally, add the variable category
varcat <- read.csv(paste(filesdir2, "Additional_Fields.csv", sep=""), header=TRUE)
varcat <- as.matrix(varcat)

# Also put these bands in the final database
x2 <- cbind(x[,1:3], rep(NA, NROW(x)), rep(NA, NROW(x)), rep(NA, NROW(x)),
            rep(NA, NROW(x)),
            x[,4], rep(NA, NROW(x)), x[,5:7])
for(j in 1:NROW(x))
{
  icomp <- x[j,3]
  iband <- out2[which(out2[,1]==icomp),3]
  x2[j,4] <- iband
  
  icomp <- x[j,4]
  iband <- varcat[which(varcat[,1]==icomp),2]
  x2[j,5] <- iband
  
  icomp <- x[j,4]
  iband <- varcat[which(varcat[,1]==icomp),3]
  x2[j,6] <- iband
  
  icomp <- x[j,4]
  iband <- varcat[which(varcat[,1]==icomp),4]
  x2[j,7] <- iband
  
  ivar <- x[j,4]
  iband <- out3[which(out3[,1]==ivar),3]
  x2[j,9] <- iband
}

rownames(x2) <- NULL
colnames(x2) <- c("Src", "RtrvDate", "ESTAT_MNE", "MNE_Band", "Var_Ctg", "Var_Type",
                  "EGR", "Var", "Var_Band", "Value", "VarDetailType",
                  "VarDetailValue")

# Before the final editing, fix some of the remaining dates
x3 <- x2
for(j in 1:NROW(x2))
{
  xtemp <- x2[j,10]
  
  xch1 <- unlist(gregexpr(pattern ="T",xtemp))
  xch2 <- unlist(gregexpr(pattern ="\\:",xtemp))
  xch3 <- unlist(gregexpr(pattern ="z",xtemp))
  if(is.na(xch1)|is.na(xch2)|is.na(xch3)){
    next
  }
  if((NROW(xch1)>0)&(NROW(xch2)>0)&(NROW(xch3)>0)){
    xtempnew <- unlist(strsplit(xtemp, "T"))[1]
    x3[j,10] <- xtempnew
  }
}

write.csv(x3, paste(filesdir1, "Output9_FinalDatabase_withBands.csv", sep=""))
