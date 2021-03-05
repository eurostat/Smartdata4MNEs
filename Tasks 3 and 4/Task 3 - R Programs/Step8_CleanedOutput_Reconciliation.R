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
filesdir1 <- "E:/Dropbox/ESTAT-MNEs/Tasks 3 and 4 - Final/Task 3 - Retrieval Dates CSV/"
filesdir2 <- "E:/Dropbox/ESTAT-MNEs/Tasks 3 and 4 - Final/Task 3 - Output CSV/"

# Read the retrieval dates
setwd(filesdir1)
d1 <- read.csv("RetrievalDate1_DBPedia.csv")
d2 <- read.csv("RetrievalDate2_Wikidata.csv")
d3 <- read.csv("RetrievalDate3_Wikipedia.csv")
d4 <- read.csv("RetrievalDate4_GLEIF.csv")
d5 <- read.csv("RetrievalDate5_PERMID.csv")
d6 <- read.csv("RetrievalDate6_OpenCorp.csv")
d7 <- read.csv("RetrievalDate7_EDGAR.csv")

# Create a simple function to obtain the dates
getdate <- function(dd){ddd <- as.character(dd); dout <- ddd[2]; return(dout)}

d1 <- getdate(d1)
d2 <- getdate(d2)
d3 <- getdate(d3)
d4 <- getdate(d4)
d5 <- getdate(d5)
d6 <- getdate(d6)
d7 <- getdate(d7)

# Read the cleaned output for the selected
# variables
# id numbers should correspond with the above!
# i.e., "1" is DBPedia, "2" is Wikidata, etc....
setwd(filesdir2)
x1 <- read.csv("Output1_DBPedia.csv")
x2 <- read.csv("Output2_Wikidata.csv")
x3 <- read.csv("Output3_Wikipedia.csv")
x4 <- read.csv("Output4_GLEIF.csv")
x5 <- read.csv("Output5_PERMID.csv")
x6 <- read.csv("Output6_OpenCorp.csv")
x7 <- read.csv("Output7_EDGAR.csv")

isrcnames <- c("DBPedia", "Wikidata", "Wikipedia", "GLEIF",
               "PermID", "OpenCorp", "EDGAR")

# Remove the first column with a simple function
makeCHRrmvFCOL <- function(xx){
  xxx <- apply(xx, 2, as.character)
  return(xxx[,2:NCOL(xxx)])
}

x1 <- makeCHRrmvFCOL(x1)
x2 <- makeCHRrmvFCOL(x2)
x3 <- makeCHRrmvFCOL(x3)
x4 <- makeCHRrmvFCOL(x4)
x5 <- makeCHRrmvFCOL(x5)
x6 <- makeCHRrmvFCOL(x6)
x7 <- makeCHRrmvFCOL(x7)

# Also remove the column with the variable numbering
x1 <- x1[,-3]
x2 <- x2[,-3]
x3 <- x3[,-3]
x4 <- x4[,-3]
x5 <- x5[,-3]
x6 <- x6[,-3]
x7 <- x7[,-3]

# Check that the variable description ("Var") is in the second column 
# everywhere
which(colnames(x1)=="Var")
which(colnames(x2)=="Var")
which(colnames(x3)=="Var")
which(colnames(x4)=="Var")
which(colnames(x5)=="Var")
which(colnames(x6)=="Var")
which(colnames(x7)=="Var")

# Currently we have the following column format:
# x1: "ESTAT_MNE" "Var" "Value"
# x2: "ESTAT_MNE" "Var" "Value" "VarDetailType" "VarDetailValue"
# x3: "ESTAT_MNE" "Var" "Value" "VarDetailType" "VarDetailValue"
# x4: "ESTAT_MNE" "Var" "Value"
# x5: "ESTAT_MNE" "Var" "Value"                 "VarDetailValue"
# x6: "ESTAT_MNE" "Var" "Value"
# x7: "ESTAT_MNE" "Var" "Value" "VarDetailType" "VarDetailValue"
#
# We need to make them all in a similar way filled with NAs
x1 <- cbind(x1, matrix(NA, NROW(x1), 2))
x4 <- cbind(x4, matrix(NA, NROW(x4), 2))
x5 <- cbind(x5[,1:3], matrix(NA, NROW(x5), 1), x5[,4])
x6 <- cbind(x6, matrix(NA, NROW(x6), 2))

# Add the source and retrieval data to each case
x1 <- cbind(rep(isrcnames[1], NROW(x1)), rep(d1, NROW(x1)), x1)
x2 <- cbind(rep(isrcnames[2], NROW(x2)), rep(d2, NROW(x2)), x2)
x3 <- cbind(rep(isrcnames[3], NROW(x3)), rep(d3, NROW(x3)), x3)
x4 <- cbind(rep(isrcnames[4], NROW(x4)), rep(d4, NROW(x4)), x4)
x5 <- cbind(rep(isrcnames[5], NROW(x5)), rep(d5, NROW(x5)), x5)
x6 <- cbind(rep(isrcnames[6], NROW(x6)), rep(d6, NROW(x6)), x6)
x7 <- cbind(rep(isrcnames[7], NROW(x7)), rep(d7, NROW(x7)), x7)

fcname <- c("Src", "RtrvDate", "ESTAT_MNE", "Var",
            "Value", "VarDetailType", "VarDetailValue")
colnames(x1) <- colnames(x2) <- colnames(x3) <- colnames(x4) <- 
  colnames(x5) <- colnames(x6) <- colnames(x7) <- fcname

# Put everything together
X <- rbind(x1, x2, x3, x4, x5, x6, x7)
colnames(X) <- fcname

# 21/01/2021
# Check the indidividual lines are OK
# and will not cause a problem opening the files
# using Excel (or other spreadsheet software)
# with different Regional Settings
#
# Go through the "Value" field an try 
# to identify if there are multiple entries
# separated by "," [which might interfere with some Excel versions]
# and split them to different entries
options(warn=2)
X2 <- NULL
for(i in 1:NROW(X))
{
  zcheck <- X[i,]
  zval <- X[i,5]
  if(is.na(zval)==FALSE){
    if(as.character(zval)==""){next}
  }
  zval_splt <- unlist(strsplit(zval, split=","))
  if(NROW(zval)==NROW(zval_splt))
  {
    X2 <- rbind(X2, as.character(zcheck))
  }else{
    # create the block entries (so making sure that it will be open)
    # as individual rows
    ztemp <- NULL
    for(j in 1:NROW(zval_splt))
    {
      ztemp <- rbind(ztemp, as.character(zcheck))
    }
    ztemp[,5] <- as.character(zval_splt)
    ztemp <- apply(ztemp, 2, as.character)
    
    X2 <- rbind(X2, ztemp)
  }
}
options(warn=1)

# if everything worked fine, let's reset
colnames(X2) <- colnames(X)
X <- X2

# Now, in case we have made mistakes in previous steps for fields
# this is the time to fix them...
# Export, inspect and change accordingly
allfields <- sort(unique(X[,"Var"]))
# write.csv(allfields, "temp_allfields.csv")

icol <- "Var"

ifind <- which(X[,icol]=="AreaServed")
X[ifind,icol] <- "AreaServed3"

ifind <- which(X[,icol]=="Director1")
X[ifind,icol] <- "Director"

ifind <- which(X[,icol]=="Employees")
X[ifind,icol] <- "Employees3"

ifind <- which(X[,icol]=="OwnedBy")
X[ifind,icol] <- "OwnedBy3"

ifind <- which(X[,icol]=="ParentCompany")
X[ifind,icol] <- "ParentCompany3"

ifind <- which(X[,icol]=="Postal1")
X[ifind,icol] <- "Postal"

ifind <- which(X[,icol]=="Services")
X[ifind,icol] <- "Services3"

ifind <- which(X[,icol]=="Start Date")
X[ifind,icol] <- "StartDate"

# Find all companies and all final variables
# and create a "square" matrix (even though it's not a matrix)
# that has NAs in case we don't have information
N <- sort(unique(X[,3]))
V <- sort(unique(X[,4]))

fdb <- NULL
for(i in 1:NROW(N))
{
  icomp <- N[i]
  ipos <- which(X[,3]==icomp)
  xtemp <- X[ipos,]
  
  for(j in 1:NROW(V))
  {
    jvar <- V[j]
    jpos <- which(xtemp[,4]==jvar)
    if(NROW(jpos)==0){
      xout <- c(xtemp[1,1:3], jvar, NA, NA, NA)
      fdb <- rbind(fdb, xout)
    }
    if(NROW(jpos)==1){
      xout <- xtemp[jpos,]
      fdb <- rbind(fdb, xout)
    }
    if(NROW(jpos)>1){
      xout <- xtemp[jpos,]
      fdb <- rbind(fdb, xout)
    }
  }
}

ipos <- which(fdb[,7]=="")
fdb[ipos,7] <- NA

# Also, some of the names seem to be wrong
# perhaps of encoding?
# fix them here
ipos <- which(fdb[,3]=="A.P. Møller Og Hustru Chastine Mc-Kinney Møllers Fond Til Almene Formaal")
fdb[ipos,3] <- "A.P. Moller"

ipos <- which(fdb[,3]=="Adolf WÃ¼rth Gmbh & Co. Kg")
fdb[ipos,3] <- "Adolf Wurth Gmbh & Co. Kg"

ipos <- which(fdb[,3]=="Adolf Würth Gmbh & Co. Kg")
fdb[ipos,3] <- "Adolf Wurth Gmbh & Co. Kg"

ipos <- which(fdb[,3]=="Coöperatieve Rabobank U.A.")
fdb[ipos,3] <- "Cooperatieve Rabobank U.A."

ipos <- which(fdb[,3]=="CoÃ¶peratieve Rabobank U.A.")
fdb[ipos,3] <- "Cooperatieve Rabobank U.A."

ipos <- which(fdb[,3]=="NestlÃ© S.A.")
fdb[ipos,3] <- "Nestle S.A."

ipos <- which(fdb[,3]=="Nestlé S.A.")
fdb[ipos,3] <- "Nestle S.A."

rownames(fdb) <- NULL
colnames(fdb) <- colnames(X)
write.csv(fdb, "Output8_FinalDatabase.csv")

# Now, export the final variables list
# map them to EGR
# create the corresponding categories
# and then, add them to the database
V <- sort(unique(fdb[,4]))
# write.csv(V, "temp_allfields.csv")






