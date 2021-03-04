# Written by: Fotis Papailias, 2020
# (c) European Commission, Eurostat
# Please do NOT use/reproduce without citing!
#
# Supply of statistical services in statistical methodology 
# Lot 1: Methodological support
# Smart data for multinational enterprises (MNEs)
# Framework Contract No 2018.0086
# Ref. No.: ESTATMET2-000050-6000054239-REQ-01

# Remove items from memory
rm(list=ls(all=TRUE))

# Set the working directory
setwd("../Github/Task 2/")

# Load the custom-made functions
source("functions1_GLEIF.R")

# Load the igraph library which makes the bubble plots
library("igraph")

# Set .pdf settings in inches - you need to adjust this
# accordingly to the image sizes.. So, setting large
# dimensions is generally safer.
wwidth <- 16.5*3
hheight <- 11.7*3

# Do you want to save the output as:
# 1 : square matrix, or
# 2 : stacked (as in panel data)?
#
# Option 2 is better as in Task 3 we can take all "stacked"
# output, standardise it and map it to the EGR.
save.option <- 2

# Load the GLEIF big file with ALL entities
entities <- read_gleif_entities_from_csv(file = "GLEIF/20200915-0000-gleif-goldencopy-lei2-golden-copy.csv")
relationships <- read_gleif_relationships_from_csv(file = "GLEIF/20200915-0000-gleif-goldencopy-rr-golden-copy.csv")

# Select some of them in case you need less later
entities_small <- entities[,c(1,2,12,37,38)]

# To start with, we need a .csv with the MNEs
x <- read.csv("MNEs_final.csv", header=TRUE)

# Companies not identified
cNAs <- NULL

# empty object to stack the output data
cDATA <- NULL

# Run the loop across all MNEs
for(i in 1:NROW(x))
{
  # Identify each company via provided LEI
  lei <- as.character(x[i,"ID_LEI"])
  
  # if the data is not available (for example, maybe a company has NA LEI)
  # then skip
  if(is.na(lei)){next}
  
  # Also, before continuing, check that LEI does not have
  # the "XXX" value that I put there in purpose
  if(substr(lei, start=1, stop=3)=="XXX"){
    lei <- unlist(strsplit(lei, split="-"))
    lei <- as.character(lei[2])
  }
  
  # and also supplied name
  ename <- as.character(x[i,"MNE_Estat_Name"])
  
  # If there is no data, keep track of this MNE here
  if(NROW(lei)==0){
    cNAs <- c(cNAs, as.character(x[i,"MNE_Estat_Name"]))
  }
  
  # Extract data
  crid <- which(entities[,"lei"]==lei)
  if(NROW(crid)==0){stop("LEI does not exist in database")}
  crdata <- entities[crid,]
  
  # Save it accordingly
  if(save.option==1){
    crdata2 <- crdata
  }
  if(save.option==2){
    colnames.crdata <- colnames(crdata)
    values.crdata <- as.character(crdata)
    N <- NROW(values.crdata)
    crdata2 <- cbind(rep(ename, N), colnames.crdata, values.crdata)
  }
  
  # and put it under cDATA so the loop can continue
  cDATA <- rbind(cDATA, crdata2)
  
  # add a tracker so we can see where we are...
  cat("Now doing ", i, " of ", NROW(x), "\n")
}

# Make sure that all companies were identified and data retrieved!
if(NROW(cNAs)>0){
  stop("There are companies with LEIs which were not found in the main DB!")
}

# The second part of the code which identifies 
# Who owns whom and makes the figure
fdrel <- NULL
fdrelSLf <- NULL

pdf("GLEIF_Rel.pdf", width=wwidth, height=hheight)
for(i in 1:NROW(x))
{
  lei <- as.character(x[i,"ID_LEI"])
  
  # if the data is not available (for example, maybe a company has NA LEI)
  # then skip
  if(is.na(lei)){next}
  
  # Also, before continuing, check that LEI does not have
  # the "XXX" value that I put there in purpose
  if(substr(lei, start=1, stop=3)=="XXX"){
    lei <- unlist(strsplit(lei, split="-"))
    lei <- as.character(lei[2])
  }
  
  # Identify the companies which are owned by the supplied MNE
  lid <- which(relationships[,"end_node_id"]==lei)
  
  # If there is no company, simply skip
  if(NROW(lid)==0){ next}
  
  rldata <- relationships[lid,]
  
  # keep the actives
  lid <- which(rldata[,"relationship_status"]=="ACTIVE")
  rldata <- rldata[lid,]
  
  # remove duplicates
  lid <- which(duplicated(rldata[,1])==FALSE)
  rldata <- rldata[lid,]

  # Then, go one level down, and identify 
  # companies which are owned by the subsidiaries
  # owned by the MNE
  rldataSLf <- NULL
  for(j in 1:NROW(rldata))
  {
    jLEI <- as.character(rldata[j,1])
    lid <- which(relationships[,"end_node_id"]==jLEI)
    if(NROW(lid)==0){ next }
    rldataSL <- relationships[lid,]
    lid <- which(duplicated(rldataSL[,1])==FALSE)
    rldataSL <- rldataSL[lid,]
    rldataSLf <- rbind(rldataSLf, rldataSL)
  }
  
  # create a vector with names instead of LEIs
  rldata2 <- rldata
  for(j in 1:NROW(rldata))
  {
    gid <- which(entities[,1]==as.character(rldata[j,1]))
    rldata2[j,1] <- as.character(entities[gid,2])
    
    gid <- which(entities[,1]==as.character(rldata[j,2]))
    rldata2[j,2] <- as.character(entities[gid,2])
  }
  
  # Do the same for the second level
  rldataSLf2 <- rldataSLf
  if(is.null(rldataSLf)==FALSE)
  {
    for(j in 1:NROW(rldataSLf))
    {
      gid <- which(entities[,1]==as.character(rldataSLf[j,1]))
      rldataSLf2[j,1] <- as.character(entities[gid,2])
      
      gid <- which(entities[,1]==as.character(rldataSLf[j,2]))
      rldataSLf2[j,2] <- as.character(entities[gid,2])
    }
  }
  
  # Put Leis and names together
  rldata3 <- cbind(rldata[,1:2], rldata2)
  rldataSLf3 <- cbind(rldataSLf[,1:2], rldataSLf2)
  
  # save them for output
  fdrel <- rbind(fdrel, rldata3)
  fdrelSLf <- rbind(fdrelSLf, rldataSLf3)
  
  # create the relationships and plot
  g <- graph.data.frame(rbind(rldata2, rldataSLf2), directed = T)
  plot(g)
  
  # add a tracker so we can see where we are...
  cat("Now doing plot ", i, " of ", NROW(x), "\n")
}
dev.off()

# Export the output in .csv files to be used for Task 3
write.csv(cDATA, "GLEIF-cDATA.csv")
write.csv(fdrel, "GLEIF-fdrel.csv")
write.csv(fdrelSLf, "GLEIF-fdrelSLf.csv")
