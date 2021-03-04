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

# load package
library(WikidataQueryServiceR)
source("functions2_WIKIDATA.R")

# To start with, we need a .csv with the MNEs
x <- read.csv("MNEs_final.csv", header=TRUE)

# create a list to store items
wlist <- list()

# And also an empty object to do the "stacked" version
wlist_stack <- NULL

# Now run the loop for each of the companies
for(i in 1:NROW(x))
{
  # Read the Wikidata ID
  qID <- as.character(x[i,"ID_Wikidata"])
  
  # if the data is not available, then skip
  if(is.na(qID)){next}
  
  # Read the ESTAT MNE name
  qName <- as.character(x[i,"MNE_Estat_Name"])
  
  # Use the custom-made function to scrap the data
  out <- getWIKIdata(qID)
  out.cnames <- colnames(out)
  
  # Put everything together
  out <- cbind(rep(qName, NROW(out)), out)
  colnames(out) <- c("CompEstatName", out.cnames)
  
  # And save them in the final object
  wlist <- c(wlist, list(out))
  wlist_stack <- rbind(wlist_stack, out)
  
  # add a tracker so we can see where we are...
  cat("Now doing ", i, " of ", NROW(x), "\n")
}

# Export the results in .csv
write.csv(wlist_stack, "Wikidata.csv")

