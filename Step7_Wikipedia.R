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

# Load necessary libraries to obtain the data
# extracting it from the XML tables
library(httr)
library(XML)

# To start with, we need a .csv with the MNEs
x <- read.csv("MNEs_final.csv", header=TRUE)

# Create empty object to save the "stacked" results
all_stacked <- NULL

# Run the loop across MNEs
for(i in 1:NROW(x))
{
  # Read the corresponding ID
  idURL <- x[i,"ID_Wikipedia"]
  
  # if the data is not available, then skip
  if(is.na(idURL)){next}
  
  # get the URL using the packages
  r <- GET(as.character(idURL))
  
  # read it as a table
  doc <- readHTMLTable(doc=content(r, "text"))
  
  # obtain the data which is usually stored
  # at the top
  k1 <- doc[1]
  k1 <- k1[[1]]
  
  # put them together with the names
  k3 <- cbind(rep(x[i,"MNE_Estat_Name"], NROW(k1)), k1)
  
  # For some reason Lufthansa has 4 columns (but data is not relevant)
  if(x[i,"MNE_Estat_Name"]=="Deutsche Lufthansa Ag"){
    k3 <- k3[,1:3]
  }
  # stack them
  all_stacked <- rbind(all_stacked, k3)
  
  # add a tracker so we can see where we are...
  cat("Now doing ", i, " of ", NROW(x), "\n")
}

# save the file in .csv
write.csv(all_stacked, "Wikipedia_data.csv")
