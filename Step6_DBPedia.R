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

# Load necessary libraries to obtain the data using JSON calls
# from the API
library(rjson)
library(purrr)
library(rlist)

# To start with, we need a .csv with the MNEs
x <- read.csv("MNEs_final.csv", header=TRUE)

# The queries have some "fixed" parts which are the followings:
query <- "http://dbpedia.org/data/"
queryg <- ".json"
# we will use the above parts, together with the varying parts
# in the loop below

# Create empty object to save the "stacked" results
all_stacked <- NULL

# Run the loop across MNEs
for(i in 1:NROW(x))
{
  # Read the corresponding ID
  id <- x[i,"ID_DBPedia"]
  
  # if the data is not available, then skip
  if(is.na(id)){next}
  
  # Take the fixed query parts with the above id
  # and put them together
  queryf <- paste(query, id, queryg, sep="")
  json_data <- fromJSON(file=queryf)
  # json_data <- json_data[[2]]
  json_data2 <- flatten(json_data)
  
  # unlist the items
  k <- list.flatten(json_data2, use.names = TRUE, classes = "ANY")
  k2 <- cbind(names(k), as.vector(unlist(k)))
  k3 <- cbind(rep(as.character(x[i,"MNE_Estat_Name"]), NROW(k2)), k2)

  # stack them to the object
  all_stacked <- rbind(all_stacked, k3)
  
  # add a tracker so we can see where we are...
  cat("Now doing ", i, " of ", NROW(x), "\n")
}

# save the file in .csv
write.csv(all_stacked, "DBPedia_data.csv")
