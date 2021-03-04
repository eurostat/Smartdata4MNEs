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

# Load the corresponding library
library(gtrendsR)

# To start with, we need a .csv with the MNEs
x <- read.csv("MNEs_final.csv", header=TRUE)

# Create empty object to save the "stacked" results
all_stacked <- NULL

# Run the loop across MNEs
for(i in 1:NROW(x))
{
  # Read the corresponding name and use it as a keyword to the WORLD
  id <- x[i,"MNE_Estat_Name"]
  
  # if the data is not available, then skip
  if(is.na(id)){next}
  
  # pause for 5 seconds
  Sys.sleep(5)
  
  # obtain the time series
  res <- gtrends(as.character(id))
  
  # extract the interest over time
  iot <- res$interest_over_time
  
  # and add the name/stack version
  k3 <- cbind(rep(x[i,"MNE_Estat_Name"], NROW(iot)), iot[,1:2])
  
  # save it to the rest
  all_stacked <- rbind(all_stacked, k3)
  
  # add a tracker so we can see where we are...
  cat("Now doing ", i, " of ", NROW(x), "\n")
}

# save the file in .csv
write.csv(all_stacked, "GTrends_data.csv")
