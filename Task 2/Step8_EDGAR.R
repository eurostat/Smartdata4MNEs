# Written by: Fotis Papailias, 2020
# (c) European Commission, Eurostat
# Please do NOT use/reproduce without citing!
#
# Supply of statistical services in statistical methodology 
# Lot 1: Methodological support
# Smart data for multinational enterprises (MNEs)
# Framework Contract No 2018.0086
# Ref. No.: ESTATMET2-000050-6000054239-REQ-01

# It is important to notice that according to the
# demo_MNEs_EDGAR2.csv, we download data for 7 MNEs
# which have accessible data for the US.
# This is due to the fact that these MNEs are required
# to fill the SEC information by the US SEC.

# Remove items from memory
rm(list=ls(all=TRUE))

# Set the working directory
setwd("../Github/Task 2/")

# Load necessary libraries to obtain the data
# extracting it from the XML tables
library(selectr)
library(xml2)
library(rvest)
library(purrr)
library(rjson)
library(rlist)

# Load some custom-made functions to be used later
source("functions3_EDGAR.R")

# To start with, we need a .csv with the MNEs
x <- read.csv("demo_MNEs_EDGAR2.csv", header=TRUE)

# Create empty object to save the "stacked" results
all_stacked <- NULL

# Run the loop across MNEs
for(i in 1:NROW(x))
{
  # Load the identifiers
  id_cik <- as.character(x[i,"ID_EDGAR_cik"])
  id_acc <- as.character(x[i,"ID_EDGAR_AccessionNo"])
  
  # Make them in a suitable format (i.e. remove "-", etc.)
  id_cik2 <- unlist(strsplit(id_cik, split="cik"))[2]
  id_acc2 <- unlist(strsplit(id_acc, split="acc"))[2]
  id_acc3 <- unlist(strsplit(id_acc2, split="-"))
  id_acc4 <- NULL
  for(j in 1:NROW(id_acc3))
  {
    id_acc4 <- paste(id_acc4, id_acc3[j], sep="")
  }
  
  # There are three useful files we can download:
  # R2.htm: Balance Sheet
  # R4.htm: Income Statement (Statements of Operations)
  # R7.htm: Statements of Cash Flow
  url_start <- "https://www.sec.gov/Archives/edgar/data/"
  # RR <- c("R2.htm", "R4.htm", "R7.htm")
  # RRd <- c("Balance Sheet", "Income Statement", "Cash Flow")
  #
  # Update: 02/10/2020 - R7.htm causes a lot of inconsistencies
  # so we drop it out
  #
  RR <- c("R2.htm", "R4.htm")
  RRd <- c("Balance Sheet", "Income Statement")
  
  # create object
  tbl3 <- NULL
  
  # Run the loop across the three htm files/tables
  for(ir in 1:NROW(RR))
  {
    # Obtain the corresponding URL and create the query
    url_page <- RR[ir]
    url <- paste(url_start, id_cik2, "/", id_acc4, "/", url_page, sep="")
    
    # Read the url, identify the table and isolate it
    w <- read_html(url)
    tbls <- html_nodes(w, "table")
    tbls_ls <- w %>% html_nodes("table") %>% .[1] %>% html_table(fill = TRUE)
    tbl <- tbls_ls[[1]]
    tbl <- rbind(colnames(tbl), tbl)
    
    # save the original version of this table as we are about to change it
    tbl.save <- tbl
    
    # Before continuing, let's make sure that the table selection is correct
    # 1. Remove any column which has the majority with NA values
    pp <- as.numeric(colSums(is.na(tbl)))
    ppout <- which(pp>=NROW(tbl)*0.75)
    if(NROW(ppout)>0){
      tbl <- tbl[,-ppout]
    }
    
    # identify if there is any row which is totally empty
    # if there exists, then delete everything after this line
    # as it is going to be footnotes
    # pp <- as.numeric(rowSums(tbl==""))
    # ppout <- which(pp==NCOL(tbl))
    ppout <- which(tbl[,1]=="")
    if(NROW(ppout)>0){
      ppout <- ppout:NROW(tbl)
      tbl <- tbl[-ppout,]
    }
    
    # Now that we have the table, we need to make it in "stacked" version
    # so it easily matches with the rest (and it's also more useful to run the loop
    # across many MNEs and different URLs)
    
    # Column 1 is always the variable we need but specific labels
    # change in R2, R4, R7 - so check below how we deal with this
    
    # Make proper row selection according to table
    if(url_page=="R2.htm"){
      row.selection <- 3:NROW(tbl)
      
      # also make sure that the first two lines are NOT! the same
      f1 <- as.character(tbl[1,])
      f2 <- as.character(tbl[2,])
      for(fi in 1:NROW(f1))
      {
        if(f1[fi]==f2[fi]){f2[fi] <- NA}
      }
      if(sum(is.na(f2))>0){
        f2[which(is.na(f2))] <- f2[which(is.na(f2)==FALSE)][1]
      }
      tbl[1,] <- f1
      tbl[2,] <- f2
    }
    if(url_page=="R4.htm"){ row.selection <- 2:NROW(tbl) }
    if(url_page=="R7.htm"){ row.selection <- 3:NROW(tbl) }
    
    # create temp tables for saving results
    tbl.v1 <- tbl[row.selection,1]
    tbl2 <- NULL
    
    # run the loop across columns
    for(j in 2:NCOL(tbl))
    {
      # Extract numbers and make them in a suitable format
      mstep1 <- tbl[row.selection,j]
      
      # Do some further processing to split them up
      xx <- mstep1
      xx1 <- xx2 <- rep(NA, NROW(xx))
      for(jj in 1:NROW(xx))
      {
        z <- xx[jj]
        z1 <- z2 <- NULL
        if(z==""){
          z1 <- NA
          z2 <- NA
          next
        }
        zz <- unlist(strsplit(z, " "))
        if(NROW(zz)==1){
          z1 <- zz[1]
          z2 <- NA
        }else{
          z1 <- zz[2]
          z2 <- zz[1]
        }
        
        # Before finishing, make sure there are no brackets
        z1 <- convert.brackets(z1)
        z2 <- convert.brackets(z2)
        
        # save them
        xx1[jj] <- z1
        xx2[jj] <- z2
      }
      
      # Remove commas - if any -
      xx1 <- gsub(",","",xx1)
      xx2 <- gsub(",","",xx2)
      
      # Add the extra info if it exists (depends on the htm file)
      if(url_page=="R2.htm"){ mstep2 <- rep(tbl[1,j], NROW(mstep1)) }
      if(url_page=="R4.htm"){ mstep2 <- rep(NA, NROW(mstep1)) }
      if(url_page=="R7.htm"){ mstep2 <- rep(tbl[1,j], NROW(mstep1)) }
      
      # make proper date
      if(url_page=="R2.htm"){ proper.date <- transform.date(as.character(tbl[2,j])) }
      if(url_page=="R4.htm"){ proper.date <- transform.date(as.character(tbl[1,j])) }
      if(url_page=="R7.htm"){
        wdate <- as.character(tbl[2,j])
        if(wdate==""){wdate <- as.character(tbl[1,j])}
        proper.date <- transform.date(wdate)
      }
      
      mstep3 <- rep(proper.date, NROW(mstep1))
      
      # Put everything together
      mfout <- cbind(rep(url_page, NROW(xx1)), tbl.v1, mstep2,
                     proper.date, xx1, xx2)
      
      # and stack them
      tbl2 <- rbind(tbl2, mfout)
    }
    
    # append the data
    tbl3 <- rbind(tbl3, tbl2)
  }
  
  # Add the company name
  k2 <- cbind(rep(as.character(x[i,"MNE_Estat_Name"]), NROW(tbl3)), tbl3)

  # Stack them
  all_stacked <- rbind(all_stacked, k2)

  # add a tracker so we can see where we are...
  cat("Now doing ", i, " of ", NROW(x), "\n")
}

# save the file in .csv
write.csv(all_stacked, "EDGAR_data.csv")
