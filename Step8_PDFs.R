# Written by: Fotis Papailias, 2020
# (c) European Commission, Eurostat
# Please do NOT use/reproduce without citing!
#
# Supply of statistical services in statistical methodology 
# Lot 1: Methodological support
# Smart data for multinational enterprises (MNEs)
# Framework Contract No 2018.0086
# Ref. No.: ESTATMET2-000050-6000054239-REQ-01

# The .pdf reports have been downloaded from:
# https://www.annualreports.com/

# Remove items from memory
rm(list=ls(all=TRUE))

# Set the working directory
setwd("../Github/Task 2/")

# Which directory has the .pdf files?
pdfdir <- "../Github/Task 2/PDFs/"

# load necessary packages
library("pdftools")
library("tidyverse")

# Load necessary custom functions
source("functions3_PDFs.R")

# To start with, we need a .csv with the MNEs
x <- read.csv("demo_MNEs_PDF.csv", header=TRUE)

# Normally, we would run through a loop
# However, .pdf reports are very heterogeneous
# and we need to go file by file...

##############################################################################
### CARREFOUR_2018.pdf
##############################################################################
i <- 1
ename <- as.character(x[i,"MNE_Estat_Name"])
fname <- as.character(x[i,"ID_PDF"])
fdir <- paste(pdfdir, fname, sep="")

# converting PDF to text
textFile <- pdf_text(fdir)

# This has the same number of pages as the file.
# We want the information on pages 23 - (page 29 is mixed up)
p <- textFile[23]
table <- t(str_split(p, "\n", simplify = TRUE))

# Extract anything useful
u <- c(11, 14, 20, 22, 30, 31, 39, 43, 48, 61, 64, 66, 71, 81, 83)
u <- split.after.doublespace(table[u])

# put it together
out <- cbind(rep(ename, NROW(u)), u)
write.csv(out, paste(pdfdir, "1_PDFs_Carrefour.csv", sep=""))
##############################################################################

##############################################################################
### DAIMLER_2018.pdf
##############################################################################
i <- 2
ename <- as.character(x[i,"MNE_Estat_Name"])
fname <- as.character(x[i,"ID_PDF"])
fdir <- paste(pdfdir, fname, sep="")

# converting PDF to text
textFile <- pdf_text(fdir)

# Everything as before using selected pages
p <- textFile[2]
table <- t(str_split(p, "\n", simplify = TRUE))
at <- remove.empty.and.split(table[3], " ")
at <- c(" ", at)
a1 <- at

at <- remove.empty.and.split(table[5], " ")
at <- at[c(1, 2, 3, 5)]
a2 <- at

at <- remove.empty.and.split(table[6], " ")
at <- c(paste(at[1], at[2], at[3], at[4], at[5], at[6], sep=" "),
        at[7:9])
a3 <- at

at <- remove.empty.and.split(table[7], " ")
at <- c(paste(at[1], at[2], at[3], at[4], sep=" "),
        at[5:7])
a4 <- at

at <- remove.empty.and.split(table[8], " ")
at <- c(paste(at[1], at[2], at[3], at[4], at[5], at[6], at[7], sep=" "),
        at[8:10])
a5 <- at

at <- remove.empty.and.split(table[9], " ")
at <- at[c(1, 2, 3, 5)]
a6 <- at

at <- remove.empty.and.split(table[10], " ")
at <- c(paste(at[1], at[2], sep=" "),
        at[c(3, 4, 6)])
a7 <- at

at <- remove.empty.and.split(table[11], " ")
at <- c(paste(at[1], at[2], at[3], at[4], at[5], sep=" "),
        at[c(6, 7, 9)])
a8 <- at

at <- remove.empty.and.split(table[12], " ")
at <- c(paste(at[1], at[2], at[3], at[4], at[5], sep=" "),
        at[c(6, 7, 8)])
a9 <- at

at <- remove.empty.and.split(table[13], " ")
at <- c(paste(at[1], at[2], at[3], sep=" "),
        at[4:6])
a10 <- at

aall <- rbind(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
u <- aall

# put it together
out <- cbind(rep(ename, NROW(u)), u)
write.csv(out, paste(pdfdir, "2_PDFs_DAIMLER.csv", sep=""))
##############################################################################