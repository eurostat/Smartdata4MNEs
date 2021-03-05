Tasks 3 and 4 are concerned with the "cleaning" and "organisation" of the raw dataset obtained in Task 2.
To facilitate the replication of programs, we have organised them in folders.  As for Task 2, we use the generic "../Github/" directory
which, obviously, should be replaced with the appropriate user directory.

Steps 1 to 7 work in a similar manner, i.e. take the output data from Task 2 and try to organise it and cleaning.
In these steps, the user needs to look close to the data to identify potential fields which can be used as variables.
This is usually done in lines about 30 to about 70 which is also explained below.
Then, we manually create the corresponding Varlists files - which can be found in the "Task 3 - Fields CSV" directory.
Once this is done for the first time we do this, there is no need to do it again.
So, the researcher who does NOT wish to do this manually, can simply download the varlist files we have available in the appropriate
directory and reprocude the results.

Step 8 code uses all the above "cleaned" data obtained from Steps 1 to 7 and puts it under a common database.

Step 9 provides some availability statistics.


#########################################################
# Load Data
fname <- paste(filesdir1, "DATAFILE.csv", sep="")
x <- read.csv(fname, header=TRUE)

# Obtain and save the date of retrieval
finfo <- file.info(fname)
fd1 <- finfo$mtime
fd2 <- finfo$ctime
fd3 <- finfo$atime
fdd <- as.Date(min(fd1, fd2, fd3))
write.csv(fdd, paste(filesdir2, "RetrievalDate1_DBPedia.csv", sep=""))  ## here we export the date we obtained the data (date time stamp)

# Obtain unique fields:
unqf <- unique(as.character(x[,3]))  ## here we obtain ALL the fields from Task 2

# save the fields output
write.csv(unqf, paste(filesdir3, "Fields1_DBPedia.csv", sep=""))  ## here we export these fields to "manually" inspect them

# Check some values using an example below
##########################################################
i <- "Compass Group Plc"
ipos <- which(as.character(x[,2])==i)
xdt <- apply(x[ipos,], 2, as.character)

j <- 279
jf <- unqf[j]
jpos <- which(xdt[,3]==jf)
xdch <- xdt[jpos,]
xdch

#########################################################

after the above, we have a good understanding of the fields which could be used as variables.
Then, we put these fields in our "Masterfile" and read the variables we want to keep from there.
The above procedure is done manually, and not automatically in R, to demonstrate to the user how 
she could also obtain and manipulate data from different sources.
