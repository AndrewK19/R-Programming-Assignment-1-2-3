#set working directory to assignment file location
setwd(readClipboard())
list.files()

#Now that the directory is set, read in the first .csv file to analyze/plot
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

#explore the data with ncol, nrow, names, str, summary, etc
nrow(outcome)
ncol(outcome)
names(outcome)

#PART 1:
#plot 30 day mortality rate from heart attack on a histogram
#be originally read the data as characters, need to coerice to numeric for histogram
outcome[,11] <- as.numeric(outcome[,11]) 
#plot hist
hist(outcome[,11])