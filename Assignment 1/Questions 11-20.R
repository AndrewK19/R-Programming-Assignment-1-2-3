#UUpload Data - ensure to change directory
x<-read.csv("hw1_data.csv")

#Print column names
colnames(x)

#Extract first two rows of the data
x[1:2,]

#How many rows are in this data frame?
nrow(x)


#extract the last two rows of this data frame
x[152:153,]

#what is the value of ozone in the 47th row?
x[[47,1]]

#count how many missing values in ozone column
#first option
table(is.na(x[,1]))
#second option
summary(x)

#What is the mean of the Ozone column in this dataset? 
#Exclude missing values (coded as NA) from this calculation.
mean(x[,"Ozone"],na.rm=TRUE)


#Extract the subset of rows of the data frame where Ozone values are above 31 and Temp values are above 90. 
#What is the mean of Solar.R in this subset?
z<-subset(x,Ozone>31 & Temp>90)
mean(z[,2])

#What is the mean of "Temp" when "Month" is equal to 6?
z<-subset(x,Month==6)
mean(z[,4])

#What was the maximum ozone value in the month of May (i.e. Month is equal to 5)?
z<-subset(x,Month==5)
max(z[,1],na.rm=TRUE)


