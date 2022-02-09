
####################################################################
#UNIVERSITY : STEVENS INSTITUE OF TECHNOLOGY
#Project : HW 02 EDA 
#Purpose : Perform the EDA analysis on breast-cancer-wisconsin data
#First Name : Sarthak
#Last Name : Ahir
#CWID : 10479028
#Date : 10/10/2021
####################################################################
rm(list=ls())

#Load the “breast-cancer-wisconsin.csv” from canvas into R and perform the EDA analysis

newDataSet <- read.csv("~/Downloads/breast-cancer-wisconsin.csv", na.string = "?")


#Summarizing each column 
summary(newDataSet)

View(newDataSet) 
is.data.frame(newDataSet) 

#Identifying missing values
is.na(newDataSet)
View(newDataSet)

#F6 missing values
is.na(newDataSet$F6)

#Replacing the missing values with the "mean" of the column.
x<-round(mean(newDataSet$F6, na.rm=TRUE))
newDataSet$F6[is.na(newDataSet$F6)]<-x

#summary of column
summary(newDataSet)

#Displaying the frequency table of "Class" vs. F6

freqTable <- table(Class = newDataSet$Class, F6 = newDataSet$F6)
print(freqTable)


#Displaying the scatter plot of F1 to F6, one pair at a time

plot(newDataSet$F1, newDataSet$F2, main="F1 and F2 Scatterplot",
     xlab="F1", ylab="F2", pch=21, bg = c("blue","yellow"))

plot(newDataSet$F2, newDataSet$F3, main="F2 and F3 Scatterplot ",
     xlab="F2", ylab="F3", pch=21, bg = c("green","yellow"))    


plot(newDataSet$F3, newDataSet$F4, main="F3 and F4 Scatterplot ",
     xlab="F3", ylab="F4", pch=21, bg = c("orange","purple"))   

plot(newDataSet$F4, newDataSet$F5, main="F4 and F5 Scatterplot ",
     xlab="F4", ylab="F5", pch=21, bg = c("red","orange"))

plot(newDataSet$F5, newDataSet$F6, main="F5 and F6 Scatterplot ",
     xlab="F5", ylab="F6", pch=21, bg = c("orange","green"))


#All pairs at once
plot(newDataSet[2:7], main = "F1 to F6 Scatter Plot", pch=21, bg = c("orange","green"))

#Show histogram plot for columns F7 to F9
hist(newDataSet$F7, col="green") 
hist(newDataSet$F8, col="blue") 
hist(newDataSet$F9, col="red")

# Box Plot for Columns F7 to F9
boxplot(newDataSet[8:10], main = "Histogram Box Plot for Columns F7 to F9")


#Delete all the objects from your R- environment. 
rm(list=ls())

#Reload the “breast-cancer-wisconsin.data.csv” from canvas into R
newDataSet <- read.csv("~/Downloads/breast-cancer-wisconsin.csv", na.string = "?")

#Remove any row with a missing value in any of the columns
newDataSet[,1:11][newDataSet[,1:11]=="?"] <- NA
newDataSet <- na.omit(newDataSet)
summary(newDataSet)
