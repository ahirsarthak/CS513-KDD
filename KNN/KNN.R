
####################################################################
#UNIVERSITY : STEVENS INSTITUE OF TECHNOLOGY
#Project : HW 03 KNN 
#Purpose : Use the knn methodology to develop a classification models 
#First Name : Sarthak
#Last Name : Ahir
#CWID : 10479028
#Date : 10/18/2021
####################################################################
rm(list=ls())

#Load the “breast-cancer-wisconsin.csv” from canvas into R and perform the analysis

newDataSet <- read.csv("~/Downloads/breast-cancer-wisconsin.csv", na.string = "?")

# column contains non-numeric characters, we can see that the summary for only column F6 has not been calculated.
newDataSet$F6 <- as.numeric(as.character(newDataSet$F6))

#Summarizing each column 
summary(newDataSet)

View(newDataSet) 
View(newDataSet) 
str(newDataSet)
is.data.frame(newDataSet) 

#deleting rows with NA values
newSet <- na.omit(newDataSet)

View(newSet)


#Convert labels to factor class
newSet$Class<- factor(newSet$Class , levels = c("2","4") , labels = c("Benign","Malignant"))

#KNN

#30% test data and 70% training
formula <- sample(1:nrow(newSet), 0.7 * nrow(newSet)) 
normalFunc <-function(x) { (x -min(x))/(max(x)-min(x))   }

##Run nomalization on first 4 coulumns of dataset 
norm <- as.data.frame(lapply(newSet[,c(2,3,4,5,6,7,8,9,10)], normalFunc))

df = newSet['Class']

#train set

train <- norm[formula,] 
classTrain <- df[formula,]

##test set

test <- norm[-formula,] 
classTest <- df[-formula,]

#load the package - class
library(class)

#running knn for k = 3
clf <- knn(train,test,cl=classTrain,k=3)

#generating confusion matrix
confusion_matrix <- table(clf, classTest)
print(confusion_matrix)

#Calculating Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(confusion_matrix)

#run knn  for k = 5
clf <- knn(train,test,cl=classTrain,k=5)

#generating confusion matrix
confusion_matrix <- table(clf, classTest)
print(confusion_matrix)

#Calculating Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(confusion_matrix)

#run knn  for k = 10
clf <- knn(train,test,cl=classTrain,k=10)

#generating confusion matrix
confusion_matrix <- table(clf, classTest)
print(confusion_matrix)

#Calculating Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(confusion_matrix)

