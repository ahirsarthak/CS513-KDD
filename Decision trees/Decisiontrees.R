
####################################################################
#UNIVERSITY : STEVENS INSTITUE OF TECHNOLOGY
#Project : HW 05 Decision Tree 
#Purpose : Use the Decision Tree methodology to develop a classification models 
#First Name : Sarthak
#Last Name : Ahir
#CWID : 10479028
#Date : 10/25/2021
####################################################################
rm(list=ls())


#installing the required libraries
library(class)
library(rpart)

#Load the “breast-cancer-wisconsin.csv” from canvas into R and perform the analysis

newDataSet = read.csv("~/Downloads/breast-cancer-wisconsin.csv",header=TRUE, sep=",")
head(newDataSet, n=5)
#Summarizing each column
summary(newDataSet)

#Here we see that the summary for only column F6 hasn't been calculated on account of non-numeric characters in the column
n <- as.numeric(as.character(newDataSet$F6))
summary(n, na.rm = TRUE)
newDataSet$F6 <- n
summary(newDataSet, na.rm = TRUE)

#checking number of rows before
nrow(newDataSet)

#Removing  rows with missing values 
newDataSet <- na.omit(newDataSet)

#number of rows after removal
nrow(newDataSet)

#Convert labels to factor class
newDataSet$Class<- factor(newDataSet$Class , levels = c("2","4") , labels = c("Benign","Malignant"))

#Check if factor class
is.factor(newDataSet$Class)

#Generate train and test in the ratio 70% to 30%
newDataSet<- newDataSet[2:11]
size <- floor(0.70 * nrow(newDataSet))

#Set the seed 
set.seed(123)
randomValue <- sample(seq_len(nrow(newDataSet)), size = size)

#70% data in train dataset
train <- newDataSet[randomValue, ]

#30% data in test dataset
test <- newDataSet[-randomValue, ]

#Implementing CART 
cart <- rpart(Class ~ ., data = train, method = "class")

#Predicting class for test set
predictedClass <- predict(cart, test, type = "class")
print(length(predictedClass))
print(length(test$Class))

#Confusion Matrix
confusionMatrix <- table(predictedClass,test$Class)
print(confusionMatrix)

#Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(confusionMatrix)

