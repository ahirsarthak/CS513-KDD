
####################################################################
#UNIVERSITY : STEVENS INSTITUE OF TECHNOLOGY
#Project : HW 04 NB 
#Purpose : Use the Naive Bayes methodology to develop a classification models 
#First Name : Sarthak
#Last Name : Ahir
#CWID : 10479028
#Date : 10/18/2021
####################################################################
rm(list=ls())

#Load the “breast-cancer-wisconsin.csv” from canvas into R and perform the analysis

newDataSet <- read.csv("~/Downloads/breast-cancer-wisconsin.csv", na.string = "?")

#Remove any row with a missing value in any of the columns.
newDataSet<-na.omit(newDataSet)


#Summarizing each column 
summary(newDataSet)

View(newDataSet) 
View(newDataSet) 
str(newDataSet)
is.data.frame(newDataSet) 

library(e1071)
library(class)

#Converting the type of column F6 from character to numeric
newDataSet$F6<-as.integer(newDataSet$F6)

#Converted the Class into type factor
newDataSet$Class<- factor(newDataSet$Class , levels = c("2","4") , labels = c("Benign","Malignant"))
is.factor(newDataSet$Class)

UpData<- newDataSet[2:11]

#70% of the sample 
sample_size <- floor(0.70 * nrow(UpData))

#Set the seed 
set.seed(123)
train_data <- sample(seq_len(nrow(UpData)), size = sample_size)

#Loading 70% Breast cancer record in training dataset
training <- UpData[train_data, ]

#Loading 30% Breast cancer in test dataset
test <- UpData[-train_data, ]

#Implementing NaiveBayes technique
model_naiveData<- naiveBayes(Class ~ ., data = training)

#Predicting target class for the Validation set in confusion matrix
predict_naive <- predict(model_naiveData, test)

confusion_matrix <- table(predict_nb=predict_naive,class=test$Class)
print(confusion_matrix)

#Output of Naive Bayes Classifier in confusion matrix
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(confusion_matrix)
