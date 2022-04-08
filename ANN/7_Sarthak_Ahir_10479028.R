
####################################################################
#UNIVERSITY : STEVENS INSTITUE OF TECHNOLOGY
#Project : HW_07_ANN
#Purpose : Use ANN methodology to develop a classification model
#First Name : Sarthak
#Last Name : Ahir
#CWID : 10479028
#Date : 11/15/2021
####################################################################
rm(list=ls())
dev.off

#installing the required libraries
library(neuralnet)

#Load the “wisc_bc_ContinuousVar” from canvas into R and perform the analysis

newDataSet<-read.csv("~/Downloads/wisc_bc_ContinuousVar.csv")

set.seed(111)
#Summarizing each column 
summary(newDataSet)
table(newDataSet$Class)

View(newDataSet) 

#check if NAN
table(is.na(newDataSet))

newDataSet=newDataSet[,-1]


# Splitting the newDataSet Data to test and training 30 - 70
data<-sort(sample(nrow(newDataSet),as.integer(.70*nrow(newDataSet))))
training<-newDataSet[data,]
test<-newDataSet[-data,]

#performing ANN for 5 nodes
dataModel <- neuralnet(diagnosis~.,data=training,hidden=5, threshold=0.01)

#Plotting the neural network of the performed ANN
plot(dataModel)

# test with input column
ANN <-compute(dataModel,test)
ANN_diag<-c('B','M')[apply(ANN$net.result,1,which.max)]
dataModel$result.matrix

#displaying results of ANN
k=apply(ANN$net.result,1,which.max)
ANN$net.result
k

#accuracy of ANN
accuracy= (test$diagnosis==ANN_diag)

acccu<-sum(accuracy)/length(accuracy)
print(paste("the accuracy is",acccu*100))


#error rate
error<- (test$diagnosis!=ANN_diag)
errorRate<-sum(error)/length(error)
print(paste("the error rate is",errorRate*100))
