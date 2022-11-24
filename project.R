#GROUP-9,TOPIC-8
#install packages if not installed before
install.packages('rpart')
install.packages('caret')
install.packages('rpart.plot')
install.packages('rattle')

#loading the libraries
library(ggplot2)
library(tidyverse)
library(corrplot)
library(dplyr)
library(rpart,quietly = TRUE)
library(caret,quietly = TRUE)
library(rpart.plot,quietly = TRUE)
library(rattle)
library(caret)

#getting the source data(set the directory to where the data files are in your device)
setwd("C:\\Users\\sreej\\OneDrive\\Documents\\programming\\R")
mydatamat<-read.csv("student-mat.csv",header=TRUE)
mydatapor<-read.csv("student-por.csv",header=TRUE)
mydata=rbind(mydatamat,mydatapor)

#checking for null values
sum(is.na(mydata))

#removing the avoidable columns in dataset to decrease the complexity of the decision tree
mydata=subset(mydata,select=-c(address,famsize,Medu,Fedu,Mjob,
                               Fjob,traveltime,famsup,schoolsup,activities,
                               nursery,reason,paid,guardian))

#separating the numerical and categorical data for correlations
mydata.num=select_if(mydata,is.numeric)
mydata.cat=select_if(mydata,Negate(is.numeric))

#correlation for numerical attributes
cor.mat=cor(mydata.num,method='pearson')
dev.new(width=5,height=5)   #every plot from here will be shown in the dialog box opened just now
corrplot(cor.mat,method='circle',type='upper')

#Checking for certain combinations of attributes from above graph
cor(mydata$Dalc,mydata$Walc)
cor(mydata.num$G1,mydata$G2)
cor(mydata.num$G2,mydata$G3)
cor(mydata.num$G3,mydata$G1)

#removing the columns if the correlation is greater than 0.9
mydata=subset(mydata,select=-c(G2))
mydata=subset(mydata,select=-c(G3))

#reassigning the numerical and categorical columns
mydata.num=select_if(mydata,is.numeric)
mydata.cat=select_if(mydata,Negate(is.numeric))

#Box plots for numerical data
num.col=colnames(mydata.num)
par(mfrow=c(2,3))
for (x in 1:6){
  boxplot(mydata.num[,num.col[x]],label=TRUE,xlab=num.col[x])
}

par(mfrow=c(2,3))
for (x in 7:11){
  boxplot(mydata.num[,num.col[x]],label=TRUE,xlab=num.col[x])
}
par(mfrow=c(1,1))

#checking the dimensions of data,before ending the pre-processing
dim(mydata)

#Frequency tables with the class atrribute-Walc for the first level of ID3
col.names=colnames(mydata)
col.names=col.names[col.names %in% "Walc" == FALSE]  
for( x in 1:16){
  print(paste('frequency table with:',col.names[x]))
  data=table(mydata$Walc,mydata[,x])
  print(data)
}

#training the model with standard sampling with 80 percent training data
#the algorithm used is ID3
train <- sample(1:nrow(mydata),size = ceiling(0.80*nrow(mydata)),replace = FALSE)
mydata.train <- mydata[train,]
mydata.test <- mydata[-train,]

model=rpart(Walc~., data=mydata.train, method="class")
rpart.plot(model, nn=TRUE)
pred <- predict(model,subset(mydata.test,select=-c(Walc)),type="class")
confusionMatrix(factor(pred,levels=1:5),factor(mydata.test$Walc,levels=1:5))

#ID3 with bootstrap resampling 
rmse_boot=list()
for (k in 5:30){
  train_control=trainControl(method='boot',number=k,p=0.80)
  #here k is number of times data is resampled
  model=train(Walc~.,data=mydata,method='rpart',trControl=train_control,metric='RMSE',maxdepth=5)
  rmse_boot[[length(rmse_boot)+1]]=model$results$RMSE[1]
  print(model)
}
print(rmse_boot)
plot(5:30, rmse_boot,type="o", col="red")
#the plot is for different number of iteration(number of times it is resampled)

#ID3 with k-fold cross-validation
rmse_cv=list()
for (k in 5:30){
  train_control=trainControl(method='cv',number=k)
  #here k is number of folds for cross validation
  model=train(Walc~.,data=mydata,method='rpart',trControl=train_control,metric='RMSE',maxdepth=5)
  rmse_cv[[length(rmse_cv)+1]]=model$results$RMSE[1]
  print(model)
}
print(rmse_cv)
plot(5:30,rmse_cv,type="o",col='blue')
#the optimal k is for which the rmse is least
k=which(rmse_cv==min(unlist(rmse_cv)))+5-1

#confusion matrix,accuracy,f1 score,recall,precision
model=rpart(Walc~., data=mydata.train,xval=k, method="class")
rpart.plot(model, nn=TRUE)
pred <- predict(model,subset(mydata.test,select=-c(Walc)),type="class")
cm=confusionMatrix(factor(pred,levels=1:5),factor(mydata.test$Walc,levels=1:5))
print(cm)

#confusion matrix
print(paste('the confusion matrix:'))
print(cm$table)

#accuracy
print(paste('the overall accuracy:',cm$overall['Accuracy']*100))

#balanced accuracy
print(paste('Balanced Accuracy by class:'))
print(cm$byClass[,'Balanced Accuracy'])

#recall
print(paste('Recall by class:'))
print(cm$byClass[,'Recall'])

#precision
print(paste('Precision by class:'))
print(cm$byClass[,'Precision'])

#F1 score
print(paste('F1 score by class:'))
print(cm$byClass[,'F1'])

