setwd("C:\\Users\\sreej\\OneDrive\\Documents\\programming\\R")
mydata<-read.csv("student-mat.csv",header=TRUE)

sum(is.na(mydata))

library(tidyverse)
library(corrplot)
library(dplyr)
mydata2=select_if(mydata,is.numeric)
mydata3=select_if(mydata,Negate(is.numeric))

cor.mat=cor(mydata2,method='pearson')
dev.new(width=5,height=5)
corrplot(cor.mat,method='circle',type='upper')

cor(mydata2$G1,mydata$G2)
cor(mydata2$G2,mydata$G3)
cor(mydata2$G3,mydata$G1)

mydata=subset(mydata,select=-c(G2))
mydata=subset(mydata,select=-c(G3))

dim(mydata)

chisq.mat=chisq.test(mydata3)


library(ggcorrplot)
model.matrix(~0+.,mydata3) %>% 
  cor(use="pairwise.complete.obs") %>% 
    ggcorrplot(show.diag = T, type="lower", lab=TRUE, lab_size=2)
