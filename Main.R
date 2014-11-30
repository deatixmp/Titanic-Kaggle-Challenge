##*********INITIALISATIONS**********
##Cleaning WSpace
rm(list=ls())

#Loading Libraries
if(!require("dplyr")){
  print("'dplyr' package required is being installed...")
  install.packages("dplyr")
  print("'dplyr' package required has finished installing...")
}
library(dplyr)

if(!require("ggplot2")){
  print("'ggplot2' package required is being installed...")
  install.packages("ggplot2")
  print("'ggplot2' package required has finished installing...")
}
library(ggplot2)
##Setting working dir
setwd("C:/Users/David Faria/Documents/Aprendizaje Personal/Kaggle Challenges/Titanic Prediction")

##Loading Data
genderclassmodel=read.csv("./Data/genderclassmodel.csv") # "PassengerId" "Survived" : 418 rows Data Frame
gendermodel=read.csv("./Data/gendermodel.csv") # "PassengerId" "Survived" : 418 rows Data Frame
train=read.csv("./Data/train.csv")
test=read.csv("./Data/test.csv")

## Loading data description
con<-file("Variables Specifications & Notes.txt")
dataDescription<-readLines(con)
close(con)
## Loading plotting library 



##***********************************
##*********Exploratory Graphs *******

Separated_Passengers=split(train,train$Survived)
#Adding Age Quantile to training set
train<-mutate(train,Agequantile=ntile(Age,10))

##Survived VS not Survived and Passenger Class
g<-qplot(Pclass ,data=train, binwidth=0.5,fill=as.factor(Survived))
g + facet_grid(Sex~.) + xlab("Passenger Class") + ylab("Nb of Passengers")

##Survived VS not Survived by Age and Passenger Class
g<-qplot(Agequantile,data=train, binwidth=0.5,fill=Sex)
g + facet_grid(Pclass~Survived) + xlab("Age") + ylab("Passengers Class")



