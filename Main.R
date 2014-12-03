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

if(!require("plyr")){
  print("'plyr' package required is being installed...")
  install.packages("plyr")
  print("'plyr' package required has finished installing...")
}
library(plyr)

## Loading plotting library 
if(!require("ggplot2")){
  print("'ggplot2' package required is being installed...")
  install.packages("ggplot2")
  print("'ggplot2' package required has finished installing...")
}
library(ggplot2)

##Setting working dir
setwd("./Titanic-Kaggle-Challenge")

##Loading Data
genderclassmodel=read.csv("./Data/genderclassmodel.csv") # "PassengerId" "Survived" : 418 rows Data Frame
gendermodel=read.csv("./Data/gendermodel.csv") # "PassengerId" "Survived" : 418 rows Data Frame
train=read.csv("./Data/train.csv")
test=read.csv("./Data/test.csv")

## Loading data description
con<-file("Variables Specifications & Notes.txt")
dataDescription<-readLines(con)
close(con)


#**********DATA CLEANSING*************
str(train)
#Adding "survivedf" as a factor and changing its levels to "Yes" and "No"
train<-mutate(train,Survivedf=revalue(as.factor(train$Survived),c("0"="No","1"="Yes")))
#Adding "Pclassf" as a factor and changing its levels to "first" and "second" and "third"
train<-mutate(train,Pclassf=revalue(as.factor(train$Pclass),c("1"="first","2"="second","3"="third")))
#Adding Age Quantile to training set
train<-mutate(train,Agequantile=ntile(Age,10))


##*********Exploratory Analysis *******


##Survived VS not Survived and Passenger Class
png("./Outputs/EDA_Survived-VS-Class&Gender.png")
g<-qplot(Pclassf ,data=train, binwidth=0.7,fill=Survivedf)
g + facet_grid(Sex~.) + xlab("Passenger Class") + ylab("Nb of Passengers") + labs(fill="Survived")
dev.off()

##Survived VS not Survived and Passenger Class as a %, stacked bar graph
  #Pretreating Data - Calculating the percentage of survivors per Sex/Class
survivedCount<-aggregate(train$Survivedf,by=train[c("Pclassf","Survivedf","Sex")],length)
survivedCountPer<-ddply(survivedCount,c("Sex","Pclassf"),transform,percentSurvived=x/sum(x)*100)
  #Creating labels position
survivedCountPer<-arrange(survivedCountPer,Pclassf,Survivedf)
survivedCountPer<-ddply(survivedCountPer,c("Sex","Pclassf"),
                        transform,y_label=cumsum(percentSurvived)-0.5*percentSurvived)
  #Finally, plotting
png("./Outputs/EDA_Survived-VS-Class&Gender_Percentage.png")
ggplot(survivedCountPer, aes(x=Pclassf, y=percentSurvived, fill=Survivedf)) +
  geom_bar(stat="identity") +
  guides(fill=guide_legend(reverse=TRUE)) +
  facet_grid(Sex~.) + 
  geom_text(aes(y=y_label, label=paste(format(percentSurvived,nsmall=0)," %")), size=4, colour="black")
dev.off()

##Survived VS not Survived by Age and Passenger Class
png("./Outputs/EDA_Survived-VS-Class&Gender&Age.png")
g<-qplot(Agequantile,data=train, binwidth=0.5,fill=Survivedf)
g + facet_grid(Pclassf~Sex) + xlab("Age") + ylab("Passengers Class")
dev.off()


