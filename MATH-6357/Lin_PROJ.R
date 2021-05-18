#libraries
library(class)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(glmulti)
library(psych)

#Data sets
Solar=read_csv("Documents/R/SolarPrediction.csv")

#Formatting/Cleaning
Solar$Duration=difftime(Solar$TimeSunSet,Solar$TimeSunRise,units="secs")
Solar$Duration=as.numeric(Solar$Duration)
Solar$Time=as.numeric(as.period(Solar$Time,units="secs"))
Solar$Data=month(as.Date(Solar$Data,format="%m/%d/%Y %H:%M:%S"))
Solar=Solar[,-c(1,10,11)]
names(Solar)[c(1,7)]=c("Month","Wind_Direction")
Solar=Solar[complete.cases(Solar),]
Solar=Solar[,-c(1,2,7)]
set.seed(1);Solar=sample_n(Solar,1000)
attach(Solar)

#investigative
pairs.panel(Solar) #adding more color and making plots more presentable
round(cor(Solar),3)
summary(Solar)

#Regression Modeling & Correlation Analysis
full_solar=lm(Radiation~.,data=Solar)
Search=glmulti(full_solar)#Runs many trials will take awhile
Best=summary(Search)$bestmodel
Best_Fit=lm(Best,data=Solar)
sim_Best=lm(Radiation~Temperature+Pressure+Humidity+Temperature*Pressure+Pressure*Humidity+Humidity*Speed,data=Solar)
summary(full_solar);anova(full_solar)
summary(Best_Fit);anova(Best_Fit)
summary(sim_Best);anova(sim_Best)


0.5881-0.627 
  