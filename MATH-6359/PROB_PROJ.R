#libraries
library(readxl)
library(tidyverse)
library(ggplot2)

#Data & Cleaning
COV=read_excel("Documents/R/Texas COVID-19 Case Count Data by County.xlsx",col_names=FALSE)
COV=t(COV)
COV=COV[,-c(1:2,259,261:268)]
names=COV[1,]
COV=as.data.frame(COV)
names(COV)=names
COV=COV[-1,-1]
COV=COV[complete.cases(COV),]
indx=sapply(COV[,-1], is.factor)
COV[indx]=lapply(COV[indx], function(x) as.numeric(as.character(x)))

#Model Creation

#Complete
days=as.Date(c(seq(from=as.Date("2020-03-04"),to=as.Date("2020-03-06"),by=1),
       seq(from=as.Date("2020-03-09"),to=as.Date("2020-03-13"),by=1),
       seq(from=as.Date("2020-03-15"),to=as.Date("2020-12-02"),by=1)))
Harris=data.frame(Days=days,Harris=COV$Harris,Total=COV$Total)
Harris$Numb_Days=seq(1,271,1)
Tots=summary(lm(Harris~Numb_Days,data=Harris))
PRED=Tots$coefficients[1,1]+Tots$coefficients[2,1]*Harris$Numb_Days
Harris$Predict=PRED
Harris$Residuals=Tots$residuals

#Sectioned
  #1-100
  Set1=Harris[1:100,]
  Tot1=summary(lm(Harris~Numb_Days,data=Set1))
  pred1=Tot1$coefficients[1,1]+Tot1$coefficients[2,1]*Set1$Numb_Days
  Set1$Predict=pred1
  Set1$Residuals=Tot1$residuals
  
  #100-200
  Set2=Harris[101:200,]
  Tot2=summary(lm(Harris~Numb_Days,data=Set2))
  pred2=Tot2$coefficients[1,1]+Tot2$coefficients[2,1]*Set2$Numb_Days
  Set2$Predict=pred2
  Set2$Residuals=Tot2$residuals
  
  #200-
  Set3=Harris[-c(1:200),]
  Tot3=summary(lm(Harris~Numb_Days,data=Set3))
  pred3=Tot3$coefficients[1,1]+Tot3$coefficients[2,1]*Set3$Numb_Days
  Set3$Predict=pred3
  Set3$Residuals=Tot3$residuals
  Set3=Harris[-c(1:200),]

Parts=rbind(Set1,Set2,Set3)
    
#MLE
u=mean(Harris$Harris)
sd=sqrt(var(Harris$Harris))
Harris$Norm=rnorm(dim(Harris)[1],u,sd)
Parts$Norm=rnorm(dim(Harris)[1],u,sd)
Mtots=summary(lm(Norm~Numb_Days,data=Harris))$residuals
Harris$Norm_Res=Mtots
Parts$Norm_Res=Mtots

#visual Complete
ggplot(Harris)+
  geom_point(aes(Days,Residuals))+
  geom_hline(aes(yintercept=0))
ggplot(Harris,aes(sample=Residuals))+
  stat_qq()+
  stat_qq_line(aes(color="red"))+
  theme(legend.position="none")
ggplot(Harris)+
  geom_point(aes(Days,Harris))+
  geom_line(aes(Days,Predict,color="red"))+
  theme(legend.position="none")

#visual Sectioned
ggplot(Set1)+
  geom_point(aes(Days,Residuals))+
  geom_hline(aes(yintercept=0))
ggplot(Set2)+
  geom_point(aes(Days,Residuals))+
  geom_hline(aes(yintercept=0))
ggplot(Set3)+
  geom_point(aes(Days,Residuals))+
  geom_hline(aes(yintercept=0))

ggplot(Set1,aes(sample=Residuals))+
  stat_qq()+
  stat_qq_line(aes(color="red"))+
  theme(legend.position="none")
ggplot(Set2,aes(sample=Residuals))+
  stat_qq()+
  stat_qq_line(aes(color="red"))+
  theme(legend.position="none")
ggplot(Set3,aes(sample=Residuals))+
  stat_qq()+
  stat_qq_line(aes(color="red"))+
  theme(legend.position="none")

ggplot(Parts)+
  geom_point(aes(Days,Harris))+
  geom_line(aes(Days,Predict,color="red"))+
  theme(legend.position="none")

#Summar
RSME=c(sqrt(mean(Tots$residuals^2)),
       sqrt(mean(Tot1$residuals^2)),
       sqrt(mean(Tot2$residuals^2)),
       sqrt(mean(Tot3$residuals^2)))
MAE=c(mean(abs(Tots$residual)),
      mean(abs(Tot1$residual)),
      mean(abs(Tot2$residual)),
      mean(abs(Tot3$residual)))
Tots;Tot1;Tot2;Tot3;RSME;MAE
