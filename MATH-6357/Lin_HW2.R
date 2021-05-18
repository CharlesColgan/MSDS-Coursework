library(ggplot2)
library(readr)
library(ALSM)
plastic=Untitled_spreadsheet_Sheet1=read_csv("Documents/R/plastic.csv",col_names=FALSE)
oldfaithful=read_csv("Documents/R/oldfaithful.csv")
air=read_table2("Desktop/CH01PR21.DAT",col_names = FALSE)
names(plastic)=c("hardness","time")
names(air)=c("broken","route")
attach(oldfaithful)
attach(plastic)
attach(air)

#1a
ggplot(oldfaithful,aes(Duration,Next))+ 
  geom_point()+
  geom_smooth(method='lm')

#1b
summary(lm(Next~Duration))

#1c
confint(lm(Next~Duration),level=0.95)

#2.7a
confint(lm(hardness~time),level=0.99)

#2.7b
summary(lm(hardness~time))
(summary(lm(hardness~time))$coefficients[2,1]-2)/summary(lm(hardness~time))$coefficients[2,2]
abs(qt(0.01,14))
2*pt(-abs((summary(lm(hardness~time))$coefficients[2,1]-2)/summary(lm(hardness~time))$coefficients[2,2]),14)

#2.16a
predict(lm(hardness~time),data.frame(time=30),se.fit=T,interval="confidence",level=.98)

#2.16b
predict(lm(hardness~time),data.frame(time=30),se.fit=T,interval="predict",level=.98)
 
#2.16c
predict(lm(hardness~time),data.frame(time=30),se.fit=T,interval="predict",level=.98,weights=10)

#2.16e
W=sqrt(2*qf(0.98,2,14))
A=predict(lm(hardness~time),newdata=data.frame(time=30),interval="predict",level=0.98,se.fit=T)
A$fit[,1] + c(-1,1)*W*A$se.fit

PRE=predict(lm(hardness~time),newdata=data.frame(time=30),interval="predict",level=0.98,se.fit=T)$fit
CON=predict(lm(hardness~time),newdata=data.frame(time=30),interval="confidence",level=0.98,se.fit=T)$fit
BAND=cbind(PRE[1],A$fit[,1]-W*A$se.fit,A$fit[,1]+W*A$se.fit)
rbind(CON,PRE,BAND)

#2.26a
anova(lm(broken~route))

#2.26b
summary(lm(broken~route))

#2.26c
dev_hat=c(-2.15008,3.84992,-5.15008,-1.115008,0.57488,2.57488,-2.42512,5.57488,3.29984,0.29984,1.29984,-3.70016,0.0248,-1.9752,3.0248,-3.9752)
dev_nom=c(-24.4124,-24.4124,-24.4124,-24.4124,-8.13738,-8.13738,-8.13738,-8.13738,-8.13738,-8.13738,-8.13738,-8.13738,24.4127,24.4127,24.4127,24.4127)
xi=c(16,16,16,16,24,24,24,24,32,32,32,32,40,40,40,40)
brut=as.data.frame(cbind(xi,dev_hat,dev_nom))
ggplot(brut,aes(xi,dev_hat))+geom_point()+labs(x="X",y="Y-Yhat")
ggplot(brut,aes(xi,dev_nom))+geom_point()+labs(x="X",y="Yhat-Ybar")

#2.26d
summary(lm(broken~route))

