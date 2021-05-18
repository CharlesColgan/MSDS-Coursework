library(ggplot2)
library(ALSM)

crime_rate=read.table("~/Desktop/crime_rate",quote="\"",comment.char="")
sales=read.table("~/Desktop/CH0",quote="\"",comment.char="")
CDI=read.table("~/Desktop/CDI",quote="\"",comment.char="")
plastic=read.table("~/Desktop/plastic",quote="\"",comment.char="")

names(crime_rate)=c("Crime_Rate","HSD")
names(sales)=c("am_sales","year")
names(CDI)=c("ID","County","State","Land_Area","tot_pop","p_18_to_34","p_65_up","Num_Physicians","Num_Hospital_Beds",
             "tot_serious_crimes","p_HS_Grad","p_Bachelors","Poverty", "p_unemploy","PCI","TPI","Geo_Region")
CDI=CDI[,-(1:3)]
CDI=CDI[,-14]
names(plastic)=c("hardness","hours")

attach(crime_rate)
attach(sales)
attach(CDI)
attach(plastic)

#1
summary(lm(Crime_Rate~HSD))
summary(lm(Crime_Rate~1))
anova(lm(Crime_Rate~HSD))
anova(lm(Crime_Rate~1))

#2

for(i in 1:dim(CDI)[2]){
  if(summary(lm(Num_Physicians~CDI[,i]))$r.squared>.1){
    print(summary(lm(Num_Physicians~CDI[,i]))$r.squared)
    print(names(CDI)[i])
  }
}

#3
summary(lm(hardness~hours))
p3=as.data.frame(cbind(predict(lm(hardness~hours)),as.vector(resid(lm(hardness~hours)))))
boxplot(resid(lm(hardness~hours)))
ggplot(p3,aes("",V2))+
  geom_boxplot()+
  labs(y="Residuals",x="")
ggplot(p3,aes(V1,V2))+
  geom_point()+
  geom_abline(aes(slope=0,intercept=0))+
  labs(x="Residuals",y="Fitted Values")
qplot(sample=hardness,data=plastic)+
  labs(x="Normal Quantiles",y="Sample Qunatiles")
shapiro.test(resid(lm(hardness~hours)))
g=rep(1,16)
g[plastic$hours<=24]=0
bftest(lm(hardness~hours),g,alpha=.05)

#4
ggplot(sales,aes(year,am_sales))+
  geom_point()+
  labs(x="Year",y="Sales")
boxcox.sse(year,am_sales)
min(boxcox.sse(year,am_sales)[,2])
boxcox.sse(year,am_sales)[c(24,25,26,27,28),]
sales_t=sales
for( i in 1:10){
 w=sqrt(sales[i,1])
 sales_t[i,1]=w
}
summary(lm(am_sales~year,data=sales))
summary(lm(am_sales~year,data=sales_t))
ggplot(sales_t,aes(year,am_sales))+
  geom_point()+
  geom_abline(aes(intercept=10.26093,slope=1.07629))+
  labs(x="Years",y="Sales")
JAMJAM=as.data.frame(cbind(predict(lm(am_sales~year,data=sales_t)),resid(lm(am_sales~year,data=sales_t))))
ggplot(JAMJAM,aes(V1,V2))+
  geom_point()+
  geom_hline(aes(yintercept=0))+
  labs(x="Fitted Values",y="Residual")
qplot(sample=am_sales,data=sales_t)+
  labs(x="Normal Quantiles",y="Sample Qunatiles")

#5
confint(lm(hardness~hours),level=(1-0.1/2))

#6
new=data.frame(X=c(20,30,40))
ci.pred <- function(fit, newdata, type = c("B", "S"), alpha)
{
  g  <- nrow(newdata)
  CI <- predict(fit, newdata, se.fit = TRUE)
  M  <- ifelse(match.arg(type) == "B", qt(1 - alpha / (2*g), fit$df), sqrt( g * qf( 1 - alpha, g, fit$df)))       
  spred <- sqrt( CI$residual.scale^2 + (CI$se.fit)^2 )   
  x <- data.frame(
    "x"     = newdata,
    "spred" = spred,
    "fit"   = CI$fit,
    "lower" = CI$fit - M * spred,
    "upper" = CI$fit + M * spred)
  return(x)
}
gimp=lm(hardness~hours)
ci.pred(gimp,new,type="B",alpha=0.05)# 1-alpha/2g
news=data.frame(hours=c(20,30,40))
CI <- predict(gimp, newdata=news, se.fit = TRUE)
M=qt(1 - alpha / (2*nrow(news)), gimp$df)
x <- data.frame(
  "x"     = news,
  "spred" = spred,
  "fit"   = CI$fit,
  "lower" = CI$fit - M * spred,
  "upper" = CI$fit + M * spred)
