library(geometry)
library(ggplot2)
library(readr)
airpollution=read_csv("Downloads/airpollution(1).csv")
attach(airpollution)

#1
table(c(6,7,8,9,10,8,9,10,11,10,11,12,12,13,14,9,10,11,12,11,12,13,13,14,15,12,13,14,14,15,16,15,16,17,18))
W=c(6,7,8,9,10,11,12,13,14,15,16,17,18)
P=round(c(1,1,2,3,4,4,5,4,4,3,2,1,1)/35,digits=3)
pmf=as.table(cbind(W,P))
dot(W,P)
dot((W-dot(W,P))^2,P)

#2
n=20;p=0.7
pbinom(10,n,p,FALSE)
pbinom(10,n,p)-pbinom(6,n,p)
sqrt(n*p*(1-p))
c((n*p)-sqrt(n*p*(1-p)),(n*p)+sqrt(n*p*(1-p)))
pbinom(10,n,p)-pbinom(2,n,p)

#3
#a
n=25;p=0.5
pbinom(18,n,p)-pbinom(6,n,p)

#b
p=0.8
pbinom(18,n,p)-pbinom(6,n,p)

#c
p=0.5
pbinom(6,n,p)
pbinom(18,n,p,FALSE)

#d
p=0.6
pbinom(18,n,p)-pbinom(6,n,p)

p=0.8
pbinom(18,n,p)-pbinom(6,n,p)

#e
p=0.5


#4
n=400;p=0.005
dbinom(1,n,p)
pbinom(3,n,p)

#5
t=140:150
r=10000
profit=function(r,t,p=.85){
  result=c()
  av_result=c()
  for(i in 1:length(t)){
    for(j in 1:r){
    X=rbinom(1,t[i],p)
    if(X>120){B=X-120}
    else{B=0}
    prd=250*t[i]-500*B
    result[j]=prd
    }
    av_result[i]=mean(result)
  }
  Tab=rbind(t,av_result)
  return(Tab)
}
for(i in 1:5){
  print(profit(r,t))
}

#6
funt=function(y){
  if(y>=0 && y<=12){res=-(y^3-18*y^2)/864}
  else{res=0}
  return(res)
}
f=function(y){
  if(y>=0 && y<=12){res=y*(1-y/12)/24}
  else{res=0}
  return(res)
}
fe=function(y){
  if(y>=0 && y<=12){res=y*(y*(1-y/12)/24)}
  else{res=0}
  return(res)
}
fe2=function(y){
  if(y>=0 && y<=12){res=y*(y*(1-y/12)/24)^2}
  else{res=0}
  return(res)
}
CDF=as.data.frame(cbind(seq(0,12,0.01),funt(seq(0,12,0.01))))
names(CDF)=c("X","Y")
ggplot(CDF,aes(X,Y))+geom_line()
integrate(fe,0,4)
integrate(f,0,6)
1-0.5
0.5-0.2592593
integrate(fe,0,12)
integrate(fe2,0,12)
sqrt(0.6-0.5^2)
integrate(f,0,4)
integrate(f,0,8)
(1-0.7407407)+0.2592593

#8
xbr=137.2;sd=1.6;
p=pnorm(135,xbr,sd,FALSE)
n=10
1-pbinom(7,n,p)
qnorm(.05)
qnorm(.05,xbr,sd)
x=135
(x-xbr)/qnorm(0.05)

#9
mu=3.5;sig=1.2
exp(mu+sig^2/2);exp(2*mu+sig^2/2)*(exp(sig^2)-1)
plnorm(250,mu,sig)-plnorm(50,mu,sig)
plnorm(mu,mu,sig)
#10
ggplot(airpollution,aes(sample=Solar))+geom_qq()
shapiro.test(airpollution$Solar)
