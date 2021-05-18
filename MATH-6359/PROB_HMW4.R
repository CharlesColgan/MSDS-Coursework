library(gtools)
library(ggplot2)
library(pracma)
library(MASS)

#1
mat=rbind(c(0.05,0.05,0.10),c(0.05,0.10,0.35),c(0,0.20,0.10))
lins=rbind(mat[1,],mat[2,],mat[3,],mat[,1],mat[,2],mat[,3])
marg=c()
for(i in 1:6){
  marg[i]=sum(lins[i,])
}
sum(mat[1:2,1:2])
A=as.data.frame(combinations(n=6,r=2,v=marg,repeats.allowed=FALSE))
tits=c("x=20","x=25","x=30","y=20","y=25","y=30")
cbind(tits,marg)
x=c(20,25,20)
r=c(0,5,10)
C=c()
for(i in 1:3){
  C[i]=x[i]*(sum(mat[,i])+sum(mat[i,]))
}
sum(C);sum(r)
c(0,5,10)%*%c(0.25,0.65,.1)

#2
F=function(x,y,k){
  if(x>=0&&y>=0&&(x+y)>=20&&(x+y)<=30){
    F=3/81250*x*y
  }
  else{
    F=0
  }
  return(F)
}

h1=function(x){
  h1=3/81250*x*(20-x)
  return(h1)
}
h2=function(x){
  h2=3/81250*x*(30-x)
  return(h2)
}

Z=cbind(seq(20,30,1),h1(seq(20,30,1)),h2(seq(20,30,1)))
Z=as.data.frame(Z)
names(Z)=c("X","Y1","Y2")
ggplot(Z)+geom_line(aes(X,Y1))+geom_line(aes(X,Y2))+labs(x="",y="")

integral2(F,xmin=0,xmax=20,ymin=20-x,ymax=30-x)

#3
lambda=20
x=c(1,2,3,4)
p=c(0.4,0.3,0.2,0.1)
mu=sum(x*p)
var=sum((x-mu)^2*p)

#4
loss=function(i,a,x,y){
  c=0
  set.seed(i)
  while(a>0){
    d=rnorm(1,x,y)
    a=a-d
    c=c+1
  }
  return(c)
}
win=0
for(i in 1:10000){
  if(loss(i,2*6*16,13,2)>14){
    win=win+1
  }
  else{
    win=win
  }
}
1-win/10000

#5
biv=function(x,mu_x=70,mu_y=170,sig_x=2,sig_y=20,p=0.9){
  sig_xy=p*sig_x*sig_y
  E=mu_y+sig_xy/sig_x^2*(x-mu_x)
  V=sig_y^2-sig_xy^2/sig_x^2
  list=c(E,V)
  return(list)
}
biv(68)
biv(70)
biv(72)
pnorm(180,182,76)
