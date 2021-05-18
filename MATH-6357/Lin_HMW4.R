library(matlib)
library(ggplot2)
plastic=read.table("~/Desktop/plastic.txt",quote="\"",comment.char="")
brand=read.table("~/Desktop/brand.txt",quote="\"",comment.char="")
names(plastic)=c("hardness","time")
names(brand)=c("pref","sweetness","moisture")
attach(plastic)
attach(brand)

#1a
X=plastic
X[,1]=1
X=as.matrix(X)
XT=t(as.matrix(X))
Y=as.matrix(plastic[,1])
YT=t(as.matrix(plastic[,1]))

inv(XT%*%X)
YT%*%Y
XT%*%Y
H=X%*%inv(XT%*%X)%*%XT

#1b
inv(XT%*%X)%*%XT%*%Y
summary(lm(hardness~time))

#1c
I=diag(16)
e=(I-H)%*%Y
summary(lm(hardness~time))$residuals

#6.5a
pairs(brand)
cor(brand)

#6.5b
Y=as.matrix(brand[,1])
X=brand
X[,1]=1
X=as.matrix(X)
XT=t(X)
b=inv(XT%*%X)%*%XT%*%Y
cbind(Y,X);b

#6.5c
Yhat=X%*%b
resid=Y-Yhat
brand$resid=resid
ggplot(brand,(aes("",resid)))+geom_boxplot()
qplot(y=brand$resid, x= 1, geom = "boxplot")

#6.5d
summary(lm(pref~sweetness+moisture))
brand$resid=resid(lm(pref~sweetness+moisture))
brand$Yhat=fitted(lm(pref~sweetness+moisture))
brand$both=I(sweetness*moisture)
ggplot(brand,aes(Yhat,resid))+geom_point()
ggplot(brand,aes(sweetness,resid))+geom_point()
ggplot(brand,aes(moisture,resid))+geom_point()
ggplot(brand,aes(both,resid))+geom_point()
ggplot(brand,aes(sample=resid))+stat_qq()

#6.6a,b
summary(lm(pref~sweetness+moisture))

#6.6c
confint(lm(pref~sweetness+moisture))

df=brand[,1:3]
names(df) <- c("y","x1", "x2")
df.lm <- lm(y ~ x1 + x2, df)
anova(df.lm)

SST = sum(anova(df.lm)$`Sum Sq`)
SSE = deviance(df.lm)
SSR = SST-SSE
MSR = SSR/2
MSE = SSE/18
Fstat=MSR/MSE

1-pf(Fstat,2,18) 
qf(1-.05,2,18) 

d.mat <- model.matrix(df.lm)
t(d.mat)%*%d.mat
t(d.mat)%*%df$y
b.mat<- solve(t(d.mat)%*%d.mat)%*%t(d.mat)%*%df$y
df.lm

(t(d.mat)%*%d.mat)%*%b.mat
t(d.mat)%*%df$y

d.mat %*% b.mat

df$y - d.mat %*% b.mat

par(mfrow = c(2, 2), pch = 20)
plot(resid(df.lm) ~ fitted(df.lm), xlab = "Fitted", ylab = "Residual")
plot(resid(df.lm) ~ x1, df, ylab = "Residual")
plot(resid(df.lm) ~ x2, df, ylab = "Residual")
plot(resid(df.lm) ~ I(x1 * x2), df, xlab = "X1X2", ylab = "Residual")

par(mfrow = c(1, 1), pch = 20)
plot(abs(resid(df.lm)) ~ fitted(df.lm), xlab = "Fitted", ylab = "Absresid")
qqnorm(resid(df.lm), main = "", xlab = "Expected", ylab = "Residual")
qqline(resid(df.lm))

s.mat <- MSE*solve(t(d.mat)%*%d.mat)

confint(df.lm)
b.mat[2] +c(-1,1) * qt(1-.05/2,16) * sqrt(s.mat[2,2])
b.mat[3] +c(-1,1) * qt(1-.05/2,16) * sqrt(s.mat[3,3])

#6.7a,b
anova(lm(pref~sweetness+moisture))
summary(lm(pref~sweetness+moisture))

#6.8a
Xh <- c(1,5,4)
Y_hat <- t(Xh)%*%b.mat
Y_hat
s.y <- t(Xh)%*%s.mat%*%Xh
s.y
Y_hat + c(-1,1) * qt(1-.005,16) * sqrt(s.y)
predict(df.lm,data.frame(x1=5,x2=4),interval="confidence", level=0.99)

#6.8b
predict(df.lm,data.frame(x1=5,x2=4),interval="prediction",levl=0.99)


