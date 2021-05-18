library(readxl)
#import and manipulate data - focusing on Harris county (row 101) thru Nov 28
covid <- read_excel('OneDrive/Documents/UH/MATH 6358/covid.xlsx')
harris <- as.data.frame(t(covid[101,]))
harris <- as.data.frame(harris[c(-1), ]) #drop first row
harris$day <- 1:269; harris
colnames(harris) <- c('cases','day')

#basic viz
plot(harris$day, harris$cases) #looks harmonic, maybe even quadratic

#try a simple reg model
mod1 <- lm(cases~day, data=harris); summary(mod1)
plot(harris$day, mod1$residuals) # this bad boy needs a curve!
#normalize time into weeklong increments
harris$week <- harris$day / 7

#split into training (75%) and testing, coerce cases variable
train <- as.data.frame(harris[1:200, ]); test <- as.data.frame(harris[201:269, ])
train$cases <- as.numeric(train$cases); test$cases <- as.numeric(test$cases)

#check for lagged correlation
plot(train$cases[1:199], train$cases[2:200], main="Lag 1 Autocorrelation") #lag 1
cor(train$cases[1:199],train$cases[2:200]) #r=0.833
lag <- NULL
for (i in 1:28) {
  lag[i] <- cor(train$cases[1:(200-i)], train$cases[(1+i):200])
}
plot(lag,type='o',xlab="Days Prior",ylab="Correlation Coefficient",main='Autocorrelation Coefficients for New Covid-19 Cases in Harris County')
#interesting spikes at 7, 14, 21

#how many differences till stationarity?
plot(train$cases[1:186], train$cases[15:200], main="14th Difference") 
#cluster at start, otherwise fairly stationary
#let's say two weeks - it's the incubation period, etc.
back1 <- NULL; back2 <- NULL
for (i in 15:200) {
  back1[i] <- train$cases[i-7]
  back2[i] <- train$cases[i-14]
}
bshift <- lm(cases~back1+back2,data=train);summary(bshift)
plot(train$cases[15:200],bshift$residuals) #positive, linear

#try sines and cosines
s <- sin(2*pi*(7/365)*train$day)
c <- cos(2*pi*(7/365)*train$day)
mod2 <- lm(cases~day+s+c, data=train); summary(mod2)
plot(train$day, mod2$residuals) #try quadratics instead

#drop s and c, try reg model of order 3
day2 <- train$day**2; day3 <- train$day**3
quadmod <- lm(cases~day+day2+day3, data=train); summary(quadmod)
plot(train$day, quadmod$residuals) #fits very well
qqnorm(quadmod$residuals); qqline(quadmod$residuals)

#unfortunately, quadratics aren't cyclical
#combine quadratics with sine & cosine
quadmod2 <- lm(cases~day+day2+day3+s+c,data=train); summary(quadmod2)
plot(train$day, quadmod2$residuals) #curves early, then kind of blows up (good)
qqnorm(quadmod2$residuals);qqline(quadmod2$residuals)

#Can I get a little backshift?
loaded <- lm(cases~day+day2+day3+s+c+back1+back2,data=train);summary(loaded)
plot(loaded$fitted.values,loaded$residuals)
plot(train$day[15:200],loaded$residuals) #harmonic pattern
qqnorm(loaded$residuals);qqline(loaded$residuals)
shapiro.test(loaded$residuals) #non normal at 0.01 level
var(loaded$residuals)

#Can I drop the higher order terms? Please, please, I must interpret.
noquadmod <- lm(cases~day+day2+s+c+back1+back2,data=train);summary(noquadmod)
shapiro.test(noquadmod$residuals)
#I cannot. Loses significance everywhere. Third-order term prob most important to model.
#going w/ loaded model, though I fear the overfit


#PREDICTION TIME, THEN WALK IT BACK####
#bind loaded's ind variables to harris df (already has cases, day, week)
harris$s <- sin(2*pi*(7/365)*harris$day); harris$c <- cos(2*pi*(7/365)*harris$day)
harris$day2 <- harris$day**2; harris$day3 <- harris$day**3
harris$cases <- as.numeric(harris$cases)

#backshift different...but I'm gonna do it anyway
back1 <- NULL; back2 <- NULL
for (i in 15:269) {
  back1[i] <- harris$cases[i] - harris$cases[i-7]
  back2[i] <- harris$cases[i] - harris$cases[i-14]
}

#bind, subset
harris <- cbind(harris,back1,back2)
train <- as.data.frame(harris[1:200, ]); test <- as.data.frame(harris[201:269, ])
bestmod <- lm(cases~.-week, data=train);summary(bestmod)

#fit bestmod to test set
pred <- predict(bestmod, newdata=test)
plot(pred,test$cases) #truly awful...recalibrate

#DRINKING FROM THE WELL, TO WHICH I'VE RETURNED####
s <- sin(2*pi*train$day*(1/14)); c <- cos(2*pi*train$day*(1/14)) #2 week period, silly

#drop quads
noquadmod <- lm(cases~back1+back2+s+c+day,data=train);summary(noquadmod)
plot(train$cases[15:200], noquadmod$residuals) #looking pretty good
qqnorm(noquadmod$residuals);qqline(noquadmod$residuals) #a bit wavy
plot(noquadmod$fitted.values,noquadmod$residuals)

#anova for noquadmod - drop s and c
red.noquad <- lm(cases~back1+back2+day,data=train)
summary(red.noquad)
anova(red.noquad,noquadmod)

#what about a backshifted harmonic term?
sback1 <- sin(2*pi*back1*(1/14)); cback1 <- cos(2*pi*back1*(1/14))
sback2 <- sin(2*pi*back2*(1/14)); cback2 <- cos(2*pi*back2*(1/14))
backharm <- lm(cases~back1+back2+sback1+cback1+day,data=train)
summary(backharm) #dud

#PREDICTION / ANALYSIS TIME####
library(caret)
noquad.pred <- predict(noquadmod,newdata=test)
plot(noquad.pred,test$cases);abline(a=0,b=1)
cor(noquad.pred,test$cases)
mse <- mean(noquad.pred-test$cases)
mse
RMSE(noquad.pred,test$cases)
R2(noquad.pred,test$cases)
MAE(noquad.pred,test$cases)

#PLOTTING####
library(ggplot2)

#entire set of cases
ggplot(harris, aes(day,cases)) + geom_line(size=1,col='red') + geom_smooth(method='lm',linetype='dashed') +
  scale_x_continuous(labels=c('March','April','May','June','July','August','September','October',
  'November','December'),breaks=seq(0,270,30)) + xlab('Month') + ylab('Case Count') +
  ggtitle("New Covid Cases in Harris County since March 4, 2020") + scale_color_manual(values=c('red','blue'))

#autocorrelation plot
plot(lag,type='o',xlab="Days Prior",ylab="Correlation Coefficient",main='Autocorrelation Coefficients for New Covid-19 Cases in Harris County')

#ggplot fitted vals vs resids, qqnorm resids
par(mfrow=c(1,2))
ggplot(noquadmod,aes(fitted.values(noquadmod), resid(noquadmod))) + geom_point(size=1.5,col='red') +
  xlab('Fitted Values') + ylab('Residuals') + ggtitle('Residuals vs. Fitted Values - Training Set') +
  geom_hline(yintercept=0)
ggplot(noquadmod, aes(sample=resid(noquadmod))) + stat_qq(col='red',size=1.5) + stat_qq_line(lwd=1.25) +
  xlab('Theoretical Values') + ylab('Actual Values') + ggtitle('Residual QQ Plot - Training Set')

#predicted test cases vs actual
ggplot(test,aes(noquad.pred,cases)) + geom_point(size=1.5,col='red') + geom_abline(intercept=0,slope=1,lwd=1.25,lty='dotdash') +
  xlab("Predicted Values") + ylab('Actual Values') + ggtitle('Predicted Values vs. Actual Values - Test Set')
