#Library
library(tidyverse)
library(dyplr)
library(ggplot2)
library(readr)
library(tidyr)
library(GGally)
library(plyr)
library(wesanderson)
library(ggpubr)
hap15 <- read_csv("R/2015.csv")
hap16 <- read_csv("R/2016.csv")
hap17 <- read_csv("R/2017.csv")
hap18 <- read_csv("R/2018.csv")
hap19 <-read_csv("R/2019(2).csv")

#EDA####
ggpairs(hap19[,-c(1,2,10)]) #rm country name, ranks, sds, life expectancy
cor(hap19[,-c(1,2,10)])
summary(hap19[,-c(1,2,10)])

#check for normality: score & GDP look Normal, others don't
hap19.shap <- lapply(hap19[,-c(1,2,10)],shapiro.test)
hap19.shaplist <- sapply(hap19.shap, `[`, c("statistic","p.value"))
hap19.shaplist 
0.05/7 #95% multiple comparisons correction
#score the only normally dist variable: great news! bc it's the response

#get 2019 to have region data, but first condense regions
for (i in 1:nrow(hap15)) {
  if (hap15$Region[i] == "Western Europe") {hap15$Region[i] <- "Europe"}
  if (hap15$Region[i] == "Central and Eastern Europe") {hap15$Region[i] <- "Europe"}
  if (hap15$Region[i] == "Southeastern Asia") {hap15$Region[i] <- "Asia"}
  if (hap15$Region[i] == "Eastern Asia") {hap15$Region[i] <- "Asia"}
  if (hap15$Region[i] == "Southern Asia") {hap15$Region[i] <- "Asia"}
}
regions <- as.data.frame(unique(cbind(hap15$Country,hap15$Region)))
colnames(regions) <- c('Country','Region')
colnames(hap19)[colnames(hap19) == 'Country or region'] <- 'Country'
hap19$Region <- with(regions, Region[match(hap19$Country,Country)]) %>% as.factor()
#fix regions that are NA
hap19$Region[c(39,63)] <- 'Latin America and Caribbean'
hap19$Region[64] <- 'Europe'
hap19$Region[c(112,113,120,156)] <- 'Sub-Saharan Africa'

#now can do boxplot
wes <- c(wes_palette('Darjeeling1'),wes_palette('Darjeeling2'))
ggplot(hap19,aes(x=reorder(Region,Score,FUN=median),y=Score,fill=Region)) + geom_boxplot() +
  theme_classic()+scale_fill_manual(values=wes)+
  scale_x_discrete(name='Region', labels=c('Africa','Asia','Middle East','Europe',
                                           'Latin America','N. America','Aus & NZ'))+
  ggtitle('Happiness Score by Region')


#simple regression####
reg19 <- lm(Score~.,data=hap19[,-c(1,2,10,11)])
summary(reg19) #generosity & corrupt not significant at 0.05, will remove

redreg19 <- lm(Score~.,data=hap19[,-c(1,2,8,9,10,11)])
summary(redreg19)
shapiro.test(redreg19$residuals) #ftr at 0.05
qqnorm(redreg19$residuals);qqline(redreg19$residuals,lwd=2)
plot(redreg19$fitted.values,redreg19$residuals,xlab="Fitted Values",ylab='Residuals',
     main='Residuals vs. Fitted Values');abline(h=0,lwd=2)

#do we have multicollinearity
car::vif(redreg19) #no multicollinearity, 3.9 highest VIF

#model comparison####
#life expectancy is column 10 in hap19 data frame

#now regression
simp <- lm(Score~`Most Recent Life Expectancy`,data=hap19)
summary(simp)
shapiro.test(simp$residuals) #p value 0.04, slightly abnormal
qqnorm(simp$residuals);qqline(simp$residuals,lwd=2)
plot(simp$fitted.values,simp$residuals,xlab='Fitted Values',ylab='Residuals',
     main='Residuals vs Fitted Values - One Predictor Regression');abline(h=0,lwd=2)

#anova and f tests for simple model and reduced regression model
anova(simp,redreg19)

#compare mse
mean(simp$residuals**2) #for simple model
mean(redreg19$residuals**2) #for WHR ideal model

#Library
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(GGally)
library(plyr)
library(wesanderson)
library(gridExtra)
library(glmulti)

#Data
X2015=read_csv("R/2015.csv")
X2016=read_csv("R/2016.csv")
X2017=read_csv("R/2017.csv")
X2018=read_csv("R/2018.csv")
X2019=read_csv("R/2019.csv")

#Cleaning
X2015$"Year"=2015
X2016$"Year"=2016
X2017$"Year"=2017
X2018$"Year"=2018
X2019$"Year"=2019
X2015$"Lower Confidence Interval"=X2015$"Happiness Score"-1.96*X2015$"Standard Error"/sqrt(dim(X2015)[1])
X2015$"Upper Confidence Interval"=X2015$"Happiness Score"+1.96*X2015$"Standard Error"/sqrt(dim(X2015)[1])
X2016$"Standard Error"=sqrt(dim(X2016)[1])*(X2016$"Happiness Score"-X2016$"Lower Confidence Interval")/1.96
names(X2017)[names(X2017)=="Whisker.high"]="Upper Confidence Interval"
names(X2017)[names(X2017)=="Whisker.low"]="Lower Confidence Interval"
names(X2017)=c("Country","Happiness Rank","Happiness Score", "Upper Confidence Interval",
               "Lower Confidence Interval","Economy (GDP per Capita)","Family",
               "Health (Life Expectancy)","Freedom","Generosity",
               "Trust (Government Corruption)","Dystopia Residual","Year")
X2017$"Standard Error"=sqrt(dim(X2017)[1])*(X2017$"Happiness Score"-X2017$"Lower Confidence Interval")/1.96
X2017$"Region"=NA
X1819=rbind(X2018,X2019)
names(X1819)=c("Happiness Rank","Country","Happiness Score",
               "Economy (GDP per Capita)","Family","Health (Life Expectancy)",
               "Freedom","Generosity","Trust (Government Corruption)","Year")
X1819$"Region"=NA
X1819$"Lower Confidence Interval"=NA
X1819$"Upper Confidence Interval"=NA
X1819$"Standard Error"=NA
X1819$"Dystopia Residual"=NA
Happy=rbind(X2015,X2016,X2017,X1819)
Happy$"Region"=region[match(Happy$Country,country)]
Happy$"Score"=Happy$"Happiness Score"
Happy$"Happiness Score"=NULL
Happy$"Rank"=Happy$"Happiness Rank"
Happy$"Happiness Rank"=NULL

#Guessed
AA=c(mean(X2015$"Standard Error")/mean(X2015$"Happiness Score"),
     mean(X2016$"Standard Error")/mean(X2017$"Happiness Score"),
     mean(X2017$"Standard Error")/mean(X2017$"Happiness Score"))
middle_mean=mean(AA)
semean18=mean(X2018$Score)*middle_mean
semean19=mean(X2019$Score)*middle_mean
BB=c(sd(X2015$"Standard Error"),sd(X2016$"Standard Error"),sd(X2017$"Standard Error"))
middle_sd=mean(BB)
SE_2018=rnorm(dim(X2018)[1],semean18,middle_sd)
SE_2019=rnorm(dim(X2019)[1],semean19,middle_sd)
Low18=X2018$Score-1.96*SE_2018/sqrt(dim(X2018)[1])
High18=X2018$Score+1.96*SE_2018/sqrt(dim(X2018)[1])
Low19=X2019$Score-1.96*SE_2019/sqrt(dim(X2019)[1])
High19=X2019$Score+1.96*SE_2019/sqrt(dim(X2019)[1])
Happy[Happy$Year==2018,]$"Standard Error"=SE_2018
Happy[Happy$Year==2018,]$"Lower Confidence Interval"=Low18
Happy[Happy$Year==2018,]$"Upper Confidence Interval"=High18
Happy[Happy$Year==2019,]$"Standard Error"=SE_2019
Happy[Happy$Year==2019,]$"Lower Confidence Interval"=Low19
Happy[Happy$Year==2019,]$"Upper Confidence Interval"=High19

#Time Series
Time=Happy[,-c(3,10,12,13)]
Time=Time[,c("Year","Country","Region","Rank","Score",
             "Economy (GDP per Capita)","Family","Health (Life Expectancy)",
             "Freedom","Trust (Government Corruption)","Generosity")]
names(Time)=c("Year","Country","Region","Rank","Score",
              "GDP","Family","Health",
              "Freedom","Trust","Generosity")
Time$Trust=as.double(Time$Trust)
#Time=Time[complete.cases(Time),]
YEAR=rep(unique(Time$Year),10)
REGION=c(rep(unique(Time$Region)[1],5),
         rep(unique(Time$Region)[2],5),
         rep(unique(Time$Region)[3],5),
         rep(unique(Time$Region)[4],5),
         rep(unique(Time$Region)[5],5),
         rep(unique(Time$Region)[6],5),
         rep(unique(Time$Region)[7],5),
         rep(unique(Time$Region)[8],5),
         rep(unique(Time$Region)[9],5),
         rep(unique(Time$Region)[10],5))
MATT=as.data.frame(cbind(YEAR,REGION))
MATT[,3:10]=0
for (k in 4:length(names(Time))){
  jam=c()
  jom=matrix(nrow=5,ncol=10)
  for (j in 1:length(unique(Time$Region))){
    for (i in 1:length(unique(Time$Year))){
      james=Time[Time$Region==unique(Time$Region)[j],]
      james=james[james$Year==unique(james$Year)[i],]
      jam[i]=mean(as.matrix(james[,k]))
    }
    jom[,j]=jam
  }
  MATT[,k-1]=round(c(jom[,1],jom[,2],jom[,3],jom[,4],jom[,5],
                     jom[,6],jom[,7],jom[,8],jom[,9],jom[,10]),2)
}
names(MATT)=c("YEAR","REGION","RANK","SCORE",
              "GDP","FAMILY","HEALTH",
              "FREEDOM","TRUST","GENEROSITY")

#Graphs
for (i in 3:length(names(MATT))){
  print(ggplot(MATT,aes(YEAR,MATT[,i]))+
          geom_point()+
          facet_wrap(vars(REGION),scales="free_y",ncol=5)+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(y=names(MATT)[i]))
}
HappyNum=Happy[,-c(1,2,8,10,15)]
YEAR=c(2015,2016,2017,2018,2019)
MEANS=rbind(colMeans(HappyNum[HappyNum$Year==2015,]),
            colMeans(HappyNum[HappyNum$Year==2016,]),
            colMeans(HappyNum[HappyNum$Year==2017,]),
            colMeans(HappyNum[HappyNum$Year==2018,]),
            colMeans(HappyNum[HappyNum$Year==2019,]))
MEANS=data.frame(MEANS[,-7])
names(MEANS)=c("AVG_ERR","GDP","FAM","HET","DOM","GEN","S_LOW","S_HIGH","SCORE")
SD=rbind(apply(HappyNum[HappyNum$Year==2015,],2,sd),
         apply(HappyNum[HappyNum$Year==2016,],2,sd),
         apply(HappyNum[HappyNum$Year==2017,],2,sd),
         apply(HappyNum[HappyNum$Year==2018,],2,sd),
         apply(HappyNum[HappyNum$Year==2019,],2,sd))
SD=data.frame(SD[,-c(1,7,8,9,10)])
names(SD)=c("GDP_ERR", "FAM_ERR", "HET_ERR", "DOM_ERR", "GEN_ERR")
AGG=data.frame(YEAR,MEANS,SD)
CONF=data.frame(G_LOW=AGG$GDP-1.96*AGG$GDP_ERR/sqrt(length(AGG$GDP)),
                G_HIGH=AGG$GDP+1.96*AGG$GDP_ERR/sqrt(length(AGG$GDP)),
                F_LOW=AGG$FAM-1.96*AGG$FAM_ERR/sqrt(length(AGG$FAM)),
                F_HIGH=AGG$FAM+1.96*AGG$FAM_ERR/sqrt(length(AGG$FAM)),
                H_LOW=AGG$HET-1.96*AGG$HET_ERR/sqrt(length(AGG$HET)),
                H_HIGH=AGG$HET+1.96*AGG$HET_ERR/sqrt(length(AGG$HET)),
                D_LOW=AGG$DOM-1.96*AGG$FAM_ERR/sqrt(length(AGG$DOM)),
                D_HIGH=AGG$DOM+1.96*AGG$FAM_ERR/sqrt(length(AGG$DOM)),
                N_LOW=AGG$GEN-1.96*AGG$GEN_ERR/sqrt(length(AGG$GEN)),
                N_HIGH=AGG$GEN+1.96*AGG$GEN_ERR/sqrt(length(AGG$GEN)))
AGG=data.frame(AGG,CONF)
BASE=ggplot(AGG,aes(x=YEAR))
A=BASE+
  geom_point(aes(y=SCORE),color="blue")+geom_line(aes(y=SCORE),color="blue")+
  geom_point(aes(y=S_LOW),color="red")+geom_line(aes(y=S_LOW),color="red")+
  geom_point(aes(y=S_HIGH),color="red")+geom_line(aes(y=S_HIGH),color="red")
B=BASE+
  geom_point(aes(y=GDP),color="blue")+geom_line(aes(y=GDP),color="blue")+
  geom_point(aes(y=G_LOW),color="red")+geom_line(aes(y=G_LOW),color="red")+
  geom_point(aes(y=G_HIGH),color="red")+geom_line(aes(y=G_HIGH),color="red")+
  labs(y="GDP")
C=BASE+
  geom_point(aes(y=FAM),color="blue")+geom_line(aes(y=FAM),color="blue")+
  geom_point(aes(y=F_LOW),color="red")+geom_line(aes(y=F_LOW),color="red")+
  geom_point(aes(y=F_HIGH),color="red")+geom_line(aes(y=F_HIGH),color="red")+
  labs(y="FAMILY")
D=BASE+
  geom_point(aes(y=HET),color="blue")+geom_line(aes(y=HET),color="blue")+
  geom_point(aes(y=H_LOW),color="red")+geom_line(aes(y=H_LOW),color="red")+
  geom_point(aes(y=H_HIGH),color="red")+geom_line(aes(y=H_HIGH),color="red")+
  labs(y="HEALTH")
E=BASE+
  geom_point(aes(y=DOM),color="blue")+geom_line(aes(y=DOM),color="blue")+
  geom_point(aes(y=D_LOW),color="red")+geom_line(aes(y=D_LOW),color="red")+
  geom_point(aes(y=D_HIGH),color="red")+geom_line(aes(y=D_HIGH),color="red")+
  labs(y="FREEDOM")
F=BASE+
  geom_point(aes(y=GEN),color="blue")+geom_line(aes(y=GEN),color="blue")+
  geom_point(aes(y=N_LOW),color="red")+geom_line(aes(y=N_LOW),color="red")+
  geom_point(aes(y=N_HIGH),color="red")+geom_line(aes(y=N_HIGH),color="red")+
  labs(y="Generosity")
ggarrange(A,B,C,D,E,F,ncol=3,nrow=2)
BASE+
  geom_point(aes(y=AVG_ERR))+geom_line(aes(y=AVG_ERR))+
  geom_point(aes(y=GDP_ERR),color="red")+geom_line(aes(y=GDP_ERR),color="red")+
  geom_point(aes(y=FAM_ERR),color="blue")+geom_line(aes(y=FAM_ERR),color="blue")+
  geom_point(aes(y=HET_ERR),color="green")+geom_line(aes(y=HET_ERR),color="green")+
  geom_point(aes(y=DOM_ERR),color="brown")+geom_line(aes(y=DOM_ERR),color="brown")+
  geom_point(aes(y=GEN_ERR),color="purple")+geom_line(aes(y=GEN_ERR),color="purple")+
  labs(y="ERROR")