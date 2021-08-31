library(ff)
library(ffbase)
library(fastmatch)
library(snow)
library(bigmemory)
library(readr)
library(tictoc)
library(doBy)
library(stringr)
library(biglm)
library(DBI)
library(foreach)
library(biganalytics)
library(bigtabulate)
library(dplyr)
library(anytime)
library(tictoc)

#T1
tic()
ecom=read_csv("eCommerce.csv")
toc()

#T2
tic()
ecom$event_time=str_remove(ecom$event_time," UTC")
ecom$event_time=anydate(ecom$event_time)
ecom$day=weekdays(ecom$event_time)
toc()

#T3
tic()
DailyVisits=ecom%>%count(event_time)
MeanDailyVisits=mean(as.numeric(DailyVisits$n))
TotalVisit=ecom%>%count(day)
MostVisistedDAy=TotalVisit[TotalVisit[,2]==max(TotalVisit[,2]),1]
toc()

#T4
tic()
MeanPriceByBrand=aggregate(ecom$price,
                           list(ecom$brand),
                           mean)
toc()

#T5
tic()
Purchases=ecom[ecom$event_type=="purchase",]%>%count(brand)
Other=ecom[ecom$event_type!="purchase",]%>%count(brand)
NoPurchase=data.frame(brand=setdiff(Other$brand,Purchases$brand),n=0)
Purchases=rbind(Purchases,NoPurchase)
Conversions=merge(Purchases,Other,"brand")
Conversions$COnversionRate=Conversions$n.x/Conversions$n.y
toc()

