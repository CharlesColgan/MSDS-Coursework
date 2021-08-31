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
ecom.bm=read.csv("eCommerce.csv")
toc()

#T2
tic()
ecom.bm$event_time=str_remove(ecom.bm$event_time," UTC")
ecom.bm$event_time=anydate(ecom.bm$event_time)
ecom.bm$day=weekdays(ecom.bm$event_time)
classes=unlist(lapply(colnames(ecom.bm),function(x){
  class(ecom.bm[,x])
}))
ind=which(classes!="numeric")
for(i in ind){ecom.bm[,i]=as.factor(ecom.bm[,i])}
ind=which(classes!="factor")
for(i in ind){ecom.bm[,i]=as.factor(ecom.bm[,i])}
ecom.bm.mat=as.big.matrix(ecom.bm)
toc()

#T3
tic()
MeanDailyVisits=mean(bigtable(ecom.bm.mat,c("event_time")))
MostVisistedDay=match(max(bigtable(ecom.bm.mat,c("day"))),bigtable(ecom.bm.mat,c("day")))
toc()

#T4
tic()
MeanPriceByBrand=sapply(bigsplit(ecom.bm.mat,
                ccols="brand",
                splitcol="price"),
       mean,
       na.rm=TRUE)
toc()

#T5
tic()
ActionByBrand=bigtable(ecom.bm.mat,c("brand","event_type"))
ConversionRate=ActionByBrand[,2]/ActionByBrand[,3]
toc()
