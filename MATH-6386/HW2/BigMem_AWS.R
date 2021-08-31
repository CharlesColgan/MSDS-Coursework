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
ecom.bm=read_csv("eCommerce.csv")
toc()

#T2
tic()
ecom.bm$event_time=str_remove(ecom.bm$event_time," UTC")
ecom.bm$event_time=anydate(ecom.bm$event_time)
ecom.bm$day=weekdays(ecom.bm$event_time)
ecom.bm[,1]=as.factor(ecom.bm[,1])
ecom.bm[,2]=as.factor(ecom.bm[,2])
ecom.bm[,3]=as.factor(ecom.bm[,3])
ecom.bm[,4]=as.factor(ecom.bm[,4])
ecom.bm[,5]=as.factor(ecom.bm[,5])
ecom.bm[,6]=as.factor(ecom.bm[,6])
ecom.bm[,7]=as.factor(ecom.bm[,7])
ecom.bm[,8]=as.factor(ecom.bm[,8])
ecom.bm[,9]=as.factor(ecom.bm[,9])
ecom.bm[,10]=as.factor(ecom.bm[,10])
ecom.bm[,11]=as.factor(ecom.bm[,11])
ecom.bm[,12]=as.factor(ecom.bm[,12])
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
