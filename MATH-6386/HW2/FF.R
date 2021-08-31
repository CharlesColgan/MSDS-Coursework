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
p=getwd()
shell("mkdir ffdf") #system
p=paste(p,"/ffdf",sep="")
options(fftempdir=p)
ecom.ff=read.csv.ffdf(file="eCommerce.csv",
                               VERBOSE=TRUE,
                               next.rows=100000)
toc()

#T2
tic()
ecom.ff$event_time=as.character(as.Date.ff_vector(ecom.ff$event_time))
labels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
ecom.ff$day=cut.ff(ecom.ff$event_time,
                   breaks=7)
toc()

#T3
tic()
MeanDailyVisits=mean(table.ff(ecom.ff$event_time))
toc()

#T4
tic()
MeanPriceByBrand.ff=ffdfdply(ecom.ff,
         ecom.ff$brand,
         function(x){
           summaryBy(price~brand,
                     data=x,
                     FUN=mean,
                     na.rm=TRUE)
         })
toc()

#T5
tic()
ActionByBrand=table.ff(ecom.ff$brand,ecom.ff$event_type)
ConversionRate=ActionByBrand[,2]/ActionByBrand[,3]
toc()

