#Libraryies and Data sets
library(readr)
library(class)
library(ggplot2)
VLADIMIR=read_csv("Documents/R/fonts/VLADIMIR.csv")
EBRIMA=read_csv("Documents/R/fonts/EBRIMA.csv")
BITSTREAMVERA=read_csv("Documents/R/fonts/BITSTREAMVERA.csv")

#Cleaning and sorting
drop_names=c("fontVariant","m_label","orientation","m_top","m_left","originalH","originalW","h","w")
CL1=subset(VLADIMIR[complete.cases(VLADIMIR),-which(names(VLADIMIR) %in% drop_names)],strength==0.4&italic==0)
CL2=subset(EBRIMA[complete.cases(EBRIMA),-which(names(EBRIMA) %in% drop_names)],strength==0.4&italic==0)
CL3=subset(BITSTREAMVERA[complete.cases(BITSTREAMVERA),-which(names(BITSTREAMVERA) %in% drop_names)],strength==0.4&italic==0)
DATA=rbind(CL1,CL2,CL3)

#(0)
#mean and standard deviation
m=apply(DATA[,-(1:3)],2,mean)
s=apply(DATA[,-(1:3)],2,sd)

#standardize
CL1=cbind(CL1[,1:3],t(t(sweep(CL1[,-(1:3)],2,m))/s))
CL2=cbind(CL2[,1:3],t(t(sweep(CL2[,-(1:3)],2,m))/s))
CL3=cbind(CL3[,1:3],t(t(sweep(CL3[,-(1:3)],2,m))/s))
SDATA=rbind(CL1,CL2,CL3)

#correlation matrix
CORR=cor(DATA[,-(1:3)])

#finding highest correlation values
up=upper.tri(CORR)
out=data.frame(which(up, arr.ind=TRUE), cor=CORR[up])
out=out[!is.na(out$cor),]
out=out[order(abs(out$cor), decreasing=TRUE),]
out$row=rownames(CORR)[out$row];out$col=colnames(CORR)[out$col]
top10Cor=out[1:10,]

#(1.0)
#assign selections ~20% of each class
r1=sort(sample(nrow(CL1),nrow(CL1)*0.2))
r2=sort(sample(nrow(CL2),nrow(CL2)*0.2))
r3=sort(sample(nrow(CL3),nrow(CL3)*0.2))
#define test&training sets
testCL1=CL1[r1,];trainCL1=CL1[-r1,]
testCL2=CL2[r2,];trainCL2=CL2[-r2,]
testCL3=CL3[r3,];trainCL3=CL3[-r3,]

TRAINSET=rbind(trainCL1,trainCL2,trainCL3)
TESTSET=rbind(testCL1,testCL2,testCL3)

#(1.1)
K=12

#train/test
train12=knn(TRAINSET[,-(1:3)],TRAINSET[,-(1:3)],TRAINSET$font,K)
test12=knn(TRAINSET[,-(1:3)],TESTSET[,-(1:3)],TRAINSET$font,K)

trainperf12=mean(train12==TRAINSET$font)
testperf12=mean(test12==TESTSET$font)

#(1.2) Determine best K by elbow method
#takes a while to run
K=seq(0,100,1)
trainperfK=NULL
testperfK=NULL
for (i in 1:max(K)){
  trainknn=knn(TRAINSET[,-(1:3)],TRAINSET[,-(1:3)],TRAINSET$font,i)
  trainperfK=c(trainperfK,mean(TRAINSET$font==trainknn))
  testknn=knn(TRAINSET[,-(1:3)],TESTSET[,-(1:3)],TRAINSET$font,i)
  testperfK=c(testperfK,mean(TESTSET$font==testknn))
}

Kval=as.data.frame(cbind(K,testperfK,trainperfK))

#plot 0<=K<=100
ggplot(Kval)+
  geom_line(aes(K,testperfK),color="red")+
  geom_line(aes(K,trainperfK),color="blue")+
  scale_x_continuous(breaks=round(seq(0,100,10),1))+
  labs(x="K",y="% Correct Classification",title="Values for K from 0 to 100")

#(1.3) Closer inspection around K=5
K_knee=c(1,2,3,4,5,6,7,8,9,10)
testperfK_knee=NULL
trainperfK_knee=NULL
for (i in K_knee){
  set.seed(1)
  trainknn_knee=knn(TRAINSET[,-(1:3)],TRAINSET[,-(1:3)],TRAINSET$font,i)
  trainperfK_knee=c(trainperfK_knee,mean(trainknn_knee==TRAINSET$font))
  testknn_knee=knn(TRAINSET[,-(1:3)],TESTSET[,-(1:3)],TRAINSET$font,i)
  testperfK_knee=c(testperfK_knee,mean(testknn_knee==TESTSET$font))
}

Kval_knee=as.data.frame(cbind(K_knee,testperfK_knee,trainperfK_knee))

#plot around K=5
ggplot(Kval_knee)+
  geom_line(aes(K_knee,testperfK_knee),color="blue")+
  geom_line(aes(K_knee,trainperfK_knee),color="red")+
  scale_x_continuous(breaks=round(K_knee,1))+
  labs(x="K",y="% Correct Classification",title="Values around K=1")

#(1.4)
Kbest=1

#train/test
train1=knn(TRAINSET[,-(1:3)],TRAINSET[,-(1:3)],TRAINSET$font,Kbest)
test1=knn(TRAINSET[,-(1:3)],TESTSET[,-(1:3)],TRAINSET$font,Kbest)

trainperf1=mean(train1==TRAINSET$font)
testperf1=mean(test1==TESTSET$font)

#confusion matrices 
trainconf1=table(TRAINSET$font,train1)
testconf1=table(TESTSET$font,test1)

#conf in %'s
trainconf1=trainconf1/apply(trainconf1,1,sum)
testconf1=testconf1/apply(testconf1,1,sum)

#(1.5) N*p(N)>4 for all 3 cases, assuming error has approximate normal distribution
#test set:
ptest=as.numeric(diag(testconf1))
sigmaCL1test=sqrt(p[1]*(1-p[1])/nrow(testCL1))
sigmaCL2test=sqrt(p[2]*(1-p[2])/nrow(testCL2))
sigmaCL3test=sqrt(p[3]*(1-p[3])/nrow(testCL3))
#90% confidence interval
intervalCL1test=c(p[1]-sigmaCL1test*qnorm(1-0.1/2), p[1]+sigmaCL1test*qnorm(1-0.1/2))
intervalCL2test=c(p[2]-sigmaCL1test*qnorm(1-0.1/2), p[2]+sigmaCL2test*qnorm(1-0.1/2))
intervalCL3test=c(p[3]-sigmaCL1test*qnorm(1-0.1/2), p[3]+sigmaCL3test*qnorm(1-0.1/2))

#training set:
ptrain=as.numeric(diag(trainconf1))
sigmaCL1train=sqrt(p[1]*(1-p[1])/nrow(trainCL1))
sigmaCL2train=sqrt(p[2]*(1-p[2])/nrow(trainCL2))
sigmaCL3train=sqrt(p[3]*(1-p[3])/nrow(trainCL3))
#90% confidence interval
intervalCL1train=c(p[1]-sigmaCL1train*qnorm(1-0.1/2), p[1]+sigmaCL1train*qnorm(1-0.1/2))
intervalCL2train=c(p[2]-sigmaCL1train*qnorm(1-0.1/2), p[2]+sigmaCL2train*qnorm(1-0.1/2))
intervalCL3train=c(p[3]-sigmaCL1train*qnorm(1-0.1/2), p[3]+sigmaCL3train*qnorm(1-0.1/2))

#90% confidence interval for differences (train-test):
std1 = sqrt(sigmaCL1test^2+sigmaCL1train^2)
std2 = sqrt(sigmaCL2test^2+sigmaCL2train^2)
std3 = sqrt(sigmaCL3test^2+sigmaCL3train^2)
intervalCL1diff = c((ptrain[1]-ptest[1])-std1*qnorm(1-0.1/2), (ptrain[1]-ptest[1])+std1*qnorm(1-0.1/2))
intervalCL2diff = c((ptrain[2]-ptest[2])-std2*qnorm(1-0.1/2), (ptrain[2]-ptest[2])+std2*qnorm(1-0.1/2))
intervalCL3diff = c((ptrain[3]-ptest[3])-std3*qnorm(1-0.1/2), (ptrain[3]-ptest[3])+std3*qnorm(1-0.1/2))

#(1.6,1.7) Binning by groups of pixels
PACK1=NULL;PACK2=NULL;PACK3=NULL;PACK4=NULL;

for(L in 0:9)  for(M in 0:9)  PACK1 = c(PACK1, sprintf('r%ic%i',L,M))
for(L in 0:9)  for(M in 10:19)  PACK2=c(PACK2,sprintf('r%ic%i',L,M))
for(L in 10:19)  for(M in 10:19)  PACK3=c(PACK3,sprintf('r%ic%i',L,M))
for(L in 10:19)  for(M in 0:9)  PACK4=c(PACK4,sprintf('r%ic%i',L,M))

P1KN=knn(TRAINSET[,PACK1],TESTSET[,PACK1],TRAINSET$font,Kbest)
P2KN=knn(TRAINSET[,PACK2],TESTSET[,PACK2],TRAINSET$font,Kbest)
P3KN=knn(TRAINSET[,PACK3],TESTSET[,PACK3],TRAINSET$font,Kbest)
P4KN=knn(TRAINSET[,PACK4],TESTSET[,PACK4],TRAINSET$font,Kbest)

w1=mean(P1KN==TESTSET$font)
w2=mean(P2KN==TESTSET$font)
w3=mean(P3KN==TESTSET$font)
w4=mean(P4KN==TESTSET$font)

#(1.8)Weighing and normalization
COM=w1+w2+w3+w4
w1=w1/(COM)
w2=w2/(COM)
w3=w3/(COM)
w4=w4/(COM)

W_TRAINSET=cbind(w1*TRAINSET[,PACK1],w2*TRAINSET[,PACK2],w3*TRAINSET[,PACK3],w4*TRAINSET[,PACK4])
W_TESTSET=cbind(w1*TESTSET[,PACK1],w2*TESTSET[,PACK2],w3*TESTSET[,PACK3],w4*TESTSET[,PACK4])

#KNN on normalized sets
W_train1=knn(W_TRAINSET,W_TRAINSET,TRAINSET$font,Kbest)
W_test1=knn(W_TRAINSET,W_TESTSET,TRAINSET$font,Kbest)

W_trainperf1=mean(W_train1==TRAINSET$font)
W_testperf1=mean(W_test1==TESTSET$font)

#confusion matrix
W_trainconf1=table(TRAINSET$font,W_train1)
W_testconf1=table(TESTSET$font,W_test1)

#conf in %'s
W_trainconf1/apply(W_trainconf1,1,sum) 
W_testconf1/apply(W_testconf1,1,sum)
