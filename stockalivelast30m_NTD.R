# Raw data
stock<-read.csv("stockalivelast30m.csv",header = FALSE)
riskfree<-read.csv("rf.csv",header=FALSE)
rf<-riskfree[,2]
y9999<-read.csv("y9999.csv", header = FALSE)

N<-length(stock)

# set up "resultsSLR1", "resultsSLR2" and "resultsDummy" tables to collect results
alpha<-seq(-1,-1,length.out = (N-1)/2)
beta<-seq(-1,-1,length.out = (N-1)/2)
Rsquared<-seq(-1,-1,length.out = (N-1)/2)
alphapv<-seq(-1,-1,length.out = (N-1)/2)
betapv<-seq(-1,-1,length.out = (N-1)/2)
observations<-seq(-1,-1,length.out = (N-1)/2)

resultsSLR1<-data.frame(alpha,alphapv,beta,betapv,Rsquared,observations)
resultsSLR2<-data.frame(alpha,alphapv,beta,betapv,Rsquared,observations)

dummybeta<-seq(-1,-1,length.out = (N-1)/2)
dummybetapv<-seq(-1,-1,length.out = (N-1)/2)
resultsDummy1<-data.frame(alpha,alphapv,beta,betapv,dummybeta,dummybetapv,Rsquared,observations)
resultsDummy2<-data.frame(alpha,alphapv,beta,betapv,dummybeta,dummybetapv,Rsquared,observations)


k<-2
j<-1
while(k<=N)
{
  # set up "tmv" table contains 
  # {1:Date, 2:Currency, 3:NAV, 4:Y9999, 5:rf, 
  #  6:Dis.ri-rf, 7:Cont.ri-rf, 8:Dis.rm-rf, 9:Cont.rm-rf}
  #**********************************************************************************
  tmv<-stock[,c(1,k,k+1)]
  tmv[,4]<-y9999[,2]
  colnames(tmv)[1:4]<-c("Date","Currency","NAV","Y9999")
  
  i<-length(tmv[,"Date"])
  while(i>1)
  {
    tmv[i,5]<-rf[i-1]
    i<-i-1
  }
  colnames(tmv)[5]<-"rf"
  
  i<-length(tmv[,"Date"])
  while(i>1)
  {
      tmv[i,6]<-as.numeric((tmv[i-1,"NAV"]-tmv[i,"NAV"])/tmv[i,"NAV"])
      tmv[i,7]<-as.numeric(log(tmv[i-1,"NAV"]/tmv[i,"NAV"]))
      i<-i-1
  }
  
  i<-2
  while(i<=length(tmv[,"Date"]))
  {
    tmv[i,6]<-tmv[i,6]-tmv[i,"rf"]
    tmv[i,7]<-tmv[i,7]-tmv[i,"rf"]
    i<-i+1
  }
  colnames(tmv)[6:7]<-c("Dis.ri-rf","Cont.ri-rf")
  
  i<-length(tmv[,"Date"])
  while(i>1)
  {
    tmv[i,8]<-(tmv[i-1,"Y9999"]-tmv[i,"Y9999"])/tmv[i,"Y9999"]-tmv[i,"rf"]
    tmv[i,9]<-log(tmv[i-1,"Y9999"]/tmv[i,"Y9999"])-tmv[i,"rf"]
    i<-i-1
  }
  colnames(tmv)[8:9]<-c("Dis.rm-rf","Cont.rm-rf")
  #**********************************************************************************
  
  # Start Calculation
  #**********************************************************************************
 
  # Simple Linear Regression (SLR)1: Y=Dis.ri-rf, X=Dis.rm-rf
  # collect alpha, alpha's p-value, beta, beta's p-value, R-squared and observations
  #**********************************************************************************
  trylm<-lm(tmv[,"Dis.ri-rf"]~tmv[,"Dis.rm-rf"])
  summary(trylm)
  resultsSLR1[j,"alpha"]<-summary(trylm)$coefficients[1,1]
  resultsSLR1[j,"alphapv"]<-summary(trylm)$coefficients[1,4]
  resultsSLR1[j,"beta"]<-summary(trylm)$coefficients[2,1]
  resultsSLR1[j,"betapv"]<-summary(trylm)$coefficients[2,4]
  resultsSLR1[j,"observations"]<-146-length(summary(trylm)$na.action)
  resultsSLR1[j,"Rsquared"]<-summary(trylm)$r.squared
  resultsSLR1[j,]
  #**********************************************************************************
  
  # Simple Linear Regression (SLR)2: Y=Cont.ri-rf, X=Cont.rm-rf
  # collect alpha, alpha's p-value, beta, beta's p-value, R-squared and observations
  #**********************************************************************************
  trylm<-lm(tmv[,"Cont.ri-rf"]~tmv[,"Cont.rm-rf"])
  summary(trylm)
  resultsSLR2[j,"alpha"]<-summary(trylm)$coefficients[1,1]
  resultsSLR2[j,"alphapv"]<-summary(trylm)$coefficients[1,4]
  resultsSLR2[j,"beta"]<-summary(trylm)$coefficients[2,1]
  resultsSLR2[j,"betapv"]<-summary(trylm)$coefficients[2,4]
  resultsSLR2[j,"observations"]<-146-length(summary(trylm)$na.action)
  resultsSLR2[j,"Rsquared"]<-summary(trylm)$r.squared
  resultsSLR2[j,]
  #**********************************************************************************
  
  # Linear Regression with Dummy1: Y=Dis.ri-rf, X1=Dis.rm-rf, X2=dummybeta
  # collect alpha, alpha's p-value, beta, beta's p-value, 
  #         dummybeta, dummybeta's p-value, R-squared and observations
  #**********************************************************************************
  
  # Create X2=dummybeta
  i<-2
  while(i<=length(tmv[,"Date"]))
  {
    if(tmv[i,"Dis.rm-rf"]>0)
    {
      tmv[i,10]<-1
    }
    else
    {
      tmv[i,10]<-0
    }
    i<-i+1
  }
  colnames(tmv)[10]<-"Dummybeta"
  
  trylm<-lm(tmv[,"Dis.ri-rf"]~tmv[,"Dis.rm-rf"]+tmv[,"Dummybeta"])
  summary(trylm)
  resultsDummy1[j,"alpha"]<-summary(trylm)$coefficients[1,1]
  resultsDummy1[j,"alphapv"]<-summary(trylm)$coefficients[1,4]
  resultsDummy1[j,"beta"]<-summary(trylm)$coefficients[2,1]
  resultsDummy1[j,"betapv"]<-summary(trylm)$coefficients[2,4]
  resultsDummy1[j,"dummybeta"]<-summary(trylm)$coefficients[3,1]
  resultsDummy1[j,"dummybetapv"]<-summary(trylm)$coefficients[3,4]
  resultsDummy1[j,"observations"]<-146-length(summary(trylm)$na.action)
  resultsDummy1[j,"Rsquared"]<-summary(trylm)$r.squared
  resultsDummy1[j,]
  #**********************************************************************************
  
  # Linear Regression with Dummy2: Y=Cont.ri-rf, X1=Cont.rm-rf, X2=dummybeta
  # collect alpha, alpha's p-value, beta, beta's p-value, 
  #         dummybeta, dummybeta's p-value, R-squared and observations
  #**********************************************************************************
  trylm<-lm(tmv[,"Cont.ri-rf"]~tmv[,"Cont.rm-rf"]+tmv[,"Dummybeta"])
  summary(trylm)
  resultsDummy2[j,"alpha"]<-summary(trylm)$coefficients[1,1]
  resultsDummy2[j,"alphapv"]<-summary(trylm)$coefficients[1,4]
  resultsDummy2[j,"beta"]<-summary(trylm)$coefficients[2,1]
  resultsDummy2[j,"betapv"]<-summary(trylm)$coefficients[2,4]
  resultsDummy2[j,"dummybeta"]<-summary(trylm)$coefficients[3,1]
  resultsDummy2[j,"dummybetapv"]<-summary(trylm)$coefficients[3,4]
  resultsDummy2[j,"observations"]<-146-length(summary(trylm)$na.action)
  resultsDummy2[j,"Rsquared"]<-summary(trylm)$r.squared
  resultsDummy2[j,]
  #**********************************************************************************
  
  k<-k+2
  j<-j+1
}
write.table(resultsSLR1,file="resultsSLR1.csv",sep=",")
write.table(resultsSLR2,file="resultsSLR2.csv",sep=",")
write.table(resultsDummy1,file="resultsDummy1.csv",sep=",")
write.table(resultsDummy2,file="resultsDummy2.csv",sep=",")

