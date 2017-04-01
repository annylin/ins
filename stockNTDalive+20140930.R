# Raw data
stock<-read.csv("stockNTDalive+20140930.csv",header = FALSE)
stockbond<-read.csv("stockbondNTDalive+20140930.csv",header = FALSE)
fixedincome<-read.csv("FixedincomeNTDalive+20140930.csv",header = FALSE)
currency<-read.csv("CurrencyNTDalive+20140930.csv",header = FALSE)
combi<-read.csv("combiNTDalive+20140930.csv",header = FALSE)
MBS<-read.csv("MBSNTDalive+20140930.csv",header = FALSE)
ETF<-read.csv("ETFNTDalive+20140930.csv",header = FALSE)
indices<-read.csv("IndicesNTDalive+20140930.csv",header = FALSE)

riskfree<-read.csv("rf.csv",header=FALSE)
rf<-riskfree[,2]
y9999<-read.csv("y9999.csv", header = FALSE)

D<-seq(0,0,length.out = 8)
D[1]<-length(stock)
D[2]<-length(stockbond)
D[3]<-length(fixedincome)
D[4]<-length(currency)
D[5]<-length(combi)
D[6]<-length(MBS)
D[7]<-length(ETF)
D[8]<-length(indices)

D
Name<-c("Stock","Stock & Bond","Fixed Income","Currency","Combination","MBS","ETF","Index Fund")
total<-data.frame(type=1,fundnum=1,alpha=1,alphapv=1,beta=1,betapv=1,Rsquared=1,observations=1)
totalD<-data.frame(type=1,fundnum=1,alpha=1,alphapv=1,beta=1,betapv=1,dummybeta=1,dummybetapv=1,Rsquared=1,observations=1)

m<-1

while(m<=8)
{
  N<-D[m]
  # set up "resultsSLR1", "resultsSLR2" and "resultsDummy" tables to collect results
  alpha<-seq(-1,-1,length.out = (N-1)/2)
  beta<-seq(-1,-1,length.out = (N-1)/2)
  Rsquared<-seq(-1,-1,length.out = (N-1)/2)
  alphapv<-seq(-1,-1,length.out = (N-1)/2)
  betapv<-seq(-1,-1,length.out = (N-1)/2)
  observations<-seq(-1,-1,length.out = (N-1)/2)
  type<-seq(-1,-1,length.out = (N-1)/2)
  fundnum<-seq(-1,-1,length.out = (N-1)/2)
  resultsSLR<-data.frame(type,fundnum,alpha,alphapv,beta,betapv,Rsquared,observations)

  
  dummybeta<-seq(-1,-1,length.out = (N-1)/2)
  dummybetapv<-seq(-1,-1,length.out = (N-1)/2)
  resultsDummy<-data.frame(type,fundnum,alpha,alphapv,beta,betapv,dummybeta,dummybetapv,Rsquared,observations)

  k<-2
  j<-1
  while(k<=N)
  {
    # set up "tmv" table contains 
    # {1:Date, 2:Currency, 3:NAV, 4:Y9999, 5:rf, 
    #  6:Dis.ri-rf, 7:Cont.ri-rf, 8:Dis.rm-rf, 9:Cont.rm-rf}
    #**********************************************************************************
    if(m==1){tmv<-stock[,c(1,k,k+1)]}
    if(m==2){tmv<-stockbond[,c(1,k,k+1)]}
    if(m==3){tmv<-fixedincome[,c(1,k,k+1)]}
    if(m==4){tmv<-currency[,c(1,k,k+1)]}
    if(m==5){tmv<-combi[,c(1,k,k+1)]}
    if(m==6){tmv<-MBS[,c(1,k,k+1)]}
    if(m==7){tmv<-ETF[,c(1,k,k+1)]}
    if(m==8){tmv<-indices[,c(1,k,k+1)]}
    
    tmv[,4]<-y9999[,2]
    colnames(tmv)[1:4]<-c("Date","Currency","NAV","Y9999")
    
    i<-length(tmv[,"Date"])
    while(i>1)
    {
      tmv[i,5]<-rf[i]
      i<-i-1
    }
    colnames(tmv)[5]<-"rf"
    
    i<-length(tmv[,"Date"])
    while(i>1)
    {
      tmv[i,6]<-as.numeric((tmv[i-1,"NAV"]-tmv[i,"NAV"])/tmv[i,"NAV"])-tmv[i,"rf"]
      i<-i-1
    }
    colnames(tmv)[6]<-"Dis.ri-rf"
    
    i<-length(tmv[,"Date"])
    while(i>1)
    {
      tmv[i,7]<-(tmv[i-1,"Y9999"]-tmv[i,"Y9999"])/tmv[i,"Y9999"]-tmv[i,"rf"]
      i<-i-1
    }
    colnames(tmv)[7]<-"Dis.rm-rf"
    #**********************************************************************************
    
    # Start Calculation
    #**********************************************************************************
    
    # Simple Linear Regression (SLR)1: Y=Dis.ri-rf, X=Dis.rm-rf
    # collect alpha, alpha's p-value, beta, beta's p-value, R-squared and observations
    #**********************************************************************************
    trylm<-lm(tmv[,"Dis.ri-rf"]~tmv[,"Dis.rm-rf"])
    summary(trylm)
    resultsSLR[j,"type"]<-Name[m]
    resultsSLR[j,"fundnum"]<-j
    resultsSLR[j,"alpha"]<-summary(trylm)$coefficients[1,1]
    resultsSLR[j,"alphapv"]<-summary(trylm)$coefficients[1,4]
    resultsSLR[j,"beta"]<-summary(trylm)$coefficients[2,1]
    resultsSLR[j,"betapv"]<-summary(trylm)$coefficients[2,4]
    resultsSLR[j,"observations"]<-146-length(summary(trylm)$na.action)
    resultsSLR[j,"Rsquared"]<-summary(trylm)$r.squared
    resultsSLR[j,]
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
        tmv[i,8]<-1*tmv[i,"Dis.rm-rf"]
      }
      else
      {
        tmv[i,8]<-0
      }
      i<-i+1
    }
    colnames(tmv)[8]<-"Dummybeta"
    
    trylm<-lm(tmv[,"Dis.ri-rf"]~tmv[,"Dis.rm-rf"]+tmv[,"Dummybeta"])
    summary(trylm)
    resultsDummy[j,"type"]<-Name[m]
    resultsDummy[j,"fundnum"]<-j
    resultsDummy[j,"alpha"]<-summary(trylm)$coefficients[1,1]
    resultsDummy[j,"alphapv"]<-summary(trylm)$coefficients[1,4]
    resultsDummy[j,"beta"]<-summary(trylm)$coefficients[2,1]
    resultsDummy[j,"betapv"]<-summary(trylm)$coefficients[2,4]
    resultsDummy[j,"dummybeta"]<-summary(trylm)$coefficients[3,1]
    resultsDummy[j,"dummybetapv"]<-summary(trylm)$coefficients[3,4]
    resultsDummy[j,"observations"]<-146-length(summary(trylm)$na.action)
    resultsDummy[j,"Rsquared"]<-summary(trylm)$r.squared
    resultsDummy[j,]
    #**********************************************************************************

    k<-k+2
    j<-j+1
  }
  total<-rbind(total,resultsSLR)
  totalD<-rbind(totalD,resultsDummy)
  
  m<-m+1
}

write.table(total,file="SLRtotal.csv",sep=",", row.names = FALSE)
write.table(totalD,file="Dummytotal.csv",sep=",", row.names = FALSE)
