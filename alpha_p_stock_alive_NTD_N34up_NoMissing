y9999<-read.csv("y9999.csv", header = FALSE)
stock<-read.csv("stockaliveNTD.csv",header = FALSE)
riskfree<-read.csv("rf.csv",header=FALSE)
xprice<-y9999[,2]
xprice
rf<-riskfree[,2]
rf
xreturn<-seq(0,0,length.out = length(xprice)-1)
i<-1
while(i<=length(xreturn))
{
  xreturn[i]<-(xprice[i]-xprice[i+1])/xprice[i+1]
  i<-i+1
}
i<-1
#xreturn
xreturn<-xreturn-rf
#xreturn

alpha<-seq(0,0,length.out = (length(stock)-1)/2)
pvalue<-seq(0,0,length.out = (length(stock)-1)/2)
observations<-seq(0,0,length.out = (length(stock)-1)/2)
#alpha
#pvalue

ypricerecord<-matrix(0,nrow = 146,ncol=182)
yreturnrecord<-matrix(0,nrow = 145,ncol=182)

j<-3
k<-1
while(j<=length(stock))
{
  N<-length(stock[!is.na(stock[,j]),j])
  observations[k]<-N-1
  yprice<-seq(0,length.out=N)
  i<-1
  while(i<=N)
  {
    yprice[i]<-stock[i,j]
    ypricerecord[i,k]<-yprice[i]
    i<-i+1
  }
  yprice
  yreturn<-seq(0,length.out=N-1)
  i<-1
  while(i<=length(yreturn))
  {
    yreturn[i]<-(yprice[i]-yprice[i+1])/yprice[i+1]-rf[i]
    yreturnrecord[i,k]<-yreturn[i]
    i<-i+1
  }
  #yreturn
  #yreturn<-yreturn-rf[1:length(yreturn)]
  #yreturn
  trylm<-lm(yreturn~xreturn[1:length(yreturn)])
  summary(trylm)
#  plot(xreturn[1:length(yreturn)],yreturn)
  alpha[k]<-summary(trylm)$coefficients[1,1]
  pvalue[k]<-summary(trylm)$coefficients[1,4]
  
  k<-k+1
  j<-j+2
}
i<-1
j<-3
#ypricerecord[,1:5]
#yreturnrecord[,1:5]
#alpha
#pvalue
report<-data.frame(alpha,pvalue,observations)
report
min(observations)
#xreturn
write.table(report,file="alpha_p_stock_alive_NTD_N34up.csv",sep=",")

