rm(list=ls())
library(quantmod);library(lubridate)

test=function(a,t1,t2){
  
  s<-get(getSymbols(`a`,from=t1,to=t2))
  x<-nrow(s)
  
  c<-s[,4] ; o<-s[,1] ; h<-s[,2] ; l<-s[,3] ; v<-s[,5]
  
  ma5 <-runMean(c,n=5)
  ma10<-runMean(c,n=10)
  ma  <-ma5-ma10
  M   <-max(h[1:x-1])
  
  q1<-ifelse(sum(ifelse(c[1:x-1]<rep(o[x],x-1)*1.05,1,0))+
             sum(ifelse(c[1:x-1]>rep(o[x],x-1)*0.95,1,0))==(x-1)*2,1,0)
  q2<-as.numeric(ifelse((c[x]+o[x])/2>=M,1,0))
  q3<-as.numeric(ifelse(v[x]>sum(v[1:x-1])/(x-1),1,0))
  q4<-ifelse(sum(ifelse(abs(ma[10:x]/ma5[10:x])<0.03,1,0))==x-9,1,0)
  q <-ifelse(q1+q2+q3+q4==4,1,0);q
}

data<-read.table("C:/Users/MSI/Desktop/chika/R/HW3.csv", header=TRUE, sep=",")
name<-paste(data[,1],"TW", sep = ".")

p<-numeric(length(name))
t<-Sys.Date()

for(i in 1:length(name))
{
  p[i]=test(name[i],t-29,t-1)
  n<-name[which(p==1)]
}
n #近一個月符合上述設定條件的所有台股代碼