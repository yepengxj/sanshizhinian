library(plyr)
library(EMD)
library(Rlibeemd)



sgn<-function(x)
{
  if(x==0)
  {
    0
  }
  else
  {
    if(x>0)
    {
      1
    }
    else
    {
      -1
    }
  }
}

x<-as.numeric(xxx["2000-01-01/","close"])

plot(x)

close_emd_smooth<-emd_smooth(x)
colnames(close_emd_smooth)<-"close_emd_smooth"
lines(close_emd_smooth,col="red")

close_emd_smooth_ts<-cbind(xxx["2000-01-01/","close"],close_emd_smooth)[,2]



get_acf<-function(x)
{
  acf(x,lag.max=length(x),plot=F)$acf
}

get_acf1<-function(x)
{
  x_exp<-aaply(0:(length(x)-1), 1, function(k,ts,N){
    sum(ts[1:(N-k)]*ts[(k+1):N])/(N-k)
  },x,length(x))
  
  x_exp<-x_exp/x_exp[1]
}
x<-as.numeric(HS300_idx_ts["2006-01-01/2009-01-22","close"])
emd_smooth<-function(x)
{
  x_emd<-EMD::emd(x,stoprule="type4")
  
  xcorr<-aaply(x_emd$imf,2,get_acf)
  
  xcorr_mean<-aaply(1:(x_emd$nimf-1),1,get_k,xcorr=xcorr,To=x_emd$nimf) 
  
  k<-which.max(aaply(xcorr_mean,1,function(x){sqrt(sum(x^2))}))
  
  imf<-x_emd$imf
  t_x<-aaply(1:(k-1),1,function(x,imf){
   # print(x)
    if(x>0)
    {
      median(abs(imf[,x])) * sqrt(2*(log10(length(imf[,x])))) /0.6745
    }
    else
    {
      0
    }
  },x_emd$imf)

  imf_smooth<-aaply(1:(k-1),1,function(x,imf,t_x){
    if(x>0)
    {
      aaply(imf[,x],1,smooth,t_x[x])
    }
    else
    {
      rep(0,length(imf[,1]))
    }
  },
  x_emd$imf,t_x
  )
 
  if(k<=2)
  {
    if(is.null(dim(imf_smooth)) )
    {
      matrix(aaply( cbind(imf_smooth,x_emd$imf[,(2:x_emd$nimf)],x_emd$residue) , 1, sum),ncol=1)
    }
    else
    {
      aaply( cbind(t(imf_smooth),x_emd$imf[,(2:x_emd$nimf)],x_emd$residue) , 1, sum)
      
    }
    
    
  }
  else
  {
    aaply( cbind(t(imf_smooth[(1:(k-1)),]),x_emd$imf[,(k:x_emd$nimf)],x_emd$residue), 1, sum)
  }
  
}

smooth_data<-eemd_smooth(xxx$close)
smooth_data1<-emd_smooth(as.numeric(xxx$close))

plot_range<-(2000:2200)
plot(as.numeric(xxx$close[plot_range]),type="l")

lines(smooth_data[plot_range],type="l",col="red")
lines(smooth_data1[plot_range],type="l",col="green")

xxx$close[1:500]-smooth_data[1:500]

eemd_smooth<-function(x)
{
  x_emd <- Rlibeemd::eemd(x, num_siftings = 50, ensemble_size = 100)
  
  nimf<-((dim(x_emd)[2])-1)
  
  imf<-x_emd[,(1:(nimf))]
  
  residue<-x_emd[,nimf+1]

  xcorr<-aaply(imf,2,get_acf)
  
  xcorr_mean<-aaply(1:(nimf-1),1,get_k,xcorr=xcorr,To=nimf) 
  
  k<-which.max(aaply(xcorr_mean,1,function(x){sqrt(sum(x^2))}))
  
  t_x<-aaply(1:(k-1),1,function(x,imf){
    if(x>0)
    {
      median(abs(imf[,x])) * sqrt(2*(log10(length(imf[,x])))) /0.6745
    }
    else
    {
      0
    }
  },imf)
  
  imf_smooth<-adply(1:(k-1),1,function(x,imf,t_x){
    
    if(x>0)
    {

      ifelse(abs(imf[,x])>t_x[x],sgn(imf[,x])*(abs(imf[,x])-t_x[x]),0)
    }
    else
    {
      rep(0,length(imf[,1]))
    }
  },
  imf,t_x
  )
  
  imf_smooth<-t(imf_smooth[,-1])
  
  if(k<=2)
  {
    aaply( cbind(imf_smooth,imf[,(2:nimf)],residue) , 1, sum)
    
  }
  else
  {
    aaply( cbind(imf_smooth[,(1:(k-1))],imf[,(k:nimf)],residue), 1, sum)
  }
  
  
}

HS300_idx_ts$close_emd_smooth<-emd_smooth(as.numeric(HS300_idx_ts[,"close"]))
plot(HS300_idx_ts["2006-05-01/2008-01-01","close"])
lines(HS300_idx_ts["2006-05-01/2008-01-01","close_emd_smooth"],col="red")

get_k<-function(k,xcorr,To)
{
 
  ( k * (To-k) / (To^2)) * ( ( aaply(xcorr[,(1:k)],1,sum) /k)  - ( aaply(xcorr[,(k+1):To],1,sum) /(To-k)) )
  
}


smooth<-function(i,t_x1)
{
  if(abs(i) > t_x1)
  {
    sgn(i)*(abs(i)-t_x1)
  }
  else
  {
    0
  }
}




rep(2,2)*rep(4,2)


xcorr[kmax]
k<-10

length(xxx["2015-01-01/","close"])
