library(plyr)
library(EMD)
library(Rlibeemd)
library(XML)



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

Rlibeemd_eemd_func<-function(x,param_list){
  x_emd_t <- Rlibeemd::eemd(x, num_siftings = 50, ensemble_size = 100)
  
  nimf<-((dim(x_emd_t)[2])-1)
  
  imf<-as.matrix(x_emd_t[,(1:(nimf))])
  
  residue<-as.numeric(x_emd_t[,nimf+1])
  
  list("imf"=imf,"nimf"=nimf,"residue"=residue)
}

EMD_emd_func<-function(x,param_list){
  EMD::emd(x, stoprule = "type4")

}


emd_smooth<-function(x,emd_func,get_acf2,get_k2,noise_t2,smooth_t2,param_list)
{
  x_emd<-emd_func(x)
  
  xcorr<-aaply(x_emd$imf,2,get_acf2)
  
  xcorr_mean<-get_k2(xcorr,x_emd)
  
  k<-which.max(aaply(xcorr_mean,1,function(x){sqrt(sum(x^2))}))
  
  imf<-x_emd$imf
  t_x<-aaply(1:(k-1),1, noise_t2  ,x_emd$imf)

  imf_smooth<-aaply(1:(k-1),1,function(x,imf,t_x){
    
    if(x>0)
    {
      
      x<-1
      aaply(as.numeric(imf[,x]),1,smooth_t2,t_x[x])
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

noise_t<-function(x,imf){
  # print(x)
  if(x>0)
  {
    median(abs(imf[,x])) * sqrt(2*(log10(length(imf[,x])))) /0.6745
  }
  else
  {
    0
  }
}

noise_t1<-function(x,imf){
  
  sd(imf[,x])*sqrt(2*log(length(imf[,x])))

}


get_k<-function(xcorr,x_emd)
{
  aaply(1:(x_emd$nimf-1),1,function(k,xcorr,To){
    ( k * (To-k) / (To^2)) * ( ( aaply(xcorr[,(1:k)],1,sum) /k)  - ( aaply(xcorr[,(k+1):To],1,sum) /(To-k)) )
  },xcorr=xcorr,To=x_emd$nimf) 
  
 
  
}

get_k1<-function(xcorr,x_emd)
{
  aaply(xcorr,1,function(x){
    sum((x[1:50])^2)/sum(x^2)
  })
  
}

smooth<-function(i,t_x1)
{
  if(abs(i) > t_x1)
  {
    sign(i)*(abs(i)-t_x1)
  }
  else
  {
    0
  }
}

smooth1<-function(i,t_x1)
{

  u<-0.5
  if(abs(i) > t_x1)
  {
    (1-u)*i+u*sign(i)*(abs(i) - t_x1 )
  }
  else
  {
    0
  }
}

