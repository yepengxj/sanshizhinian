source("./ems_smooth.R")

library(EMD)
library(plyr)
library("e1071")
library("kernlab")
library("GA")
library(psych)
library(XML)
library(Rlibeemd)


tickers<-"0000300"
path<-"~/temp"
start<-19990101
end<-as.numeric(format(Sys.Date() , "%Y%m%d"))

cntrade(c('0000300'), path = "~/temp", start = 19900101, end = end)

pca_eemd_svm_ga_func<-function(ts,eemd_l,date_range,factor_col,prd_col,pred_range,delay_range)
{
  print(sprintf("data_range:%s, factor_col:%s, pred_col:%s",date_range,factor_col,prd_col))
  curr_ts<-ts[,factor_col]
  data_eemd<-eval(call(eemd_l,as.matrix(curr_ts[,factor_col])))
  
  eemd_pcv<-principal(data_eemd$imf,nfactors=(data_eemd$nimf),rotate="none")
  t<-max(which(eemd_pcv$values>0.85))

  eemd_pcv_denoise<-data_eemd$imf%*%eemd_pcv$loadings[,(1:t)]
  
  train_data<-as.matrix(ts[1:(nrow(ts)-delay_range),prd_col])
  
  train_set<-nrow(train_data)-pred_range
  training<-eemd_pcv_denoise[1:train_set,]
  trainingTarget<-train_data[1:train_set,prd_col]
  
  
  testing<-matrix(eemd_pcv_denoise[(train_set:nrow(train_data)),],ncol=dim(eemd_pcv_denoise)[2])
  testingTarget<-matrix(train_data[(train_set:nrow(train_data)),prd_col],ncol=length(prd_col))
 
  predecting<-matrix(eemd_pcv_denoise[((nrow(eemd_pcv_denoise)-pred_range):nrow(eemd_pcv_denoise)),],ncol=dim(eemd_pcv_denoise)[2])
  predectTarget<-matrix(ts[((nrow(ts)-pred_range):nrow(ts)),prd_col],ncol=length(prd_col))

  GA <- ga(type = "real-valued",
           fitness = svm_mse_fitness, training, trainingTarget, testing, testingTarget,
           min = c(0, 0), max = c(100, 100), popSize = 50,
           maxiter = 50)
  
  svm_model<-svm(x=training,y=trainingTarget,cost=summary(GA)$solution[1,1],gamma=summary(GA)$solution[1,2])
  predict_data<-predict(svm_model,predecting)
  res<-data.frame("date"=as.numeric(format(as.Date(strsplit(date_range,"/",fixed=T)[[1]][2]),"%Y%m%d")), 
    "pred"=as.numeric(predict_data),
    "test"=predectTarget,
    "factor_col"=factor_col,
    "prd_col"=prd_col,
    "data_range"=date_range
  )
  
  print(res)
  
  res
}

#####准备数据from tdx
hs300_idx<-read.csv("~/temp/0000300.csv",header=F,skip=1,encoding = "UTF-8")

HS300_idx_ts<-xts(hs300_idx[,c(4,5,6,7,12)],order.by=as.Date(hs300_idx[,1],"%Y-%m-%d"))


colnames(HS300_idx_ts)<-c("close","high","low","open","vol")
HS300_idx_ts<-HS300_idx_ts[!HS300_idx_ts$close==0]

#####SVR
###ema15降噪
HS300_idx_ts$ema15_close<-EMA(HS300_idx_ts[,"close"],15)
HS300_idx_ts$ema15_last1day_close<-xts_shitf_days(HS300_idx_ts[,"ema15_close"],-1)
HS300_idx_ts$ema15_last2day_close<-xts_shitf_days(HS300_idx_ts[,"ema15_close"],-2)
HS300_idx_ts$ema15_last3day_close<-xts_shitf_days(HS300_idx_ts[,"ema15_close"],-3)

###ema10 20 3
HS300_idx_ts$ema10_close<-EMA(HS300_idx_ts[,"close"],10)
HS300_idx_ts$ema20_close<-EMA(HS300_idx_ts[,"close"],20)
HS300_idx_ts$ema3_close<-EMA(HS300_idx_ts[,"close"],3)
HS300_idx_ts$ema3_next1day_close<-xts_shitf_days(HS300_idx_ts[,"ema3_close"],1)
HS300_idx_ts$ema3_next1day_roc<-(HS300_idx_ts[,"ema3_next1day_close"]-HS300_idx_ts[,"ema3_close"])/HS300_idx_ts[,"ema3_close"]

HS300_idx_ts$ema3_next2day_close<-xts_shitf_days(HS300_idx_ts[,"ema3_close"],2)
HS300_idx_ts$ema3_next2day_roc<-(HS300_idx_ts[,"ema3_next2day_close"]-HS300_idx_ts[,"ema3_close"])/HS300_idx_ts[,"ema3_close"]

HS300_idx_ts$ema3_next3day_close<-xts_shitf_days(HS300_idx_ts[,"ema3_close"],3)
HS300_idx_ts$ema3_next3day_roc<-(HS300_idx_ts[,"ema3_next3day_close"]-HS300_idx_ts[,"ema3_close"])/HS300_idx_ts[,"ema3_close"]

HS300_idx_ts$ema3_next5day_close<-xts_shitf_days(HS300_idx_ts[,"ema3_close"],5)
HS300_idx_ts$ema3_next5day_roc<-(HS300_idx_ts[,"ema3_next5day_close"]-HS300_idx_ts[,"ema3_close"])/HS300_idx_ts[,"ema3_close"]

HS300_idx_ts$ema3_next6day_close<-xts_shitf_days(HS300_idx_ts[,"ema3_close"],6)
HS300_idx_ts$ema3_next6day_roc<-(HS300_idx_ts[,"ema3_next6day_close"]-HS300_idx_ts[,"ema3_close"])/HS300_idx_ts[,"ema3_close"]


###5日收益率
HS300_idx_ts$roc5_close<-ROC(HS300_idx_ts[,"close"],5)
HS300_idx_ts$roc5_last1day_close<-xts_shitf_days(HS300_idx_ts[,"roc5_close"],-1)
HS300_idx_ts$roc5_last2day_close<-xts_shitf_days(HS300_idx_ts[,"roc5_close"],-2)
HS300_idx_ts$roc5_last3day_close<-xts_shitf_days(HS300_idx_ts[,"roc5_close"],-3)
HS300_idx_ts$roc5_last4day_close<-xts_shitf_days(HS300_idx_ts[,"roc5_close"],-4)



###10日收益率
HS300_idx_ts$roc10_close<-ROC(HS300_idx_ts[,"close"],10)
HS300_idx_ts$roc10_last1day_close<-xts_shitf_days(HS300_idx_ts[,"roc10_close"],-1)
HS300_idx_ts$roc10_last2day_close<-xts_shitf_days(HS300_idx_ts[,"roc10_close"],-2)
HS300_idx_ts$roc10_last3day_close<-xts_shitf_days(HS300_idx_ts[,"roc10_close"],-3)
HS300_idx_ts$roc10_last4day_close<-xts_shitf_days(HS300_idx_ts[,"roc10_close"],-4)

###15日收益率
HS300_idx_ts$roc15_close<-ROC(HS300_idx_ts[,"close"],15)
HS300_idx_ts$roc15_last1day_close<-xts_shitf_days(HS300_idx_ts[,"roc15_close"],-1)
HS300_idx_ts$roc15_last2day_close<-xts_shitf_days(HS300_idx_ts[,"roc15_close"],-2)
HS300_idx_ts$roc15_last3day_close<-xts_shitf_days(HS300_idx_ts[,"roc15_close"],-3)
HS300_idx_ts$roc15_last4day_close<-xts_shitf_days(HS300_idx_ts[,"roc15_close"],-4)

###20日收益率
HS300_idx_ts$roc20_close<-ROC(HS300_idx_ts[,"close"],20)
HS300_idx_ts$roc20_last1day_close<-xts_shitf_days(HS300_idx_ts[,"roc20_close"],-1)
HS300_idx_ts$roc20_last2day_close<-xts_shitf_days(HS300_idx_ts[,"roc20_close"],-2)
HS300_idx_ts$roc20_last3day_close<-xts_shitf_days(HS300_idx_ts[,"roc20_close"],-3)
HS300_idx_ts$roc20_last4day_close<-xts_shitf_days(HS300_idx_ts[,"roc20_close"],-4)

##OBV
HS300_idx_ts$obv<-OBV(HS300_idx_ts[,"close"],HS300_idx_ts[,"vol"])

##log2high_low
HS300_idx_ts$log2high_low5<-sqrt(runSum(log2(HS300_idx_ts[,"high"]/HS300_idx_ts[,"low"]),5)/5)*80

##SVM
datarange_list<-c("2014-03-09/2014-03-12","2006-01-01/2008-01-01","2008-01-01/2009-01-01","2006-01-01/2009-01-01","2009-01-01/2014-07-01","2014-07-01/2015-02-11")
#datarange_list<-c("205-03-01/")
eemd_list<-c("Rlibeemd_eemd_func")

factor_col_list<-c("close","ema3_close")

pred_col_list<-list(data.frame(pred_col="ema3_next3day_roc",delay_range=3),
                    data.frame(pred_col="ema3_next2day_roc",delay_range=2),
                    data.frame(pred_col="ema3_next1day_roc",delay_range=1))           


library(doParallel)
cl <- makeCluster(4,outfile="~/temp/test.log")
registerDoParallel(cl)
clusterExport(cl,"pca_eemd_svm_ga_func")
  for(eemd_l in (eemd_list)){
    for(datarange_l in datarange_list)
    {
      for(fator_col in factor_col_list )
      {
        for(pred_col in pred_col_list)
        {
          ts<-HS300_idx_ts["2000-01-01/",]
          write.csv(
            foreach(x=as.numeric(.indexDate(HS300_idx_ts[datarange_l,])), .combine=rbind) %dopar%
            {
              library(EMD)
              library(plyr)
              library("e1071")
              library("kernlab")
              library("GA")
              library(psych)
              
              source("~/git_home/sanshizhinian/ems_smooth.R")
              end_date_id<-which(.indexDate(ts)==x)
              start_date<-.indexDate(ts)[(end_date_id-40)]
              start_end_str<-paste(as.Date(start_date,format="%Y-%m-%d",origin = "1970-01-01"),
                                   "/",
                                   as.Date(x,format="%Y-%m-%d",origin = "1970-01-01"),
                                   sep="")
              
              pca_eemd_svm_ga_func(ts[start_end_str,],eemd_l,start_end_str,factor_col,as.character(pred_col$pred_col),0,pred_col$delay_range)
              
            }

          ,file = paste("~/temp/","pred_res",paste("pca_eemd_svm",fator_col,as.character(pred_col$pred_col),eemd_l,sub("\\/","_",datarange_l),sep="_"),sep=""),row.names=F)  
        }
      }
      
    }
  }

if(1==0)
{
  ptime3 <- system.time({
    res<-foreach(eemd_l = eemd_list) %:%
      foreach(datarange_l = datarange_list) %:%
      foreach(factor_col = factor_col_list ) %:%
      foreach(pred_col = pred_col_list) %:%
      foreach(x=as.numeric(.indexDate(HS300_idx_ts[datarange_l,])), .combine=rbind) %dopar%
{
  library(EMD)
  library(plyr)
  library("e1071")
  library("kernlab")
  library("GA")
  library(psych)
  
  source("~/git_home/sanshizhinian/ems_smooth.R")
  end_date_id<-which(.indexDate(ts)==x)
  start_date<-.indexDate(ts)[(end_date_id-40)]
  start_end_str<-paste(as.Date(start_date,format="%Y-%m-%d",origin = "1970-01-01"),
                       "/",
                       as.Date(x,format="%Y-%m-%d",origin = "1970-01-01"),
                       sep="")
  
  pca_eemd_svm_ga_func(ts[start_end_str,],eemd_l,start_end_str,factor_col,prd_col,0)
  
}

  })

  
}
