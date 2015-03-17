source("./ems_smooth.R")

library(EMD)
library(plyr)
library("e1071")
library("kernlab")
library("GA")
library(psych)
library(XML)
library(Rlibeemd)
library(quantmod)


tickers<-"0000300"
path<-"~/temp"
start<-19990101
end<-as.numeric(format(Sys.Date() , "%Y%m%d"))

#cntrade(c('0000300'), path = "~/temp", start = 19900101, end = end)

pca_eemd_svm_ga_func<-function(ts,eemd_l,date_range,factor_col,prd_col,pred_range,delay_range,if_pca)
{
  print(sprintf("data_range:%s, factor_col:%s, pred_col:%s",date_range,factor_col,prd_col))
  curr_ts<-ts[,factor_col]
  data_eemd<-eval(call(eemd_l,as.matrix(curr_ts[,factor_col])))
  
  if(if_pca==1)
  {
    eemd_pcv<-principal(data_eemd$imf,nfactors=(data_eemd$nimf),rotate="none")
    t<-max(which(eemd_pcv$values>0.85))
    
    eemd_pcv_denoise<-data_eemd$imf%*%eemd_pcv$loadings[,(1:t)]
  }
  else
  {
    eemd_pcv_denoise<-data_eemd$imf
  }
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
    "data_range"=date_range,
    "pred_range"=pred_range,
    "delay_range"=delay_range,
    "if_pca"=if_pca
  )
  
  print(res)
  
  res
}

kpca_svm_ga_func<-function(ts,date_range,factor_col,prd_col,pred_range,delay_range,kpca_features)
{
  print(sprintf("data_range:%s,pred_range:%d,delay_range:%d,kpca_features:%d",date_range,pred_range,delay_range,kpca_features))
  model_data<-as.matrix(ts[,c(factor_col,prd_col)])
  
  #model_data_normalize<-cbind(t(aaply(model_data[,normalize_col],2,normalize_func)),model_data[,non_normalize_col] )
  model_data_normalize<-t(aaply(model_data,2,normalize_func))
  
  model_data_normalize_kpca<-pcv(kpca(~.,data=data.frame(model_data_normalize[,factor_col]),kernel="rbfdot",
                                      kpar=list(sigma=0.2),features=kpca_features))
  
  train_data<-as.matrix(model_data_normalize_kpca[1:(nrow(model_data_normalize_kpca)-delay_range),])
  #train_set<-as.integer(nrow(model_data_normalize)*0.9)
  train_set<-nrow(train_data)-pred_range
  training<-train_data[1:train_set,]
  trainingTarget<-model_data_normalize[1:train_set,prd_col]
  
  testing<-matrix(train_data[(train_set:nrow(train_data)),],ncol=dim(train_data)[2])
  testingTarget<-matrix(model_data_normalize[(train_set:nrow(train_data)),prd_col],ncol=length(prd_col))
  
  predecting<-matrix(model_data_normalize_kpca[nrow(model_data_normalize_kpca),],ncol=dim(model_data_normalize_kpca)[2])
  predectTarget<-matrix(model_data[nrow(model_data_normalize),prd_col],ncol=length(prd_col))
  
  
  GA <- ga(type = "real-valued",
           fitness = svm_mse_fitness, training, trainingTarget, testing, testingTarget,
           min = c(0, 0), max = c(100, 100), popSize = 50,
           maxiter = 50)
  
  svm_model<-svm(x=training,y=trainingTarget,cost=summary(GA)$solution[1,1],gamma=summary(GA)$solution[1,2])
  predict_data<-denormalize_func(predict(svm_model,predecting),max(model_data[,prd_col]),min(model_data[,prd_col]))
  
  res<-data.frame("date"=as.numeric(format(as.Date(strsplit(date_range,"/",fixed=T)[[1]][2]),"%Y%m%d")), 
                  "pred"=as.numeric(predict_data),
                  "test"=predectTarget,
                  "prd_col"=prd_col,
                  "data_range"=date_range,
                  "kpca_features"=kpca_features,
                  "pred_range"=pred_range,
                  "delay_range"=delay_range
                  
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

###close-ema15
HS300_idx_ts$"close_ema15_close" <- HS300_idx_ts$close - HS300_idx_ts$ema15_close

HS300_idx_ts$"close_ema15_close_last1day"<-xts_shitf_days(HS300_idx_ts[,"close_ema15_close"],-1)
HS300_idx_ts$"close_ema15_close_last2day"<-xts_shitf_days(HS300_idx_ts[,"close_ema15_close"],-2)
###ema10 20 3
HS300_idx_ts$ema20_close<-EMA(HS300_idx_ts[,"close"],20)
HS300_idx_ts$ema10_close<-EMA(HS300_idx_ts[,"close"],10)

HS300_idx_ts$ema10_ema20_close<-HS300_idx_ts$ema10_close-HS300_idx_ts$ema20_close


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


HS300_idx_ts$next1day_close<-xts_shitf_days(HS300_idx_ts[,"close"],1)
HS300_idx_ts$next1day_roc<-(HS300_idx_ts[,"next1day_close"]-HS300_idx_ts[,"close"])/HS300_idx_ts[,"close"]

HS300_idx_ts$next2day_close<-xts_shitf_days(HS300_idx_ts[,"close"],2)
HS300_idx_ts$next2day_roc<-(HS300_idx_ts[,"next2day_close"]-HS300_idx_ts[,"close"])/HS300_idx_ts[,"close"]

HS300_idx_ts$next3day_close<-xts_shitf_days(HS300_idx_ts[,"close"],3)
HS300_idx_ts$next3day_roc<-(HS300_idx_ts[,"next3day_close"]-HS300_idx_ts[,"close"])/HS300_idx_ts[,"close"]


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

if_pca_list<-c(1,0)

pred_range_list<-c(0,1,3,5)

pred_col_list<-list(data.frame(pred_col="ema3_next3day_roc",delay_range=3),
                    data.frame(pred_col="ema3_next2day_roc",delay_range=2),
                    data.frame(pred_col="ema3_next1day_roc",delay_range=1),
                    data.frame(pred_col="next3day_roc",delay_range=3),
                    data.frame(pred_col="next2day_roc",delay_range=2),
                    data.frame(pred_col="next1day_roc",delay_range=1))           

library(doParallel)
cl <- makeCluster(4,outfile="~/temp/test.log")
registerDoParallel(cl)
clusterExport(cl,"pca_eemd_svm_ga_func")
for(pred_range in pred_range_list)
{
  for(if_pca in if_pca_list)
  {
    for(eemd_l in (eemd_list))
    {
      for(datarange_l in datarange_list)
      {
        for(factor_col in factor_col_list )
        {
          for(pred_col in pred_col_list)
          {
            clusterExport(cl,"pred_range")
            clusterExport(cl,"if_pca")
            clusterExport(cl,"eemd_l")
            clusterExport(cl,"datarange_l")
            clusterExport(cl,"factor_col")
            clusterExport(cl,"pred_col")
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
                library(quantmod)
                
                source("~/git_home/sanshizhinian/ems_smooth.R")
                end_date_id<-which(.indexDate(ts)==x)
                start_date<-.indexDate(ts)[(end_date_id-40)]
                start_end_str<-paste(as.Date(start_date,format="%Y-%m-%d",origin = "1970-01-01"),
                                     "/",
                                     as.Date(x,format="%Y-%m-%d",origin = "1970-01-01"),
                                     sep="")
                
                pca_eemd_svm_ga_func(ts[start_end_str,],eemd_l,start_end_str,factor_col,as.character(pred_col$pred_col),pred_range,pred_col$delay_range,if_pca)
                
              }

              ,file = paste("~/temp/","pred_res",paste(as.character(pred_range),as.character(if_pca),"pca_eemd_svm",factor_col,as.character(pred_col$pred_col),eemd_l,sub("\\/","_",datarange_l),sep="_"),sep=""),row.names=F)  
          }
        }

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


datarange_list<-c("2014-03-09/2014-03-12","2006-01-01/2008-01-01","2008-01-01/2009-01-01","2006-01-01/2009-01-01","2009-01-01/2014-07-01","2014-07-01/2015-02-11")
#datarange_list<-c("205-03-01/")

factor_col<-c("ema15_close", "ema15_last1day_close","ema15_last2day_close",
              "ema15_last3day_close", "ema10_close", "ema20_close","ema3_close",
              "roc5_close", "roc5_last1day_close", "roc5_last2day_close",
              "roc5_last3day_close", "roc5_last4day_close", "roc10_close", "roc10_last1day_close", 
              "roc10_last2day_close", "roc10_last3day_close", "roc10_last4day_close", "roc15_close", 
              "roc15_last1day_close", "roc15_last2day_close", "roc15_last3day_close", "roc15_last4day_close", 
              "roc20_close", "roc20_last1day_close", "roc20_last2day_close", "roc20_last3day_close", 
              "roc20_last4day_close", "obv", "log2high_low5")

pred_range_list<-c(0,1,3,5)
kpca_features_list<-c(2,4,6,8)
pred_col_list<-list(data.frame(pred_col="ema3_next3day_roc",delay_range=3),
                    data.frame(pred_col="ema3_next2day_roc",delay_range=2),
                    data.frame(pred_col="ema3_next1day_roc",delay_range=1),
                    data.frame(pred_col="next3day_roc",delay_range=3),
                    data.frame(pred_col="next2day_roc",delay_range=2),
                    data.frame(pred_col="next1day_roc",delay_range=1))           


library(doParallel)
cl <- makeCluster(4,outfile="~/temp/test.log")
registerDoParallel(cl)
clusterExport(cl,"kpca_svm_ga_func")

for(datarange_l in datarange_list)
{
    for(pred_range in pred_range_list)
    {
      for(kpca_features in kpca_features_list )
      {
        for(pred_col in pred_col_list)
        {
          clusterExport(cl,"pred_range")
          clusterExport(cl,"datarange_l")
          clusterExport(cl,"kpca_features")
          clusterExport(cl,"pred_col")
          ts<-HS300_idx_ts["2000-01-01/",]
          
          print(sprintf("pred_res1begin pred_range:%s,datarange_l:%s,kpca_features:%s,pred_col:%s",pred_range,datarange_l,kpca_features,pred_col$pred_col))
          pred_result1<-foreach(x=as.numeric(.indexDate(HS300_idx_ts[datarange_l,])), .combine=rbind) %dopar%
          {
            library(EMD)
            library(plyr)
            library("e1071")
            library("kernlab")
            library("GA")
            library(psych)
            library(quantmod)
                                
            end_date_id<-which(.indexDate(ts)==x)
            start_date<-.indexDate(ts)[(end_date_id-40)]
            
            start_end_str<-paste(as.Date(start_date,format="%Y-%m-%d",origin = "1970-01-01"),
                                 "/",
                                 as.Date(x,format="%Y-%m-%d",origin = "1970-01-01"),
                                 sep="")
            
            # (ts,date_range,factor_col,prd_col,pred_range,delay_range,kpca_features)
            kpca_svm_ga_func(ts[start_end_str],start_end_str,factor_col,as.character(pred_col$pred_col),pred_range,pred_col$delay_range,kpca_features)
                                
           }
          
         write.csv(pred_result1, file = paste("~/temp/","pred_res","kpca_svm",paste(kpca_features,as.character(pred_range),as.character(pred_col$pred_col),sub("\\/","_",datarange_l),sep="_"),sep=""),row.names=F) 
        }
      }
    }
}
