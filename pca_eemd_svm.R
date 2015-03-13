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




xts_shitf_days<-function(xts_data,shitf_days)
{
  if(shitf_days>0)
  {
    new_idx_date<-.indexDate(xts_data)[1:(length(.indexDate(xts_data))-shitf_days)]
    
    shitf_data<-as.data.frame(xts_data)[(1+shitf_days):length(.indexDate(xts_data)),]
    
    xts(shitf_data,order.by=(as.Date(new_idx_date,format="%Y-%m-%d",origin = "1970-01-01")))
    
  }
  else
  {
    new_idx_date<-.indexDate(xts_data)
    shitf_data<-as.data.frame(xts_data)
    dim_data<-dim(shitf_data)
    for(i in (1:abs(shitf_days)))
    {
      shitf_data<-rbind(rep(NA,dim_data[2]),shitf_data)
    }
    
    xts(shitf_data[1:length(new_idx_date),],order.by=(as.Date(new_idx_date,format="%Y-%m-%d",origin = "1970-01-01")))
    
  }
  
}

#=====EEMD趋势拟合测试

#归一化
normalize_func<-function(x)
{
  (x-min(x))/(max(x)-min(x))+1
}

denormalize_func<-function(x,max_x,min_x)
{
  (x-1)*(max_x-min_x)+min_x
}


svm_mse_fitness<-function(x,training,trainingTarget,testing,testingTarget){
  #建立SVM
  svm_model<-svm(x=training,y=trainingTarget,cost=x[1],gamma=x[2])
  
  #做预测
  pred<-predict(svm_model,testing)
  
  #calc_fitness(MSE)
  -sqrt(sum((testingTarget-pred)^2))/length(testingTarget)
  
}


kpca_svm_ga_func<-function(ts,date_range,factor_col,prd_col,pred_range)
{
  
  model_data<-as.matrix(ts[,c(factor_col,prd_col)])
  
  #model_data_normalize<-cbind(t(aaply(model_data[,normalize_col],2,normalize_func)),model_data[,non_normalize_col] )
  model_data_normalize<-t(aaply(model_data,2,normalize_func))
  model_data_normalize_kpca<-pcv(kpca(~.,data=data.frame(model_data_normalize[,factor_col]),kernel="rbfdot",
                                      kpar=list(sigma=0.2),features=2))
  #train_set<-as.integer(nrow(model_data_normalize)*0.9)
  train_set<-nrow(model_data_normalize_kpca)-pred_range
  training<-model_data_normalize_kpca[1:train_set,]
  trainingTarget<-model_data_normalize[1:train_set,prd_col]
  
  
  testing<-matrix(model_data_normalize_kpca[(train_set:nrow(model_data_normalize_kpca)),],ncol=dim(model_data_normalize_kpca)[2])
  testingTarget<-matrix(model_data_normalize[(train_set:nrow(model_data_normalize)),prd_col],ncol=length(prd_col))
  
  GA <- ga(type = "real-valued",
           fitness = svm_mse_fitness, training, trainingTarget, testing, testingTarget,
           min = c(0, 0), max = c(100, 100), popSize = 50,
           maxiter = 50)
  
  svm_model<-svm(x=training,y=trainingTarget,cost=summary(GA)$solution[1,1],gamma=summary(GA)$solution[1,2])
  print("svm_model")
  predict_data<-predict(svm_model,testing)
  print(date_range)
  c(as.numeric(format(as.Date(strsplit(date_range,"/",fixed=T)[[1]][2]),"%Y%m%d")), 
    as.numeric(denormalize_func(predict_data,max(model_data[,prd_col]), min(model_data[,prd_col]))),
    as.numeric(denormalize_func(testingTarget,max(model_data[,prd_col]), min(model_data[,prd_col])))
  ) 
}

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
for(eemd_l in (eemd_list)){
  for(datarange_l in datarange_list)
  {
    for(factor_col in factor_col_list )
    {
      for(pred_col in pred_col_list)
      {
      
        pred_result1<-aaply(as.numeric(.indexDate(HS300_idx_ts[datarange_l,])), 1, 
                            function(x,ts,factor_col,prd_col,eemd_l){
                              end_date_id<-which(.indexDate(ts)==x)
                              start_date<-.indexDate(ts)[(end_date_id-40)]
                              start_end_str<-paste(as.Date(start_date,format="%Y-%m-%d",origin = "1970-01-01"),
                                                   "/",
                                                   as.Date(x,format="%Y-%m-%d",origin = "1970-01-01"),
                                                   sep="")
                             
                              pca_eemd_svm_ga_func(ts[start_end_str,],eemd_l,start_end_str,factor_col,as.character(pred_col$pred_col),0,pred_col$delay_range)
                              
                            },HS300_idx_ts["2000-01-01/",],factor_col,pred_col,eemd_l)
        
        file_name<-paste("pca_eemd_svm",factor_col,as.character(pred_col$pred_col),eemd_l$func_name,sub("\\/","_",datarange_l),sep="_")
        
        file_path<-paste("~/temp/","pred_res",file_name,sep="")
        
        write.csv(pred_result1, file = file_path,row.names=F)  
      }
    }
    
  }
}
