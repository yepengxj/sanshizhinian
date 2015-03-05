library(EMD)
library(quantstrat)
library(dtw)
library(plyr)
library("e1071")
library("kernlab")
library("GA")





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
ts<-HS300_idx_ts["2014-01-01/2015-02-03",]
date_range<-"2014-01-01/2015-02-03"
pred_range<-2
prd_col<-"ema3_next2day_roc"
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

factor_col<-c("ema15_close", "ema15_last1day_close","ema15_last2day_close",
              "ema15_last3day_close", "ema10_close", "ema20_close","ema3_close",
              "roc5_close", "roc5_last1day_close", "roc5_last2day_close",
              "roc5_last3day_close", "roc5_last4day_close", "roc10_close", "roc10_last1day_close", 
              "roc10_last2day_close", "roc10_last3day_close", "roc10_last4day_close", "roc15_close", 
              "roc15_last1day_close", "roc15_last2day_close", "roc15_last3day_close", "roc15_last4day_close", 
              "roc20_close", "roc20_last1day_close", "roc20_last2day_close", "roc20_last3day_close", 
              "roc20_last4day_close", "obv", "log2high_low5")

prd_col<-c("ema3_next1day_roc","ema3_next2day_roc","ema3_next3day_roc")


pred_result1<-aaply(as.numeric(.indexDate(HS300_idx_ts["2014-01-01/2015-02-03",])), 1, 
                   function(x,ts,factor_col,prd_col){
                     
                     end_date_id<-which(.indexDate(ts)==x)
                     start_date<-.indexDate(ts)[(end_date_id-40)]
                     
                     start_end_str<-paste(as.Date(start_date,format="%Y-%m-%d",origin = "1970-01-01"),
                                          "/",
                                          as.Date(x,format="%Y-%m-%d",origin = "1970-01-01"),
                                          sep="")
                     
                     kpca_svm_ga_func(ts[start_end_str],start_end_str,factor_col,prd_col,0)
                     
                   },HS300_idx_ts["2000-01-01/",],factor_col,"ema3_next1day_roc")

write.csv(pred_result1, file = "~/temp/kpca_svm_pred_result1.txt",row.names=F)

pred_result2<-aaply(as.numeric(.indexDate(HS300_idx_ts["2014-01-01/2015-02-03",])), 1, 
                    function(x,ts,factor_col,prd_col){
                      
                      end_date_id<-which(.indexDate(ts)==x)
                      start_date<-.indexDate(ts)[(end_date_id-40)]
                      
                      start_end_str<-paste(as.Date(start_date,format="%Y-%m-%d",origin = "1970-01-01"),
                                           "/",
                                           as.Date(x,format="%Y-%m-%d",origin = "1970-01-01"),
                                           sep="")
                      
                      kpca_svm_ga_func(ts[start_end_str],start_end_str,factor_col,prd_col,0)
                      
                    },HS300_idx_ts["2000-01-01/",],factor_col,"ema3_next2day_roc")

write.csv(pred_result2, file = "~/temp/kpca_svm_pred_result2.txt",row.names=F)

pred_result3<-aaply(as.numeric(.indexDate(HS300_idx_ts["2014-01-01/2015-02-03",])), 1, 
                    function(x,ts,factor_col,prd_col){
                      
                      end_date_id<-which(.indexDate(ts)==x)
                      start_date<-.indexDate(ts)[(end_date_id-40)]
                      
                      start_end_str<-paste(as.Date(start_date,format="%Y-%m-%d",origin = "1970-01-01"),
                                           "/",
                                           as.Date(x,format="%Y-%m-%d",origin = "1970-01-01"),
                                           sep="")
                      
                      kpca_svm_ga_func(ts[start_end_str],start_end_str,factor_col,prd_col,0)
                      
                    },HS300_idx_ts["2000-01-01/",],factor_col,"ema3_next3day_roc")

write.csv(pred_result3, file = "~/temp/kpca_svm_pred_result3.txt",row.names=F)