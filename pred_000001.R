library(EMD)
library(quantstrat)
library(plyr)
library("e1071")
library("hht")
library("kernlab")

#####准备数据from tdx
#file_list<-c("SZ150009.txt","SZ150052.txt","SZ150077.txt","SZ150105.txt","SZ150141.txt","SZ150168.txt")







file_list<-c("SZ150107.txt","SZ150086.txt","SZ150091.txt","SZ150058.txt","SZ150075.txt")

#HS300_fund<-read.table('~/Downloads/data/SZ399300.txt',header=F,sep="\t",skip = 2,fill=T)
HS300_fund<-read.table('~/Downloads/data/SZ399101.txt',header=F,sep="\t",skip = 2,fill=T)
HS300_fund<-HS300_fund[-nrow(HS300_fund),]
HS300_fund_ts<-xts(HS300_fund[,-1],order.by=as.Date(HS300_fund[,1],format="%m/%d/%Y",origin = "1970-01-01"))
colnames(HS300_fund_ts)<-c("open","high","low","close","vol","amount")

cor_data<-HS300_fund_ts[,"close"]

x<-"SZ150009.txt"
test_data<-aaply(file_list,1,function(x,cor_data) {
  print(x)
  B_close<-read.table(paste('~/Downloads/data/',x,sep=""),header=F,sep="\t",skip = 2,fill=T)
  B_close<-B_close[-nrow(B_close),]
  B_close_ts<-xts(B_close[,-1],order.by=as.Date(B_close[,1],format="%m/%d/%Y",origin = "1970-01-01"))
  colnames(B_close_ts)<-c("open","high","low","close","vol","amount")
  print(tail(B_close_ts,5))
  B_close_ts<-merge(cor_data,B_close_ts[,"close"])
  return(B_close_ts[,-1])
},cor_data)


test_data<-merge(cor_data,t(test_data))

test_data<-test_data[!(is.na(test_data[,2]) | 
              is.na(test_data[,3]) | 
              is.na(test_data[,4]) | 
              is.na(test_data[,5])| 
              is.na(test_data[,6]) |
              is.na(test_data[,7])),]

cor(test_data[,-1],method = c("pearson"))
cor(test_data[,-1],method = c("kendall"))
cor(test_data[,-1],method = c("spearman"))
method = c("pearson", "kendall", "spearman")
 !(is.na(test_data[,2]) | 
    is.na(test_data[,3]) | 
    is.na(test_data[,4]) | 
    is.na(test_data[,5])| 
    is.na(test_data[,6]) | 
    is.na(test_data[,7]) )


test_data[,c(5,7)]

plot(test_data[,5],ylim=c(min(test_data[,-1],na.rm=T),max(test_data[,-1])))
lines(test_data[,7])

HS300_fund<-read.table('~/Downloads/data/SZ399300.txt',header=F,sep="\t",skip = 2,fill=T)
HS300_fund<-HS300_fund[-nrow(HS300_fund),]
HS300_fund_ts<-xts(HS300_fund[,-1],order.by=as.Date(HS300_fund[,1],format="%m/%d/%Y",origin = "1970-01-01"))
colnames(HS300_fund_ts)<-c("open","high","low","close","vol","amount")

HS300_idx<-read.table('~/Downloads/SZ399300.txt',header=F,sep="\t",skip = 2,fill=T)
HS300_idx<-HS300_idx[-nrow(HS300_idx),]
HS300_idx_ts<-xts(HS300_idx[,-1],order.by=as.Date(HS300_idx[,1],format="%m/%d/%Y",origin = "1970-01-01"))
colnames(HS300_idx_ts)<-c("open","high","low","close","vol","amount")



####sma5降噪
HS300_idx_ts$sma5_close<-SMA(HS300_idx_ts[,"close"],5)

#ac预测
result_data_hs300_sma5_smooth<-aaply(as.numeric(.indexDate(HS300_idx_ts["2011-01-01/2015-01-19","sma5_close"])), 1, 
                                    function(x,ts,i,j){
                                      start_date_id<-which(.indexDate(ts)==x)
                                      end_date<-.indexDate(ts)[(start_date_id+i-1)]
                                      if(!is.na(end_date))
                                      {
                                        start_end_str<-paste(as.Date(x,format="%Y-%m-%d",origin = "1970-01-01"),
                                                             "/",
                                                             as.Date(end_date,format="%Y-%m-%d",origin = "1970-01-01"),
                                                             sep="")
                                        start_str<-paste("/",as.Date(x,format="%Y-%m-%d",origin = "1970-01-01"),sep="")
                                        print(start_end_str)
                                        print(start_str)
                                        ac_indicator(ts[start_end_str],ts[start_str],i,j)
                                      }
                                      
                                    },HS300_idx_ts["2006-01-01/","sma5_close"],10,3)


####ema5降噪
HS300_idx_ts$ema5_close<-EMA(HS300_idx_ts[,"close"],5)

result_data_hs300_ema5_smooth<-aaply(as.numeric(.indexDate(HS300_idx_ts["2011-01-01/2015-01-19","ema5_close"])), 1, 
                                     function(x,ts,i,j){
                                       start_date_id<-which(.indexDate(ts)==x)
                                       end_date<-.indexDate(ts)[(start_date_id+i-1)]
                                       if(!is.na(end_date))
                                       {
                                         start_end_str<-paste(as.Date(x,format="%Y-%m-%d",origin = "1970-01-01"),
                                                              "/",
                                                              as.Date(end_date,format="%Y-%m-%d",origin = "1970-01-01"),
                                                              sep="")
                                         start_str<-paste("/",as.Date(x,format="%Y-%m-%d",origin = "1970-01-01"),sep="")
                                         print(start_end_str)
                                         print(start_str)
                                         ac_indicator(ts[start_end_str],ts[start_str],i,j)
                                       }
                                       
                                     },HS300_idx_ts["2006-01-01/","ema5_close"],10,3)

#####emd降噪
HS300_idx_ts$close_emd_smooth<-emd_smooth(as.numeric(HS300_idx_ts[,"close"]))

#ac预测   2009-01-16/2015-01-30
result_data_hs300_emd_smooth<-aaply(as.numeric(.indexDate(HS300_idx_ts["2009-01-01/2015-01-19","close"])), 1, 
                       function(x,ts,i,j){
                         start_date_id<-which(.indexDate(ts)==x)
                         end_date<-.indexDate(ts)[(start_date_id+i-1)]
                         start_end_str<-paste(as.Date(x,format="%Y-%m-%d",origin = "1970-01-01"),
                                              "/",
                                              as.Date(end_date,format="%Y-%m-%d",origin = "1970-01-01"),
                                              sep="")
                         start_str<-paste("/",as.Date(x,format="%Y-%m-%d",origin = "1970-01-01"),sep="")
                         end_str<-paste("/",as.Date(end_date,format="%Y-%m-%d",origin = "1970-01-01"),sep="")
                         #print(start_end_str)
                         #print(start_str)
                         #print(end_str)
                         ts_curr<-ts[end_str]
                         ts_curr$emd_smmoth<-emd_smooth(as.numeric(ts[end_str]))
                         ac_indicator(ts_curr[start_end_str,"emd_smmoth"],ts_curr[start_str,"emd_smmoth"],i,j)
                         
                       },HS300_idx_ts["2006-01-01/","close"],10,3)

#####EEMD降噪
HS300_idx_ts$close_eemd_smooth<-eemd_smooth(as.numeric(HS300_idx_ts[,"close"]))

#ac预测2009-02-06  2008-01-15/2015-01-30
result_data_hs300_eemd_smooth<-aaply(as.numeric(.indexDate(HS300_idx_ts["2008-01-01/2015-01-19","close"])), 1, 
                                    function(x,ts,i,j){
                                      start_date_id<-which(.indexDate(ts)==x)
                                      end_date<-.indexDate(ts)[(start_date_id+i-1)]
                                      if(!is.na(end_date))
                                      {
                                        start_end_str<-paste(as.Date(x,format="%Y-%m-%d",origin = "1970-01-01"),
                                                             "/",
                                                             as.Date(end_date,format="%Y-%m-%d",origin = "1970-01-01"),
                                                             sep="")
                                        start_str<-paste("/",as.Date(x,format="%Y-%m-%d",origin = "1970-01-01"),sep="")
                                        end_str<-paste("/",as.Date(end_date,format="%Y-%m-%d",origin = "1970-01-01"),sep="")
                                        #print(start_end_str)
                                        #print(start_str)
                                        #print(end_str)
                                        ts_curr<-ts[end_str]
                                        ts_curr$eemd_smmoth<-eemd_smooth(as.numeric(ts[end_str]))
                                        ac_indicator(ts_curr[start_end_str,"eemd_smmoth"],ts_curr[start_str,"eemd_smmoth"],i,j)
                                      }
                                      
                                    },HS300_idx_ts["2000-01-01/","close"],10,3)

pred_result_ts<-as.xts(result_data_hs300_eemd_smooth,order.by=time(HS300_idx_ts["2009-02-06/2015-02-16"]))
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
HS300_idx_ts$ema3_next5day_close<-xts_shitf_days(HS300_idx_ts[,"ema3_close"],5)
HS300_idx_ts$ema3_next5day_roc<-(HS300_idx_ts[,"ema3_next5day_close"]-HS300_idx_ts[,"ema3_close"])/HS300_idx_ts[,"ema3_close"]



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
kernel_type <- "radial"
max_gamma <- -2
min_gamma <- -6
max_cost <- 3
min_cost <- -6

tuned <- tune.svm(survival_days ~ ., data = train_set,
                  kernel = kernel_type,
                  gamma = 10^(min_gamma:max_gamma),
                  cost = 10^(min_cost:max_cost))
tuned<-tune.svm(HS300_idx_ts["2015-01-15/2015-02-06",
           c("ema15_close", "ema15_last1day_close","ema15_last2day_close",
             "ema15_last3day_close", "ema10_close", "ema20_close","ema3_close",
              "roc5_close", "roc5_last1day_close", "roc5_last2day_close",
             "roc5_last3day_close", "roc5_last4day_close", "roc10_close", "roc10_last1day_close", 
             "roc10_last2day_close", "roc10_last3day_close", "roc10_last4day_close", "roc15_close", 
             "roc15_last1day_close", "roc15_last2day_close", "roc15_last3day_close", "roc15_last4day_close", 
             "roc20_close", "roc20_last1day_close", "roc20_last2day_close", "roc20_last3day_close", 
             "roc20_last4day_close", "obv", "log2high_low5")],
           HS300_idx_ts["2015-01-15/2015-02-06", "ema3_next5day_roc"],
           kernel_type="radial",
           gamma = 10^(min_gamma:max_gamma),
           cost = 10^(min_cost:max_cost))
               
predict_data<-predict(svm_model, HS300_idx_ts["2015-02-09",
                                c("ema15_close", "ema15_last1day_close","ema15_last2day_close",
                                  "ema15_last3day_close", "ema10_close", "ema20_close","ema3_close",
                                  "roc5_close", "roc5_last1day_close", "roc5_last2day_close",
                                  "roc5_last3day_close", "roc5_last4day_close", "roc10_close", "roc10_last1day_close", 
                                  "roc10_last2day_close", "roc10_last3day_close", "roc10_last4day_close", "roc15_close", 
                                  "roc15_last1day_close", "roc15_last2day_close", "roc15_last3day_close", "roc15_last4day_close", 
                                  "roc20_close", "roc20_last1day_close", "roc20_last2day_close", "roc20_last3day_close", 
                                  "roc20_last4day_close", "obv", "log2high_low5")])


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

# Decompose with EEMD
imfs <- Rlibeemd::eemd(normalize_func(xxx$close), num_siftings = 1000, ensemble_size = 100, threads = 2)
head(imfs,10)
plot(imfs[,c(1:5)])
plot(imfs[,c(6:11)])
# High frequencies
ts.plot(rowSums(imfs[,1:3]))
# Low frequencies
ts.plot(rowSums(imfs[,4:ncol(imfs)]))

test<-HS300_idx_ts["2006-01-01/","close_emd_smooth"]
test[!is.na(test[,1]),]
testdata<-close_emd_smooth_ts

testdata<-SMA(xxx[,"close"],5)
testdata<-testdata[!is.na(testdata)]

plot(testdata["2014-01-01/2015-01-30",1],ylim=c(min(testdata["2014-01-01/2015-01-30",1],na.rm=T),max(testdata["2014-01-01/2015-01-30",1],na.rm=T)))

which(.indexDate(testdata["2011-01-01/2015-01-30",1])==16085)
result_data_emd<-result_data

result_data_sma<-aaply(as.numeric(.indexDate(testdata["2011-01-01/2015-01-19",1])), 1, 
function(x,ts,i,j){
  start_date_id<-which(.indexDate(ts)==x)
  end_date<-.indexDate(ts)[(start_date_id+i-1)]
  if(!is.na(end_date))
  {
    start_end_str<-paste(as.Date(x,format="%Y-%m-%d",origin = "1970-01-01"),
                         "/",
                         as.Date(end_date,format="%Y-%m-%d",origin = "1970-01-01"),
                         sep="")
    start_str<-paste("/",as.Date(x,format="%Y-%m-%d",origin = "1970-01-01"),sep="")
    print(start_end_str)
    print(start_str)
    ac_indicator(ts[start_end_str],ts[start_str],i,j)
  }
  
  },testdata[,1],10,3)



xxx_close_normalize<-(xxx[,"close"]-min(xxx[,"close"]))/(max(xxx[,"close"])-min(xxx[,"close"]))+1
eemd_test<-Rlibeemd::eemd(xxx_close_normalize)

xxx_sma_5<-SMA(xxx[,"close"],5)
xxx_sma_5<-xxx_sma_5[!is.na(xxx_sma_5)]
normalize_func(xxx_sma_5,max_sma5,min_sma5)
xxx_sma_5_normalize<-(xxx_sma_5-min(xxx_sma_5))/(max(xxx_sma_5)-min(xxx_sma_5))+1
xxx_sma_5_normalize<-cbind(Rlibeemd::eemd(xxx_sma_5_normalize),xxx_sma_5_normalize)
xxx_sma_5_normalize_xts<-xts(xxx_sma_5_normalize,order.by=as.Date(.indexDate(xxx_sma_5)))  


xxx_close<-cbind(xxx_close_normalize,Rlibeemd::eemd(xxx_close_normalize))
xxx_close_xts<-xts(xxx_close,order.by=as.Date(.indexDate(xxx)))  


result_data_eemd2<-aaply(as.numeric(.indexDate(xxx_close_xts["2014-01-01/2015-01-19",3])), 1, 
function(x,ts,i,j){

  start_date_id<-which(.indexDate(ts)==x)
  end_date<-.indexDate(ts)[(start_date_id+i-1)]
  if(!is.na(end_date))
  {
    start_end_str<-paste(as.Date(x,format="%Y-%m-%d",origin = "1970-01-01"),
                         "/",
                         as.Date(end_date,format="%Y-%m-%d",origin = "1970-01-01"),
                         sep="")
    start_str<-paste("/",as.Date(x,format="%Y-%m-%d",origin = "1970-01-01"),sep="")
    print(start_end_str)
    print(start_str)
    ac_indicator(ts[start_end_str],ts[start_str],i,j)
  }
  
},xxx_close_xts[,3],10,3)

result_data<-ac_indicator(xxx_close_xts["2011-01-10/2015-01-30"],xxx_close_xts["/2011-01-01"],10,3)

plot_data<-cbind(xxx["2011-01-17/2015-01-30","close"],result_data_emd)
plot_data<-merge(testdata["2011-01-01/2011-01-30"],result_data,join="left")
plot_data<-result_data[,c(1,6,7,8)]

plot(plot_data["2014-10-17/2015-01-30",1],ylim=c(min(plot_data["2014-01-17/2015-01-30"],na.rm=T),max(plot_data["2014-01-17/2015-01-30"],na.rm=T)))
lines(plot_data["2014-10-17/2015-01-30",2],col="red")
lines(plot_data["2014-10-17/2015-01-30",3],col="blue")
lines(plot_data["2014-10-17/2015-01-30",4],col="yellow")



pred_close_price<-HS300_idx_ts["2008-01-15/2015-01-30"]
pred_close_price$sell_signal<-((result_data_hs300_eemd_smooth[,1]>result_data_hs300_eemd_smooth[,2])&(result_data_hs300_eemd_smooth[,2]>result_data_hs300_eemd_smooth[,3]))
pred_close_price$buy_signal<-((result_data_hs300_eemd_smooth[,1]<result_data_hs300_eemd_smooth[,2])&(result_data_hs300_eemd_smooth[,2]<result_data_hs300_eemd_smooth[,3]))
pred_close_price$close<-pred_close_price$close/1000
pred_close_price$high<-pred_close_price$high/1000
pred_close_price$low<-pred_close_price$low/1000
pred_close_price$open<-pred_close_price$open/1000


result_data1<-plot_data[,(1:4)]
result_data1$sell_signal<-((result_data1[,2]>result_data1[,3])&(result_data1[,3]>result_data1[,4]))

result_data1$buy_signal<-((result_data1[,2]<result_data1[,3])&(result_data1[,3]<result_data1[,4]))

chuquan<-as.Date(c("2014-06-10","2013-06-18",'2012-10-17'),format="%Y-%m-%d",origin = "1970-01-01")
is_chuquan<-rep(1,length(chuquan))
chuquan_ts<-xts(is_chuquan,order.by=chuquan)
colnames(chuquan_ts)<-"chuquan"

result_data1$sma<-SMA(result_data1[,"close"],5)
result_data1$bias<-(result_data1[,"close"]-result_data1[,"sma"])/result_data1[,"sma"]*100
result_data1<-merge(xxx["2011-01-17/2015-01-30"],sell_signal,join="left")
result_data1<-merge(result_data1["2011-01-17/"],buy_signal,join="left")
result_data1<-merge(result_data1["2011-01-17/"],chuquan_ts,join="left")
result_data1$chuquan[is.na(result_data1$chuquan)]<-0
result_data1<-result_data1[!result_data1$close==0]

result_data1$buy_signal<-(result_data1$buy_signal==1)&(result_data1$bias < -3)


1] "2012-01-11 00:00:00 ZSYH 9000 @ 0"
[1] "2012-01-12 00:00:00 ZSYH -9000 @ 0"
[1] "2012-01-17 00:00:00 ZSYH 9000 @ 0"


result_data1["2012-01-11/2012-01-17"]
xxx[!xxx$close==0]
