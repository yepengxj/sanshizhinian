library(EMD)
library(quantstrat)
library(dtw)
library(plyr)


######时间窗口截取函数#####
##输入
##x:截取开始位置
##y:被截取序列
##k:截取长度
linestep<-function(x,y,k)
{
  #窗口开始时间+窗口结束时间+窗口中数据内容
  c(as.numeric(.indexDate(y[x])),as.numeric(.indexDate(y[x+k-1])),as.numeric(y[x:(x+k-1)]))
}


######时间窗矩阵函数#####
#将时间序列按制定长度转换成矩阵，以方便后续最小二乘转换和距离计算
##输入
##list_data:待转换序列
##k:截取长度
list2matrix_stepbystep<-function(list_data,k)
{
  
  list_len <-length(list_data)
  
  if(list_len < k)
  {
    matrix_tmp<-matrix(c(as.numeric(.indexDate(list_data[1])),as.numeric(list_data)),nrow=1,ncol=list_len+1)
  }
  else
  {
    #matrix_tmp<-matrix(nrow=list_len-k+1,ncol=k+1)
      
    #for(i in 1:(list_len-k+1))
    #{
    #  matrix_tmp[i,]<-c(as.numeric(.indexDate(list_data[i])),as.numeric(list_data[seq(i,(i+k-1))]))
    #}
    matrix_tmp<-matrix(nrow = list_len-k+1,ncol=k)
    matrix_tmp<-sapply(1:(list_len-k+1),linestep,y=list_data,k=k)
  }
  
  matrix_tmp
}

######最小二乘转换并计算dtw距离#####
##输入
##x:被拟合向量,被拟合向量长度为样本向量长度+预测长度，目前预测长度固定为3天
##y:样本向量
##i:预测长度
lsfit_x<-function(x,y)
{
  i=length(x[c(-1,-2)])-length(y)
    
  if(all(x[c(-1,-2,((length(x)-(i-1)):length(x))*-1)]==y))
  {
    print("dfdfdfdfdfdf")
  }
  
  #确定最小二乘线性拟合参数，其中x去除开始时间、结束时间以及预测长度
  tmpmodel<-lsfit(x=as.numeric(x[c(-1,-2,((length(x)-(i-1)):length(x))*-1)]),y=as.numeric(y))
  #计算本拟合向量与样本向量的距离
  dis<-dtw(x[c(-1,-2,((length(x)-(i-1)):length(x))*-1)],y)$distance
 
  if(dis>0)
  {
    sim_rato<- 1/dis
  }
  else
  {
    sim_rato<- 0
  }

  return(c(x[1:2],sim_rato,x[c(-1,-2)]*tmpmodel$coefficients["X"]+tmpmodel$coefficients["Intercept"]))
}


######相似体拟合模型#####
##输入
##x:预测时间序列,向量
##y:样本时间序列,矩阵
##j:预测长度
##i:相似度排名
ac_model<-function(x,y,i)
{
  print(as.Date(x[2],format="%Y-%m-%d",origin = "1970-01-01"))
  dfddfd<-aaply(y,1,"lsfit_x",as.numeric(x)[3:length(x)], .parallel = F)
  # dfddfd<-t(apply(y,1,"lsfit_x",as.numeric(x)[3:length(x)]))
  t<-ncol(y)-length(x)
 # write.table(dfddfd,'~/Downloads/log.txt',sep=",")
 dfddfd<-dfddfd[dfddfd[,3]!=0,]
 head_res<-head(dfddfd[order(-dfddfd[,3]),],i)
 c("start_date"=x[1],"end_date"=x[2],colSums((head_res[,3])/sum(head_res[,3])*(head_res[,(ncol(head_res)-t+1):ncol(head_res)])))

 print( c("start_date"=x[1],"end_date"=x[2],colSums((head_res[,3])/sum(head_res[,3])*(head_res[,(ncol(head_res)-t+1):ncol(head_res)]))))
 
}


######相似体拟合模型#####
##输入
##x:预测时间序列
##y:样本时间序列
##i:预测时间滑动窗口
##j:预测长度
x<-xxx_close_xts["2011-01-04/2011-01-30",2]
y<-xxx_close_xts["/2011-01-04",2]
i<-10
j<-3

ac_indicator(x,y,i,j)
ac_indicator<-function(x,y,i,j)
{
  dim(x)
  dim(y)
  #将预测数据序列按预测滑动窗口长度i转换为矩阵
  pred_data<-t(list2matrix_stepbystep(x,i))

  
  #将样本数据按i+j长度转换为矩阵
  sample_data<-t(list2matrix_stepbystep(y,(i+j)))

  #result<-t(apply(pred_data,1,"ac_model",y=sample_data,i=5))
  result<-adply(pred_data,1,"ac_model",y=sample_data,i=5, .parallel = F)
  result_ts<-xts(result[,c(-1,-2,-3)],order.by=as.Date(result[,3],format="%Y-%m-%d",origin = "1970-01-01"))
  
  
  colnames(result_ts)<-sapply(c(1:ncol(result_ts)),function(x){paste("pred_result",x,sep="")})
  #result
  result_ts
}

#EMD分解加入
EMD_test_by_step<-function(emd_data,s,k,j)
{
  emd_result<-emd(as.numeric(emd_data))
  
  date_seq<-as.numeric(.indexDate(emd_data))
  
  prd_result <- NULL
  
  for(i in 1:emd_result$nimf)
  { 
    emd_tmp<- cbind(date_seq,emd_result$imf[,i])
    emd_tmp<-xts(emd_tmp[,-1],order.by=as.Date(emd_tmp[,1],format="%Y-%m-%d",origin = "1970-01-01"))
    
    if(is.null(prd_result))
    {
      prd_result <- ac_indicator(emd_tmp[s],emd_tmp,k,j)
    }
    else
    {
      prd_result <- cbind(prd_result,ac_indicator(emd_tmp[s],emd_tmp,k,j))
    }
  }
  
  emd_tmp<- cbind(date_seq,emd_result$residue)
  emd_tmp<-xts(emd_tmp[,-1],order.by=as.Date(emd_tmp[,1],format="%Y-%m-%d",origin = "1970-01-01"))
  if(is.null(prd_result))
  {
    prd_result <- ac_indicator(emd_tmp[s],emd_tmp,k,j)
  }
  else
  {
    prd_result <- cbind(prd_result, ac_indicator(emd_tmp[s],emd_tmp,k,j))
  }
  
  prd_result
}


result_data<-EMD_test_by_step(testdata,"2014-09-20/",10,3)
result_data<-ac_indicator(xxx_close_xts["2011-01-04/2011-01-30",2],xxx_close_xts["/2011-01-04",2],10,3)
SMA(Cl(xxx), 3)
testdata<-SMA(Cl(xxx), 5)
testdata<-testdata[!is.na(testdata)]

result_data<-merge(xxx["2012-01-17/2012-12-30"],sell_signal,join="left")
result_data<-merge(result_data["2012-01-17/"],buy_signal,join="left")

plot_data<-merge(plot_data,(plot_data[,1]>plot_data[,2]&plot_data[,2]>plot_data[,3]),join="left")

sell_signal<-((result_data[,1]>result_data[,2])&(result_data[,2]>result_data[,3]))
colnames(sell_signal)<-"sell_signal"

buy_signal<-((result_data[,1]<result_data[,2])&(result_data[,2]<result_data[,3]))
colnames(buy_signal)<-"buy_signal"

plot_data<-merge(testdata["2014-10-10/"],result_data,join="left")
plot_data<-result_data[,c(1,6,7,8)]
plot(plot_data["2014-10-10/",1],ylim=c(min(plot_data,na.rm=T),max(plot_data,na.rm=T)))
lines(plot_data["2014-01-01/",2],col="red")
lines(plot_data["2014-01-01/",3],col="blue")
lines(plot_data["2014-01-01/",4],col="yellow")

######载入数据#####
SZZS_tdx<-read.table('~/Downloads/stock_data/SH999999.txt',header=F,sep=",",skip = 2,fill=T)
SZZS_tdx<-SZZS_tdx[-nrow(SZZS_tdx),]

wka_tdx<-read.table('~/Downloads/SZ000002.txt',header=F,sep=",")

htzq_tdx<-read.table('~/Downloads/SH600837.txt',header=F,sep=",")
jfkj_tdx<-read.table('~/Downloads/SZ002202.txt',header=F,sep=",")
hyxd_tdx<-read.table('~/Downloads/SZ300027.txt',header=F,sep=",")

dlt_tdx<-read.table('~/Downloads/dlt_seq.txt',header=F,sep=" ")
######时间序列转换#####
SZZS_tdx<-xts(SZZS_tdx[,-1],order.by=as.Date(SZZS_tdx[,1],format="%Y-%m-%d",origin = "1970-01-01"))
######设置时间序列列名#####
colnames(SZZS_tdx)<-c("open","high","low","close","volume","total")

wka_tdx<-xts(wka_tdx[,-1],order.by=as.Date(wka_tdx[,1],format="%Y-%m-%d",origin = "1970-01-01"))
colnames(wka_tdx)<-c("open","high","low","close","volume","total")

jfkj_tdx<-xts(jfkj_tdx[,-1],order.by=as.Date(jfkj_tdx[,1],format="%Y-%m-%d",origin = "1970-01-01"))
colnames(jfkj_tdx)<-c("open","high","low","close","volume","total")

htzq_tdx<-xts(htzq_tdx[,-1],order.by=as.Date(htzq_tdx[,1],format="%Y-%m-%d",origin = "1970-01-01"))
colnames(htzq_tdx)<-c("open","high","low","close","volume","total")

hyxd_tdx<-xts(hyxd_tdx[,-1],order.by=as.Date(hyxd_tdx[,1],format="%Y-%m-%d",origin = "1970-01-01"))
colnames(hyxd_tdx)<-c("open","high","low","close","volume","total")

dlt_tdx<-xts(dlt_tdx[,-1],order.by=as.Date(dlt_tdx[,1],format="%Y-%m-%d",origin = "1970-01-01"))
colnames(dlt_tdx)<-c("id","c1","c2","c3","c4","c5","c6","c7","vol")

testdata<-SMA(Cl(SZZS_tdx), 5)
testdata<-testdata[!is.na(testdata)]

wka_sma5<-SMA(Cl(wka_tdx), 5)
wka_sma5<-wka_sma5[!is.na(wka_sma5)]

htzq_sma5<-SMA(Cl(htzq_tdx), 5)
htzq_sma5<-htzq_sma5[!is.na(htzq_sma5)]

jfkj_sma5<-SMA(Cl(jfkj_tdx), 5)
jfkj_sma5<-jfkj_sma5[!is.na(jfkj_sma5)]

hyxd_sma5<-SMA(Cl(hyxd_tdx), 5)
hyxd_sma5<-hyxd_sma5[!is.na(hyxd_sma5)]

library("parallel")
library("foreach")
library("doParallel")
cl<-makeCluster(detectCores())
registerDoParallel(cl)

clusterEvalQ(cl,library(xts))
clusterEvalQ(cl,library(quantstrat))
clusterEvalQ(cl,library(dtw))
clusterEvalQ(cl,library(plyr))
clusterExport(cl,"xts")
clusterExport(cl,"ac_indicator")
clusterExport(cl,"linestep")
clusterExport(cl,"list2matrix_stepbystep")
clusterExport(cl,"lsfit_x")
clusterExport(cl,"ac_model")
clusterExport(cl,"get_pred_data")
clusterExport(cl,"SZZS")
clusterExport(cl,"pre_date")
clusterExport(cl,"stockdata_dir")
system.time({
result_data<-ac_indicator(testdata["2014-10-10/"],testdata,10,3)
})
stopCluster(cl)
plot_data1<-merge(testdata["2014-10-10/"],result_data,join="left")

plot(plot_data1["2014-10-10/",1],ylim=c(min(plot_data1,na.rm=T),max(plot_data1,na.rm=T)))
lines(plot_data1["2014-10-10/",2],col="red")
lines(plot_data1["2014-10-10/",3],col="blue")
lines(plot_data1["2014-10-10/",4],col="yellow")

result_data<-ac_indicator(Cl(SZZS_tdx["2014-10-20/"]),Cl(SZZS_tdx),10,3)
plot_data1<-merge(Cl(SZZS_tdx["2014-10-30/"]),result_data,join="left")

result_data<-EMD_test_by_step(Cl(SZZS_tdx),  "2014-09-20/",10,3)
plot_data1<-merge(Cl(SZZS_tdx["2014-10-10/"]),result_data1,join="left")
result_data1<-cbind(result_data,cbind(as.numeric(apply(result_data[,c(1,4,7,10,13,16,19,22,25,28,31)],1,sum)),
                                      
                                      as.numeric(apply(result_data[,(c(1,4,7,10,13,16,19,22,25,28,31)+1)],1,sum)),
                                      
                                      as.numeric(apply(result_data[,(c(1,4,7,10,13,16,19,22,25,28,31)+2)],1,sum))))[,c(34,35,36)]


result_data<-ac_indicator(wka_sma5["2014-05-20/"],testdata,10,3)
plot_data1<-merge(wka_sma5["2014-05-20/"],result_data,join="left")

result_data<-ac_indicator(htzq_sma5["2014-09-20/"],testdata,10,3)
plot_data1<-merge(htzq_sma5["2014-10-10/"],result_data,join="left")

result_data<-ac_indicator(jfkj_sma5["2014-06-20/"],testdata,10,3)
plot_data1<-merge(jfkj_sma5["2014-06-20/"],result_data,join="left")


result_data<-ac_indicator(hyxd_sma5["2014-07-20/"],testdata,10,3)
plot_data1<-merge(hyxd_sma5["2014-07-20/"],result_data,join="left")


plot(plot_data1["2014-01-10/",1],ylim=c(min(plot_data1,na.rm=T),max(plot_data1,na.rm=T)))

date_add<-c(as.numeric(.indexDate(plot_data1["2014-01-01/",2]))[-1],as.numeric(.indexDate(plot_data1["2014-01-01/",2]))[length(as.numeric(.indexDate(plot_data1["2014-01-01/",2])))]+1)
ext_data<-cbind(date_add,as.numeric(plot_data1["2014-01-01/",2]))
ext_data<-xts(ext_data[,-1],order.by=as.Date(ext_data[,1],format="%Y-%m-%d",origin = "1970-01-01"))
lines(ext_data,col="red")

date_add<-c(as.numeric(.indexDate(plot_data1["2014-01-01/",2]))[c(-1,-2)],as.numeric(.indexDate(plot_data1["2014-01-01/",2]))[length(as.numeric(.indexDate(plot_data1["2014-01-01/",2])))]+c(1,2))
ext_data<-cbind(date_add,as.numeric(plot_data1["2014-01-01/",3]))
ext_data<-xts(ext_data[,-1],order.by=as.Date(ext_data[,1],format="%Y-%m-%d",origin = "1970-01-01"))
lines(ext_data,col="blue")

date_add<-c(as.numeric(.indexDate(plot_data1["2014-01-01/",2]))[c(-1,-2,-3)],as.numeric(.indexDate(plot_data1["2014-01-01/",2]))[length(as.numeric(.indexDate(plot_data1["2014-01-01/",2])))]+c(1,2,3))
ext_data<-cbind(date_add,as.numeric(plot_data1["2014-01-01/",4]))
ext_data<-xts(ext_data[,-1],order.by=as.Date(ext_data[,1],format="%Y-%m-%d",origin = "1970-01-01"))
lines(ext_data,col="yellow")



as.numeric(.indexDate(plot_data1["2014-01-01/",3])+2)
lines(plot_data1["2014-01-01/",3],col="blue")

as.numeric(.indexDate(plot_data1["2014-01-01/",4])+3)
lines(plot_data1["2014-01-01/",4],col="yellow")

#获取文件夹中文件名
list.files("~/Downloads/stock_data/")
#读取文件,跳过起始2行,忽略最后1行

SZZS<-read.table('~/Downloads/stock_data/SH999999.txt',header=F,sep=",",skip = 2,fill=T)
SZZS<-SZZS[-nrow(SZZS),]
SZZS<-xts(SZZS[,-1],order.by=as.Date(SZZS[,1],format="%Y-%m-%d",origin = "1970-01-01"))
colnames(SZZS)<-c("open","high","low","close","volume","total")
SZZS<-SMA(Cl(SZZS), 5)
SZZS<-SZZS[!is.na(SZZS)]

pre_date<-"2014-10-29/"
stockdata_dir<-"~/Downloads/stock_data/"
result_data<-NULL
for(stock_id in list.files(stockdata_dir))
{
  if(stock_id == "SH999999.txt")
  {
    next
  }
  print(stock_id)
  test111<-read.table(paste(stockdata_dir,stock_id,sep=""),header=F,sep=",",skip = 2,fill=T)
  test111<-test111[-nrow(test111),]
  test111<-xts(test111[,-1],order.by=as.Date(test111[,1],format="%Y-%m-%d",origin = "1970-01-01"))
  colnames(test111)<-c("open","high","low","close","volume","total")
  testdata<-SMA(Cl(test111), 5)
  testdata<-testdata[!is.na(testdata)]
  if( all(testdata[pre_date] != 0) & ( length(testdata[pre_date])>=10 ) )
  {
    result_data_tmp<-ac_indicator(testdata[pre_date],SZZS,10,3)
    
    if(is.null(result_data))
    {
      result_data<-c(unlist(strsplit(stock_id,".", fixed = TRUE))[1],result_data_tmp)
    }
    else
    {
      result_data<-rbind(result_data,c(unlist(strsplit(stock_id,".", fixed = TRUE))[1],result_data_tmp))
    }
  }
}

unlist(strsplit(stock_id,".", fixed = TRUE))[1]
SZZS_tdx<-xts(SZZS_tdx[,-1],order.by=as.Date(SZZS_tdx[,1],format="%Y-%m-%d",origin = "1970-01-01"))
######设置时间序列列名#####
colnames(SZZS_tdx)<-c("open","high","low","close","volume","total")
testdata<-SMA(Cl(SZZS_tdx), 5)
testdata<-testdata[!is.na(testdata)]
result_data<-ac_indicator(testdata["2014-10-20/"],testdata,10,3)
plot_data1<-merge(testdata["2014-10-30/"],result_data,join="left")

#数据流量预测
dataflow<-read.table('~/Downloads/data_flow.txt',header=F,sep=" ")
ncol(dataflow)

dataflow[,c(1,2)][1,2]*100+dataflow[-1,1]
cbind(dataflow[,c(1,3)][1,2]*100+dataflow[-1,1],dataflow[-1,3])

tmp2<-NULL

for(i in seq(2,ncol(dataflow)))
{
  tmp1<- cbind(dataflow[1,i]*100+dataflow[-1,1],dataflow[-1,i]) 
  
  if(is.null(tmp2))
  {
    tmp2<-tmp1
  }
  else
  {
    tmp2<-rbind(tmp2,tmp1)
  }
}

tmp2<-tmp2[!is.na(tmp2[,2]),]

tmp2_ts<-xts(tmp2[,-1],order.by=as.Date(as.character(tmp2[,1]),format="%Y%m%d",origin = "1970-01-01"))

result_data<-ac_indicator(tmp2_ts["2014-01-20/"],tmp2_ts,10,3)
plot_data1<-merge(tmp2_ts["2014-01-30/"],result_data,join="left")

result<-aaply(as.matrix(sample_data[seq((nrow(sample_data)-10),nrow(sample_data)),]),1,"ac_model",y=as.matrix(pred_data),.parallel=F)


add.indicator(strategy = q.strategy, name = "SMA", arguments = list(x = quote(Cl(mktdata)[,"close"]), 
                                                                    n = 5), label = "nFAST")

result_ts<-xts(result[,c(-1,-2)],order.by=as.Date(result[,2],format="%Y-%m-%d",origin = "1970-01-01"))



plot(testdata["2014-10-21/2014-11-04",1])
lines(result_ts["2014-01-01/",1],col="red")
lines(result_ts["2014-01-01/",2],col="blue")
lines(result_ts["2014-01-01/",3],col="yellow")

weekdays(as.Date(SZZS_tdx[,1],format="%Y-%m-%d",origin = "1970-01-01"),abbreviate = T)


##################################quanstrat指标测试#################################
currency("RMB")
q.strategy <- "qFaber"
try(rm("order_book.qFaber",pos=.strategy),silent=TRUE)
try(rm("account.qFaber","portfolio.qFaber",pos=.blotter),silent=TRUE)
stock("ZSYH", currency = "RMB", multiplier = 1)
q.strategy <- "qFaber"
initPortf(q.strategy, "SZZS_tdx", initDate = "2007-12-31")
initOrders(portfolio = q.strategy, initDate = "2007-12-31")
strategy(q.strategy, store = TRUE)
add.indicator(strategy = q.strategy, name = "ac_indicator", arguments = list(x = quote(SMA(Cl(mktdata)["2014-06-01/","close"],5)[!is.na(SMA(Cl(mktdata)["2014-06-01/","close"],5))]), 
                                                                             y = quote(SMA(Cl(mktdata)[,"close"],5)[!is.na(SMA(Cl(mktdata)[,"close"],5))]),
                                                                    i = 10, j=3), label = "ac_model")

out <- applyStrategy(strategy = q.strategy, portfolios = q.strategy)



========================================以下无效==========================

ac_data<-tail(as.numeric(Cl(SZZS_tdx)),10)

list_data<-as.numeric(Cl(SZZS_tdx))

list_len <-length(list_data)
k<-10
matrix_tmp<-matrix(nrow=list_len-k+1,ncol=k)
i<-10
matrix_tmp[i,]<-list_data[seq(i,(i+k-1)]

dfddfd<-(apply(list2matrix_stepbystep(as.numeric(Cl(SZZS_tdx)),10),1,"lsfit_x",y=ac_data))
lsfit_x(list2matrix_stepbystep(Cl(SZZS_tdx),10)[,-1],ac_data)
lsfit(list2matrix_stepbystep(as.numeric(Cl(SZZS_tdx)),10)[4000,],ac_data)
dfddfd<-t(dfddfd)
unlist(lapply(dfddfd, resid))


ddddd<-list2matrix_stepbystep(Cl(SZZS_tdx),10)



dfddfd<-apply(sdsssdsd,1,"lsfit_x",y=ac_data)

matrix(nrow=5,ncol=5)

number2date <-function(date_number)
{
  as.Date(date_number,origin="1970-01-01")
}

ac_test<-function(j=10,his_data,ac_data,pred_length=3)
{
  lsmode<-NULL
  moderes<-NULL
  k<-length(ac_data)
  
  for(i in 1:(length(his_data)-(k+3)))
  {
    tmpmodel<-lsfit(his_data[seq(i,i+k-1)], ac_data )
    sim_rato<- 1/(sum((his_data[seq(i,i+k-1)]*tmpmodel$coefficients["X"]+tmpmodel$coefficients["Intercept"]- his_data[seq(i,i+k-1)])^2))^(1/2)
    if(is.null(lsmode))
    {
      index_result<-c(i,as.numeric(his_data[seq(i,i+k+1)]*tmpmodel$coefficients["X"]+tmpmodel$coefficients["Intercept"]))
      
      lsmode<-c("i"=i,tmpmodel$coefficients,"sim_rato"=sim_rato)
    }
    else
    {
      lsmode<-rbind(lsmode,c("i"=i,tmpmodel$coefficients,"sim_rato"=sim_rato))
      index_result<-rbind(index_result, c(i,as.numeric(his_data[seq(i,i+k+1)]*tmpmodel$coefficients["X"]+tmpmodel$coefficients["Intercept"])))
    }
  }
  
  #print(head(lsmode[order(-lsmode[,"sim_rato"]),],j))
  #print(index_result[ head(lsmode[order(-lsmode[,"sim_rato"]),],j)[,"i"],c(k,k+1,k+2)])
  
  apply((index_result[ head(lsmode[order(-lsmode[,"sim_rato"]),],j)[,"i"],c(k,k+1,k+2)])*(  (head(lsmode[order(-lsmode[,"sim_rato"]),],j)[,"sim_rato"])/sum(head(lsmode[order(-lsmode[,"sim_rato"]),],j)[,"sim_rato"])),2,sum)
}

ac_test_bydtw<-function(j=10,his_data,ac_data,pred_length=3)
{
  lsmode<-NULL
  moderes<-NULL
  k<-length(ac_data)
  
  for(i in 1:(length(his_data)-(k+3)))
  {
    tmpmodel<-lsfit(his_data[seq(i,i+k-1)], ac_data )
    sim_rato<- 1/(dtw(as.numeric(his_data[seq(i,i+k-1)]),as.numeric(ac_data))$distance)
    if(is.null(lsmode))
    {
      index_result<-c(i,as.numeric(his_data[seq(i,i+k+1)]*tmpmodel$coefficients["X"]+tmpmodel$coefficients["Intercept"]))
      
      lsmode<-c("i"=i,tmpmodel$coefficients,"sim_rato"=sim_rato)
    }
    else
    {
      lsmode<-rbind(lsmode,c("i"=i,tmpmodel$coefficients,"sim_rato"=sim_rato))
      index_result<-rbind(index_result, c(i,as.numeric(his_data[seq(i,i+k+1)]*tmpmodel$coefficients["X"]+tmpmodel$coefficients["Intercept"])))
    }
  }
  
  #print(head(lsmode[order(-lsmode[,"sim_rato"]),],j))
  #print(index_result[ head(lsmode[order(-lsmode[,"sim_rato"]),],j)[,"i"],c(k,k+1,k+2)])
  
  apply((index_result[ head(lsmode[order(-lsmode[,"sim_rato"]),],j)[,"i"],c(k,k+1,k+2)])*(  (head(lsmode[order(-lsmode[,"sim_rato"]),],j)[,"sim_rato"])/sum(head(lsmode[order(-lsmode[,"sim_rato"]),],j)[,"sim_rato"])),2,sum)
}

ac_test_bydtw_cp<-cmpfun(ac_test_bydtw)

EMD_test_by_step<-function(emd_data,k,j)
{
  emd_result<-emd(emd_data)
  prd_result <- NULL
  
  for(i in 1:emd_result$nimf)
  {    
    if(is.null(prd_result))
    {
      prd_result <- ac_test_bydtw_cp(j,emd_result$imf[seq(1,length(emd_result$imf[,i])-k),i],tail(emd_result$imf[,i],k))
    }
    else
    {
      prd_result <- rbind(prd_result,ac_test_bydtw_cp(j,emd_result$imf[seq(1,length(emd_result$imf[,i])-k),i],tail(emd_result$imf[,i],k)))
    }
  }
  
  if(is.null(prd_result))
  {
    prd_result <- ac_test_bydtw_cp(j,
                          emd_result$residue[seq(1,length(emd_result$residue)-k)],
                          tail(emd_result$residue,k))
  }
  else
  {
    prd_result <- rbind(prd_result,
                        ac_test_bydtw_cp(j,emd_result$residue[seq(1,length(emd_result$residue)-k)],
                                tail(emd_result$residue,k)))
  }
  apply(prd_result,2,sum)
}

testdata<-SMA(Cl(SZZS_tdx), 5)
testdata<-testdata[!is.na(testdata)]
data_length <-length(testdata)
sample_tag <-as.integer(data_length*0.997)
pred_result_ts<-NULL

for(i in sample_tag:data_length)
{
  i_date<-number2date(as.numeric(.indexDate(testdata[i])))
  print(paste("begin:",i_date))
  EMD_res<-EMD_test_by_step(as.numeric(testdata[paste("/",i_date,sep="")]),8,5)
  if(is.null(pred_result_ts))
  {
    pred_result_ts<-data.frame("date"=i_date,"pred_day1"=EMD_res[1],"pred_day2"=EMD_res[2],"pred_day3"=EMD_res[3])
  }
  else
  {
    pred_result_ts<-rbind(pred_result_ts,data.frame("date"=i_date,"pred_day1"=EMD_res[1],"pred_day2"=EMD_res[2],"pred_day3"=EMD_res[3]))
  }
  print( data.frame("date"=i_date,"pred_day1"=EMD_res[1],"pred_day2"=EMD_res[2],"pred_day3"=EMD_res[3]))
}

rownames(pred_result_ts)<-pred_result_ts[,"date"]
pred_result_ts<-as.xts(pred_result_ts[,c(2,3,4)])
tmp_xts<-as.xts(data.frame(row.names=number2date(as.numeric(.indexDate(pred_result_ts))+1),as.numeric(pred_result_ts[,1])))
plot(tmp_xts["2014-01-01/",1])
#lines(Cl(SZZZ["2014-01-01/2014-09-30"]),col="green") 
lines(SMA(Cl(SZZS_tdx["2014-01-01/"]),5),col="red")

tmp_xts<-as.xts(data.frame(row.names=number2date(as.numeric(.indexDate(pred_result_ts))+2),as.numeric(pred_result_ts[,2])))
lines(tmp_xts["2014-01-01/",1],col="orange")

tmp_xts<-as.xts(data.frame(row.names=number2date(as.numeric(.indexDate(pred_result_ts))+3),as.numeric(pred_result_ts[,3])))
lines(tmp_xts["2014-01-01/",1],col="blue")

merge(dailyReturn(testdata["2014-09-01/2014-10-20"])*100,
dailyReturn(pred_result_ts["2014-09-01/2014-10-20",1])*100)

tmpdd<-c(1,2,3,4,5)

for (i in tmpdd)
{
  print(i)
}

for(i in sample_tag:(data_length))
{
  print(number2date(as.numeric(.indexDate(testdata[i]))))
}

Sys.setenv(TZ = "UTC+8")

HS300_idx<-read.table('~/Downloads/SZ399300.txt',header=F,sep="\t",skip = 2,fill=T)
HS300_idx<-HS300_idx[-nrow(HS300_idx),]
HS300_idx_ts<-xts(HS300_idx[,-1],order.by=as.Date(HS300_idx[,1],format="%m/%d/%Y",origin = "1970-01-01"))

HS300_fund<-read.table('~/Downloads/SZ159919.txt',header=F,sep="\t",skip = 2,fill=T)
HS300_fund<-HS300_fund[-nrow(HS300_fund),]
HS300_fund_ts<-xts(HS300_fund[,-1],order.by=as.Date(HS300_fund[,1],format="%m/%d/%Y",origin = "1970-01-01"))


SZZS_tdx<-read.table('~/Downloads/SZ399300.txt',header=F,sep=",")


rownames(SZZS_tdx)<-as.numeric(as.POSIXct(SZZS_tdx[,1],format="%Y-%m-%d",origin = "1970-01-01",tz=Sys.getenv("TZ")))

SZZS_tdx<-as.xts(SZZS_tdx[,-1],tzone=Sys.getenv("TZ"))

colnames(SZZS_tdx)<-c("open","high","low","close","volume","total")

tmpxts<-xts(SZZS_tdx,order.by=as.Date(SZZS_tdx[,1],format="%Y-%m-%d",origin = "1970-01-01",tz=Sys.getenv("TZ")))

as.Date(.indexDate(tail(tmpxts,10)),origin="1970-01-01")

