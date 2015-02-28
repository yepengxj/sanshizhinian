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
  dfddfd<-aaply(y,1,"lsfit_x",as.numeric(x)[3:length(x)],.parallel = F)
  # dfddfd<-t(apply(y,1,"lsfit_x",as.numeric(x)[3:length(x)]))
  t<-ncol(y)-length(x)
  # write.table(dfddfd,'~/Downloads/log.txt',sep=",")
  dfddfd<-dfddfd[dfddfd[,3]!=0,]
  c("start_date"=x[1],"end_date"=x[2],apply((head(dfddfd[order(-dfddfd[,3]),],i)[,3])/sum(head(dfddfd[order(-dfddfd[,3]),],i)[,3])*(head(dfddfd[order(-dfddfd[,3]),],i)[,(ncol(dfddfd)-t+1):ncol(dfddfd)]),2,"sum"))
  print( c("start_date"=x[1],"end_date"=x[2],apply((head(dfddfd[order(-dfddfd[,3]),],i)[,3])/sum(head(dfddfd[order(-dfddfd[,3]),],i)[,3])*(head(dfddfd[order(-dfddfd[,3]),],i)[,(ncol(dfddfd)-t+1):ncol(dfddfd)]),2,"sum"))
  )
  
}


######相似体拟合模型#####
##输入
##x:预测时间序列
##y:样本时间序列
##i:预测时间滑动窗口
##j:预测长度
ac_indicator<-function(x,y,i,j)
{
  #将预测数据序列按预测滑动窗口长度i转换为矩阵
  pred_data<-t(list2matrix_stepbystep(x,i))
  
  #将样本数据按i+j长度转换为矩阵
  sample_data<-t(list2matrix_stepbystep(y,(i+j)))
  
  #result<-t(apply(pred_data,1,"ac_model",y=sample_data,i=5))
  result<-aaply(pred_data,1,"ac_model",y=sample_data,i=5,.parallel = F)
  #result_ts<-xts(result[,c(-1,-2)],order.by=as.Date(result[,2],format="%Y-%m-%d",origin = "1970-01-01"))
  
  #colnames(result_ts)<-sapply(c(1:ncol(result_ts)),function(x){paste("pred_result",x,sep="")})
  result
  #result_ts
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


SZZS<-read.table('~/Downloads/stock_data/SH999999.txt',header=F,sep=",",skip = 2,fill=T)
SZZS<-SZZS[-nrow(SZZS),]
SZZS<-xts(SZZS[,-1],order.by=as.Date(SZZS[,1],format="%Y-%m-%d",origin = "1970-01-01"))
colnames(SZZS)<-c("open","high","low","close","volume","total")
SZZS<-SMA(Cl(SZZS), 5)
SZZS<-SZZS[!is.na(SZZS)]

pre_date<-"2014-10-31/"
stockdata_dir<-"~/Downloads/stock_data/"
result_data<-NULL
get_pred_data<-function(stock_id,stockdata_dir,pre_date,SZZS)
{
  print(stock_id)
  test111<-read.table(paste(stockdata_dir,stock_id,sep=""),header=F,sep=",",skip = 2,fill=T)
  if(nrow(test111)>20)
  {
    test111<-test111[-nrow(test111),]
    test111<-xts(test111[,-1],order.by=as.Date(test111[,1],format="%Y-%m-%d",origin = "1970-01-01"))
    colnames(test111)<-c("open","high","low","close","volume","total")
    testdata<-SMA(Cl(test111), 5)
    testdata<-testdata[!is.na(testdata)]
    if( all(testdata[pre_date] != 0) & ( length(testdata[pre_date])>=10 ) )
    {
      result_data_tmp<-ac_indicator(testdata[pre_date],SZZS,10,3)
      result_data<-c(unlist(strsplit(stock_id,".", fixed = TRUE))[1],result_data_tmp)
      write.table(t(result_data),"~/sma5_pred_res.txt",sep=",",append=T,row.names=F,col.names=F)
      t(result_data)
      #if(is.null(result_data))
      #{
      #  write.table(t(result_data),"~/sma5_pred_res.txt",sep=",",col.names=F,row.names=F)
        
      #}
      #else
      #{
      #  result_data<-c(unlist(strsplit(stock_id,".", fixed = TRUE))[1],result_data_tmp)
      #  write.table(as.matrix(result_data,row=1),"~/sma5_pred_res.txt",sep=",",append=T)    
      #}
    }
  }
}

library("parallel")
library("foreach")
library("doParallel")
cl <- makeCluster(mc <- getOption("cl.cores", 4))
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
system.time({testdata112121<-laply(list.files(stockdata_dir),"get_pred_data",stockdata_dir,pre_date,SZZS,.parallel = F)
})

