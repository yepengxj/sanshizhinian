library(plyr)
library(EMD)
library(XML)
library(dtw)



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
  -sqrt(sum((testingTarget-pred)^2)/length(testingTarget))
  
}

cntrade <- function(tickers, path = "", start = 19910101, end = "") {
  
  address <- "http://quotes.money.163.com/service/chddata.html"
  field <- "&fields=TCLOSE;HIGH;LOW;TOPEN;LCLOSE;CHG;PCHG;TURNOVER;VOTURNOVER;VATURNOVER;TCAP;MCAP"
  
  if (path == "") {
    path <- getwd()
  }
  
  if (!file.exists(path)) {
    dir.create(path)
  }
  
  if (substr(path, nchar(path), nchar(path)) != "/") {
    path <- paste(path, "/", sep = "")
  }
  
  if (end == "") {
    year <- substr(Sys.time(), 1, 4)
    month <- substr(Sys.time(), 6, 7)
    day <- substr(Sys.time(), 9, 10)
    end <- paste(year, month, day, sep = "")
  }
  
  count <- 0
  tickers <- as.character(tickers)
  for (name in tickers) {
    while (nchar(name) < 6) {
      name <- paste("0", name, sep = "")
    }
    
    if (nchar(name) > 6) {
      url <- paste(address, "?code=", name, "&start=", start, "&end=", end, field, sep = "")
      
    }
    else
    {
      if (as.numeric(name) >= 600000) {
        url <- paste(address, "?code=0", name, "&start=", start, "&end=", end, field, sep = "")
      } else {
        url <- paste(address, "?code=1", name, "&start=", start, "&end=", end, field, sep = "")
      }
    }
    
    destfile <- paste(path, name, ".csv", sep = "")
    data<- htmlParse(url,encoding="GBK")
    
    data<-getNodeSet(data,"//body/p")
    data<-xmlValue(data[[1]])
    write(data,file=destfile)
    
    count <- count + 1
  }
  
  if (count == 0) {
    cat("一个数据文件都没下载下来！\n")
  } else {
    cat("数据下载完成！\n")
    cat(paste("共下载", count, "个文件\n", sep = ""))
  }
}

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

ac_indicator<-function(x,y,i,j)
{
  #dim(x)
  #dim(y)
  #将预测数据序列按预测滑动窗口长度i转换为矩阵
  pred_data<-t(list2matrix_stepbystep(x,i))
  
  
  #将样本数据按i+j长度转换为矩阵
  sample_data<-t(list2matrix_stepbystep(y,(i+j)))
  
  #result<-t(apply(pred_data,1,"ac_model",y=sample_data,i=5))
  result<-adply(pred_data,1,"ac_model",y=sample_data,i=5, .parallel = F)
  
  colnames(result)<-c(colnames(result)[1:3],sapply(c(1:j),function(x){paste("pred_result",x,sep="")}))
  #result_ts<-xts(result[,c(-1)],order.by=as.Date(result[,3],format="%Y-%m-%d",origin = "1970-01-01"))
  
  
  #colnames(result_ts)<-sapply(c(1:j),function(x){paste("pred_result",x,sep="")})
  result[,-1]
  #result_ts
}


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

