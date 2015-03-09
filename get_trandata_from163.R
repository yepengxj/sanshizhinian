source("./ems_smooth.R")


library(XML)
library(plyr)

tickers<-"0000300"
path<-"~/temp"
start<-19990101
end<-as.numeric(format(Sys.Date() , "%Y%m%d"))
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

cntrade(c('0000300'), path = "~/temp", start = 19900101, end = end)

xxx<-read.csv("~/temp/0000300.csv",header=F,skip=1,encoding = "UTF-8")

xxx<-xts(xxx[,c(4,5,6,7,12)],order.by=as.Date(xxx[,1],"%Y-%m-%d"))


colnames(xxx)<-c("close","high","low","open","vol")
xxx<-xxx[!xxx$close==0]
plot(xxx["2006-01-01/2008-01-01"])
tail(xxx,10)
eemd_list<-list(
  list(func_name="Rlibeemd_eemd_func",func=Rlibeemd_eemd_func)
  )

acf_list<-list(
  list(func_name="get_acf",func=get_acf),
  list(func_name="get_acf1",func=get_acf1)
  )

get_k_list<-list(
  list(func_name="get_k",func=get_k),
  list(func_name="get_k1",func=get_k1)
  )  

noise_t_list<-list(
  list(func_name="noise_t",func=noise_t),
  list(func_name="noise_t1",func=noise_t1)
  )  
  
smooth_list<-list(
  list(func_name="smooth",func=smooth),
  list(func_name="smooth1",func=smooth1))

datarange_list<-c("2006-01-01/2008-01-01","2008-01-01/2009-01-01","2006-01-01/2009-01-01","2009-01-01/2014-07-01","2014-07-01/2015-02-11")

for(eemd_l in (eemd_list)){
  for(datarange_l in datarange_list)
  {

    pred_result<-aaply(as.numeric(.indexDate(curr_xxx[datarange_l,"close"])), 1, 
                       function(x,ts,i,j){
                         start_date_id<-which(.indexDate(ts)==x)
                         end_date<-.indexDate(ts)[(start_date_id+i-1)]
                         print(end_date)
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
                           curr_ts<-ts[end_str]
                           print(dim(curr_ts))
                           data_eemd<-eemd_l$func(as.numeric(curr_ts[,"close"]))
                           
                           print(data_eemd$nimf)
                           eemd_pcv<-principal(data_eemd$imf,nfactors=(data_eemd$nimf),rotate="none")
                           t<-max(which(eemd_pcv$values>0.95))
                           print(t)
                           dim(data_eemd$imf)
                           eemd_pcv_denoise<-data_eemd$imf%*%eemd_pcv$loadings[,(1:t)]
                           dim(eemd_pcv_denoise)
                           curr_ts$smooth_close<-rowSums(eemd_pcv_denoise)+data_eemd$residue
                           
                           
                           ac_indicator(curr_ts[start_end_str,"smooth_close"],curr_ts[start_str,"smooth_close"],i,j)
                         }
                         
                       },curr_xxx["2000-01-01/","close"],10,3)
    
    
    file_name<-paste(eemd_l$func_name,acf_l$func_name,datarange_l,sep="_")

    file_path<-paste("~/temp/","pred_res",file_name,sep="")
    
    write.csv(pred_result[,-1], file = file_path,row.names=F)
  }
}


for(eemd_l in (eemd_list))
{
  for(acf_l in (acf_list))
  {
    for (get_k_l in (get_k_list))
    {
      for(noise_t_l in (noise_t_list))
      {
        for(smooth_l in (smooth_list))
        {   
          for(datarange_l in datarange_list)
          {
            print(eemd_l$func_name)
            print(acf_l$func_name)
            print(get_k_l$func_name)
            print(noise_t_l$func_name)
            print(smooth_l$func_name)
            curr_xxx<-xxx
            
            pred_result<-aaply(as.numeric(.indexDate(curr_xxx[datarange_l,"close"])), 1, 
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
                                   #print(start_end_str)
                                   #print(start_str)
                                   curr_ts<-ts
                                   curr_ts$smooth_close<-emd_smooth(as.numeric(curr_ts[,"close"]),eemd_l$func,acf_l$func,get_k_l$func,noise_t_l$func,smooth_l$func)
                                   ac_indicator(ts[start_end_str],ts[start_str],i,j)
                                 }
                                 
                               },curr_xxx["2000-01-01/","close"],10,3)
            
            
            file_name<-paste(eemd_l$func_name,acf_l$func_name,get_k_l$func_name,noise_t_l$func_name,smooth_l$func_name,sep="_")
            file_path<-paste("~/temp/","smooth_res",file_name,sep="")
            write.csv(curr_xxx$smooth_close, file = file_path,row.names=F)
            
            file_path<-paste("~/temp/","pred_res",file_name,sep="")
            
            write.csv(pred_result[,-1], file = file_path,row.names=F)
          }
        }
      }
    }
  }
}

