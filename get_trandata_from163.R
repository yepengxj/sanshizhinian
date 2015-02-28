library(XML)
library(plyr)

tickers<-"0000300"
path<-"~/temp"
start<-19990101
end<-20150226
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

cntrade(c('0000300'), path = "~/temp", start = 19900101, end = 20150226)

xxx<-read.csv("~/temp/0000300.csv",header=F,skip=1,encoding = "UTF-8")

xxx<-xts(xxx[,c(4,5,6,7,12)],order.by=as.Date(xxx[,1],"%Y-%m-%d"))

tail(xxx,10)

colnames(xxx)<-c("close","high","low","open","vol")
xxx<-xxx[!xxx$close==0]

iconv(readLines("http://hq.sinajs.cn/list=sh000300"),"GB2312","UTF-8")
cur_data_str<-strsplit(iconv(readLines("http://hq.sinajs.cn/list=sh000300")[[1]],"GB2312","UTF-8"),",",fixed=T)[[1]]

cur_data<-cur_data_str[c(31,4,5,6,2,9)]
cur_data_xts<-xts(matrix(as.numeric(cur_data[c(2:6)]),nrow=1),order.by=as.Date(cur_data[1],"%Y-%m-%d"))
xxx<-rbind(xxx,cur_data_xts)
tail(xxx,10)

cur_xxx<-xxx["/2015-02-25",]
cur_xxx$close_eemd_smooth<-eemd_smooth(as.numeric(cur_xxx["/2015-02-25","close"]))
xxx$close_eemd_smooth<-eemd_smooth(as.numeric(xxx[,"close"]))
xxx$close_ema5<-EMA(xxx[,"close"],5)
xxx$close_sma5<-SMA(xxx[,"close"],5)



plot(xxx["2015-02-04/","close"])
lines(cur_xxx["2015-02-04/","close_eemd_smooth"])
lines(xxx["2015-02-04/","close_eemd_smooth"])
lines(xxx["2015-02-04/","close_ema5"])
lines(xxx["2015-02-04/","close_sma5"])


result_data_hs300_eemd_smooth<-aaply(as.numeric(.indexDate(xxx["2015-02-04/2015-02-06","close_sma5"])), 1, 
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
                                       
                                     },xxx["2006-01-01/","close_sma5"],10,3)



xxx$nFAST <- SMA(Cl(xxx), 3)
xxx$nSLOW <- SMA(Cl(xxx), 20)
ZSYH<-xxx

