library(XML)
library(plyr)
library(stringr)

#获取股票代码列表
path = "~/temp"

address <- "http://quote.eastmoney.com/stocklist.html"

urldata= htmlParse(address,encoding="GBK")
xpath<-"//div[@id='quotesearch']//ul//li//a"
data<-getNodeSet(urldata, xpath)
data1<-sub("\\(","\\ ",ldply(data,xmlValue)[,1])
data1<-sub(")","",data1)
#View(t(as.data.frame(x<-strsplit(data1," ",fixed=TRUE))))
all_stock_list<-t(as.data.frame(strsplit(data1," ",fixed=TRUE)))
row.names(all_stock_list)<-NULL
sz_fund_list<-all_stock_list[substr(all_stock_list[,2],1,2)=="15",2]
sh_fund_list<-all_stock_list[substr(all_stock_list[,2],1,2)=="51",2]

sz_fund_list_str<-str_c(sz_fund_list,collapse = "','sz")
sz_fund_list_str<-paste("'sz",sz_fund_list_str,"'",sep="")
write(paste("sz_fund_list_str<-c(",sz_fund_list_str,")",seq=""),"~/temp/fund_list_str.R",sep="")

sh_fund_list_str<-str_c(sh_fund_list,collapse = "','sh")
sh_fund_list_str<-paste("'sh",sh_fund_list_str,"'",sep="")
write(paste("sh_fund_list_str<-c(",sh_fund_list_str,")",seq=""),"~/temp/fund_list_str.R",sep="",append=TRUE)
