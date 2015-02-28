library(XML)
library(plyr)
library(stringr)

source("~/temp/fund_list_str.R")
sh_fund_num<-length(sh_fund_list_str)
sz_fund_num<-length(sz_fund_list_str)

get_tran_data_part<-function(code_list){
  base_url<-"http://hq.sinajs.cn/list="
  base_url<-paste(base_url,code_list,sep="")
  print(base_url)
  
  write.table(iconv(readLines(base_url),"GB2312","UTF-8")
              ,file = "~/data.txt",col.names = F, row.names = F, quote = F,append=T)
  
}

get_tran_data<-function(code_list){
  length<-length(code_list)
  i<-1
  step<-10
  while(i<=length)
  {
    if(i+step>length)
    {
      code_list_str<-str_c(code_list[seq(i:length)],collapse = ",")
    }
    else
    {
      code_list_str<-str_c(code_list[seq(i:(i+step))],collapse = ",")
    }
    print(code_list_str)
    i<-i+step
    print(i)
    get_tran_data_part(code_list_str)
  }
}

get_tran_data(sh_fund_list_str)
get_tran_data(sz_fund_list_str)
