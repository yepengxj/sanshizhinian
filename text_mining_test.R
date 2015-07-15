library(xlsx)
presentations <- read.xlsx("/home/datagrid/text_mining_test.xls", sheetName="Sheet1") #读excel
summary(presentations)
presentations$Title <- as.character(presentations[,1]) #转文本
Encoding(presentations$Title) <- "UTF-8" #转换编码
presentations$Title
presentations$Abstract <- as.character(presentations[,2])
Encoding(presentations$Abstract) <- "UTF-8"
presentations$Abstract


library("rmmseg4j")
presentations$seg <- mmseg4j(presentations$Abstract) #分词

library("tm")
presebtation_seg <- Corpus(DataframeSource(presentations[,c("Title","seg")])) #转换到tm专用格式
presebtation_term <- TermDocumentMatrix(presebtation_seg, control = list(stopwords = TRUE)) #生成词频矩

presebtation_term <- t(as.matrix(presebtation_term)) #转换为matrix并转置
summary(presebtation_term)
presebtation_kmeans <- kmeans(presebtation_term, 7) #kmeans聚为7类

summary(presebtation_kmeans)

library("plyr")
#高频词统计
presentations$seg2 <- unique((strsplit(presentations$seg,split=" "))) #断词
#all_key_words <- iconv(unlist(presentations$seg2), from="UTF-8", to="GBK") #转换到GBK编码
all_key_words <- unlist(presentations$seg2)

all_key_words_fre <- as.data.frame(table(all_key_words)) #统计词频
names(all_key_words_fre)
all_key_words_fre <- arrange(all_key_words_fre,desc(Freq)) #按词频排序
all_key_words_fre[1:20,] #100个高频词


library(XML)
library(RCurl)
library(Rwordseg)
library("plyr")
installDict('/home/datagrid/backdata/sogou_dict/stock.scel',"stock")
installDict('/home/datagrid/backdata/sogou_dict/wusuzhaoshi.scel',"jinyong")
installDict('/home/datagrid/backdata/sogou_dict/reci.txt',"reci")

tt<-curl("http://finance.eastmoney.com/yaowen_cgnjj.html",open="r")
tt<-readLines("http://finance.eastmoney.com/yaowen_cgnjj.html")
base_data<-htmlParse(tt)
title<-getNodeSet(base_data,"//body/div[@class='page']/div[@class='main']/div[@class='gl']/div[@class='artitleList']/ul/li/div/p[@class='title']/a")
title<-getNodeSet(base_data,"//body/div[@class='page']/div[@class='main']/div[@class='gl']/div[@class='artitleList']/ul/li//p[@class='title']/a")

desc<-getNodeSet(base_data,"//body/div[@class='page']/div[@class='main']/div[@class='gl']/div[@class='artitleList']/ul/li//p[@class='info']")

ldply(title,xmlValue)
laply(title,xmlGetAttr,"href")
ldply(desc,xmlValue)
xmlGetAttr
xmlValue(title)
title<-laply(title,xmlValue)
desc<-laply(desc,xmlValue)
segmentCN(title)
desc_seg<-segmentCN(as.character(t[,3]))

desc_seg_split <- unique(desc_seg) #断词
#all_key_words <- iconv(unlist(presentations$seg2), from="UTF-8", to="GBK") #转换到GBK编码
all_key_words <- unlist(desc_seg)
#all_key_words <- unique(all_key_words)

all_key_words_fre <- as.data.frame(table(all_key_words)) #统计词频
names(all_key_words_fre)
all_key_words_fre <- arrange(all_key_words_fre,desc(Freq)) #按词频排序
all_key_words_fre[1:20,] #100个高频词

write.csv(all_key_words_fre,"~/test.csv")

myHttpheader<- c(
  "User-Agent"="Mozilla/5.0(Windows;U;Windows NT 5.1;zh-CN;rv:1.9.1.6",
  "Accept"="text/htmal,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language"="en-us",
  "Connection"="keep-alive",
  "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7"
)

myHttpheader<- c(
  'Host'= 'finance.eastmoney.com',
  'Connection'= 'keep-alive',
  'Cache-Control'= 'max-age=0',
  'Accept'= 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8',
  'User-Agent'= 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.81 Safari/537.36'
)

base_data<- getURL("http://finance.eastmoney.com/yaowen_cgnjj.html")
base_data<- getURL("http://finance.eastmoney.com/yaowen_cgnjj_2.html")
base_data<-readLines("http://finance.eastmoney.com/yaowen_cgnjj_3.html")
base_data<-sub('–','',base_data,fixed=T)
iconv(base_data,to="gb2312")
text_temp<-readLines(base_data)
temp_html<-htmlParse(base_data,asText = TRUE)

base_data1<-gsub('gb2312','',base_data)
temp_html<-htmlParse(base_data1,asText = TRUE)
temp_html<-htmlParse("http://finance.eastmoney.com/yaowen_cgnjj.html",encoding = 'gb2312')
temp_html<-htmlParse("http://finance.eastmoney.com/yaowen_cgnjj_2.html",encoding = 'gb2312')


title<-getNodeSet(temp_html,"//body/div[@class='page']/div[@class='main']/div[@class='gl']/div[@class='artitleList']/ul/li/div/p[@class='title']/a")
desc<-getNodeSet(temp_html,"//body/div[@class='page']/div[@class='main']/div[@class='gl']/div[@class='artitleList']/ul/li//p[@class='info']")
time<-getNodeSet(temp_html,"//body/div[@class='page']/div[@class='main']/div[@class='gl']/div[@class='artitleList']/ul/li//p[@class='time']")
pagenav<-getNodeSet(temp_html,"//body/div[@class='page']/div[@class='main']/div[@class='gl']/div[@class='artitleList']/div[@class='moreTen']//a")
desc<-laply(desc,xmlValue)
time<-laply(time,xmlValue)
iconv(desc,from='UTF-8',to="GB2312")
strptime(time,format = "%m月%e日 %H:%M")
pagenav_val<-laply(pagenav,xmlValue)
max(as.numeric(pagenav_val),na.rm=T)

t<-read.csv("~/thefile.txt",sep="\t")
