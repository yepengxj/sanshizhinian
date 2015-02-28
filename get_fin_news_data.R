library(XML)
library(plyr)
library(stringr)

#解析东方财务网新闻网页，提取股票代码

saveXML2file<-function(x,filepath){
  write(saveXML(x),filepath,append=T,sep="")
}

saveNews2file<-function(address)
{
  print(address)
 # address <- "http://stock.eastmoney.com/news/1412,20150202474202784.html"
  temp<-strsplit(as.character(address),split="/",fixed=T)[[1]]
  filename<-temp[length(temp)]
  data_filepath<-paste("~/temp/data",filename,sep="")
  stock_list_filepath<-paste("~/temp/stocklist",filename,sep="")
  urldata<- htmlParse(address,encoding="GBK")
  
  stock_list_xpath<-"//div[@class='mainFrame']//div[@class='main_left']//div[@class='titlebox']//div[@class='newsContent']//div[@class='newText new']//div[@id='ContentBody']//p//span//a[@class='keytip']"
  
  content_xpath<-"//div[@class='mainFrame']//div[@class='main_left']//div[@class='titlebox']//div[@class='newsContent']//div[@class='newText new']//div[@class='Body']//p"
  
  content_data<-getNodeSet(urldata, content_xpath)
  ldply(content_data,saveXML2file,filepath=data_filepath)
  
  stock_list_data<-getNodeSet(urldata, stock_list_xpath)
  ldply(stock_list_data,saveXML2file,filepath=stock_list_filepath)
  
}
ldply(news_data$data_href_list,saveNews2file)

getNodeSet(xmlParse(saveXML(temp[[11]])),"//p//span//a[@class='keytip']")
xmlGetAttr(data[[1]],'href')

#获取东方财务网新闻列表
#板块聚焦 
address <- "http://stock.eastmoney.com/news/cbkjj.html"
urldata<- htmlParse(address,encoding="GBK")
xpath<-"//div[@class='mainCont']//div[@class='listBox']//div[@class='list']//ul//li"
data<-getNodeSet(urldata, xpath)
xmlValue(data)


#获取ｘｍｌ节点内容并转换成数字


#宏观经济
address <- "http://finance.eastmoney.com/news/chgjj.html"
#财经导读http://finance.eastmoney.com/news/ccjdd.html
#上市公司http://finance.eastmoney.com/news/cssgs.html
#要闻精华http://finance.eastmoney.com/news/cywjh.html
#国内经济http://finance.eastmoney.com/news/cgnjj.html
#宏观经济http://finance.eastmoney.com/news/chgjj.html
#国际经济http://finance.eastmoney.com/news/cgjjj.html
#金融资本http://finance.eastmoney.com/news/cjrzb.html
#证券要闻http://finance.eastmoney.com/news/czqyw.html
#产业经济http://finance.eastmoney.com/news/ccyjj.html
#产经新闻http://finance.eastmoney.com/news/ccjxw.html

news_data<-ldply(c("chgjj","ccjdd","cssgs","cywjh","cgnjj","chgjj","cgjjj","cjrzb","czqyw","ccyjj","ccjxw"),get_eastmoney_news_data)
View(news_data)
news_data<-get_eastmoney_news_data("chgjj")

news_channel<-"chgjj"

get_eastmoney_news_data <- function(news_channel)
{
  
  base_url<-"http://finance.eastmoney.com/news/"
  
  address<-paste(base_url,news_channel,".html",sep="")
  urldata<- htmlParse(address,encoding="GBK")
  print(address)
  #获取信息分页参数
  pages_xpath<-"//div[@class='mainCont']//div[@class='listBox']//div[@class='list']//div[@class='PageBox']//div[@class='Page']//a"
  
  data_pages<-getNodeSet(urldata, pages_xpath)
  
  max_pages<-max(ldply(data_pages,xmlValue2num),na.rm=T)
  
  news_data<-get_eastmoney_news_pagedata(urldata)
   
  for(i in 2:max_pages)
  {
    address<-paste(base_url,news_channel,"_",i,".html",sep="")
    urldata<- htmlParse(address,encoding="GBK")
    print(address)
    news_data<-rbind(news_data,get_eastmoney_news_pagedata(urldata))
  }
  
  news_data
}

xmlValue2num<-function(x)
{
  as.numeric(xmlValue(x))
}

get_eastmoney_news_pagedata<-function(urldata)
{  
  list_time_xpath<-"//div[@class='mainCont']//div[@class='listBox']//div[@class='list']//ul//li//span"
  data_time_list<-getNodeSet(urldata, list_time_xpath)
  data_time_list<-ldply(data_time_list,xmlValue)
  
  
  list_href_xpath<-"//div[@class='mainCont']//div[@class='listBox']//div[@class='list']//ul//li//a/@href"
  data_href_list<-getNodeSet(urldata, list_href_xpath)
  data_href_list<-unlist(data_href_list,recursive=F,use.names=F)
  
  list_title_xpath<-"//div[@class='mainCont']//div[@class='listBox']//div[@class='list']//ul//li//a/@title"
  data_title_list<-getNodeSet(urldata, list_title_xpath)
  data_title_list<-unlist(data_title_list,recursive=F,use.names=F)
  
  data.frame(data_time_list,data_href_list,data_title_list)
}



#和讯新闻
#pages
"//div[@class='hx_paging']//ul//li//a"
#list_time
"//div[@class='temp01']//ul//li//span//div"
#list_data
"//div[@class='temp01']//ul//li//span//a"
#list_href
"//div[@class='temp01']//ul//li//span//a/@href"
#公司要闻http://stock.hexun.com/gsxw/
#个股推荐http://stock.hexun.com/real/
#公司评级http://stock.hexun.com/research/
#个股数据http://stock.hexun.com/data/
#基金重仓股http://stock.hexun.com/zhongcang/
#主力研究http://stock.hexun.com/zlyj/
#主力数据http://stock.hexun.com/zlsj/
#焦点透视http://stock.hexun.com/focus/
#证券要闻http://stock.hexun.com/news/


#策略研究http://yanbao.stock.hexun.com/listnews.aspx?type=5
#宏观研究http://yanbao.stock.hexun.com/listnews.aspx?type=4
#行业研究http://yanbao.stock.hexun.com/listnews.aspx?type=2
#公司研究http://yanbao.stock.hexun.com/listnews.aspx?type=1
#券商晨会http://yanbao.stock.hexun.com/listnews.aspx?type=3

#腾讯股票
#pages
"//div[@class='hx_paging']//ul//li//a"
#list_time
"//div[@class='jrtjMod']//div[@class='listZone']//div[@class='nrC']//span"
#list_data
"//div[@class='jrtjMod']//div[@class='listZone']//div[@class='nrC']//fl//a"
#list_href
"//div[@class='jrtjMod']//div[@class='listZone']//div[@class='nrC']//fl//a/@href"

#板块http://finance.qq.com/stock/bkzx.htm
#大盘http://finance.qq.com/stock/scfx.htm
#大盘http://finance.qq.com/stock/zhibo.htm

#pages
"//div[@class='main']//div[@class='pageNav']//a"
#list_time
"//div[@class='mod newslist']//ul/li//span"
#list_data
"//div[@class='mod newslist']//ul/li//a"
#list_href
"//div[@class='mod newslist']//ul/li//a/@href"
#上市公司http://finance.qq.com/stock/scfx.htm
