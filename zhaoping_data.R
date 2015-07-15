library(XML)
library("plyr")

base_data<-htmlParse("http://sou.zhaopin.com/jobs/searchresult.ashx?jl=北京&kw=大数据&p=1")
jl<-c('北京','上海','广州','深圳')
#kw<-c('大数据','hadoop')
kw<-c('java')
result_hadoop<-result
result<-t(as.matrix(c('zwyx','zwmc'),nrow=1))
for(jl_c in jl)
{
  for(kw_c in kw)
  {
    data_url<-sprintf("http://sou.zhaopin.com/jobs/searchresult.ashx?jl=%s&kw=%s&p=%d",jl_c,kw_c,1)
    
    base_data<-htmlParse(data_url)
    data<-getNodeSet(base_data,"//span[@class='search_yx_tj']/em")
    totoal_num<-as.integer(xmlValue(data[[1]]))
    for(i in (1:as.integer(totoal_num/40)))
    {
      data_url<-sprintf("http://sou.zhaopin.com/jobs/searchresult.ashx?jl=%s&kw=%s&p=%d",jl_c,kw_c,i)
      print(data_url)
      table<-htmlParse(data_url)
      zwyx<-getNodeSet(table,"//table[@class='newlist']//td[@class='zwyx']")
      zwyx<-laply(zwyx,xmlValue)
      zwmc<-getNodeSet(table,"//table[@class='newlist']//td[@class='zwmc']//a")
      zwmc<-laply(zwmc,xmlGetAttr,"href")
      temp<-cbind(zwyx,zwmc)
      result<-rbind(result,temp)
    }
  }
}

table<-getNodeSet(base_data,"//table[@class='newlist']//td[@class='zwyx']")
laply(table,xmlValue)

write.csv(result_hadoop,'~/result_hadoop.csv')
write.csv(result,'~/result_java.csv')

readHTMLTable(table[[1]])
