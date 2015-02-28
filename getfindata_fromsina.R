library(XML)

#资产负债表
#http://money.finance.sina.com.cn/corp/go.php/vFD_BalanceSheet/stockid/000002/ctrl/2014/displaytype/4.phtml

#公司利润表
#http://money.finance.sina.com.cn/corp/go.php/vFD_ProfitStatement/stockid/000002/ctrl/2014/displaytype/4.phtml

#现金流量表
#http://money.finance.sina.com.cn/corp/go.php/vFD_CashFlow/stockid/000002/ctrl/2014/displaytype/4.phtml

get.finance=function(id,year){
  web=paste("http://money.finance.sina.com.cn/corp/go.php/vFD_FinancialGuideLine/stockid/",id,"/ctrl/",year,"/displaytype/4.phtml",sep="")
  print(web)
  tables <- readHTMLTable(web)[[20]]
  #rownames(tables)<-iconv(tables[,1],"UTF-8","gbk")
  rownames(tables)<-tables[,1]
  return(tables[,-1])
}

get.finance("000002","2012")

#融资融券"
test<-readHTMLTable("http://vip.stock.finance.sina.com.cn/q/go.php/vInvestConsult/kind/rzrq/index.phtml")[[2]]
test<-test[c(-1,-2),-1]
colnames(test)<-c("股票代码","股票名称","融资余额(元)","融资买入额(元)","融资偿还额(元)","融资余量金额(元)","融券余量(股)","融券卖出量(股)","融券偿还量(股)","融券余额(元)")
rownames(test)<-NULL


test<-readHTMLTable("http://data.eastmoney.com/rzrq/detail/000001.html")