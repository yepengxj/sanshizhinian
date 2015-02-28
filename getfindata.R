library(quantmod)

tt<-readHTMLTable("http://stockdata.stock.hexun.com/2008/zxcwzb.aspx?stockid=601328&accountdate=2014.03.15", header = TRUE)
tt[[3]][1,1]

tt2<-readHTMLTable("http://stockdata.stock.hexun.com/2008/cwbl.aspx?stockid=601328&accountdate=2014.03.15", header = TRUE)
tt3<-readHTMLTable("http://stockdata.stock.hexun.com/2008/zcfz.aspx?stockid=601328&accountdate=2014.03.15", header = TRUE)
tt4<-readHTMLTable("http://stockdata.stock.hexun.com/2008/lr.aspx?stockid=601328&accountdate=2014.03.15", header = TRUE)
tt5<-readHTMLTable("http://stockdata.stock.hexun.com/2008/xjll.aspx?stockid=601328&accountdate=2014.03.15", header = TRUE)



#分红转增股本
tt6<-readHTMLTable("http://stockdata.stock.hexun.com/2009_fhzzgb_000001.shtml", header = F)[[3]]


tt6<-readHTMLTable("http://pinggu.stock.hexun.com/StockProfit.aspx?code=000001", header = F)


#财务数据
library(XML)

total_tt_r <- NULL
stock_list<-c("600000","600016","600030","600048","600111","600348","600489","600547","600837","601006","601169","601318","601398","601668","601699","601818","601901","600010","600019","600031","600050","600123","600362","600518","600549","600887","601088","601288","601328","601601","601669","601766","601857","601989","600015","600028","600036","600104","600256","600383","600519","600585","600999","601166","601299","601336","601628","601688","601788","601899")
accountdate_list<-c(".03.15",".06.30",".09.30",".12.31")
statyear_list<-c("2014","2013","2012")
#fininfo_list<-c("zxcwzb.aspx","cwbl.aspx","zcfz.aspx","lr.aspx","xjll.aspx")
fininfo_list<-c("cwbl.aspx","zcfz.aspx","lr.aspx","xjll.aspx")

for(stock_indx in 1:50) 
{  for(accountdate_indx in 1:4)
  {  for(statyear_indx in 1:3)
    { 
      total_tt <- NULL      
      for(fininfo_indx in 1:length(fininfo_list))
      {       
         cur_url<-paste("http://stockdata.stock.hexun.com/2008/",fininfo_list[fininfo_indx],"?stockid=",stock_list[stock_indx],"&accountdate=",statyear_list[statyear_indx],accountdate_list[accountdate_indx],sep = "")
         cur_tt<-readHTMLTable(cur_url,header = FALSE)[[3]]
         
         if( is.null(cur_tt) )
         {
           print(cur_url)
           next;
         }
         
         
         if(fininfo_list[fininfo_indx] == "cwbl.aspx")
         {
           cur_tt<-cur_tt[cur_tt$V1 %in% c("流动比率","速动比率","净资产收益率","应收帐款周转率","总资产周转率(金融企业指标名称变化）","资产负债率","每股经营现金净流量(元)","每股净资产"),]
         }
         
         if(fininfo_list[fininfo_indx] == "zcfz.aspx")
         {
           cur_tt<-cur_tt[cur_tt$V1 %in% c("资产总计","所有者权益（或股东权益）合计"),]
         }
         
         if(fininfo_list[fininfo_indx] == "lr.aspx")
         {
           cur_tt<-cur_tt[cur_tt$V1 %in% c("一、营业收入","减：营业成本","财务费用","三、利润总额","四、净利润","五、每股收益"),]
         }
         
         if(fininfo_list[fininfo_indx] == "xjll.aspx")
         {
           cur_tt<-cur_tt[cur_tt$V1 %in% c("净利润"),]
         }
         
         rownames(total_tt)<-NULL
         if(is.null(total_tt))
         {
           total_tt<-cur_tt
         }
         else
         {
           total_tt<-rbind(total_tt,cur_tt)
         }
      }
      
      if(is.null(total_tt) )
      {
        next
      }
      
      total_tt <- total_tt[!is.na(total_tt$V1),]
      total_tt <- total_tt[total_tt$V1 != "",]
    
      total_tt$V1<-as.vector(total_tt$V1)
      total_tt$V2<-as.vector(total_tt$V2)
      total_tt<-rbind(total_tt,c("stockid",stock_list[stock_indx]))
      total_tt<-rbind(total_tt,c("statyear",statyear_list[statyear_indx]))
      total_tt<-rbind(total_tt,c("accountdate",accountdate_list[accountdate_indx]))
      
      total_tt$V1<-factor(total_tt[,1])
      total_tt$V2<-factor(total_tt[,2])
      
      if( is.null(total_tt_r))
      {
        total_tt_r <- t(total_tt)
      }
      else
      {
        total_tt_r <- rbind(total_tt_r,t(total_tt[2]))
      }
     
    }
  }
}

#股本变动
tt7<-readHTMLTable("http://stockdata.stock.hexun.com/2009_gbjg_300077.shtml", header = TRUE)[[4]][,1:3]
tt7<-tt7[tt7$V1!="",]


total_tt_na[total_tt_na$index_id %in% c("偿还债务支付的现金","每股净资产（元)","总资产（元)","境外会计准则净利润（元)"),]


sse<-getSymbols("510050.ss",from = "2002-01-01",to = Sys.Date(),src = "yahoo",auto.assign=FALSE)
fastMA <- SMA(Cl(sse["2013-04-20/2013-05-31"]), n = 5)
slowMA <- SMA(Cl(sse["2013-03-01/2013-05-31"]), n = 30)
fastMA <- SMA(Cl(sse), n = 3)
slowMA <- SMA(Cl(sse), n = 20)

sse150131.sz<-getSymbols("150131.sz",from = "2002-01-01",to = Sys.Date(),src = "yahoo",auto.assign=FALSE)
fastMA <- SMA(Cl(sse150131.sz), n = 3)
slowMA <- SMA(Cl(sse150131.sz), n = 20)

write.table(slowMA,"/home/kettle/rstudio-0.98.507/slowMA",sep=",",row.names = FALSE,col.names = TRUE)
write.table(fastMA,"/home/kettle/rstudio-0.98.507/fastMA",sep=",",row.names = FALSE,col.names = TRUE)

write.table(sse150131.sz[,1],"/home/kettle/rstudio-0.98.507/open",sep=",")

signal1 <- fastMA >= slowMA
x <- which(signal1["2013-05-01/2013-05-31", ])[1]
chartSeries(sse, subset = "2013-05-01/2013-05-31", theme = "white", TA = "addSMA(n=5,col=\"red\");addSMA(n=30,col=\"blue\")")
ss <- sse["2013-05-01/2013-05-30"]
addTA(ss[x, "510050.SS.Low"] , pch = 17, type = "p", col = "red", on = 1)



currency("RMB")
stock("ZSYH", currency = "RMB", multiplier = 1)
get("RMB", envir = FinancialInstrument:::.instrument)
get("ZSYH", envir = FinancialInstrument:::.instrument)
Sys.setenv(TZ = "UTC") 
ZSYH <- getSymbols("600036.ss", from = "2008-01-01", to = Sys.Date(), src = "yahoo",auto.assign = FALSE)
ZSYH <- to.monthly(ZSYH, indexAt = "endof")
ZSYH$SMA10m <- SMA(Cl(ZSYH), 10)
head(ZSYH$SMA10m)
myTheme <- chart_theme()
myTheme$col$dn.col <- "lightgreen"
myTheme$col$up.col <- "lightblue"
myTheme$col$dn.border <- "grey"
myTheme$col$up.border <- "grey"
chart_Series(x = ZSYH, theme = myTheme, name = "ZSYH", TA = "add_SMA(n=10,col=4)")
b.strategy <- "bFaber"
initPortf(b.strategy, "ZSYH", initDate = "2007-12-31")
initAcct(b.strategy, portfolios = b.strategy, initDate = "2007-12-31", initEq = 10000) 
for( i in 1:nrow(ZSYH) )
   {
         #对日期更新
           CurrentDate <- time(ZSYH)[i]
         equity<-getEndEq(b.strategy, CurrentDate)
         ClosePrice <- as.numeric(Cl(ZSYH[i,]))
         Posn <- getPosQty(b.strategy, Symbol='ZSYH', Date=CurrentDate)
         UnitSize <-as.numeric(trunc(equity/ClosePrice))#全仓
         MA <- as.numeric(ZSYH[i,'SMA10m'])
         #如有必要改变头寸
           if(!is.na(MA)) #如果移动均线开始
            {
                   if( Posn == 0 ) {#没有头寸，测试是否买入
                         if( ClosePrice > MA ) {
                               #进入多头头寸（买入）
                                 addTxn(b.strategy, Symbol='ZSYH', TxnDate=CurrentDate,
                                         +                        TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=0) }
                     } else {#有头寸，检测是否退出
                           if( ClosePrice < MA ) {
                                 #退出头寸
                                   addTxn(b.strategy, Symbol='ZSYH', TxnDate=CurrentDate,
                                           +                        TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0) }
                       }
               }
         #计算盈亏并更新
           updatePortf(b.strategy, Dates = CurrentDate)
         updateAcct(b.strategy, Dates = CurrentDate)
         updateEndEq(b.strategy, Dates = CurrentDate)
     }


chart.Posn(b.strategy, Symbol = "ZSYH", Dates = "2008::", theme = myTheme)


#获取和讯债券总列表
##[[2]]沪国债表
##[[3]]深国债表
##[[4]]沪企债表
##[[5]]深企债表
##[[6]]沪转债表
##[[7]]深转债表
bond_list<-readHTMLTable("http://bond.money.hexun.com/quote/alltables.htm", header = F)
bond_list[[6]]$V1[1]

#获取和讯债券明细信息
bond_detail<-readHTMLTable("http://bond.money.hexun.com/all_bond/113005.shtml", header = F)[[3]]

bond_detail<-readHTMLTable("http://bond.money.hexun.com/corporate_bond/124507.shtml", header = F)[[3]]

#获取和讯债券交易信息
bond_trade<-readLines("http://flashquote.stock.hexun.com/Stock_Combo.ASPX?mc=2_112063&dt=MX,DL")
strsplit(bond_trade,";")

#获取和讯债券行情列表
#沪
bond_detail<-readHTMLTable("http://bond.money.hexun.com/quote/default.aspx?market=1", header = T)

#深
bond_detail<-readHTMLTable("http://bond.money.hexun.com/quote/default.aspx?market=2", header = T)
http://quote.tool.hexun.com/hqzx/zq_quote.aspx?type=[1-9]&time=155750


bond_detail<-readHTMLTable("http://finance.baidu.com/n?cmd=4&class=hg_guonei&cls=hg_guonei", header = T)

bond_trade<-readLines("http://pan.baidu.com/s/1xQgma")

