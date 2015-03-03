library(EMD)
library(quantstrat)
library(dtw)
library(plyr)
library("e1071")
library("hht")

setwd("~/temp/")
print(getwd())
xxx<-read.csv("~/temp/0000300.csv",header=F,skip=1,encoding = "UTF-8")
xxx<-xts(xxx[,c(4,5,6,7,12)],order.by=as.Date(xxx[,1],"%Y-%m-%d"))
colnames(xxx)<-c("close","high","low","open","vol")
q.strategy <- "qFaber"
backtest_func<-function(date_range,xxx){
  
  aaply(list.files("~/temp/",pattern="^pred_resRlibeemd_eemd"),1,function(x,xxx,date_range){
    
    filepath<-paste("~/temp/",x,sep="")
    pred_xxx<-read.csv(filepath,header=T,encoding="UTF-8")
    
    pres_xxx_ts<-xts(pred_xxx[,-1],order.by=as.Date(pred_xxx[,1],origin = "1970-01-01"))
    pred_close_price<-merge(pres_xxx_ts[date_range],xxx,join="left")
    pred_close_price$sell_signal<-((pred_close_price[,1]>pred_close_price[,2])&(pred_close_price[,2]>pred_close_price[,3]))
    pred_close_price$buy_signal<-((pred_close_price[,1]<pred_close_price[,2])&(pred_close_price[,2]<pred_close_price[,3]))
    pred_close_price$close<-pred_close_price$close/1000
    pred_close_price$high<-pred_close_price$high/1000
    pred_close_price$low<-pred_close_price$low/1000
    pred_close_price$open<-pred_close_price$open/1000
    print(head(pred_close_price,4))
    
    currency("RMB")
    try(rm("order_book.qFaber",pos=.strategy),silent=TRUE)
    try(rm("account.qFaber","portfolio.qFaber",pos=.blotter),silent=TRUE)
    ## [1] "RMB"
    stock("ZSYH", currency = "RMB", multiplier = 1)
    ## [1] "ZSYH"
    # 设定时区
    Sys.setenv(TZ = "GMT+8")
    ZSYH<-pred_close_price
    print(head(ZSYH,4))
    # 初始化组合和账户
    q.strategy <- "qFaber"
    initPortf(q.strategy, "ZSYH", initDate = "2002-01-31")

    initAcct(q.strategy, portfolios = q.strategy, initDate = "2002-01-31", initEq = 100000)

    initOrders(portfolio = q.strategy, initDate = "2002-01-31")

    strategy(q.strategy, store=TRUE)
    
    add.signal(q.strategy,name="sigThreshold",
               arguments = list(column="buy_signal",
                                relationship="eq",
                                threshold=1,
                                cross=TRUE),
               label="buy_signal_add"
    )
    print("add.signal")
    
    add.signal(q.strategy,name="sigThreshold",
               arguments = list(column="sell_signal",
                                relationship="eq",
                                threshold=1),
               label="sell_signal_add"
    )
    
    print("add.signal")
    
    add.rule(q.strategy, name = "ruleSignal", arguments = list(sigcol = "buy_signal_add", 
                                                               sigval = TRUE, orderqty = 50000, ordertype = "market", orderside = "long", 
                                                               pricemethod = "market"), type = "enter", path.dep = TRUE) # 买入数量为900股
    
    add.rule(q.strategy, name = "ruleSignal", arguments = list(sigcol = "sell_signal_add", 
                                                               sigval = TRUE, orderqty = "all", ordertype = "market", orderside = "long", 
                                                               pricemethod = "market"), type = "exit", path.dep = TRUE)

    print("add.rul")
    
    applyStrategy(strategy = q.strategy, portfolios = q.strategy,mktdata=pred_close_price)
    
    
    updatePortf(q.strategy)
    
    updateAcct(q.strategy)
    
    updateEndEq(q.strategy)
    
    myTheme <- chart_theme()
    myTheme$col$dn.col <- "lightgreen"
    myTheme$col$up.col <- "lightblue"
    myTheme$col$dn.border <- "grey"
    myTheme$col$up.border <- "grey"
    
    #chart.Posn(q.strategy, Symbol = "ZSYH", Dates = date_range, theme=myTheme)
    #(tstats <- tradeStats(Portfolio = q.strategy, Symbol = "ZSYH"))
    tradeStats_def<-data.frame("Portfolio"=q.strategy,"Symbol"="ZSYH",
                         "Num.Txns"=0,
                         "Num.Trades"=0,
                         "Net.Trading.PL"=0,
                         "Avg.Trade.PL"=0,
                         "Med.Trade.PL"=0,
                         "Largest.Winner"=0,
                         "Largest.Loser"=0,
                         "Gross.Profits"=0,
                         "Gross.Losses"=0,
                         "Std.Dev.Trade.PL"=0,
                         "Percent.Positive"=0,
                         "Percent.Negative"=0,
                         "Profit.Factor"=0,
                         "Avg.Win.Trade"=0,
                         "Med.Win.Trade"=0,
                         "Avg.Losing.Trade"=0,
                         "Med.Losing.Trade"=0,
                         "Avg.Daily.PL"=0,
                         "Med.Daily.PL"=0,
                         "Std.Dev.Daily.PL"=0,
                         "Ann.Sharpe"=0,
                         "Max.Drawdown"=0,
                         "Profit.To.Max.Draw"=0,
                         "Avg.WinLoss.Ratio"=0,
                         "Med.WinLoss.Ratio"=0,
                         "Max.Equity"=0,
                         "Min.Equity"=0,
                         "End.Equity"=0)
    
    
    
    ifelse(is.null(tradeStats(Portfolio = q.strategy, Symbol = "ZSYH")),
                   trades<-tradeStats_def,
                   trades<-tradeStats(Portfolio = q.strategy, Symbol = "ZSYH"))
    trades$name<-x
    print(trades)
    
    trades
   # data.frame("name"=x,
  #       ifelse(is.null(tradeStats(Portfolio = q.strategy, Symbol = "ZSYH")),
   #             tradeStats_def,
    #            tradeStats(Portfolio = q.strategy, Symbol = "ZSYH")))
  },xxx,date_range)
  
} 

backtest_2009_07_01_2014_07_01<-backtest_func("2009-07-01/2014-07-01",xxx)
backtest_2014_07_01<-backtest_func("2014-07-01/",xxx)

write.csv(backtest_2009_07_01_2014_07_01, file = "~/temp/backtest_2009_07_01_2014_07_01.txt",row.names=F)
write.csv(backtest_01_2014_07_01, file = "~/temp/backtest_2014_07_01.txt",row.names=F)
