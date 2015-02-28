"PVMA" <-
  function(open,close,volume, n=10) {
    
    # Simple Moving Average
    
    # http://www.fmlabs.com/reference/SimpleMA.htm
    # http://www.equis.com/Customer/Resources/TAAZ/Default.aspx?c=3&p=74
    # http://linnsoft.com/tour/techind/movAvg.htm
    # http://stockcharts.com/education/IndicatorAnalysis/indic_movingAvg.html
    
    ma <- runSum(volume*(open+close)/2,n)/runSum(volume,n)
    return(ma)
}


library(quantstrat)
# 金融产品初始化
currency("RMB")
q.strategy <- "qFaber"
try(rm("order_book.qFaber",pos=.strategy),silent=TRUE)
try(rm("account.qFaber","portfolio.qFaber",pos=.blotter),silent=TRUE)


## [1] "RMB"
stock("ZSYH", currency = "RMB", multiplier = 1)
## [1] "ZSYH"
# 设定时区
Sys.setenv(TZ = "GMT+8")
# 读取金融交易数据并转化为月数据
ZSYH <- getSymbols("510050.ss", from = "2008-01-01", to = Sys.Date(), src = "yahoo", 
                   auto.assign = FALSE)
colnames(ZSYH)<-c("open","high","low","close","volume","adj")
## As of 0.4-0, 'getSymbols' uses env=parent.frame() and auto.assign=TRUE by
## default.
## 
## This behavior will be phased out in 0.5-0 when the call will default to
## use auto.assign=FALSE. getOption("getSymbols.env") and
## getOptions("getSymbols.auto.assign") are now checked for alternate
## defaults
## 
## This message is shown once per session and may be disabled by setting
## options("getSymbols.warning4.0"=FALSE). See ?getSymbol for more details

#ZSYH <- to.monthly(ZSYH, indexAt = "endof")
#ZSYH$nFAST <- SMA(Cl(ZSYH), 5)
#ZSYH$nSLOW <- SMA(Cl(ZSYH), 20)
#head(ZSYH$nFAST)
# 初始化组合和账户
q.strategy <- "qFaber"
initPortf(q.strategy, "ZSYH", initDate = "2007-12-31")
## [1] "qFaber"
initAcct(q.strategy, portfolios = q.strategy, initDate = "2007-12-31", initEq = 100000)
## [1] "qFaber"
# 初始化指定和策略
initOrders(portfolio = q.strategy, initDate = "2007-12-31")
strategy(q.strategy, store = TRUE)
ls(all = T) 
#quantstrat创建了.strategy环境
## [1] ".blotter" ".strategy" "q.strategy" "ZSYH"
# 策略是什么呢？看一下
strategy <- getStrategy(q.strategy)
summary(strategy)

#下面是quantstrat包的关键：加入指标、信号和规则。

# 加入一个指标，10月均线
add.indicator(strategy = q.strategy, name = "PVMA", arguments = list(close = quote(Cl(mktdata)[,"close"]), open=quote(Op(mktdata)[,"open"]),
                                                                     volume=quote(Vo(mktdata)[,"vol"]), n = 5), label = "nFAST")
add.indicator(strategy = q.strategy, name = "PVMA", arguments = list(close = quote(Cl(mktdata)[,"close"]), open=quote(Op(mktdata)[,"open"]),
                                                                     volume=quote(Vo(mktdata)[,"volume"]), n = 10), label = "nSLOW")

# 加入信号，向上交叉10月线，向下交叉10月线
add.signal(q.strategy, name = "sigCrossover", arguments = list(columns = c("nFAST",  "nSLOW"), 
                                                               relationship = "gt"),
           label = "nFAST.gt.nSLOW")

add.signal(q.strategy, name = "sigCrossover", arguments = list(columns = c("nFAST", "nSLOW"), 
                                                               relationship = "lt"), 
           label = "nFAST.lt.nSLOW")

# 加入规则，买入规则和卖出规则
add.rule(q.strategy, name = "ruleSignal", arguments = list(sigcol = "nFAST.gt.nSLOW", 
                                                           sigval = TRUE, orderqty = 900, ordertype = "market", orderside = "long", 
                                                           pricemethod = "market"), type = "enter", path.dep = TRUE) # 买入数量为900股

add.rule(q.strategy, name = "ruleSignal", arguments = list(sigcol = "nFAST.lt.nSLOW", 
                                                           sigval = TRUE, orderqty = "all", ordertype = "market", orderside = "long", 
                                                           pricemethod = "market"), type = "exit", path.dep = TRUE)


summary(getStrategy(q.strategy))

out <- applyStrategy(strategy = q.strategy, portfolios = q.strategy)

summary(out)

#mktdata["2013"]

updatePortf(q.strategy)

updateAcct(q.strategy)

updateEndEq(q.strategy)

myTheme <- chart_theme()
myTheme$col$dn.col <- "lightgreen"
myTheme$col$up.col <- "lightblue"
myTheme$col$dn.border <- "grey"
myTheme$col$up.border <- "grey"

chart.Posn(q.strategy, Symbol = "ZSYH", Dates = paste('::',as.Date(Sys.time()),sep=''), theme = myTheme)
(tstats <- tradeStats(Portfolio = q.strategy, Symbol = "ZSYH"))

ob <- getOrderBook(q.strategy)
head(ob$qFaber$ZSYH)

chart.ME(Portfolio = q.strategy, Symbol = "ZSYH", type = "MAE", scale = "percent")
chart.ME(Portfolio = q.strategy, Symbol = "ZSYH", type = "MFE", scale = "percent")




.FastSMA = (1:30)
.SlowSMA = (20:80)


add.distribution(q.strategy,
                 paramset.label = 'PVMA_set',
                 component.type = 'indicator',
                 component.label = 'nFAST',
                 variable = list(n = .FastSMA),
                 label = 'PVMA_set_nFAST')

add.distribution(q.strategy,
                 paramset.label = 'PVMA_set',
                 component.type = 'indicator',
                 component.label = 'nSLOW',
                 variable = list(n = .SlowSMA),
                 label = 'PVMA_set_nSLOW')

add.distribution.constraint(q.strategy,
                            paramset.label = 'PVMA_set',
                            distribution.label.1 = 'PVMA_set_nFAST',
                            distribution.label.2 = 'PVMA_set_nSLOW',
                            operator = '<',
                            label = 'PVMA',
                            store=TRUE)

require(doMC)
registerDoMC(cores=8)

results <- apply.paramset(q.strategy,
                          paramset.label='PVMA_set',
                          portfolio.st=q.strategy,
                          account.st=q.strategy,
                          nsamples = 200,
                          audit = NULL,
                          verbose=TRUE)
save.strategy(q.strategy)
View(results$tradeStats)

