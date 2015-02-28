library(quantstrat)
# 金融产品初始化
currency("RMB")

try(rm("order_book.qFaber",pos=.strategy),silent=TRUE)
try(rm("account.qFaber","portfolio.qFaber",pos=.blotter),silent=TRUE)


## [1] "RMB"
stock("ZSYH", currency = "RMB", multiplier = 1)
## [1] "ZSYH"
# 设定时区
Sys.setenv(TZ = "UTC")
# 读取金融交易数据并转化为月数据
ZSYH <- getSymbols("510050.ss", from = "2008-01-01", to = Sys.Date(), src = "yahoo", 
                   auto.assign = FALSE)
colnames(ZSYH)<-c("open","high","low","close","vol","adj")
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
ZSYH$SMA5 <- SMA(Cl(ZSYH), 5)
ZSYH$SMA20 <- SMA(Cl(ZSYH), 20)
#head(ZSYH$SMA5)
# 初始化组合和账户
q.strategy <- "qFaber"
initPortf(q.strategy, "ZSYH", initDate = "2007-12-31")
## [1] "qFaber"
initAcct(q.strategy, portfolios = q.strategy, initDate = "2007-12-31", initEq = 1e+06)
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
add.indicator(strategy = q.strategy, name = "SMA", arguments = list(x = quote(Cl(mktdata)[,"close"]), 
                                                                    n = 5), label = "SMA5")
add.indicator(strategy = q.strategy, name = "SMA", arguments = list(x = quote(Cl(mktdata)[,"close"]), 
                                                                         n = 20), label = "SMA20")

add.indicator(strategy = q.strategy, name = "MACD", arguments = list(x = quote(Cl(mktdata)[,"close"]), 
                                                                    nFast = 5,nSlow=20,nSig=9,maType="EMA"), label = "MDCA")


# 加入信号，向上交叉10月线，向下交叉10月线
add.signal(q.strategy, name = "sigCrossover", arguments = list(columns = c("SMA5",  "SMA20"), 
                                                               relationship = "gt"),
           label = "SMA5.gt.SMA20")

add.signal(q.strategy, name = "sigCrossover", arguments = list(columns = c("SMA5", "SMA20"), 
                                                               relationship = "lt"), 
           label = "SMA5.lt.SMA20")
# 加入规则，买入规则和卖出规则
add.rule(q.strategy, name = "ruleSignal", arguments = list(sigcol = "SMA5.gt.SMA20", 
                                                           sigval = TRUE, orderqty = 900, ordertype = "market", orderside = "long", 
                                                           pricemethod = "market"), type = "enter", path.dep = TRUE) # 买入数量为900股

add.rule(q.strategy, name = "ruleSignal", arguments = list(sigcol = "SMA5.lt.SMA20", 
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

save.strategy(q.strategy)

