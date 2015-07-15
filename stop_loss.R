#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - May 2013
#
# Tested and found to work correctly using blotter r1457
#
# After Jaekle & Tomasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)
#
# Advanced luxor strategy implementation including exit management using ordersets and orderchains

require(quantstrat)

##### PLACE DEMO AND TEST DATES HERE #################
#
#if(isTRUE(options('in_test')$in_test))
#  # use test dates
#  {initDate="2011-01-01" 
#  endDate="2012-12-31"   
#  } else
#  # use demo defaults
#  {initDate="1999-12-31"
#  endDate=Sys.Date()}

source(paste0(path.package("quantstrat"),"/demo/luxor.include.R"))

### define strategy

strategy(strategy.st, store=TRUE)

### indicators

add.indicator(strategy.st, name = "SMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = .fast
              ),
              label="nFast"
)

add.indicator(strategy.st, name="SMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = .slow
              ),
              label="nSlow"
)

### signals

add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="gte"
           ),
           label='long'
)


### rules ############

# normal exit rules



# normal entry rules

add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        replace=FALSE,
                        orderside='long' ,
                        ordertype='stoplimit',
                        prefer='High',
                        threshold=.threshold,
                        TxnFees=0,
                        orderqty=+.orderqty,
                        osFUN=osMaxPos,
                        orderset='ocolong'
         ),
         type='enter',
         timespan = .timespan,
         label='EnterLONG'
)

### parameter sets

# SMA

add.distribution(strategy.st,
                 paramset.label = 'SMA',
                 component.type = 'indicator',
                 component.label = 'nFast',
                 variable = list(n = .FastSMA),
                 label = 'nFAST'
)

add.distribution(strategy.st,
                 paramset.label = 'SMA',
                 component.type = 'indicator',
                 component.label = 'nSlow',
                 variable = list(n = .SlowSMA),
                 label = 'nSLOW'
)

add.distribution.constraint(strategy.st,
                            paramset.label = 'SMA',
                            distribution.label.1 = 'nFAST',
                            distribution.label.2 = 'nSLOW',
                            operator = '<',
                            label = 'SMA'
)

# stop-loss

add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        replace=FALSE,
                        orderside='long',
                        ordertype='stoplimit', tmult=TRUE, threshold=quote(.stoploss),
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocolong'
         ),
         type='chain', parent='EnterLONG',
         label='StopLossLONG',
         enabled=FALSE
)

add.distribution(strategy.st,
                 paramset.label = 'StopLoss',
                 component.type = 'chain',
                 component.label = 'StopLossLONG',
                 variable = list(threshold = .StopLoss),
                 label = 'StopLossLONG'
)


# stop-trailing

add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        replace=FALSE,
                        orderside='long',
                        ordertype='stoptrailing', tmult=TRUE, threshold=quote(.stoptrailing),
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocolong'
         ),
         type='chain', parent='EnterLONG',
         label='StopTrailingLONG',
         enabled=FALSE
)


add.distribution(strategy.st,
                 paramset.label = 'StopTrailing',
                 component.type = 'chain',
                 component.label = 'StopTrailingLONG',
                 variable = list(threshold = .StopTrailing),
                 label = 'StopTrailingLONG'
)


# take-profit

add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        replace=FALSE,
                        orderside='long',
                        ordertype='limit', tmult=TRUE, threshold=quote(.takeprofit),
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocolong'
         ),
         type='chain', parent='EnterLONG',
         label='TakeProfitLONG',
         enabled=FALSE
)

add.distribution(strategy.st,
                 paramset.label = 'TakeProfit',
                 component.type = 'chain',
                 component.label = 'TakeProfitLONG',
                 variable = list(threshold = .TakeProfit),
                 label = 'TakeProfitLONG'
)



# Walk Forward Analysis

add.distribution(strategy.st,
                 paramset.label = 'WFA',
                 component.type = 'indicator',
                 component.label = 'nFast',
                 variable = list(n = .FastWFA),
                 label = 'nFAST'
)

add.distribution(strategy.st,
                 paramset.label = 'WFA',
                 component.type = 'indicator',
                 component.label = 'nSlow',
                 variable = list(n = .SlowWFA),
                 label = 'nSLOW'
)

add.distribution.constraint(strategy.st,
                            paramset.label = 'WFA',
                            distribution.label.1 = 'nFAST',
                            distribution.label.2 = 'nSLOW',
                            operator = '<',
                            label = 'WFA'
)

###############################################################################

#save.strategy(strategy.st)
SZZS_tdx<-read.table('/home/datagrid/backdata/quant/SH510050.txt',header=F,sep="\t",skip = 2,fill=T)
SZZS_tdx<-SZZS_tdx[-nrow(SZZS_tdx),]
SZZS_tdx<-xts(SZZS_tdx[,-1],order.by=as.Date(SZZS_tdx[,1],format="%m/%d/%Y",origin = "1970-01-01"))
######设置时间序列列名#####
colnames(SZZS_tdx)<-c("open","high","low","close","volume","total")
head(SZZS_tdx,10)


initPortf(portfolio.st, symbols='SZZS_tdx', initDate=initDate)
addPosLimit(
  portfolio=portfolio.st,
  symbol='SZZS_tdx',
  timestamp=initDate,
  maxpos=.orderqty)

initAcct(account.st, portfolios=portfolio.st, initDate=initDate)

###

initOrders(portfolio.st, initDate=initDate)
enable.rule('luxor', 'chain', 'StopLoss')

##### PLACE THIS BLOCK AT END OF DEMO SCRIPT ################### 
# book  = getOrderBook(port)
# stats = tradeStats(port)
# rets  = PortfReturns(acct)
################################################################
require(foreach)
#registerDoSEQ()

if (!"doMC" %in% installed.packages()[,1]) {
  install.packages("doMC")
}
require(doMC)
registerDoMC(cores=4)

#require(doParallel)
#registerDoParallel(cores=2)

#require(doRedis)
#registerDoRedis('jobs')

############################

results <- apply.paramset(strategy.st, paramset.label='StopLoss', portfolio.st=portfolio.st, account.st=account.st, nsamples=80, verbose=TRUE)

stats <- results$tradeStats

View(t(stats))

plot(100*stats$StopLossLONG, stats$Net.Trading.PL, type='b', xlab='Stoploss %', ylab='Net.Trading.PL', main='Luxor')
