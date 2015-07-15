library(quantstrat)

q.strategy <- "qFaber"
load.strategy(q.strategy)
.FastSMA = (3:10)
.SlowSMA = (20:30)


add.distribution(q.strategy,
                 paramset.label = 'SMA',
                 component.type = 'indicator',
                 component.label = 'nFAST',
                 variable = list(n = .FastSMA),
                 label = 'nFAST')

add.distribution(q.strategy,
                 paramset.label = 'SMA',
                 component.type = 'indicator',
                 component.label = 'nSLOW',
                 variable = list(n = .SlowSMA),
                 label = 'nSLOW')

add.distribution.constraint(q.strategy,
                            paramset.label = 'SMA',
                            distribution.label.1 = 'nFAST',
                            distribution.label.2 = 'nSLOW',
                            operator = '<',
                            label = 'SMA',
                            store=TRUE)

library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)
addPosLimit(q.strategy, 'ZSYH', timestamp="2005-01-01", maxpos=50000, minpos=0)

results <- apply.paramset(q.strategy,
                          paramset.label='SMA',
                          portfolio.st=q.strategy,
                          account.st=q.strategy,
                          nsamples = 200,
                          audit = NULL,
                          verbose=TRUE)
save.strategy(q.strategy)
View(results$tradeStats)
