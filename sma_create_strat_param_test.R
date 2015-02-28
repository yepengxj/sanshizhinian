library(quantstrat)

save.strategy(q.strategy)

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

require(doMC)
registerDoMC(cores=8)

results <- apply.paramset(q.strategy,
                          paramset.label='SMA',
                          portfolio.st=q.strategy,
                          account.st=q.strategy,
                          nsamples = 200,
                          audit = NULL,
                          verbose=TRUE)
save.strategy(q.strategy)
View(results$tradeStats)
