library(quantstrat)
library(EMD)
library(tseries)
data(kospi200)

xt <- kospi200$index
length(xt) #896

ttt <- as.POSIXct(strptime(kospi200$date, format="%Y-%m-%d"))
att <- seq(as.Date("1990-1-1"), as.Date("2007-1-1"), by="year")

.indexDate(ZSYH)
head(ZSYH,10)
xxt <- 55555555(ttt, xt)

windows(10, 3.5); par(mfrow=c(1,1), mar=0.1+c(2,2,1.0,0.2))
plot(xxt, ylab = "", xlab = "", xaxt="n", main = "")
abline(v = axis.POSIXct(1, ttt, at = att), col = "lightgray", lty = "dotted", lwd = par("lwd"))
abline(h = axTicks(2),col = "lightgray", lty = "dotted", lwd = par("lwd"))

# EMD
par(mfrow=c(3,1), mar=c(1,2,3,1), oma=c(0,0,0,0))
kospi200d <- emd(xt, plot.imf = TRUE)  

emd(as.numeric(ZSYH[,1]))

, as.POSIXct(as.numeric(.indexDate(ZSYH))*86400, origin = "1970-01-01", format="%Y-%m-%d", tz = "UTC") )
ZSYH[,"row.names"]
View(ZSYH[,"510050.SS.Close"])
as.numeric(ZSYH[,1])

# Plot of each imf
par(mfrow=c(kospi200d$nimf+1, 1), mar=c(3,2,2,1)) 
rangeimf <- range(kospi200d$imf)
for(i in 1:kospi200d$nimf) {
  imf <- irts(ttt, kospi200d$imf[,i])
  plot(imf, xlab="", ylab="", xaxt="n", main=paste("IMF ", i, sep=""))
  abline(v = axis.POSIXct(1, ttt, at = att), col = "lightgray", lty = "dotted", lwd = par("lwd"))
  abline(h=0)
}
plot(irts(ttt, kospi200d$residue), xlab="", ylab="", xaxt="n", main="residue")
abline(v = axis.POSIXct(1, ttt, at= att), col = "lightgray", lty = "dotted", lwd = par("lwd"))

# Hilbert transformation of IMF's
testall <- hilbertspec(kospi200d$imf)

# Spectrogram
windows(10, 3.5); par(mfrow=c(1,1), mar=0.1+c(2,2,1.0,1))
spectrogram(testall$amplitude, testall$instantfreq, tt=ttt, nlevel=256, size=c(256,256))

# Selecting imf's for prediction
windows(10, 3.5); par(mfrow=c(1,1), mar=0.1+c(4,4,1.0,1))
plot(testall$energy, xlab="imf", ylab="Cumulative energy", type="b")

# Hilbert transformation of 6-7 IMF's
windows(10, 3.5); par(mfrow=c(1,1), mar=0.1+c(2,2,1.0,1))
spectrogram(testall$amplitude[,6:7], testall$instantfreq[,6:7], tt=ttt, nlevel=256, size=c(256,256))

# VAR Modeling

library(vars)
n.ahead <- 30
y <- cbind(kospi200d$imf[,6:7])
colnames(y) <- c(paste("imf", 6:7, sep=""))

# Define the lag
varorder <- VARselect(y, lag.max = 10)

# VAR with lag 8
vary <- VAR(y, p = varorder$selection[2])

# Prediction of each IMF component by VAR model
vary.p30 <- predict(vary, n.ahead = n.ahead, ci = 0.95)

# Prediction of residue (trend) by polynomial regression of degree 3
trend30 <- lm(y~poly(x, 3), data.frame(x=1:896, y=kospi200d$residue))
tp30 <- predict(trend30, data.frame(x=897:926), se=T)

# Prediction of KOSPI200
windows(10, 3.5); par(mfrow=c(1,1), mar=0.1+c(4,4,1.0,1))
p30 <- emd.pred(vary.p30, tp30, ci=0.95)