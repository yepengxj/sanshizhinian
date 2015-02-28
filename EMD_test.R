library(EMD)
library(quantstrat)
library(tseries)
### Identify the extrema and zero-crossings

# Generating a composite signal
ndata <- 3000
X11(); par(mfrow=c(1,1), mar=c(1,1,1,1))
tt2 <- seq(0, 9, length=ndata)
xt2 <- sin(pi * tt2) + sin(2* pi * tt2) + sin(6 * pi * tt2)  + 0.5 * tt2
plot(tt2, xt2, xlab="", ylab="", type="l", axes=FALSE); box()

# Generating a component of a composite signal
xt <- sin(pi*tt2)

X11(); par(mar=c(1,1,1,1))
plot(tt2, xt, xlab="", ylab="", type="l", axes=FALSE, lty=2); abline(h=0); box()
lines(tt2[tt2 >= 0 & tt2 <= 2], xt[tt2 >= 0 & tt2 <= 2], col="red", lwd=5)
lines(tt2[tt2 >= 4.5 & tt2 <= 6.5], xt[tt2 >= 4.5 & tt2 <= 6.5], col="blue", lwd=5, lty=2)

# Identify the extrema and zero-crossings
extrema(xt)



### Extracting the first IMF by sifting process

# WHEN 'check=TRUE' or 'plot=TRUE', To start the next step, CLICK THE PLOT.
tryimf <- extractimf(xt2, tt2, check=TRUE)



### EMD with no boundary adjustment
ndata <- 3000
X11(); par(mfrow=c(1,1), mar=c(1,1,1,1))
tt22 <- seq(0, 9, length=ndata)
xt22 <- sin(pi * tt2) + sin(2* pi * tt2) + sin(6 * pi * tt2) 

par(mfrow=c(3,1), mar=c(2,1,2,1))
try22 <- emd(xt22, tt22, boundary="none")

# Ploting the IMF's
par(mfrow=c(try22$nimf+1, 1), mar=c(2,3,2,1))
rangeimf <- c(-3, 3) #range(c(xt22, try22$imf))
plot(tt22, xt22, type="l", xlab="", ylab="", ylim=rangeimf, main="Signal")
for(i in 1:try22$nimf) {
  plot(tt22, try22$imf[,i], type="l", xlab="", ylab="", ylim=rangeimf,
       main=paste("IMF ", i, sep="")); abline(h=0)
}



### EMD with boundary adjustment
par(mfrow=c(3,1), mar=c(2,1,2,1))
try <- emd(xt2, tt2, boundary="wave")

# Ploting the IMF's
par(mfrow=c(3,1), mar=c(2,1,2,1))
X11(); par(mfrow=c(try$nimf+1, 1), mar=c(2,1,2,1))
rangeimf <- range(try$imf)
for(i in 1:try$nimf) {
  plot(tt2, try$imf[,i], type="l", xlab="", ylab="", ylim=rangeimf,
       main=paste(i, "-th IMF", sep="")); abline(h=0)
}
plot(tt2, try$residue, xlab="", ylab="", main="residue", type="l", axes=FALSE); box()



### Interpolation and smoothing                                                          
ndata <- 3000                                                                      

tt3 <- seq(0, 9, length=ndata)                                                     
xt3 <- sin(pi * tt3) + sin(2* pi * tt3) + sin(6 * pi * tt3)  + 0.5 * tt3    
set.seed(1)
xt3 <- xt3 + rnorm(ndata, 0, sd(xt3)/7)

imfbyint <- extractimf(xt3, tt3, check=FALSE)                                          
imfbysm <- extractimf(xt3, tt3, sm="spline", weight=20, check=FALSE)

X11(); par(mfcol=c(3,2), mar=c(1,1,2,1))                                                 
rangext <- range(xt3); rangeimf <- rangext - mean(rangext)
plot(tt3, xt3, xlab="", ylab="", main="Interpolation", ylim=rangext, type="l", axes=FALSE); box()                          
plot(tt3, imfbyint$imf, xlab="", ylab="", ylim=rangeimf,  type="l", axes=FALSE); box()
plot(tt3, imfbyint$residue, xlab="", ylab="", ylim=rangext, type="l", axes=FALSE); box()                
plot(tt3, xt3, xlab="", ylab="", main="Smoothing", ylim=rangext, type="l", axes=FALSE); box()                          
plot(tt3, imfbysm$imf, xlab="", ylab="", ylim=rangeimf,  type="l", axes=FALSE); box()
plot(tt3, imfbysm$residue, xlab="", ylab="", ylim=rangext, type="l", axes=FALSE); box()



### Interpolation and smoothing for extreme values
eindex <- sample(1:ndata, 2)
xt4 <- xt3; xt4[eindex] <- xt3[eindex] + c(4, -4)

imfbyint2 <- extractimf(xt4, tt3, check=FALSE)                                          
imfbysm2 <- extractimf(xt4, tt3, sm="spline", weight=20, check=FALSE)

X11(); par(mfcol=c(3,2), mar=c(1,1,2,1))                                                 
rangext <- range(xt4); rangeimf <- rangext - mean(rangext)
plot(tt3, xt4, xlab="", ylab="", main="Interpolation", ylim=rangext, type="l", axes=FALSE); box()                          
plot(tt3, imfbyint2$imf, xlab="", ylab="", ylim=rangeimf,  type="l", axes=FALSE); box()
plot(tt3, imfbyint2$residue, xlab="", ylab="", ylim=rangext, type="l", axes=FALSE); box()                
plot(tt3, xt4, xlab="", ylab="", main="Smoothing", ylim=rangext, type="l", axes=FALSE); box()                          
plot(tt3, imfbysm2$imf, xlab="", ylab="", ylim=rangeimf,  type="l", axes=FALSE); box()
plot(tt3, imfbysm2$residue, xlab="", ylab="", ylim=rangext, type="l", axes=FALSE); box()



### Mode mixing 
tt <- seq(0, 0.1, length = 2001)[1:2000]           
f1 <- 1776; f2 <- 1000
xt <- sin(2*pi*f1*tt) * (tt <= 0.033 | tt >= 0.067) + sin(2*pi*f2*tt)

# EMD without treating intermittence
interm1 <- emd(xt, tt, boundary="wave", max.imf=2, plot.imf=FALSE)  

# Ploting the IMF's
par(mfrow=c(3, 1), mar=c(3,2,2,1))
plot(tt, xt, main="Signal", type="l")
rangeimf <- range(interm1$imf)
plot(tt, interm1$imf[,1], type="l", xlab="", ylab="", ylim=rangeimf, main="IMF 1")
plot(tt, interm1$imf[,2], type="l", xlab="", ylab="", ylim=rangeimf, main="IMF 2")

# Histogram of empirical period
par(mfrow=c(1,1), mar=c(2,4,1,1)) 
tmpinterm <- extrema(interm1$imf[,1])
zerocross <- as.numeric(round(apply(tmpinterm$cross, 1, mean)))
hist(diff(tt[zerocross[seq(1, length(zerocross), by=2)]]), freq=FALSE, xlab="", main="")

# EMD with treating intermittence
interm2 <- emd(xt, tt, boundary="wave", max.imf=2, plot.imf=FALSE, interm=0.0007)

# Ploting the IMF's
X11();par(mfrow=c(2,1), mar=c(2,2,3,1), oma=c(0,0,0,0))
rangeimf <- range(interm2$imf)
plot(tt,interm2$imf[,1], type="l", main="IMF 1 after treating intermittence",
     xlab="", ylab="", ylim=rangeimf)
plot(tt,interm2$imf[,2], type="l", main="IMF 2 after treating intermittence",
     xlab="", ylab="", ylim=rangeimf)



### Spectrogram : X - Time, Y - frequency, Z (Image) - Amplitude 

par(mfrow=c(2,1), mar=c(2,2,2,1))
test1 <- hilbertspec(interm1$imf)
spectrogram(test1$amplitude[,1], test1$instantfreq[,1])

test2 <- hilbertspec(interm2$imf, tt=tt)
spectrogram(test2$amplitude[,1], test2$instantfreq[,1])



### Decomposition of Lean's solar irradiance proxy data

data(solar.lean); names(solar.lean)

par(mar=c(5,3,3,1))
plot(solar.lean$year, solar.lean$solar, type="l", xlab="year", ylab="", main="Lean's solar irradiance proxy data")

data(sunspot); names(sunspot)
length(solar.lean$year); range(solar.lean$year)
#[1] 391
#[1] 1610 2000
length(sunspot$year); range(sunspot$year)
#[1] 386
#[1] 1610 1995

comparesun <- cbind(ts(solar.lean$solar, start=min(solar.lean$year)), ts(sunspot$sunspot, start=min(sunspot$year)))
colnames(comparesun) <- c("Lean's solar irradiance proxy data", "Sunspot")

par(mar=c(1,2,0,1))
plot(comparesun, plot.type = c("multiple"), main="", xlab="year")

# EMD for Lean's solar irradiance proxy data
par(mfrow=c(3,1), mar=c(2,2,3,1))
solardecom <- emd(solar.lean$solar, plot.imf=TRUE)

comparesolar <- cbind(ts(solardecom$imf[,c(4, 2)], start=min(solar.lean$year)), 
                      ts(sunspot$sunspot, start=min(sunspot$year)))
colnames(comparesolar) <- c("IMF 4", "IMF 2", "Sunspot")

par(mar=c(1,2,0,1))
plot(comparesolar, plot.type = c("multiple"), main="", xlab="year")



### Hierarchical smoothing

# EMD
ndata <- 1024
tt <- seq(0, 9, length=ndata)
meanf <- (sin(pi*tt) + sin(2*pi*tt) + sin(6*pi*tt)) * (0.0 < tt & tt <= 3.0) + 
  (sin(pi*tt) +               sin(6*pi*tt)) * (3.0 < tt & tt <= 6.0) +
  (sin(pi*tt) + sin(6*pi*tt) + sin(12*pi*tt)) * (6.0 < tt & tt <= 9.0)
snr <- 3.0;  sigma <- c(sd(meanf[tt<=3]) / snr, sd(meanf[tt<=6 & tt>3]) / snr, sd(meanf[tt>6]) / snr)
set.seed(1)
error <- c(rnorm(sum(tt<=3), 0, sigma[1]), rnorm(sum(tt<=6 & tt>3), 0, sigma[2]), rnorm(sum(tt>6), 0, sigma[3]))
xt <- meanf + error 

try00 <- emd(xt, boundary="symmetric",  max.imf=2, plot.imf=FALSE) 

X11(); par(mfrow=c(3,1), mar=c(2,2,3,1)); rangeimf <- range(c(xt))
plot(tt, xt, type="l", xlab="", ylab="", main=expression(paste("Signal x(t)")))
plot(tt, try00$imf[,1], type="l", xlab="", ylab="", ylim=rangeimf, main=expression(imf[1]))
plot(tt, try00$imf[,2], type="l", xlab="", ylab="", ylim=rangeimf, main=expression(imf[2]))

# Cross-validation scheme
cv.index <- cvtype(n=ndata, cv.kfold=2, cv.random=FALSE)$cv.index

# Denoising by CV
try10 <- emddenoise(xt, cv.index=cv.index, cv.level=2, by.imf=TRUE)
try10$optlambda
#[1] 0.8112607 0.4407426

X11(); par(mfrow=c(3,1), mar=c(2,2,3,1))
plot(tt, try10$demd$imf[,1], type="l", xlab="", ylab="", ylim=rangeimf, main=expression(paste("Thresholded IMF, ", d[1], sep="")))
plot(tt, try10$demd$imf[,2], type="l", ylab="", ylim=rangeimf, main=expression(paste("Thresholded IMF, ", d[2], sep="")))
plot(tt, try10$dxt, type="l", xlab="", ylab="", ylim=rangeimf, main=expression(paste("Denoised Signal ", hat(f), " = ", d[1], " + ", d[2], " + r", sep="")))



### Prediction of KOSPI200
library(tseries)
data(kospi200)

xt <- kospi200$index
length(xt) #896

ttt <- as.POSIXct(strptime(kospi200$date, format="%Y-%m-%d"))
att <- seq(as.Date("1990-1-1"), as.Date("2007-1-1"), by="year")

xxt <- irts(ttt, xt)

windows(10, 3.5); par(mfrow=c(1,1), mar=0.1+c(2,2,1.0,0.2))
plot(xxt, ylab = "", xlab = "", xaxt="n", main = "")
abline(v = axis.POSIXct(1, ttt, at = att), col = "lightgray", lty = "dotted", lwd = par("lwd"))
abline(h = axTicks(2),col = "lightgray", lty = "dotted", lwd = par("lwd"))

# EMD
par(mfrow=c(3,1), mar=c(1,2,3,1), oma=c(0,0,0,0))
kospi200d <- emd(xt, plot.imf = TRUE)  

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



#### Image
data(lena)
z <- lena[seq(1, 512, by=4), seq(1, 512, by=4)]

par(mfrow=c(1,1), mar=c(0, 0.5, 2, 0.5))
image(z, main="Lena", xlab="", ylab="", col=gray(0:100/100), axes=FALSE)

par(mfrow=c(3,1), mar=c(1,2,3,1), oma=c(0,0,0,0))
lenadecom <- emd2d(z, max.imf = 4)

# Decomposition result of Lena image
imageEMD(z=z, emdz=lenadecom, extrema=TRUE, col=gray(0:100/100))


lsmodel<-lsfit(test_row1,test_row20)
> lsmodel$coefficients
Intercept          X 
4.1288372 -0.6162791 
> lsmodel$coefficients["Intercpet"]
<NA> 
  NA 
> lsmodel$coefficients["Intercept"]
Intercept 
4.128837 
> lsmodel$coefficients["X"]
X 
-0.6162791 
> lsmodel$coefficients["Intercept"]+lsmodel$coefficients["X"]*test_row20
[1] 3.155116 3.204419 3.198256 3.161279 3.179767 3.204419 3.210581 3.235233 3.315349 3.272209
> test_row1
[1] 4.14 4.16 4.20 4.25 4.27 4.33 4.34 4.40 4.44 4.37
> 
  
  SZZZ[seq(1:10)+1,]

ac_test<-function(k=3,ac_data)
{
    #SZZZ_close <- SMA(Cl(SZZZ), 5)
    lsmode<-NULL
    moderes<-NULL
    testdata<-tail(SZZZ_close,k)
    
    for(i in 1:(length(SZZZ_close)-(k+3)))
    {
      tmpmodel<-lsfit(as.numeric(SZZZ_close[seq(i,i+k-1),]),as.numeric(testdata))
      sim_rato<- 1/(cumsum((SZZZ_close[seq(i,i+k-1),]*tmpmodel$coefficients["X"]+tmpmodel$coefficients["Intercept"]- SZZZ_close[seq(i,i+k-1),])^2)[k])^(1/2)
      if(is.null(lsmode))
      {
        index_result<-c(i,as.numeric(SZZZ_close[seq(i,i+k+1),]*tmpmodel$coefficients["X"]+tmpmodel$coefficients["Intercept"]))
        
        lsmode<-c("i"=i,tmpmodel$coefficients,"sim_rato"=sim_rato)
      }
      else
      {
        lsmode<-rbind(lsmode,c("i"=i,tmpmodel$coefficients,"sim_rato"=sim_rato))
        index_result<-rbind(index_result, c(i,as.numeric(SZZZ_close[seq(i,i+k+1),]*tmpmodel$coefficients["X"]+tmpmodel$coefficients["Intercept"])))
      }
    }
    
    #weight<-head(lsmode[order(-lsmode[,"sim_rato"]),],k)[,"sim_rato"]/cumsum(head(lsmode[order(-lsmode[,"sim_rato"]),],k)[,"sim_rato"])[k]
    
    head(lsmode[order(-lsmode[,"sim_rato"]),],10)[,"i"]
    index_result[ head(lsmode[order(-lsmode[,"sim_rato"]),],10)[,"i"],c(k,k+1,k+2)]
    
    #apply(index_result[ head(lsmode[order(-lsmode[,"sim_rato"]),],10)[,"i"],c(k,k+1,k+2)],2,mean)
    apply((index_result[ head(lsmode[order(-lsmode[,"sim_rato"]),],10)[,"i"],c(k,k+1,k+2)])*(  (head(lsmode[order(-lsmode[,"sim_rato"]),],10)[,"sim_rato"])/sum(head(lsmode[order(-lsmode[,"sim_rato"]),],10)[,"sim_rato"])),2,sum)
}

lsmode[which.max(lsmode[,"sim_rato"]),]

weight<-head(lsmode[order(-lsmode[,"sim_rato"]),],k)[,"sim_rato"]/
  
  (head(lsmode[order(-lsmode[,"sim_rato"]),],10)[,"sim_rato"])/sum(head(lsmode[order(-lsmode[,"sim_rato"]),],10)[,"sim_rato"])

apply((index_result[ head(lsmode[order(-lsmode[,"sim_rato"]),],10)[,"i"],c(10,10+1,10+2)])*(  (head(lsmode[order(-lsmode[,"sim_rato"]),],10)[,"sim_rato"])/sum(head(lsmode[order(-lsmode[,"sim_rato"]),],10)[,"sim_rato"])),2,sum)

