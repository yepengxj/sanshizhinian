library(EMD)
library(quantstrat)


SZZZ <- getSymbols("000001.sz", from = "2002-01-01", to = Sys.Date(), src = "yahoo", 
                   auto.assign = F)

wk<-getSymbols("000001.sz", from = "2000-01-01", to = Sys.Date(), src = "yahoo", 
           auto.assign = F)
ZSYH <- getSymbols("510050.ss", from = "2000-01-01", to = Sys.Date(), src = "yahoo", 
                   auto.assign = F)

testdata<-SMA(Cl(wk), 5)

number2date <-function(date_number)
{
  as.POSIXct(date_number*86400, origin = "1970-01-01", format="%Y-%m-%d", tz = "UTC") 
  
}

ac_test<-function(j=10,his_data,ac_data,pred_length=3)
{
  lsmode<-NULL
  moderes<-NULL
  k<-length(ac_data)
  
  for(i in 1:(length(his_data)-(k+3)))
  {
    tmpmodel<-lsfit(his_data[seq(i,i+k-1)], ac_data )
    sim_rato<- 1/(sum((his_data[seq(i,i+k-1)]*tmpmodel$coefficients["X"]+tmpmodel$coefficients["Intercept"]- his_data[seq(i,i+k-1)])^2))^(1/2)
    if(is.null(lsmode))
    {
      index_result<-c(i,as.numeric(his_data[seq(i,i+k+1)]*tmpmodel$coefficients["X"]+tmpmodel$coefficients["Intercept"]))
      
      lsmode<-c("i"=i,tmpmodel$coefficients,"sim_rato"=sim_rato)
    }
    else
    {
      lsmode<-rbind(lsmode,c("i"=i,tmpmodel$coefficients,"sim_rato"=sim_rato))
      index_result<-rbind(index_result, c(i,as.numeric(his_data[seq(i,i+k+1)]*tmpmodel$coefficients["X"]+tmpmodel$coefficients["Intercept"])))
    }
  }
    
  #print(head(lsmode[order(-lsmode[,"sim_rato"]),],j))
  #print(index_result[ head(lsmode[order(-lsmode[,"sim_rato"]),],j)[,"i"],c(k,k+1,k+2)])
  
  apply((index_result[ head(lsmode[order(-lsmode[,"sim_rato"]),],j)[,"i"],c(k,k+1,k+2)])*(  (head(lsmode[order(-lsmode[,"sim_rato"]),],j)[,"sim_rato"])/sum(head(lsmode[order(-lsmode[,"sim_rato"]),],j)[,"sim_rato"])),2,sum)
}

EMD_test<-function(emd_data,start,k,j)
{
  emd_result<-emd(emd_data)
  prd_result <- NULL
  
  for(i in 1:emd_result$nimf)
  {
    ac_tmp_result<-ac_test(j,emd_result$imf[(seq(start,start+k-1)*-1),i],emd_result$imf[seq(start,start+k-1),i])
    
    if(is.null(prd_result))
    {
      prd_result <- ac_test(j,emd_result$imf[(seq(start,start+k-1)*-1),i],emd_result$imf[seq(start,start+k-1),i])
    }
    else
    {
      prd_result <- rbind(prd_result,ac_test(j,emd_result$imf[(seq(start,start+k-1)*-1),i],emd_result$imf[seq(start,start+k-1),i]))
    }
  }
  
  if(is.null(prd_result))
  {
    prd_result <- ac_test(j,emd_result$residue[(seq(start,start+k-1)*-1)],emd_result$residue[seq(start,start+k-1)])
  }
  else
  {
    prd_result <- rbind(prd_result,ac_test(j,emd_result$residue[(seq(start,start+k-1)*-1)],emd_result$residue[seq(start,start+k-1)]))
  }
  print(prd_result)
  apply(prd_result,2,sum)
}

EMD_test_by_step<-function(emd_data,k,j)
{
  emd_result<-emd(emd_data)
  prd_result <- NULL
  
  for(i in 1:emd_result$nimf)
  {    
    if(is.null(prd_result))
    {
      prd_result <- ac_test(j,emd_result$imf[seq(1,length(emd_result$imf[,i])-k),i],tail(emd_result$imf[,i],k))
    }
    else
    {
      prd_result <- rbind(prd_result,ac_test(j,emd_result$imf[seq(1,length(emd_result$imf[,i])-k),i],tail(emd_result$imf[,i],k)))
    }
  }
  
  if(is.null(prd_result))
  {
    prd_result <- ac_test(j,
                          emd_result$residue[seq(1,length(emd_result$residue)-k)],
                          tail(emd_result$residue,k))
  }
  else
  {
    prd_result <- rbind(prd_result,
                        ac_test(j,emd_result$residue[seq(1,length(emd_result$residue)-k)],
                                tail(emd_result$residue,k)))
  }
  print(prd_result)
  apply(prd_result,2,sum)
}

for(k in 3:10)
{
  for(j in 3:10)
  {
    print(k)
    print(j)
    print(EMD_test(as.numeric(SZZZ_close[c(-1,-2,-3,-4,-5)]),k,j))
  }
}

number2date <-function(date_number)
{
   as.POSIXct(date_number*86400, origin = "1970-01-01", format="%Y-%m-%d", tz = "UTC") 

}

as.POSIXct(as.numeric(.indexDate(ZSYH))*86400, origin = "1970-01-01", format="%Y-%m-%d", tz = "UTC") )

as.POSIXct(as.numeric(.indexDate(SZZZ_close[c(-1,-2,-3,-4,-5)][1]))*86400, origin = "1970-01-01", format="%Y-%m-%d", tz = "UTC")

SZZZ_close[paste("/",as.POSIXct(as.numeric(.indexDate(SZZZ_close[c(-1,-2,-3,-4,-5)][10]))*86400, origin = "1970-01-01", format="%Y-%m-%d", tz = "UTC"),sep="")]

SZZZ_close[!is.na(SZZZ_close)]

SZZZ_close["2014-08-15/2014-08-20"]

as.numeric(.indexDate(SZZZ_close[as.integer(length(SZZZ_close)*0.75)]))*86400

number2date(as.numeric(.indexDate(SZZZ_close[as.integer(length(SZZZ_close)*0.75)])))


testdata<-testdata[!is.na(testdata)]
data_length <-length(testdata)
sample_tag <-as.integer(data_length*0.99)
pred_result_ts<-NULL

for(i in sample_tag:data_length)
{
  i_date<-number2date(as.numeric(.indexDate(testdata[i])))
  
  EMD_res<-EMD_test_by_step(as.numeric(testdata[paste("/",i_date,sep="")]),8,5)
  if(is.null(pred_result_ts))
  {
    pred_result_ts<-data.frame("date"=i_date,"pred_day1"=EMD_res[1],"pred_day2"=EMD_res[2],"pred_day3"=EMD_res[3])
  }
  else
  {
    pred_result_ts<-rbind(pred_result_ts,data.frame("date"=i_date,"pred_day1"=EMD_res[1],"pred_day2"=EMD_res[2],"pred_day3"=EMD_res[3]))
  }
  print( data.frame("date"=i_date,"pred_day1"=EMD_res[1],"pred_day2"=EMD_res[2],"pred_day3"=EMD_res[3]))
}

rownames(pred_result_ts)<-pred_result_ts[,"date"]
pred_result_ts<-as.xts(pred_result_ts[,c(2,3,4)])
tmp_xts<-as.xts(data.frame(row.names=number2date(as.numeric(.indexDate(pred_result_ts))+1),as.numeric(pred_result_ts[,1])))
plot(tmp_xts["2014-01-01/",1])
#lines(Cl(SZZZ["2014-01-01/2014-09-30"]),col="green") 
lines(SMA(Cl(wk["2014-01-01/"]),5),col="red")

tmp_xts<-as.xts(data.frame(row.names=number2date(as.numeric(.indexDate(pred_result_ts))+2),as.numeric(pred_result_ts[,2])))
lines(tmp_xts["2014-01-01/",1],col="orange")

tmp_xts<-as.xts(data.frame(row.names=number2date(as.numeric(.indexDate(pred_result_ts))+3),as.numeric(pred_result_ts[,3])))
lines(tmp_xts["2014-01-01/",1],col="blue")


wk$ma5<-SMA(Cl(wk), 5)

wk$BIAS<-(wk$X000001.SZ.Close-wk$ma5)/wk$ma5*100


merge( ((pred_result_ts$pred_day2 > pred_result_ts$pred_day1) & 
  (pred_result_ts$pred_day3 > pred_result_ts$pred_day2)),
  as.xts(data.frame(wk$BIAS) ) < -3,join="inner")

as.numeric(testdata)

testdata_matrix<-data.frame(testdata)
rownames(testdata_matrix)<-as.POSIXct(as.numeric(.indexDate(testdata))*86400, origin = "1970-01-01", format="%Y-%m-%d", tz = "UTC") 
as.xtx(testdata_matrix)
str(as.xts(testdata_matrix))
merge(as.xts(testdata_matrix),pred_result_ts,join="right")[,c(1,2)]
plot(merge(as.xts(testdata_matrix),pred_result_ts,join="right")[,c(1,2)])
> plot(merge(as.xts(testdata_matrix),pred_result_ts,join="right")[,2])
> lines(merge(as.xts(testdata_matrix),pred_result_ts,join="right")[,1])

plot(Cl(SZZZ["2014-08-01/2014-09-30"]))

lines(SMA(Cl(SZZZ["2014-08-01/2014-09-30"])))
lines(pred_result_ts[,1])
SZZZ$ma5<-SMA(Cl(SZZZ), 5)
SZZZ$BIAS<-(SZZZ$X000001.SS.Close-SZZZ$ma5)/SZZZ$ma5*100

str(pred_result_ts)
View(data.frame(SZZZ))
