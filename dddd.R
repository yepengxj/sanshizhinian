chartSeries(ZSYH, TA="addVo();") 

#只要使用e1071就可以做相应的SVM分析了。

library(e1071)
nrow(ZSYH["2013-06-18/2013-09-02","close"])
nrow(ZSYH["2013-06-20/2013-09-04","close"])
nrow(ZSYH["2013-06-17/2013-09-01","close"] ) 

training<-as.numeric(ZSYH["2013-06-18/2013-09-02","close"] )             ###训练集
training<-cbind(training,as.numeric(ZSYH["2013-06-17/2013-09-01","close"] ) )


testing<-as.numeric(ZSYH["2013-06-25/2013-09-23","close"] )                ###测试集
testing<-cbind(testing,as.numeric(ZSYH["2013-06-24/2013-09-22","close"] ) )

trainingTarget<-as.numeric(ZSYH["2013-06-20/2013-09-04","close"] )        ###训练集的已知结果

#建立SVM

svm<-svm(x=training,y=trainingTarget)

#做预测

pred<-predict(svm,testing)

plot(as.numeric(ZSYH["2013-06-26/2013-09-24","close"] ) , type="l" )
plot(pred, type="l" )
lines(pred,col="blue",type="l")

lines(as.numeric(ZSYH["2013-06-18/2013-09-04","close"]),type="l",col="red")


##cost与gamma是两个参数，我们此时选的kernel默认是sigmoid



#eemd
eemd_res<-eemd(as.numeric(ZSYH["2013-06-25/2013-09-23","close"]))
acf(eemd_res[,1])
acf(eemd_res[,2])
acf(eemd_res[,3])
acf(eemd_res[,4])

library(plyr)
library(Rlibeemd)
eng_con<-aaply(eemd_res,2,function(x){
  acf_x<-acf(x)$acf
  
  (acf_x[1])^2/sum((acf_x)^2)
})

x<-2
con1<-eng_con
aaply( (2:length(eng_con)) , 1 ,function(x,con1){
  
  abs(con1[x]-sum(con1[1:(x-1)])/(x-1))/con1[x]
  
},eng_con)

t<-sd(eemd_res[,1])*(2*log(length(eemd_res[,1])))^2
u<-0.5
ifelse(eemd_res[,1]>t,(1-u)*eemd_res[,1]+u*sign(eemd_res[,1])*(abs(eemd_res[,1]) - t ) ,0)


tt<- seq(from=0,ti=1,by=0.0005)
t<-cos(100*pi*tt)+cos(70*pi*tt)
t_noise<-rnorm(2001, mean = 0, sd = 0.3)
t_s<-t+t_noise
plot(t,type="l")
t_eemd<-eemd(t_s,ensemble_size=100,noise_strength=0.01)
plot(t_eemd)
t_eemd_acf<-aaply(t_eemd,2,function(x){
  acf_x<-acf(x,plot=F)$acf
})

t_eemd_exp<-aaply(t_eemd,2,function(x){
  x_exp<-aaply(0:(length(x)-1),1,function(k,ts,N){
    sum(ts[1:(N-k)]*ts[(k+1):N])/(N-k)
    
  },x,length(x))
  
  x_exp<-x_exp/x_exp[1]
})


t_eemd_con<-aaply(t_eemd,2,function(x){
  acf_x<-acf(x,plot=F,lag.max=length(x))$acf
  
  sum((acf_x[1:50])^2)/sum((acf_x)^2)
})



t_eemd_con_exp<-aaply(t_eemd_exp,1,function(x){
  sum((x[1:50])^2)/sum(x^2)

})

aaply( (2:length(t_eemd_con)) , 1 ,function(x,con1){
  
  abs(con1[x]-sum(con1[1:(x-1)])/(x-1))/con1[x]
  
},t_eemd_con)
0.05334486 0.19030255 3.61326095 0.56037889 2.85505817 0.88495390 0.75063058 1.74947206 0.80067188 
0.09686464 0.03634382 5.24978084 2.08910500 0.64280410 0.08787650 0.05553871 0.81018415 0.73374354 
0.13300208 0.08415758 5.12203531 3.56760216 1.40872282 0.48712338 0.06269417 0.87777370 0.86872730 
0.04084988 0.49484966 4.06570629 0.48980233 1.52199201 0.21339948 0.04838434 0.65789602 0.84670728 
0.11780983 0.01195037 5.08711914 4.03355282 1.20784038 0.17433081 0.31146455 0.31730432 0.90394641 
0.09617564 0.02540952 5.13314440 2.16113952 1.46921770 0.53877354 0.19853021 1.42009322 1.09988478 
0.04000565 0.16282694 4.33640873 3.94166064 1.58507891 0.25423445 0.05033399 0.67967810 1.36900349 


aaply( (2:length(t_eemd_con_exp)) , 1 ,function(x,con1){
  
  abs(con1[x]-sum(con1[1:(x-1)])/(x-1))/con1[x]
  
},t_eemd_con_exp)
0.37827770 1.57178522 7.42119522 0.46511595 1.77170121 0.09319359 0.68940470 0.56874078 0.48133717 
0.0971480 9.6001346 1.4997247 1.8511040 0.2901319 2.2511129 0.5080486 0.6556625 0.3601375 
0.12737230 38.02570644  2.55630005  1.21928520  2.22139431  0.44630992  0.02286901  0.35433477  7.99414009 
1.26676748 17.69742744  1.22740544  3.75374046  1.03337135  0.09415486  1.14867543  0.36573307  2.50776489
0.7457802 13.6440843  1.2542785  0.8993130  0.2589771  1.6291096  0.6444921  0.6330445  7.9121090 
0.1867451 326.1300628   0.8881724   4.2405875   0.4061616   0.3270002   0.4780339   0.2229430   1.7299451 
0.3099380 30.3321581  0.4349960  0.2374430  0.5806982  0.3710375  0.4856997  0.1009427  3.1194402 

t<-sd(t_eemd[,3])*(2*log(length(t_eemd[,3])))^2
u<-0.5
ifelse(t_eemd[,3]>t,(1-u)*t_eemd[,3]+u*sign(t_eemd[,3])*(abs(t_eemd[,3]) - t ) ,0)


x<-t_eemd[,1]
denoise_res<-aaply(t_eemd[,c(1:3)],2,function(x){
  t<-sd(x)*sqrt(2*log(length(x)))
  u<-0.5
  matrix(ifelse(abs(x)>t,(1-u)*x+u*sign(x)*(abs(x) - t ) ,0),ncol=1)

})

plot(rowSums(cbind(t(denoise_res),t_eemd[,c(3:(dim(t_eemd)[2]))])),type="l")

 plot(t(t_eemd_acf[]))
head(t_eemd,10)
plot(t_eemd)


EXP()