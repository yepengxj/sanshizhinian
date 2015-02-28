library("e1071")
library(EMD)
library(quantstrat)
library(dtw)
library(plyr)

kmeans<-kmeans(xxx,8)

x<-xxx["2014-12-01",]

ts<-xxx["2008-01-01/",]
result_data_kmeans_svm<-adply(xxx["2011-01-01/2015-02-09",] ,  1, 
                       
  function(x,ts){
    
  end_date_id<-which(.indexDate(ts)==.indexDate(x))
  end_date<-.indexDate(ts)[(end_date_id-1)]
  
  start_end_str<-paste("/",
                       as.Date(end_date,format="%Y-%m-%d",origin = "1970-01-01"),
                       sep="")
  
  print(start_end_str)
  nrows_ts<-nrow(ts[start_end_str])
  
  test_next<-cbind(ts[c(1:(nrows_ts-1)),],as.numeric(ts[as.Date(cbind(.indexDate(ts)[1:(nrows_ts-1)],.indexDate(ts)[2:nrows_ts])[,2],formate="%Y-%M-%d"),"close"]))
  
  kmeans_res<-kmeans(ts[start_end_str],5)
  
  cluster_id<-kmeans_res$cluster[as.character( as.Date(end_date,format="%Y-%m-%d",origin = "1970-01-01"))]
  
  
  svm_cluster3<-svm(as.data.frame(test_next[names(kmeans_res$cluster[kmeans_res$cluster==cluster_id])][,1:5]),as.numeric(test_next[names(kmeans_res$cluster[kmeans_res$cluster==cluster_id])][,6]))
  
  cbind(x,predict(svm_cluster3, x))
  
 
},xxx["2008-01-01/"])


result_data_svm<-adply(xxx["2011-01-01/2015-02-09",] ,  1, 
                              
                              function(x,ts){
                                
                                end_date_id<-which(.indexDate(ts)==.indexDate(x))
                                end_date<-.indexDate(ts)[(end_date_id-1)]
                                
                                start_end_str<-paste("/",
                                                     as.Date(end_date,format="%Y-%m-%d",origin = "1970-01-01"),
                                                     sep="")
                                
                                print(start_end_str)
                                nrows_ts<-nrow(ts[start_end_str])
                                
                                test_next<-cbind(ts[c(1:(nrows_ts-1)),],as.numeric(ts[as.Date(cbind(.indexDate(ts)[1:(nrows_ts-1)],.indexDate(ts)[2:nrows_ts])[,2],formate="%Y-%M-%d"),"close"]))
                                
                                kmeans_res<-kmeans(ts[start_end_str],5)
                                
                                cluster_id<-kmeans_res$cluster[as.character( as.Date(end_date,format="%Y-%m-%d",origin = "1970-01-01"))]
                                
                                
                                svm_cluster3<-svm(as.data.frame(test_next[start_end_str,1:5]),as.numeric(test_next[start_end_str,6]))
                                
                                cbind(x,predict(svm_cluster3, x))
                                
                                
                              },xxx["2008-01-01/"])

plot(result_data_svm[,2],type="l")
lines(result_data_svm[,7],type="l")

s_e_date<-"2014-07-01/2015-02-09"
plot(result_data_svm[s_e_date,2],type="l")
lines(result_data_svm[s_e_date,6],type="l",col="red")

test_next<-cbind(xxx[c(1:5665),],as.numeric(xxx[as.Date(cbind(.indexDate(xxx)[1:5665],.indexDate(xxx)[2:5666])[,2],formate="%Y-%M-%d"),"close"]))

svm_cluster3<-svm(as.data.frame(test_next[names(kmeans$cluster[kmeans$cluster==3])][,1:5]),as.numeric(test_next[names(kmeans$cluster[kmeans$cluster==3])][,6]))

svm_nocluster<-svm(as.data.frame(test_next[,1:5]),as.numeric(test_next[,6]))

predict(svm_nocluster,(xxx["2015-02-09"])