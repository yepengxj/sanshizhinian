library(plyr)

#sort data.txt | uniq > data_uniq.txt
cur_time<-as.character(Sys.time())
sub_test<-iconv(readLines("http://hq.sinajs.cn/list=sh510050,sz150097,sh601633,sz002292,sz150182,sz150131"),
      "GB2312","UTF-8")

iconv(readLines("http://hq.sinajs.cn/list=sh510050,sz150097,sh601633,sz002292,sz150182,sz150131"),
      "GB2312","UTF-8")

iconv(readLines(curl("http://hq.sinajs.cn/list=sh510050,sz150097,sh601633,sz002292,sz150182,sz150131"))   ,   "GB2312","UTF-8")

sub_test<-gsub('\\"',"",(sub_test))
sub_test<-gsub('hq_str_',"",(sub_test))
data_split<-strsplit(gsub('\\"',"",(sub_test)),"[ =,/\\]")

data_split<-data_split[laply(data_split,length)==35]
data_dataframe<-laply(data_split,function(x){ as.matrix(x,nrow=1)})
colnames(data_dataframe)<-c('var','stock_id','stock_name','open','last_open','close','high','low','now_buy','now_sell','amount','volumn','buy1_amount','buy1_price','buy2_amount','buy2_price','buy3_amount','buy3_price','buy4_amount','buy4_price','buy5_amount','buy5_price','sell1_amount','sell1_price','sell2_amount','sell2_price','sell3_amount','sell3_price','sell4_amount','sell4_price','sell5_amount','sell5_price','date','time','00')
data_dataframe<-cbind(data_dataframe,rep(cur_time,dim(data_dataframe)[1]))
data_dataframe<-as.data.frame(data_dataframe)


strptime(data_dataframe[,'time'], format = "%H:%M:%S")  + 60

data_dataframe[,2:34]

 
rownames(data_dataframe)<-NULL
colnames(data_dataframe)<-c('var','stock_id','stock_name','open','last_open','close','high','low','now_buy','now_sell','amount','volumn','buy1_amount','buy1_price','buy2_amount','buy2_price','buy3_amount','buy3_price','buy4_amount','buy4_price','buy5_amount','buy5_price','sell1_amount','sell1_price','sell2_amount','sell2_price','sell3_amount','sell3_price','sell4_amount','sell4_price','sell5_amount','sell5_price','date','time','00')
data_dataframe<-data_dataframe[!data_dataframe$open==0,]
View(data_dataframe)


jsonlite::toJSON(data_dataframe)


as.data.frame(strsplit(gsub('\\"',"",unlist(sub_test)),"[ =,/\\]"))

library(plyr)

sub_test1<-readLines("~/data_uniq.txt", n = -1)

sub_test1<-gsub('\\"',"",(sub_test1))
sub_test1<-gsub('hq_str_',"",(sub_test1))
data_split1<-strsplit(gsub('\\"',"",(sub_test1)),"[ =,/\\]")

data_split1<-data_split1[laply(data_split1,length)==35]
data_dataframe1<-laply(data_split1,function(x){ as.matrix(x,nrow=1)})

rownames(data_dataframe1)<-NULL
colnames(data_dataframe1)<-c('var','stock_id','stock_name','open','last_open','close','high','low','now_buy','now_sell','amount','volumn','buy1_amount','buy1_price','buy2_amount','buy2_price','buy3_amount','buy3_price','buy4_amount','buy4_price','buy5_amount','buy5_price','sell1_amount','sell1_price','sell2_amount','sell2_price','sell3_amount','sell3_price','sell4_amount','sell4_price','sell5_amount','sell5_price','date','time','00')

data_dataframe2<-data_dataframe1[data_dataframe1[,'date']!='2015-01-30',]
data_dataframe2<-data_dataframe2[data_dataframe2[,'date']!='2015-01-29',]
data_dataframe2<-data_dataframe2[data_dataframe2[,'date']!='2015-02-03',]
data_dataframe2<-data_dataframe2[data_dataframe2[,'close']!='0',]



aaply(data_dataframe2[,'time'],1,function(x){
  minute<- as.integer(strsplit(x,':',fixed=T)[[1]][2])
  
  print(minute%%5)
  
  if( (minute%%5) == 4 )
  {
    return (as.character(trunc(strptime(x, format = "%H:%M:%S") + 60 , "mins")))
  }
  
  if( (minute%%5) == 3 )
  {
    return (as.character(trunc(strptime(x, format = "%H:%M:%S") + 120 , "mins")))
  }
  
  if( (minute%%5) == 1 )
  {
    return (as.character(trunc(strptime(x, format = "%H:%M:%S") - 60 , "mins")))
  }
  
  if( (minute%%5) == 2 )
  {
    return (as.character(trunc(strptime(x, format = "%H:%M:%S") - 120 , "mins")))
  }
  
  if( (minute%%5) == 0 )
  {
    return (as.character(trunc(strptime(x, format = "%H:%M:%S") , "mins")))
  }
  
})


test1<-aaply(data_dataframe2[,c('date','time')],1,function(x){
  minute<- as.integer(strsplit(x[2],':',fixed=T)[[1]][2])
  print(minute%%5)
  return(c(minute%%5,x))
  
})

temp_data<-NULL

data_dataframe2<-as.data.frame(data_dataframe2)

library(doParallel)
cl <- makeCluster(4,outfile="~/temp/test.log")
registerDoParallel(cl)
clusterExport(cl,"data_dataframe2")
clusterExport(cl,"temp_data")
ddply(data_dataframe2,.(stock_id,date),
         function(per_stock_date){
           temp1<-per_stock_date
           if(dim(temp1)[1]>1)
           {
             temp1<-temp1[order(strptime(temp1[,'time'],format='%H:%M:%S')),]
           }
           
           
           temp1<-adply(1:(dim(temp1)[1]) ,1,function(x,org_data){
             cell_temp<-org_data[x,]
             if(x==1)
             {
               cell1<- as.character(as.numeric(org_data[x,c('volumn','amount')]))
             }
             else
             {
               cell1<-as.character(as.numeric(org_data[x,c('volumn','amount')])
                                   -as.numeric(org_data[(x-1),c('volumn','amount')]))
             }
             cell_temp$volumn1<-cell1[1]
             cell_temp$amount1<-cell1[2]
             cell_temp
           },temp1)
           
           if(is.null(dim(temp_data)))
           {
             assign('temp_data',temp1,envir = .GlobalEnv)
           }
           else
           {
             assign('temp_data',rbind(temp_data,temp1),envir = .GlobalEnv)
           }
           print(dim(per_stock_date))
},.parallel=T)
         
  temp_data<-NULL
  ddply(data_dataframe2,'stock_id',function(per_stock){
    ddply( per_stock,'date',
           function(per_stock_date){
             print(sprintf("data:%s,stock_id:%s",date,stock_id))
             temp1<-data_dataframe2[data_dataframe2[,'stock_id']==stock_id &
                                      data_dataframe2[,'date']==date,,drop=F]
             print('order')
             if(dim(temp1)[1]>1)
             {
               temp1<-temp1[order(strptime(temp1[,'time'],format='%H:%M:%S')),]
             }
             

             print(dim(temp1))
             
             
             print(dim(temp_data))
           },stock_id)
    print(dim(data_dataframe2[data_dataframe2[,'stock_id']==stock_id,,drop=F]))
  })

  test1<-data_dataframe2[data_dataframe2[,'stock_id']=='sh510010' &
                    data_dataframe2[,'date']=='2015-03-16',,drop=F]
  test1<-View(test1[order(strptime(test1[,'time'],format='%H:%M:%S')),])
View(data_dataframe2[data_dataframe2[,'date']=='2014-09-03',])

order(strptime(test1[,'time'],format='%H:%M:%S'))

aaply( 1:100 ,1,function(x,org_data){
  
  if(x==1)
  {
    print(org_data[x,c('volumn','amount')])
  }
  else
  {
    print(as.character(as.numeric(org_data[x,c('volumn','amount')])
    -as.numeric(org_data[(x-1),c('volumn','amount')])))
  }
  
},data_dataframe2)



round( strptime(sz150182[1:10,'time'] , format = "%H:%M:%S"), "mins")
round( strptime(head(sz150182,10)$time, format = "%H:%M:%S") + 60, "mins")

as.integer(strsplit("08:59:57",':',fixed=T)[[1]][2])

trunc(  strptime(head(sz150182,10)$time, format = "%H:%M:%S") + 120 , "mins")

sz150182_xts<-as.xts(sz150182,order.by=as.POSIXlt(paste(sz150182[,"date"],sz150182[,"time"],sep=" ")))

View(sz150182_xts)

strsplit(as.character( data_dataframe$time),':',fixed=T)

str(data_dataframe$time)
library(curl)
library(jsonlite)
diamonds2 <- stream_in(curl("https://jeroenooms.github.io/data/diamonds.json"))


stream_out(data_dataframe, con = file("~/data_dataframe.json",open='at'))
