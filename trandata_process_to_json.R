
library(plyr)
library(jsonlite)

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


data_dataframe2<-as.data.frame(data_dataframe2)

temp_data<-NULL

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
          cell_temp[,c('stock_id','stock_name','open','last_open','close','high','low','amount','volumn','amount1','volumn1','date','time')]
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
      })

stream_out(temp_data, con = file("~/data_dataframe.json",open='at'))


read.csv("~/thefile.txt",sep="\t")

stream_test[stream_test$volumn1<0,]
