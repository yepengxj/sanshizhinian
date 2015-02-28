
#add current data
#http://hq.sinajs.cn/list=sh510050
#var hq_str_sh510050="50ETF,1.617,1.617,1.615,1.621,1.613,1.615,1.616,319048260,515749671,800800,1.615,2287558,1.614,1073667,1.613,530600,1.612,348100,1.611,1221400,1.616,1558800,1.617,660900,1.618,276600,1.619,1359000,1.620,2014-08-27,15:03:02,00";
#strsplit
for( i in length(readLines("http://hq.sinajs.cn/list=sh510050,sh000001")) )
{
  write.csv( strsplit(iconv(readLines("http://hq.sinajs.cn/list=sh510050,sh000001"),"GB2312","UTF-8"),",")
             ,file = "~/data.txt", row.names = F, quote = F) 
}

write.table( iconv(readLines("http://hq.sinajs.cn/list=sh510050,sh000001"),"GB2312","UTF-8")
             ,file = "~/data.txt",col.names = F, row.names = F, quote = F,append=T) 

readLines("http://hq.sinajs.cn/list=sh510050,sh000001")[i]
length(readLines("http://hq.sinajs.cn/list=sh510050,sh000001"))
strsplit(readLines("http://hq.sinajs.cn/list=sh510050,sh000001")[2],",")
iconv(readLines("http://hq.sinajs.cn/list=sh510050,sh000001"),"GB2312","UTF-8")
cur_data_str<-strsplit(readLines("http://hq.sinajs.cn/list=sh510050,sh000001")[[1]],";")
as.xts(matrix(c(as.numeric(cur_data_str[[1]][2]),as.numeric(cur_data_str[[1]][5]),as.numeric(cur_data_str[[1]][6]),as.numeric(cur_data_str[[1]][4]),as.numeric(cur_data_str[[1]][9]),as.numeric(cur_data_str[[1]][4])),nrow=1),as.Date(cur_data_str[[1]][31]))
tail(rbind(SZ50,as.xts(matrix(c(as.numeric(temp2[[1]][2]),as.numeric(temp2[[1]][5]),as.numeric(temp2[[1]][6]),as.numeric(temp2[[1]][4]),as.numeric(temp2[[1]][9]),as.numeric(temp2[[1]][4])),nrow=1),as.Date(temp2[[1]][31]))))

SZ50<-rbind(SZ50,as.xts(matrix(c(as.numeric(temp2[[1]][2]),as.numeric(temp2[[1]][5]),as.numeric(temp2[[1]][6]),as.numeric(temp2[[1]][4]),as.numeric(temp2[[1]][9]),as.numeric(temp2[[1]][4])),nrow=1),as.Date(temp2[[1]][31])))
