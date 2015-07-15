library(XML)
library("plyr")

#base_data<-htmlParse("http://hbasecon.com/archive.html")
base_data<-htmlParse("~/Desktop/1111.html")
data<-getNodeSet(base_data,"//a")

data_href<-laply(data,xmlGetAttr,"href")
data_href<-data_href[grep('www.slideshare.net',data_href)]
aaply(data_href, 1,function(x){strsplit(x,'/',fixed=T)[[1]][5] })
#add_href<-c("the-most-valuable-customer-on-earth1298-comic-book-analysis-with-oracels-big-data-tools",
#            "sqoop-on-spark-for-dataingestion",
#           "Self-Service Provisioning and Hadoop Management with Apache Ambari_files",
#            "is-olap-dead-in-the-age-of-big-data"
#)

#data_href<-paste0("http://www.slideshare.net/Hadoop_Summit/",add_href)

for(url in data_href)
{
  print(url)
  slide_name<-strsplit(url,'/',fixed=T)[[1]][c(4,5)]
  slide_dir<-paste("~/hbasecon/",slide_name[1],"/",slide_name[2],sep="")
  dir.create(slide_dir,recursive = T)
  tryCatch({slide_data<-htmlParse(url)},
           error = function(e) {
            ""
          },finally = {
            slide_img_list<-getNodeSet(slide_data,"//img[@class='slide_image']")
            
            slide_img<-laply(slide_img_list,xmlGetAttr,"data-full")
            
            i<-0
            for(slid_img_url in slide_img)
            {
              print(slid_img_url)
              try(download.file(slid_img_url, paste(slide_dir,"/",sprintf("%003d", i),".jpg",sep=""), 'wget', quiet = FALSE, mode = "w",cacheOK = TRUE),
                  silent = T)
              i<-i+1
            }
          })
  
}

