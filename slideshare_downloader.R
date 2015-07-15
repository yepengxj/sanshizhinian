library(XML)
library("plyr")

#base_data<-htmlParse("~/Desktop/Untitled Document.html")

#data<-getNodeSet(base_data,"//span[@class='slideslink']/a")

#data_href<-laply(data,xmlGetAttr,"href")

#strsplit(data_href,'/',fixed=T)
add_href<-c("the-most-valuable-customer-on-earth1298-comic-book-analysis-with-oracels-big-data-tools",
  "sqoop-on-spark-for-dataingestion",
  "Self-Service Provisioning and Hadoop Management with Apache Ambari_files",
  "is-olap-dead-in-the-age-of-big-data"
)
data_href<-paste0("http://www.slideshare.net/Hadoop_Summit/",add_href)
for(url in data_href)
{
  print(url)
  slide_name<-strsplit(url,'/',fixed=T)[[1]][5]
  slide_dir<-paste("~/hadoop2015/",slide_name,sep="")
  dir.create(slide_dir)
  slide_data<-htmlParse(url)
  slide_img_list<-getNodeSet(slide_data,"//img[@class='slide_image']")
  
  slide_img<-laply(slide_img_list,xmlGetAttr,"data-normal")
  
  i<-0
  for(slid_img_url in slide_img)
  {
    print(slid_img_url)
    try(download.file(slid_img_url, paste(slide_dir,"/",sprintf("%003d", i),".jpg",sep=""), 'wget', quiet = FALSE, mode = "w",cacheOK = TRUE),
        silent = T)
    i<-i+1
  }
}

