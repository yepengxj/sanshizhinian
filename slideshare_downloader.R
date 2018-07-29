library(XML)
library("plyr")
library(RCurl)

#base_data<-htmlParse("~/Desktop/Untitled Document.html")

#data<-getNodeSet(base_data,"//span[@class='slideslink']/a")

#data_href<-laply(data,xmlGetAttr,"href")

#strsplit(data_href,'/',fixed=T)
add_href<-c("https://www.slideshare.net/econsultancy/digital-transformation-ashley-friedlein-ceo-econsultancy",
            "https://www.slideshare.net/tsachtje/how-to-digital-transformation-for-marketing"
)
data_href<-c("https://www.slideshare.net/ISMeCompany/masterclass-online-marketingstrategie"
)
#data_href<-paste0("http://www.slideshare.net/Hadoop_Summit/",add_href)
for(url in data_href)
{
  print(url)
  slide_name<-strsplit(url,'/',fixed=T)[[1]][5]
  slide_dir<-paste("~/digitaltransform/",slide_name,sep="")
  dir.create(slide_dir)
  xData <- getURL(url)
  slide_data<-htmlParse(xData)
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
}
