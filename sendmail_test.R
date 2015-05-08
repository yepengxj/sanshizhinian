library("mailR")
library(XML)
tt<-readHTMLTable(htmlParse("http://abcfund.cn/data/realdata.php?p=95",encoding="GBK"), header = T)[[1]]
test_data<-tt[order(-as.numeric(sub("%","",tt[,23])),na.last=T) ,c(1,2,9,16,22,23)]
test_data<-test_data[test_data[,6]!="-",]

html_content<-"<html><table>"

html_content<-paste(html_content,"<tr>",sep = "")
for(head_colname in colnames(test_data))
{
  html_content<-paste(html_content,sprintf("<td>%s</td>",head_colname),sep = "")
}
html_content<-paste(html_content,"</tr>",sep = "")

top5_data<-head(test_data,5)
for(i in 1:dim(top5_data)[1])
{
  
  html_content<-paste(html_content,"<tr>",sep = "")
  for(j in 1:dim(top5_data)[2])
  {
    html_content<-paste(html_content,sprintf("<td>%s</td>",top5_data[i,j]),sep = "")
  }
  html_content<-paste(html_content,"</tr>",sep = "")
}

bot5_data<-tail(test_data,5)
for(i in 1:dim(bot5_data)[1])
{
  
  html_content<-paste(html_content,"<tr>",sep = "")
  for(j in 1:dim(bot5_data)[2])
  {
    html_content<-paste(html_content,sprintf("<td>%s</td>",bot5_data[i,j]),sep = "")
  }
  html_content<-paste(html_content,"</tr>",sep = "")
}
html_content<-paste(html_content,"</table></html>",sep = "")

send.mail(from = "13999806237@139.com",
          to = c("yepeng@xj.chinamobile.com"),
          subject = "Subject of the email",
          body = html_content,
          html = TRUE,
          inline = TRUE,
          encoding="utf-8",
          smtp = list(host.name = "smtp.139.com", port = 25, user.name = "13999806237", passwd = "yepeng@1", ssl = F),
          authenticate = TRUE,
          send = TRUE)

