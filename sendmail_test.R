library("mailR")
library(XML)
tt<-readHTMLTable(htmlParse("http://abcfund.cn/data/realdata.php?p=95",encoding="GBK"), header = F)[[1]]
test_data<-head(tt[order(-as.numeric(sub("%","",tt[,23])),na.last=T) ,c(1,2,9,16,22,23)],5)

html_content<-"<html><table>"
for(i in 1:dim(test_data)[1])
{
  html_content<-paste(html_content,"<tr>",sep = "")
  for(j in 1:dim(test_data)[2])
  {
    html_content<-paste(html_content,sprintf("<td>%s</td>",test_data[i,j]),sep = "")
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
          smtp = list(host.name = "smtp.139.com", port = 25, user.name = "13999806237", passwd = "yp1111", ssl = F),
          authenticate = TRUE,
          send = TRUE)

