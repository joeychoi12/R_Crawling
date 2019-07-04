### Using R Selenium to crawl data from email (Naver.com)

setwd("d:/workspace/R_Crawling/Project/")
install.packages("RSelenium")
library(RSelenium)
library(rvest)
library(stringr)
trim <- function(x) gsub("^\\s+|\\s+$","",x)

remDr <- remoteDriver(remoteServerAddr="localhost", port=4445L, browserName = "chrome")
remDr$open()

remDr$navigate("http://nid.naver.com/nidlogin.login")
txt_id <- remDr$findElement(using="css selector","#id")
txt_pw <- remDr$findElement(using = "id",value = "pw")
login_btn <- remDr$findElement(using = "class", value = "btn_global")

txt_id$setElementAttribute("value","joeychoi12")
txt_pw$setElementAttribute("value","********")
login_btn$clickElement()

remDr$navigate("http://mail.naver.com")
mail_texts <- remDr$findElement(using = "id",value = "list_for_view") 
                                #(using = 'css selector','subject')
mail_texts
mail_texts <- mail_texts$getElementText()
tmp <- str_split(mail_texts, '\n') %>% .[[1]]

sender <- c()
subject <- c()
time <- c()
for (i in 1:15) {
  sender <- c(sender, tmp[4*i-3])
  subject <- c(subject, tmp[4*i-2])
  time <- c(time, tmp[4*i-1])
}
str(sender)
str(time) 
str(subject)


df_mail <- data.frame(sender=sender, subject = subject, time = time) 
View(df_mail)
remDr$close()
write.csv(df_mail,"navermaildb.csv")

#subjects <-unlist(lapply(mail_subjects,function(x){x$getElementText()}))
#subjects