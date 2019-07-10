library(rvest)
library(stringr)
library(XML)
library(dplyr)
library(extrafont)
library(wordcloud)
library(wordcloud2)
library(rJava)
windowsFonts()
library(RSelenium)
windowsFonts(malgun = "맑은 고딕")
#theme_update(text = element_text(family = "malgun"))

trim <- function (x) gsub("^\\s+|\\s+$", "", x)


url <- 











url <- "https://map.kakao.com/?from=total&nil_suggest=btn&tab=place&q=%EB%8C%80%EC%A0%84+%EB%A7%9B%EC%A7%91"
review <- c()
date <- c()
read_html(url)
html <- read_html(url)
url

html <- html_node(html,".Info")
html_node(html,".header") 
html2 <-html_nodes(html,".section")
html2
html_nodes(html,".places") %>% html_text()
html_node(html2,"head_item")
html2 <- html_node(html,".placelist")
html_nodes(html2,"li")
html_nodes(html,".restaurants-list-ListCell__nameBlock--1hL7F") %>% html_text()
View(html)





url <- "https://map.kakao.com/?from=total&sw=&oldw=&nil_suggest=btn&q=%EB%8C%80%EC%A0%84%20%EB%A7%9B%EC%A7%91&page=1&cidx=1#topPosition"
html <- read_html(url)
html1 <- html_nodes(html,".section")
lis <- html_nodes(html1,"li")
html_node(html2,"PlaceItem clickArea")
html_node(lis,".PlaceItem")


for (li in lis) {
  star_score <- html_node(li, ".utile_item")
}
star_score

remDr<-remoteDriver(remoteServerAddr="myIP", port=4445L, browserName="chrome")
remDr$open()

remDr <- remoteDriver(remoteServerAddr="localhost", port=4445L, browserName = "chrome")
remDr$open()
checkForServer()

docker pull selenium/standalone-chrome
