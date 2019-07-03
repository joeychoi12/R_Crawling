# Text Crawling from Investing.com News article
# 한빛아카데미 도서 크롤링
setwd("d:/workspace/R_Crawling/")
library(rvest)
library(dplyr)
library(stringr)
library(openxlsx)
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

base_url <- 'https://kr.investing.com/news/economy-news/'

url <- paste0(base_url, 1)

html <- read_html(url)
html1 <- html_nodes(html,".wrapper");html1
title <- trim(html_nodes(html1, ".textDiv") %>% html_text());title
article <- c()
df.article <- data.frame()
for (i in 1:10) {
  url <- paste0(base_url, i)
  html <- read_html(url)
  
  html <- read_html(url)
  html1 <- html_nodes(html,".wrapper");html1
  title <- trim(html_nodes(html1, ".textDiv") %>% html_text())
  article <- c(article,title)
  
}

#View(article)


#Wordcloud
library(KoNLP)
write(article, "data.txt")
data <- readLines('data.txt') 
data1 <- sapply(data, extractNoun, USE.NAMES = F)

data3 <- unlist(data1)
data3 <- gsub("\\d+","",data3) ## 숫자 없애기 
data3 <- gsub("서울시","",data3)
data3 <- gsub("서울","",data3)
data3 <- gsub("곡성","",data3)
data3 <- gsub("세종","",data3)
data3 <- gsub("-","",data3)
data3 <- gsub("부터","",data3)
data3 <- gsub("전","",data3)
data3 <- gsub("일","",data3)
data3 <- gsub("시간","",data3)
data3 <- gsub("월","",data3)
data3 <- gsub("뉴스핌<U+A><U+A>","",data3)
data3 <- gsub("기","",data3)
data3 <- gsub("원","",data3)
data3 <- gsub("한","",data3)
data3 <- gsub(".","",data3)

write(data3,"economy1.txt")
data4 <- read.table("economy1.txt")
nrow(data4)
wordcount <- table(data4)
wordcloud <- head(sort(wordcount,decreasing = T),800)
head(wordcloud)
