# Text Crawling from Investing.com News article
setwd("d:/workspace/R_Crawling/")
library(rvest)
library(dplyr)
library(stringr)
library(openxlsx)
library(wordcloud2)
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

base_url <- 'https://kr.investing.com/news/'

url <- paste0(base_url, 1)
category <- c('economy-news/','stock-market-news/','economic-indicators/',
             'commodities-news/','forex-news/','cryptocurrency-news/')

df <- c()
article <- c()

for (i in 1:6) {
  df.books <- data.frame()
  for (j in 1:10) {
    url <- paste0(base_url,category[i], j)
    html <- read_html(url)
    html1 <- html_nodes(html,".wrapper");html1
    title <- trim(html_nodes(html1, ".textDiv") %>% html_text())
    article <- c(article,title)
    
  }
  books <- data.frame(aritcle = article)
  df.books <- rbind(df.books,books)
}

View(df.books)

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
data3 <- gsub("\\.","",data3)
data3 <- gsub("\\[]","",data3)
data3 <- Filter(function(x) {nchar(x) >= 2}, data3) #2글자 이상 필터 
data4 <- str_replace_all(data3, "[^[:alpha:]]","") #한글, 영어이외는 삭제
data4 <- gsub("UAUA","",data4)


txt2 <- readLines("경제gsub.txt")
for(i in 1:length(txt2)) {
  data3 <- gsub(txt2[i],"",data3)
}



write(data4,"economy.txt")
data5 <- read.table("economy.txt")
nrow(data5)
wordcount <- table(data5)
wordcloud <-sort(wordcount,decreasing = T)
head(wordcloud)

wordcloud2(wordcloud)

