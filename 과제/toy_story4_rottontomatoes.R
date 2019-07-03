#Rotten tomatoes Toy Story 4 Review 
library(rvest)
library(dplyr)
library(stringr)
library(XML)
library(RCurl)
library(openxlsx)

trim <- function(x) gsub("^\\s+|\\s+$",x)

#url 자료 확인
url.exists("https://www.rottentomatoes.com/m/toy_story_4/reviews/")

base_url <- "https://www.rottentomatoes.com/m/toy_story_4/reviews/"
page <- "?page="

wb <- createWorkbook()

for (i in 1:36) {
  df_reviews <- data.frame(name = c(), rating = c(), desc = c())
  
for (i in 1:36)  
  url <- paste0(base_url,page,i)
  
  html <- read_html(url)
  html %>% 
    html_nodes('body_main container')
  
  
}

url <- paste0(base_url,page,1)

html <- read_html(url)
View(html)

html1 <- html_nodes(html,".body");html1

html %>% 
  html_nodes('.the_review') %>% 
  html_text(".the_review")
