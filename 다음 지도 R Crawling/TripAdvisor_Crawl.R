#http://localhost:8080/new.html?
  
# Trip Advisor Crawler Program 
library(rvest)
library(dplyr)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


url <-"https://www.tripadvisor.co.kr/RestaurantSearch-g297887-oa0-Daejeon.html#EATERY_OVERVIEW_BOX"
html <- read_html(url)
html_nodes(html,".list") %>% html_text()
html_nodes(html,".ui_columns") 
html_nodes( 
  %>% html_text() %>% trim()
  
