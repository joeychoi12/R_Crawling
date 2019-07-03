#한빛 아카데미 도서 크롤링
install.packages("rvest")
install.packages("RCurl")
install.packages("XML")
library(openxlsx)
library(rvest)
library(dplyr)
library(stringr)
trim <- function(x) gsub("^\\s+|\\s+$","",x)
#category dataframe variables
computer_books <- data.frame()
IT_books <- data.frame()
eng_books <- data.frame()
programming_books <- data.frame()
graphic_design_books <- data.frame()


base_url <- "http://www.hanbit.co.kr/academy/books/category_list.html?"
page <- 'page='
category <-  "&cate_cd=004007&srt=p_pub_date"

for (i in 1:6) {
  
  url <- paste0(base_url,page,i,category)
  
  html <- read_html(url)
  html
  book_list <- html_node(html,'.sub_book_list_area')
  book_list
  lis <- html_nodes(book_list, "li")
  
  html %>% 
    html_node('.sub_book_list_area') %>% 
    html_nodes('li') -> lis
  lis
  
  price <- c()
  title <- c()
  writer <- c()
  for (li in lis) {
    pr <- html_node(li, '.price') %>% html_text()
    pr <- gsub('\\\\',"",pr)
    price <- c(price,pr)
    title <- c(title, html_node(li, '.book_tit') %>% html_text())
    writer <- c(writer,html_node(li, '.book_writer') %>% html_text())
    #cat(title, writer, price, '\n')
  }
  
  books <- data.frame(title = title, writer = writer, price = price)
  computer_books <- rbind.data.frame(computer_books,books)
  
  
}
View(computer_books)

base_url <- "http://www.hanbit.co.kr/academy/books/category_list.html?"
page <- 'page='
category <-  "&cate_cd=004007&srt=p_pub_date"
url <- paste0(base_url,page,'2',category)

html <- read_html(url)
html
book_list <- html_node(html,'.sub_book_list_area')
book_list
lis <- html_nodes(book_list, "li")

html %>% 
  html_node('.sub_book_list_area') %>% 
  html_nodes('li') -> lis
lis

price <- c()
title <- c()
writer <- c()
for (li in lis) {
  pr <- html_node(li, '.price') %>% html_text()
  pr <- gsub('\\\\',"",pr)
  price <- c(price,pr)
  title <- c(title, html_node(li, '.book_tit') %>% html_text())
  writer <- c(writer,html_node(li, '.book_writer') %>% html_text())
  #cat(title, writer, price, '\n')
}

books <- data.frame(title = title, writer = writer, price = price)
c_books <- rbind(books,c_books)
View(books)
View(c_books)


base_url <- "http://www.hanbit.co.kr/academy/books/category_list.html?"
page <- 'page='
category <-  "&cate_cd=004008&srt=p_pub_date"

urltest <- "http://www.hanbit.co.kr/academy/books/category_list.html?page=5&cate_cd=004008&srt=p_pub_date"
html_test <- read_html(urltest)
book_list <- html_node(html_test,'.sub_book_list_area')
book_list
lis <- html_nodes(book_list, "li")
lis

price <- c()
title <- c()
writer <- c()
for (li in lis) {
  pr <- html_node(li, '.price') %>% html_text()
  pr <- gsub('\\\\',"",pr)
  price <- c(price,pr)
  title <- c(title, html_node(li, '.book_tit') %>% html_text())
  writer <- c(writer,html_node(li, '.book_writer') %>% html_text())
  #cat(title, writer, price, '\n')
}


for (i in 1:10) {
  url <- paste0(base_url,page,i,category)
  
  html <- read_html(url)
  html
  book_list <- html_node(html,'.sub_book_list_area')
  book_list
  lis <- html_nodes(book_list, "li")
  
    price <- c()
    title <- c()
    writer <- c()
    for (li in lis) {
      pr <- html_node(li, '.price') %>% html_text()
      pr <- gsub('\\\\',"",pr)
      price <- c(price,pr)
      title <- c(title, html_node(li, '.book_tit') %>% html_text())
      writer <- c(writer,html_node(li, '.book_writer') %>% html_text())
      #cat(title, writer, price, '\n')
  
    }
    if(is.null(price)) {
      break
    }else {
      books <- data.frame(title = title, writer = writer, price = price)
      IT_books <- rbind.data.frame(IT_books,books)
    
    }
  
}



View(computer_books)







price <- c()
title <- c()
writer <- c()
for (li in lis) {
  pr <- html_node(li, '.price') %>% html_text()
  pr <- gsub('\\\\',"",pr)
  price <- c(price,pr)
  title <- c(title, html_node(li, '.book_tit') %>% html_text())
  writer <- c(writer,html_node(li, '.book_writer') %>% html_text())
  #cat(title, writer, price, '\n')
}















