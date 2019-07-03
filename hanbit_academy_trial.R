base_url <- "http://www.hanbit.co.kr/academy/books/category_list.html?"
page <- 'page='
category <-  "&cate_cd=004007&srt=p_pub_date"

#finding the link and list of categories
text <- read_html(base_url)
category_list <- html_nodes(text, ".lnb_area") %>% html_nodes('li')
link <- c()
category_name <- c()
category_list1 <- for(li in category_list) {
  link <- c(link,html_node(category_list,'category_books') %>% html_text())

}

html_category <- category_list %>% 
  html_nodes('li') #%>%
  link <- gsub("<li><a href=","",html_category)
  link <- gsub("<li><a href=\"?http://www.hanbit.co.kr/academy/books/category_list.html?","",html_category)
  link <- gsub("</a></li>","",link)
  category_list1 <-substr(link,1,16)
as.data.frame(category_list1)
catagory_list1 <- gsub("?","&",category_list1)

   for (i in 1:6) {
  
  url <- paste0(base_url,page,i,category)
  
  html <- read_html(url)
  html
  book_list <- html_node(html,'category_books')
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
