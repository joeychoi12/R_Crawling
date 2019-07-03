setwd("d:/workspace/R_Crawling/")
library(rvest)
library(stringr)
library(dplyr)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

url <- 'https://www.metacritic.com/movie/toy-story-4/critic-reviews'
#url <- paste(url_base,encoding="euc-kr",sep='')
html <- read_html(url)
head(html)
html

html1 <- html_nodes(html, '.summary')
html2 <- html_nodes(html, '.title'); html2
html3 <- html_nodes(html, ".left");html3
html1
author <- html_nodes(html2, '.author') %>% html_text()
score <- html_nodes(html3, '.metascore_w') %>% html_text()
summary <- html_nodes(html1, '.no_hover')%>% html_text();summary
author
score <- score[2:58]
str(score)
str(author)
str(summary)
summary
score[32] 
author2 <- c(author[1:31],NA,author[32:56])
summary[32] <- NULL 
author[32] 
metacritic <- data.frame(writer = author2, score = score, review = summary)
View(metacritic)

write.csv(metacritic,"Toy Story 4_Metacritic.csv")


#create word cloud for the reviews of Toy Story in metacritic 
install.packages("wordcloud2")
library(tm)
library(wordcloud2)
data <-read.csv('Toy Story 4_Metacritic.csv')
str(data)
data <- data %>% select(review)
data <- as.matrix(data)
View(data)
write(data, "data.txt")
data1 <-readLines('data.txt')
data1
docs <- Corpus(VectorSource(data1))
inspect(docs)

toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ",x))
docs <- tm_map(docs,toSpace,'/')
docs <- tm_map(docs, toSpace,"@")
docs <- tm_map(docs, toSpace,"\\|")
docs <- gsub("Toy", "",docs)
docs <- gsub("Story", "", docs)
docs <- gsub("'s", "", docs)
docs <- gsub("film", "",docs)
docs <- gsub("series", "", docs)
docs <- gsub("movie", "", docs)
docs <- gsub("Pixar", "",docs)

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)



dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

install.packages("wordcloud")
library(wordcloud)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

wordcloud2(d)
