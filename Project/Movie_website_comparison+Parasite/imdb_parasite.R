setwd("c:/Users/Joey/Documents/R")
library(rvest)
library(stringr)
library(dplyr)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

url <- 'https://www.imdb.com/title/tt6751668/reviews?ref_=tt_urv'
html <- read_html(url)
head(html)

html1 <- html_nodes(html,".text") %>% html_text() %>% trim();html1
score <- html_nodes(html3, '.metascore_w') %>% html_text()

metacritic <- data.frame(writer = author2, score = score, review = summary)
View(metacritic)


#create word cloud for the reviews of Toy Story in metacritic 
write(html1, "data.txt")
data <- readLines('data.txt')
data<- data %>%
  str_remove("\\n") %>%                   # remove linebreaks
  removeWords(stopwords("english")) %>%   # Remove common words (a, the, it etc.)
  removeWords(c("amp"))  
data <- gsub("film","",data)
data <- gsub("The","",data)
data <- gsub("movie","",data)
data <- gsub("can","",data)
data <- gsub("will","",data)
data <- gsub("one","",data)
data <- gsub("�","",data)
data <- gsub("get","",data)
data <- gsub("��","",data)


txt <- readLines("RTPgsub.txt") 
for(i in 1:length(txt)) {
  data <-gsub((txt[i]), "", data)     
}
data <- Filter(function(x) {nchar(x) >= 2}, data) #2글자 이상 필터 

Sys.setlocale("LC_ALL","English")
Sys.getlocale()

textCorpus <- 
  Corpus(VectorSource(data)) %>%
  TermDocumentMatrix() %>%
  as.matrix()

textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
pie(head(textCorpus,20),main="RTP Top 20 words",radius=1)
head(textCorpus,10)
textCorpus <- data.frame(word = names(textCorpus), freq=textCorpus, row.names = NULL)

library(wordcloud2)  
wordcloud <- wordcloud2(data = textCorpus, minRotation = 0, maxRotation = 0, ellipticity = 0.6)
wordcloud
