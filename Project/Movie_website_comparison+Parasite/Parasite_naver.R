# 기생충 네티즌 영화 분석 
# 네이버 분석
setwd("d:/workspace/R_Crawling/Project/Movie_website_comparison+Parasite/")
library(rvest)
library(stringr)
library(XML)
library(dplyr)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
trim2 <- function(x) gsub("\\n","",x)
trim3 <- function(x) gsub("\\t","",x)
trim4 <- function(x) gsub("\\r","",x)

url <- "https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=161967&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page="
url
html_test <- read_html(paste0(url,ifr,page,1))

rating <- c()
review <- c()
date <- c()

#2929

for (i in 1:2929) {
  if (i %% 500 == 0) {print(i)}
  html <- read_html(paste0(url,i))
  html %>% 
    html_node(".score_result") %>%
    html_nodes("li") -> lis
  
  for (li in lis) {
    rating <- c(rating, html_node(li, '.star_score') %>% html_text('em') %>% trim())
    li %>%
      html_node('.score_reple') %>%
      html_text('p') %>%
      trim() -> tmp
    idx <- str_locate(tmp, "\r")
    review <- c(review, str_sub(tmp, 1, idx[1]-1))
    tmp <- trim(str_sub(tmp, idx[1], -1))
    idx <- str_locate(tmp, "\r")
    writer <- c(writer, str_sub(tmp, 1, idx[1]-1))
    tmp <- trim(str_sub(tmp, idx[1], -1))
    idx <- str_locate(tmp, "\r")
    date <- c(date, str_sub(tmp, 1, idx[1]-1))
    #print(time)
  }
    
}
df.review <- data.frame(rating = rating, user_review = review, date = date)
df_naver.review <- df.review
df_naver.review$user_review
names()
View(df_naver.review)

View(review)


# Word cloud of review words to see what words user used to describe the movie 
library(wordcloud2)
library(KoNLP)
write(review, "naver_review.txt")
data <- readLines('naver_review.txt')
data1 <- sapply(data, extractNoun, USE.NAMES = F)

data3 <- unlist(data1)
data3 <- gsub("\\d+","",data3) ## 숫자 없애기 
data3 <- Filter(function(x) {nchar(x) >= 2}, data3) #2글자 이상 필터 
data4 <- str_replace_all(data3, "[^[:alpha:]]","") #한글, 영어이외는 삭제


txt2 <- readLines("네이버영화gsub.txt")
for(i in 1:length(txt2)) {
  data4 <- gsub(txt2[i],"",data3)
}
length(txt2)


write(data4,"parasite_Naver.txt")
data5 <- read.table("parasite_Naver.txt")
nrow(data5)
wordcount <- table(data5)
wordcloud <-sort(wordcount,decreasing = T)
head(wordcloud)

wordcloud2(wordcloud)





# Plot of user rating and time to see how the user rating went as the movie came out
library(ggplot2)
theme_set(theme_classic()) # change the theme to my preferece 
time.sort <- df_daum.review %>% 
  select(rating,date) %>%
  group_by(date) %>%
  summarise(review_mean = mean(as.numeric(rating)))
df_daum.review$rating <- as.numeric(as.character(df_daum.review$rating))
df_daum.review$time <- substr(df_daum.review$date_time,13,18)
df_daum.review$time
time.sort
class(df_daum.review$date)                  
df_daum.review$date <-as.Date(df_daum.review$date, "%Y.%m.%d")
df_daum.review$time <- as.Time

# line Plot from 2017 way before the movie was released 
theme_set(theme_grey()) # change the theme to my preferece 
ggplot(aes(x = date, y = review_mean), data = time.sort) + geom_line() + geom_vline(xintercept = ps.release.date, linetype="dashed", color = "red")
ps.release.date <- as.Date("05.30.2019","%m.%d.%Y")


# line Plot from 3 days before the movie was released
time.sort.2019 <- time.sort %>%
  filter(date >= "2019-05-27")
ggplot(aes(x = date, y = review_mean), data = time.sort.2019) + 
  geom_line(color = "blue") + 
  geom_vline(xintercept = ps.release.date, linetype="dashed", color = "red") +
  ggtitle("[다음] 개봉 이후 네티즌 평점")
ps.release.date <- as.Date("05.30.2019","%m.%d.%Y")

names(df_daum.review) <- c("rating", "user_review","date_time",  "date"   )
df_daum.review$date2 <- substr(df_daum.review$date,1,10)


# bar graph of time and review
time.sort.release <- time.sort %>%
  filter(date >= "2019-05-30")
ggplopt(time.sort.release,aes())
ggplot(time.sort.release, aes(date,review_mean)) + geom_col()

# bar graph of day and num of people who wrote reviews
time.sort.people <- df_daum.review %>% 
  filter(date >= "2019-05-21") %>%
  select(rating,date,time) %>%
  group_by(date) %>%
  summarise(Count = n(), Rating = mean(rating))
View(time.sort.people)
ggplot(time.sort.people, aes(date,Count,fill=Rating)) + geom_col() + geom_vline(xintercept = ps.release.date, linetype="dashed", color = "red",label = "Release Date") +
  scale_fill_gradient(low="blue", high="red") + ggtitle("Number of Reviews written per day: [Daum Movie]") +
  geom_label(aes(x=ps.release.date, label="Release Date \n 692 People", y=760), colour="black", text=element_text(size=11),fill = "white")
# line plot of time and review
colfunc <- colorRampPalette("blue")

