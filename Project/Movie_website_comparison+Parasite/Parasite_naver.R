# 기생충 네티즌 영화 분석 
# 네이버 분석
setwd("d:/workspace/R_Crawling/Project/Movie_website_comparison+Parasite/")
library(rvest)
library(stringr)
library(XML)
library(dplyr)
library(extrafont)
windowsFonts()
windowsFonts(malgun = "맑은 고딕")
theme_update(text = element_text(family = "malgun"))


trim <- function (x) gsub("^\\s+|\\s+$", "", x)
trim2 <- function(x) gsub("\\n","",x)
trim3 <- function(x) gsub("\\t","",x)
trim4 <- function(x) gsub("\\r","",x)

url <- "https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=161967&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page="
#url
#html_test <- read_html(paste0(url,ifr,page,1))

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
head(df_naver.review)
View(df_naver.review)

View(review)

# Word cloud of review words to see what words user used to describe the movie 
library(wordcloud2)
library(KoNLP)
write(review, "naver_review1.txt")
data <- readLines('naver_review.txt')
data1 <- sapply(data, extractNoun, USE.NAMES = F)

data3 <- unlist(data1)
data3 <- gsub("\\d+","",data3) ## 숫자 없애기 
data3 <- Filter(function(x) {nchar(x) >= 2}, data3) #2글자 이상 필터 
data4 <- str_replace_all(data3, "[^[:alpha:]]","") #한글, 영어이외는 삭제


txt2 <- readLines("네이버영화gsub.txt")
for(i in 1:length(txt2)) {
  data5 <- gsub(txt2[i],"",data4)
}
txt2[1]
data5 <- gsub("영화","",data5)
data5 <- gsub("관람객","",data5)

write(data5,"parasite_Naver2.txt")
data6 <- read.table("parasite_Naver2.txt")
nrow(data6)
wordcount <- table(data6)
wordcloud <-sort(wordcount,decreasing = T)
head(wordcloud)

wordcloud2(wordcloud)

# Plot of user rating and time to see how the user rating went as the movie came out
library(ggplot2)
#theme_set(theme_classic()) # change the theme to my preferece 

View(df_naver.review)
df_naver.review$rating <- as.numeric(as.character(df_naver.review$rating))
names(df_naver.review) <- c("rating"   ,   "user_review", "date_time"    )
df_naver.review$date <- substr(df_naver.review$date,1,10)
df_naver.review$time <- substr(df_naver.review$date_time,12,16)
df_naver.review$hour <- substr(df_naver.review$date_time,12,13)

View(df_naver.review)
time.sort <- df_naver.review %>% 
  select(rating,date) %>%
  group_by(date) %>%
  summarise(review_mean = mean(as.numeric(rating)))
View(time.sort)
df_naver.review$date <-as.Date(df_naver.review$date, "%Y.%m.%d")

# line Plot of rating per day from release date
naver.p1 <- ggplot(time.sort,aes(x = date, y = review_mean,group = 1)) + 
  geom_point() +
  geom_line(color = "blue") + 
  theme(axis.text.x = element_text(size = 8, angle = 90)) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue")) + 
  ylab("평점") + xlab("")

View(df_naver.review)
# Line graph of time and review
df_naver.review$hour <- substr(df_naver.review$time,1,2)
df_naver.review$time
sort.byTime <- df_naver.review %>% 
  select(rating,time,hour) %>%
  group_by(hour)%>%
  summarise(rating = mean(rating))
class(sort.byTime$hour)
sort.byTime$hour <- as.numeric(sort.byTime$hour)
View(sort.byTime)

naver.p2 <- ggplot(sort.byTime,aes(hour,rating,group=1)) + geom_point() + geom_line(color="red") + 
  ggtitle("[네이버] 개봉 이후 시간별 네티즌 평점 변화 그래프") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

ggplot(time.sort.release, aes(date,review_mean)) + geom_col()

# bar graph of day and num of people who wrote reviews
sort.byPeople <- df_naver.review %>% 
  select(rating,date,time) %>%
  group_by(date) %>%
  summarise(Count = n(), Rating = mean(rating))
View(sort.byPeople)
firstday <- sort.byPeople$Count[1]
firstday
ps.release.date <- as.Date("2019-05-30","%Y-%m-%d")
naver.p3 <- ggplot(sort.byPeople, aes(date,Count,fill=Rating,)) + geom_col() + 
  scale_fill_gradient(low="blue", high="red") + ggtitle("[네이버] 기생충 날짜별 네티즌 리뷰 건수") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue")) +
  geom_vline(xintercept = ps.release.date, linetype="dashed", color = "red",label = "Release Date") +
  geom_label(aes(x=sort.byPeople$date[1], label="개봉일 \n 3340 명", y=3800), colour="black", size = 2.8,fill = "white")

install.packages("xlsx")
library(xlsx)


write.csv(df_naver.review,"네이버평점_df")
write.xlsx(df_naver.review,file = "naver_Parasite.xlsx")

df_naver.review
# bar graph on hour and num of peopel who wrote reviews 
df_naver.review$rating <- as.numeric(as.character(df_naver.review$rating))
names(df_naver.review) <- c("rating"   ,   "user_review", "date_time"    )
df_naver.review$date <- substr(df_naver.review$date,1,10)
df_naver.review$time <- substr(df_naver.review$date_time,12,16)
df_naver.review$hour <- substr(df_naver.review$date_time,12,13)

bar_count_hour_reviews <- df_naver.review %>% 
  group_by(hour) %>%
  summarise(Count = n(),rating = mean(rating))
bar_count_hour_reviews

par(mfrow=c(2,2))
p1
p1 <- ggplot(bar_count_hour_reviews, aes(hour,Count,fill = rating)) + geom_col()+
  scale_fill_gradient(low="blue", high="red") + ggtitle("[네이버] 시간별 리뷰건수")+
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue"))
install.packages("gridExtra")
library(gridExtra)
grid.arrange(p1,p2)
