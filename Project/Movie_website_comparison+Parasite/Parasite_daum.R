# 기생충 네티즌 영화 분석 
## 다음 분석
setwd("d:/workspace/R_Crawling/Project/Movie_website_comparison+Parasite/")
library(rvest)
library(stringr)
library(XML)
library(dplyr)
library(xlsx)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
trim2 <- function(x) gsub("\\n","",x)
trim3 <- function(x) gsub("\\t","",x)
trim4 <- function(x) gsub("\\r","",x)

url <- "https://movie.daum.net/moviedb/grade?movieId=111292&type=netizen&page="

html_test <- read_html(paste0(url,1))
html_nodes(html_test,".desc_review") %>% html_text()
content_test <- html_node(html_test,".review_info")
grade <- html_nodes(html_test,".emph_grade") %>% html_text()
date <- html_nodes(html_test,".info_append") %>% html_text() %>% trim3() %>% trim4() %>% trim2()

content <- html_nodes(htxt,".review_info") 

daum.grade <- c()
daum.review <- c()
daum.date <- c() 


for (i in 1:625) {
  if (i %% 100 == 0) print(i)
  html <- read_html(paste0(url,i))
  daum.grade <-  c(daum.grade,html_nodes(html,".emph_grade") %>% html_text())
  daum.review <- c(daum.review,html_nodes(html,".desc_review") %>% html_text() %>% trim3() %>% trim4() %>% trim2())
  daum.date <- c(daum.date, html_nodes(html,".info_append") %>% html_text() %>% trim3() %>% trim4() %>% trim2())
}
tail(daum.review)
daum.grade[6250] <- NA
daum.review[6250] <- NA
daum.date[6250] <- NA
str(daum.grade)
str(daum.review) 
str(daum.date)
df_daum.review <-  data.frame(rating = daum.grade, user_review = daum.review, date = daum.date)

write.xlsx(df_daum.review,"다음 기생충 평점.xlsx")
mean(as.numeric(as.character(df_daum.review$rating)))
mean(as.numeric((as.character(daum.grade))))
df_daum.review <- read.xlsx(sheetName = "다음 기생충 평점.xlsx")
View(df_daum.review)
df_daum.review <-read.xlsx("다음 기생충 평점.xlsx", sheetName = "Sheet1",encoding = "UTF-8")

# Word cloud of review words to see what words user used to describe the movie 
library(wordcloud2)
library(KoNLP)
write(daum.review, "daum_review1.txt")
data <- readLines('daum_review1.txt') 
data1 <- sapply(data, extractNoun, USE.NAMES = F)


data3 <- unlist(data1)
data3 <- gsub("\\d+","",data3) ## 숫자 없애기 
data3 <- Filter(function(x) {nchar(x) >= 2}, data3) #2글자 이상 필터 
data4 <- str_replace_all(data3, "[^[:alpha:]]","") #한글, 영어이외는 삭제


txt2 <- readLines("다음영화gsub.txt")
for(i in 1:length(txt2)) {
  data4 <- gsub(txt2[i],"",data3)
}

write(data4,"parasite_daum.txt")
data5 <- read.table("parasite_daum.txt")
nrow(data5)
wordcount <- table(data5)
wordcloud <-sort(wordcount,decreasing = T)
head(wordcloud)

wordcloud2(wordcloud)





# Plot of user rating and time to see how the user rating went as the movie came out
library(ggplot2)
theme_set(theme_classic()) # change the theme to my preferece 
View(df_daum.review)
df_daum.review$rating <- as.numeric(as.character(df_daum.review$rating))
names(df_daum.review)
df_daum.review <- df_daum.review %>%
  select(rating,user_review,date)
names(df_daum.review) <- c("rating"   ,   "user_review", "date_time"  )
df_daum.review$date <- substr(df_daum.review$date_time,1,10)
df_daum.review$time <- substr(df_daum.review$date_time,13,17)
df_daum.review$date <-as.Date(df_daum.review$date, "%Y.%m.%d")
time.sort <- df_daum.review %>% 
  select(rating,date) %>%
  group_by(date) %>%
  summarise(review_mean = mean(as.numeric(rating)))
View(time.sort)
# line Plot from 2017 way before the movie was released 
theme_set(theme_grey()) # change the theme to my preferece 
ggplot(aes(x = date, y = review_mean), data = time.sort) + geom_line(col="blue") + 
  geom_vline(xintercept = ps.release.date, linetype="dashed", color = "red") + 
  ggtitle("[다음]기생충 2017년 부터 네티즌 평가") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue"))
  
  
ps.release.date <- as.Date("05.30.2019","%m.%d.%Y")


# line Plot from 3 days before the movie was released
time.sort.2019 <- time.sort %>%
  filter(date >= "2019-05-26")
daum.p1 <- ggplot(aes(x = date, y = review_mean), data = time.sort.2019) + 
  geom_line(color = "blue") + geom_point() +
  geom_vline(xintercept = ps.release.date, linetype="dashed", color = "red") +
  ggtitle("[Daum] 개봉 이후 네티즌 평점") +   theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue"))+
  geom_label(aes(x=ps.release.date,label="개봉일",y=9.5))
daum.p1
  
ps.release.date <- as.Date("05.30.2019","%m.%d.%Y")



### bar graph of time and review
time.sort.release <- time.sort %>%
  filter(date >= "2019-05-30")
ggplot(time.sort.release, aes(date,review_mean)) + geom_col()

## Line graph of time and review
df_daum.review$hour <- substr(df_daum.review$time,1,2)

#df_daum.review$rating <- as.numeric(as.character(df_daum.review$rating))
sort.byTime <- df_daum.review %>% 
  select(rating,hour) %>%
  group_by(hour)%>%
  summarise(rating = mean(rating))
sort.byTime$hour <- as.numeric(sort.byTime$hour)
class(sort.byTime$hour)
View(sort.byTime)

daum.p2 <- ggplot(sort.byTime,aes(hour,rating,group=1)) + geom_point() + geom_line(color="red") + 
  ggtitle("[Daum] 개봉 이후 시간별 네티즌 평점 변화 그래프") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue"))
daum.p2


# bar graph of day and num of people who wrote reviews
sort.byPeople <- df_daum.review %>% 
  filter(date >= "2019-05-30") %>%
  select(rating,date,time) %>%
  group_by(date) %>%
  summarise(Count = n(), Rating = mean(rating))
View(sort.byPeople)
daum.p3 <- ggplot(sort.byPeople, aes(date,Count,fill=Rating)) + geom_col() + geom_vline(xintercept = ps.release.date, linetype="dashed", color = "red",label = "Release Date") +
  scale_fill_gradient(low="blue", high="red") + ggtitle("Number of Reviews written per day: [Daum Movie]") +
  geom_label(aes(x=ps.release.date, label="Release Date \n 692 People", y=760), colour="black", size = 3,fill = "white")
daum.p3
# line plot of time and review
colfunc <- colorRampPalette("blue")


# bar graph on time and num of peopel who wrote reviews 
sort.people.hour <- time.sort.people.time %>% 
  group_by(as.numerica(hour)) %>%
  summarise(Count = n(), Rating = mean(rating))

hour <- substr(time.sort.people.time$time,1,2)
time.
sort.people.hour
View(time.sort.people.time)

sort.people.time$hour <- as.numeric(substr(sort.people.time$time,1,2))
View(sort.people.time$hour)
sort.people.time <- sort.people.time[2:333,]


ggplot(sort.people.time, aes(hour,Count)) + geom_col() + geom_vline(xintercept = ps.release.date, linetype="dashed", color = "red",label = "Release Date") +
  scale_fill_gradient(low="blue", high="red") + ggtitle("Number of Reviews written per time") +
  geom_text(aes(x=ps.release.date, label="Release Date \n 692 People", y=730), colour="black", text=element_text(size=11))



# bar graph on hour and num of peopel who wrote reviews 
bar_count_hour_reviews <- df_daum.review %>% 
  group_by(hour) %>%
  summarise(Count = n(),rating = mean(rating))
bar_count_hour_reviews
hour <- substr(time.sort.people.time$time,1,2)
time.
sort.people.hour
View(time.sort.people.time)



p2 <- ggplot(bar_count_hour_reviews, aes(hour,Count,fill = rating)) + geom_col()+
  scale_fill_gradient(low="blue", high="red") + ggtitle("[다음] 시간별 리뷰건수")+
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue"))
  