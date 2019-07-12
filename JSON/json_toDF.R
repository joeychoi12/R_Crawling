# JSON 파일로 부터 읽어서 데이터프레임 만들기 
library(jsonlite)
setwd('D:/workspace/R_Crawling/JSON/03_JSON/')

#person.json 파일로 부터 읽기 
wiki_person <- fromJSON("person.json")
str(wiki_person)
class(wiki_person)

#sample.json
data <- fromJSON('sample.json') #JSON 파일 읽기 
str(data)

data <- as.data.frame(data) # Data Frame 으로 변환 
names(data) <- c('id','like','share','comment','unique','msg','time')
data$like <- as.numeric(as.character((data$like)))

#csv file as save
write.csv(data,"data.csv")

#DF to JSON file
json_data <- toJSON(data)
write(json_data, 'data.json')
prettify(json_data)
