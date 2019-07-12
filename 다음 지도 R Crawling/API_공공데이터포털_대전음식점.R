# 공공데이터포털 API 이용하여 데이터 가져오기
# 대전광역시 문화관광 음식점 조회 서비스   

library(jsonlite)

base_url <- "http://apis.data.go.kr/6300000/tourFoodDataService/tourFoodDataListJson"
ServiceKey <- 'Z1ljPSNFytYVR9ojM%2FolIZrOSLCoZf69DWPfPGQ7JMnAfX4mIBDuG5fKT6yJWBkXlyUpm3ptCcMb5tNGc66bzQ%3D%3D'
searchCondition <- 1
dgu <- 'C0601'    # 대전광역시
dcode <- 'C0301'  # 서구
searchKeyword <- ''
pageNo <- 1
numOfRows <- 10
  
# http://apis.data.go.kr/B552061/frequentzoneLg/getRestFrequentzoneLg?ServiceKey=서비스키&searchYearCd=2017&siDo=26&guGun=110&numOfRows=10&pageNo=1
callback_url <- paste0(base_url, '?ServiceKey=', ServiceKey, '&pageNo=', pageNo, '&numOfRows=', numOfRows,'&dgu=', dgu,
                       '&dcode=', dcode, '&searchCondition=', searchCondition)
callback_url
url <- 'http://apis.data.go.kr/6300000/tourFoodDataService/tourFoodDataListJson?serviceKey=Z1ljPSNFytYVR9ojM%2FolIZrOSLCoZf69DWPfPGQ7JMnAfX4mIBDuG5fKT6yJWBkXlyUpm3ptCcMb5tNGc66bzQ%3D%3D&numOfRows=200&pageNo=2'
responsData2 <- fromJSON(callback_url)
responsData <- fromJSON(url)
str(responsData)
cat("결과 코드 =", responsData$comMsgHeader$returnCode)
cat("결과 메시지 =", responsData$comMsgHeader$returnMessage)
cat("총 건수 =", responsData$msgHeader$totalCount)

str(responsData1)
df_djFood_pg2 <- responsData$msgBody
str(df_djFood)
View(df_djFood_pg2)


setwd('D:/Workspace/R_Crawling/JSON/')
write.csv(df_djFood, '대전광역시 문화관광 음식점_2.csv')

geoms <- df_accidents$geom_json
str(geoms)
# g1<-fromJSON(geoms[1])
# str(g1)
# str(g1$coordinates[1,,])
# write.csv(g1$coordinates[1,,], "poly.csv")

library(openxlsx)
wb <- createWorkbook()
for (i in 1:length(geoms)) {
  geom <- fromJSON(geoms[i])
  str(geom)
  # write.csv(geom$coordinates[1,,], paste0("olygon", i, ".csv"))
  df_geom <- as.data.frame(geom$coordinates[1,,])
  names(df_geom) <- c("경도", "위도")
  addWorksheet(wb, paste0("polygon", i))
  writeDataTable(wb, paste0("polygon", i), df_geom)
}
saveWorkbook(wb, file="polygon.xlsx")
