library(forecast)
library(lubridate)
library(sweep)
library(hms)
library(chron)
library(dplyr)
library(zoo)

# Read in WeMo data
wemo.df <- read.csv("C:/Users/wen/Documents/Data_Jan_to_Aug.csv")
wemo.df$service_hour_date <- dmy(wemo.df$service_hour_date)
wemo.df$shift  <- chron(times=wemo.df$shift)
wemo.df$service_hour = ymd_hms(paste(wemo.df$service_hour_date, wemo.df$shift))

#Read in holiday
holiday.df <- read.csv("C:/Users/wen/Documents/holiday.csv")
holiday.df$Date <- dmy(holiday.df$Date)
holiday.df$Date  <- as.character(holiday.df$Date )

#Read in rain weather
rain.df <- read.csv("C:/Users/wen/Documents/rain.csv")
rain.df$Date <- dmy(rain.df$Date)
rain.df$Date  <- as.character(rain.df$Date )

# Filter area & time (days without 3 shifts)
wemo.df.new <- wemo.df%>%
  filter(admin_town_en != "Sanchong Dist" & admin_town_en != "out of boundary"& admin_town_en != "Taishan Dist"
         & admin_town_en != "Wugu Dist" & admin_town_en != "Tucheng Dist" & admin_town_en != "Shulin Dist" & 
           admin_town_en != "Xizhi Dist" & service_hour_date != "2020-01-31" & service_hour_date != "2020-08-31")

# Change type to character
wemo.df.new$service_hour_date <- as.character(wemo.df.new$service_hour_date)

#join holiday & rain to WeMo data
wemo.df.new <-
  filter(
    left_join(wemo.df.new, holiday.df, by = c("service_hour_date" = "Date")))

wemo.df.new <-
  filter(
    left_join(wemo.df.new, rain.df, by = c("service_hour_date" = "Date" , "admin_town_en" = "admin_town_en")))

# Filter columns
wemo.df.new <- wemo.df.new %>% 
  dplyr::select(admin_town_en, sum_offline_scooter, service_hour_date, shift, Holiday, rain)

#see the correlation of external information
cor(wemo.df.new$sum_offline_scooter,wemo.df.new$Holiday,use="complete.obs",method="pearson")
cor(wemo.df.new$sum_offline_scooter,wemo.df.new$rain,use="complete.obs",method="pearson")

# Shifts + Whole week
shift_1 <- wemo.df.new%>%
  filter(shift == '00:00:00')
shift_2 <- wemo.df.new%>%
  filter(shift == '08:00:00')
shift_3 <- wemo.df.new%>%
  filter(shift == '16:00:00')


#seperate by district- shift1
Zhonghe1.df <- shift_1%>% filter(admin_town_en == "Zhonghe Dist" )
Banqiao1.df <- shift_1%>% filter(admin_town_en == "Banqiao Dist" )
Beitou1.df <- shift_1%>% filter(admin_town_en == "Beitou Dist" )
Daan1.df <- shift_1%>% filter(admin_town_en == "Daan Dist" )
Datong1.df <- shift_1%>% filter(admin_town_en == "Datong Dist" )
Luzhou1.df <- shift_1%>% filter(admin_town_en == "Luzhou Dist" )
Nangang1.df <- shift_1%>% filter(admin_town_en == "Nangang Dist" )
Neihu1.df <- shift_1%>% filter(admin_town_en == "Neihu Dist" )
Shilin1.df <- shift_1%>% filter(admin_town_en == "Shilin Dist" )
Songshan1.df <- shift_1%>% filter(admin_town_en == "Songshan Dist" )
Tamsui1.df <- shift_1%>% filter(admin_town_en == "Tamsui Dist" )
Wanhua1.df <- shift_1%>% filter(admin_town_en == "Wanhua Dist" )
Wenshan1.df <- shift_1%>% filter(admin_town_en == "Wenshan Dist" )
Xindian1.df <- shift_1%>% filter(admin_town_en == "Xindian Dist" )
Xinyi1.df <- shift_1%>% filter(admin_town_en == "Xinyi Dist" )
Xinzhuang1.df <- shift_1%>% filter(admin_town_en == "Xinzhuang Dist" )
Yonghe1.df <- shift_1%>% filter(admin_town_en == "Yonghe Dist" )
Zhongshan1.df <- shift_1%>% filter(admin_town_en == "Zhongshan Dist" )
Zhongzheng1.df <- shift_1%>% filter(admin_town_en == "Zhongzheng Dist" )

#data partitioning
train_data <-function(data, date){
  train <- data%>%
    filter(service_hour_date <= as.Date(date))
  return(train)
}

test_data <-function(data, date){
  test <- data%>%
    filter(service_hour_date > as.Date(date))
  return(test)
}

# shift1
train_Zhonghe1 <- train_data(Zhonghe1.df, '2020-07-31')
test_Zhonghe1 <- test_data(Zhonghe1.df, '2020-07-31')

train_Banqiao1 <- train_data(Banqiao1.df, '2020-07-31')
test_Banqiao1 <- test_data(Banqiao1.df, '2020-07-31')

train_Beitou1 <- train_data(Beitou1.df, '2020-07-31')
test_Beitou1 <- test_data(Beitou1.df, '2020-07-31')

train_Daan1 <- train_data(Daan1.df, '2020-07-31')
test_Daan1 <- test_data(Daan1.df, '2020-07-31')

train_Datong1 <- train_data(Datong1.df, '2020-07-31')
test_Datong1 <- test_data(Datong1.df, '2020-07-31')

train_Luzhou1 <- train_data(Luzhou1.df, '2020-07-31')
test_Luzhou1 <- test_data(Luzhou1.df, '2020-07-31')

train_Nangang1 <- train_data(Nangang1.df, '2020-07-31')
test_Nangang1 <- test_data(Nangang1.df, '2020-07-31')

train_Neihu1 <- train_data(Neihu1.df, '2020-07-31')
test_Neihu1 <- test_data(Neihu1.df, '2020-07-31')

train_Shilin1 <- train_data(Shilin1.df, '2020-07-31')
test_Shilin1 <- test_data(Shilin1.df, '2020-07-31')

train_Songshan1 <- train_data(Songshan1.df, '2020-07-31')
test_Songshan1 <- test_data(Songshan1.df, '2020-07-31')

train_Tamsui1 <- train_data(Tamsui1.df, '2020-07-31')
test_Tamsui1 <- test_data(Tamsui1.df, '2020-07-31')

train_Wanhua1 <- train_data(Wanhua1.df, '2020-07-31')
test_Wanhua1 <- test_data(Wanhua1.df, '2020-07-31')

train_Wenshan1 <- train_data(Wenshan1.df, '2020-07-31')
test_Wenshan1 <- test_data(Wenshan1.df, '2020-07-31')

train_Xindian1 <- train_data(Xindian1.df, '2020-07-31')
test_Xindian1 <- test_data(Xindian1.df, '2020-07-31')

train_Xinyi1 <- train_data(Xinyi1.df, '2020-07-31')
test_Xinyi1 <- test_data(Xinyi1.df, '2020-07-31')

train_Xinzhuang1 <- train_data(Xinzhuang1.df, '2020-07-31')
test_Xinzhuang1 <- test_data(Xinzhuang1.df, '2020-07-31')

train_Yonghe1 <- train_data(Yonghe1.df, '2020-07-31')
test_Yonghe1 <- test_data(Yonghe1.df, '2020-07-31')

train_Zhongshan1 <- train_data(Zhongshan1.df, '2020-07-31')
test_Zhongshan1 <- test_data(Zhongshan1.df, '2020-07-31')

train_Zhongzheng1 <- train_data(Zhongzheng1.df, '2020-07-31')
test_Zhongzheng1 <- test_data(Zhongzheng1.df, '2020-07-31')


#auto arima with rain
arima.extra.model <- function(train_data){
  arima.extra<- auto.arima(train_data[,2], xreg=train_data[,5], stepwise = FALSE,
                           approximation = FALSE, biasadj= TRUE)
}

#Zhonghe Dist
arima.extra.Zhonghe1 <- arima.extra.model(train_Zhonghe1)
arima.extra.Zhonghe1.pred <- forecast(arima.extra.Zhonghe1, xreg=test_Zhonghe1[,5], h=30)
summary(arima.extra.Zhonghe1.pred)
checkresiduals(arima.extra.Zhonghe1.pred)
accuracy.test.Zhonghe1 <- forecast::accuracy(arima.extra.Zhonghe1.pred$mean, test_Zhonghe1$sum_offline_scooter)
RMSE.test.Zhonghe1 <- accuracy.test.Zhonghe1[2]

#Banqiao Dist
arima.extra.Banqiao1 <- arima.extra.model(train_Banqiao1)
arima.extra.Banqiao1.pred <- forecast(arima.extra.Banqiao1, xreg=test_Banqiao1[,5], h=30)
accuracy.test.Banqiao1 <- forecast::accuracy(arima.extra.Banqiao1.pred$mean, test_Banqiao1$sum_offline_scooter)
RMSE.test.Banqiao1 <- accuracy.test.Banqiao1[2]

#Beitou Dist
arima.extra.Beitou1 <- arima.extra.model(train_Beitou1)
arima.extra.Beitou1.pred <- forecast(arima.extra.Beitou1, xreg=test_Beitou1[,5], h=30)
accuracy.test.Beitou1 <- forecast::accuracy(arima.extra.Beitou1.pred$mean, test_Beitou1$sum_offline_scooter)
RMSE.test.Beitou1 <- accuracy.test.Beitou1[2]

#Daan Dist
arima.extra.Daan1 <- arima.extra.model(train_Daan1)
arima.extra.Daan1.pred <- forecast(arima.extra.Daan1, xreg=test_Daan1[,5], h=30)
accuracy.test.Daan1 <- forecast::accuracy(arima.extra.Daan1.pred$mean, test_Daan1$sum_offline_scooter)
RMSE.test.Daan1 <- accuracy.test.Daan1[2]

#Datong Dist
arima.extra.Datong1 <- arima.extra.model(train_Datong1)
arima.extra.Datong1.pred <- forecast(arima.extra.Daan1, xreg=test_Datong1[,5], h=30)
accuracy.test.Datong1 <- forecast::accuracy(arima.extra.Datong1.pred$mean, test_Datong1$sum_offline_scooter)
RMSE.test.Datong1 <- accuracy.test.Datong1[2]

#Luzhou Dist
arima.extra.Luzhou1 <- arima.extra.model(train_Luzhou1)
arima.extra.Luzhou1.pred <- forecast(arima.extra.Luzhou1, xreg=test_Luzhou1[,5], h=30)
accuracy.test.Luzhou1 <- forecast::accuracy(arima.extra.Luzhou1.pred$mean, test_Luzhou1$sum_offline_scooter)
RMSE.test.Luzhou1 <- accuracy.test.Luzhou1[2]

#Nangang Dist
arima.extra.Nangang1 <- arima.extra.model(train_Nangang1)
arima.extra.Nangang1.pred <- forecast(arima.extra.Nangang1, xreg=test_Nangang1[,5], h=30)
accuracy.test.Nangang1 <- forecast::accuracy(arima.extra.Nangang1.pred$mean, test_Nangang1$sum_offline_scooter)
RMSE.test.Nangang1 <- accuracy.test.Nangang1[2]

#Neihu Dist
arima.extra.Neihu1 <- arima.extra.model(train_Neihu1)
arima.extra.Neihu1.pred <- forecast(arima.extra.Neihu1, xreg=test_Neihu1[,5], h=30)
accuracy.test.Neihu1 <- forecast::accuracy(arima.extra.Neihu1.pred$mean, test_Neihu1$sum_offline_scooter)
RMSE.test.Neihu1 <- accuracy.test.Neihu1[2]

#Shilin Dist
arima.extra.Shilin1 <- arima.extra.model(train_Shilin1)
arima.extra.Shilin1.pred <- forecast(arima.extra.Shilin1, xreg=test_Shilin1[,5], h=30)
accuracy.test.Shilin1 <- forecast::accuracy(arima.extra.Shilin1.pred$mean, test_Shilin1$sum_offline_scooter)
RMSE.test.Shilin1 <- accuracy.test.Shilin1[2]

#Songshan Dist
arima.extra.Songshan1 <- arima.extra.model(train_Songshan1)
arima.extra.Songshan1.pred <- forecast(arima.extra.Songshan1, xreg=test_Songshan1[,5], h=30)
accuracy.test.Songshan1 <- forecast::accuracy(arima.extra.Songshan1.pred$mean, test_Songshan1$sum_offline_scooter)
RMSE.test.Songshan1 <- accuracy.test.Songshan1[2]

#Tamsui Dist
arima.extra.Tamsui1 <- arima.extra.model(train_Tamsui1)
arima.extra.Tamsui1.pred <- forecast(arima.extra.Tamsui1, xreg=test_Tamsui1[,5], h=30)
accuracy.test.Tamsui1 <- forecast::accuracy(arima.extra.Tamsui1.pred$mean, test_Tamsui1$sum_offline_scooter)
RMSE.test.Tamsui1 <- accuracy.test.Tamsui1[2]

#Wanhua Dist
arima.extra.Wanhua1 <- arima.extra.model(train_Wanhua1)
arima.extra.Wanhua1.pred <- forecast(arima.extra.Wanhua1, xreg=test_Wanhua1[,5], h=30)
accuracy.test.Wanhua1 <- forecast::accuracy(arima.extra.Wanhua1.pred$mean, test_Wanhua1$sum_offline_scooter)
RMSE.test.Wanhua1 <- accuracy.test.Wanhua1[2]

#Wenshan Dist
arima.extra.Wenshan1 <- arima.extra.model(train_Wenshan1)
arima.extra.Wenshan1.pred <- forecast(arima.extra.Wenshan1, xreg=test_Wenshan1[,5], h=30)
accuracy.test.Wenshan1 <- forecast::accuracy(arima.extra.Wenshan1.pred$mean, test_Wenshan1$sum_offline_scooter)
RMSE.test.Wenshan1 <- accuracy.test.Wenshan1[2]

#Xindian Dist
arima.extra.Xindian1 <- arima.extra.model(train_Xindian1)
arima.extra.Xindian1.pred <- forecast(arima.extra.Xindian1, xreg=test_Xindian1[,5], h=30)
accuracy.test.Xindian1 <- forecast::accuracy(arima.extra.Xindian1.pred$mean, test_Xindian1$sum_offline_scooter)
RMSE.test.Xindian1 <- accuracy.test.Xindian1[2]

#Xinyi Dist
arima.extra.Xinyi1 <- arima.extra.model(train_Xinyi1)
arima.extra.Xinyi1.pred <- forecast(arima.extra.Xinyi1, xreg=test_Xinyi1[,5], h=30)
accuracy.test.Xinyi1 <- forecast::accuracy(arima.extra.Xinyi1.pred$mean, test_Xinyi1$sum_offline_scooter)
RMSE.test.Xinyi1 <- accuracy.test.Xinyi1[2]

#Xinzhuang Dist
arima.extra.Xinzhuang1 <- arima.extra.model(train_Xinzhuang1)
arima.extra.Xinzhuang1.pred <- forecast(arima.extra.Xinzhuang1, xreg=test_Xinzhuang1[,5], h=30)
accuracy.test.Xinzhuang1 <- forecast::accuracy(arima.extra.Xinzhuang1.pred$mean, test_Xinzhuang1$sum_offline_scooter)
RMSE.test.Xinzhuang1 <- accuracy.test.Xinzhuang1[2]

#Yonghe Dist
arima.extra.Yonghe1 <- arima.extra.model(train_Yonghe1)
arima.extra.Yonghe1.pred <- forecast(arima.extra.Yonghe1, xreg=test_Yonghe1[,5], h=30)
accuracy.test.Yonghe1 <- forecast::accuracy(arima.extra.Yonghe1.pred$mean, test_Yonghe1$sum_offline_scooter)
RMSE.test.Yonghe1 <- accuracy.test.Yonghe1[2]

#Zhongshan Dist
arima.extra.Zhongshan1 <- arima.extra.model(train_Zhongshan1)
arima.extra.Zhongshan1.pred <- forecast(arima.extra.Zhongshan1, xreg=test_Zhongshan1[,5], h=30)
accuracy.test.Zhongshan1 <- forecast::accuracy(arima.extra.Zhongshan1.pred$mean, test_Zhongshan1$sum_offline_scooter)
RMSE.test.Zhongshan1 <- accuracy.test.Zhongshan1[2]

#Zhongzheng Dist
arima.extra.Zhongzheng1 <- arima.extra.model(train_Zhongzheng1)
arima.extra.Zhongzheng1.pred <- forecast(arima.extra.Zhongzheng1, xreg=test_Zhongzheng1[,5], h=30)
accuracy.test.Zhongzheng1 <- forecast::accuracy(arima.extra.Zhongzheng1.pred$mean, test_Zhongzheng1$sum_offline_scooter)
RMSE.test.Zhongzheng1 <- accuracy.test.Zhongzheng1[2]

# Calculate the average mean of 19 district
RMSE1 <- c(RMSE.test.Zhonghe1,RMSE.test.Banqiao1, RMSE.test.Beitou1,RMSE.test.Daan1,RMSE.test.Datong1,RMSE.test.Luzhou1,
           RMSE.test.Nangang1,RMSE.test.Neihu1,RMSE.test.Shilin1,RMSE.test.Songshan1,RMSE.test.Tamsui1,RMSE.test.Wanhua1 ,
           RMSE.test.Wenshan1,RMSE.test.Xindian1,RMSE.test.Xinyi1,RMSE.test.Xinzhuang1,RMSE.test.Yonghe1,
           RMSE.test.Zhongshan1 ,RMSE.test.Zhongzheng1 )
RMSE1

#233.9322
mean(RMSE1)
RMSE1
# [1] 174.00278 369.95824 231.68452 338.05170 780.06307  59.76569 106.26134 268.59691 216.07411 464.79723  47.63572
# [12] 107.87261 101.84034 167.89707 279.23190  84.17182  67.03393 358.41272 221.35926

########################################################
#shift2
#seperate by district- shift2
Zhonghe2.df <- shift_2%>% filter(admin_town_en == "Zhonghe Dist" )
Banqiao2.df <- shift_2%>% filter(admin_town_en == "Banqiao Dist" )
Beitou2.df <- shift_2%>% filter(admin_town_en == "Beitou Dist" )
Daan2.df <- shift_2%>% filter(admin_town_en == "Daan Dist" )
Datong2.df <- shift_2%>% filter(admin_town_en == "Datong Dist" )
Luzhou2.df <- shift_2%>% filter(admin_town_en == "Luzhou Dist" )
Nangang2.df <- shift_2%>% filter(admin_town_en == "Nangang Dist" )
Neihu2.df <- shift_2%>% filter(admin_town_en == "Neihu Dist" )
Shilin2.df <- shift_2%>% filter(admin_town_en == "Shilin Dist" )
Songshan2.df <- shift_2%>% filter(admin_town_en == "Songshan Dist" )
Tamsui2.df <- shift_2%>% filter(admin_town_en == "Tamsui Dist" )
Wanhua2.df <- shift_2%>% filter(admin_town_en == "Wanhua Dist" )
Wenshan2.df <- shift_2%>% filter(admin_town_en == "Wenshan Dist" )
Xindian2.df <- shift_2%>% filter(admin_town_en == "Xindian Dist" )
Xinyi2.df <- shift_2%>% filter(admin_town_en == "Xinyi Dist" )
Xinzhuang2.df <- shift_2%>% filter(admin_town_en == "Xinzhuang Dist" )
Yonghe2.df <- shift_2%>% filter(admin_town_en == "Yonghe Dist" )
Zhongshan2.df <- shift_2%>% filter(admin_town_en == "Zhongshan Dist" )
Zhongzheng2.df <- shift_2%>% filter(admin_town_en == "Zhongzheng Dist" )

# shift2 data partitioning
train_Zhonghe2 <- train_data(Zhonghe2.df, '2020-07-31')
test_Zhonghe2 <- test_data(Zhonghe2.df, '2020-07-31')

train_Banqiao2 <- train_data(Banqiao2.df, '2020-07-31')
test_Banqiao2 <- test_data(Banqiao2.df, '2020-07-31')

train_Beitou2 <- train_data(Beitou2.df, '2020-07-31')
test_Beitou2 <- test_data(Beitou2.df, '2020-07-31')

train_Daan2 <- train_data(Daan2.df, '2020-07-31')
test_Daan2 <- test_data(Daan2.df, '2020-07-31')

train_Datong2 <- train_data(Datong2.df, '2020-07-31')
test_Datong2 <- test_data(Datong2.df, '2020-07-31')

train_Luzhou2 <- train_data(Luzhou2.df, '2020-07-31')
test_Luzhou2 <- test_data(Luzhou2.df, '2020-07-31')

train_Nangang2 <- train_data(Nangang2.df, '2020-07-31')
test_Nangang2 <- test_data(Nangang2.df, '2020-07-31')

train_Neihu2 <- train_data(Neihu2.df, '2020-07-31')
test_Neihu2 <- test_data(Neihu2.df, '2020-07-31')

train_Shilin2 <- train_data(Shilin2.df, '2020-07-31')
test_Shilin2 <- test_data(Shilin2.df, '2020-07-31')

train_Songshan2 <- train_data(Songshan2.df, '2020-07-31')
test_Songshan2 <- test_data(Songshan2.df, '2020-07-31')

train_Tamsui2 <- train_data(Tamsui2.df, '2020-07-31')
test_Tamsui2 <- test_data(Tamsui2.df, '2020-07-31')

train_Wanhua2 <- train_data(Wanhua2.df, '2020-07-31')
test_Wanhua2 <- test_data(Wanhua2.df, '2020-07-31')

train_Wenshan2 <- train_data(Wenshan2.df, '2020-07-31')
test_Wenshan2 <- test_data(Wenshan2.df, '2020-07-31')

train_Xindian2 <- train_data(Xindian2.df, '2020-07-31')
test_Xindian2 <- test_data(Xindian2.df, '2020-07-31')

train_Xinyi2 <- train_data(Xinyi2.df, '2020-07-31')
test_Xinyi2 <- test_data(Xinyi2.df, '2020-07-31')

train_Xinzhuang2 <- train_data(Xinzhuang2.df, '2020-07-31')
test_Xinzhuang2 <- test_data(Xinzhuang2.df, '2020-07-31')

train_Yonghe2 <- train_data(Yonghe2.df, '2020-07-31')
test_Yonghe2 <- test_data(Yonghe2.df, '2020-07-31')

train_Zhongshan2 <- train_data(Zhongshan2.df, '2020-07-31')
test_Zhongshan2 <- test_data(Zhongshan2.df, '2020-07-31')

train_Zhongzheng2 <- train_data(Zhongzheng2.df, '2020-07-31')
test_Zhongzheng2 <- test_data(Zhongzheng2.df, '2020-07-31')



#Zhonghe Dist
arima.extra.Zhonghe2 <- arima.extra.model(train_Zhonghe2)
arima.extra.Zhonghe2.pred <- forecast(arima.extra.Zhonghe2, xreg=test_Zhonghe2[,5], h=30)
summary(arima.extra.Zhonghe2.pred)
checkresiduals(arima.extra.Zhonghe2.pred)
accuracy.test.Zhonghe2 <- forecast::accuracy(arima.extra.Zhonghe2.pred$mean, test_Zhonghe2$sum_offline_scooter)
RMSE.test.Zhonghe2 <- accuracy.test.Zhonghe2[2]

#Banqiao Dist
arima.extra.Banqiao2 <- arima.extra.model(train_Banqiao2)
arima.extra.Banqiao2.pred <- forecast(arima.extra.Banqiao2, xreg=test_Banqiao2[,5], h=30)
accuracy.test.Banqiao2 <- forecast::accuracy(arima.extra.Banqiao2.pred$mean, test_Banqiao2$sum_offline_scooter)
RMSE.test.Banqiao2 <- accuracy.test.Banqiao2[2]

#Beitou Dist
arima.extra.Beitou2 <- arima.extra.model(train_Beitou2)
arima.extra.Beitou2.pred <- forecast(arima.extra.Beitou2, xreg=test_Beitou2[,5], h=30)
accuracy.test.Beitou2 <- forecast::accuracy(arima.extra.Beitou2.pred$mean, test_Beitou2$sum_offline_scooter)
RMSE.test.Beitou2 <- accuracy.test.Beitou2[2]

#Daan Dist
arima.extra.Daan2 <- arima.extra.model(train_Daan2)
arima.extra.Daan2.pred <- forecast(arima.extra.Daan2, xreg=test_Daan2[,5], h=30)
accuracy.test.Daan2 <- forecast::accuracy(arima.extra.Daan2.pred$mean, test_Daan2$sum_offline_scooter)
RMSE.test.Daan2 <- accuracy.test.Daan2[2]

#Datong Dist
arima.extra.Datong2 <- arima.extra.model(train_Datong2)
arima.extra.Datong2.pred <- forecast(arima.extra.Daan2, xreg=test_Datong2[,5], h=30)
accuracy.test.Datong2 <- forecast::accuracy(arima.extra.Datong2.pred$mean, test_Datong2$sum_offline_scooter)
RMSE.test.Datong2 <- accuracy.test.Datong2[2]

#Luzhou Dist
arima.extra.Luzhou2 <- arima.extra.model(train_Luzhou2)
arima.extra.Luzhou2.pred <- forecast(arima.extra.Luzhou2, xreg=test_Luzhou2[,5], h=30)
accuracy.test.Luzhou2 <- forecast::accuracy(arima.extra.Luzhou2.pred$mean, test_Luzhou2$sum_offline_scooter)
RMSE.test.Luzhou2 <- accuracy.test.Luzhou2[2]

#Nangang Dist
arima.extra.Nangang2 <- arima.extra.model(train_Nangang2)
arima.extra.Nangang2.pred <- forecast(arima.extra.Nangang2, xreg=test_Nangang2[,5], h=30)
accuracy.test.Nangang2 <- forecast::accuracy(arima.extra.Nangang2.pred$mean, test_Nangang2$sum_offline_scooter)
RMSE.test.Nangang2 <- accuracy.test.Nangang2[2]

#Neihu Dist
arima.extra.Neihu2 <- arima.extra.model(train_Neihu2)
arima.extra.Neihu2.pred <- forecast(arima.extra.Neihu2, xreg=test_Neihu2[,5], h=30)
accuracy.test.Neihu2 <- forecast::accuracy(arima.extra.Neihu2.pred$mean, test_Neihu2$sum_offline_scooter)
RMSE.test.Neihu2 <- accuracy.test.Neihu2[2]

#Shilin Dist
arima.extra.Shilin2 <- arima.extra.model(train_Shilin2)
arima.extra.Shilin2.pred <- forecast(arima.extra.Shilin2, xreg=test_Shilin2[,5], h=30)
accuracy.test.Shilin2 <- forecast::accuracy(arima.extra.Shilin2.pred$mean, test_Shilin2$sum_offline_scooter)
RMSE.test.Shilin2 <- accuracy.test.Shilin2[2]

#Songshan Dist
arima.extra.Songshan2 <- arima.extra.model(train_Songshan2)
arima.extra.Songshan2.pred <- forecast(arima.extra.Songshan2, xreg=test_Songshan2[,5], h=30)
accuracy.test.Songshan2 <- forecast::accuracy(arima.extra.Songshan2.pred$mean, test_Songshan2$sum_offline_scooter)
RMSE.test.Songshan2 <- accuracy.test.Songshan2[2]

#Tamsui Dist
arima.extra.Tamsui2 <- arima.extra.model(train_Tamsui2)
arima.extra.Tamsui2.pred <- forecast(arima.extra.Tamsui2, xreg=test_Tamsui2[,5], h=30)
accuracy.test.Tamsui2 <- forecast::accuracy(arima.extra.Tamsui2.pred$mean, test_Tamsui2$sum_offline_scooter)
RMSE.test.Tamsui2 <- accuracy.test.Tamsui2[2]

#Wanhua Dist
arima.extra.Wanhua2 <- arima.extra.model(train_Wanhua2)
arima.extra.Wanhua2.pred <- forecast(arima.extra.Wanhua2, xreg=test_Wanhua2[,5], h=30)
accuracy.test.Wanhua2 <- forecast::accuracy(arima.extra.Wanhua2.pred$mean, test_Wanhua2$sum_offline_scooter)
RMSE.test.Wanhua2 <- accuracy.test.Wanhua2[2]

#Wenshan Dist
arima.extra.Wenshan2 <- arima.extra.model(train_Wenshan2)
arima.extra.Wenshan2.pred <- forecast(arima.extra.Wenshan2, xreg=test_Wenshan2[,5], h=30)
accuracy.test.Wenshan2 <- forecast::accuracy(arima.extra.Wenshan2.pred$mean, test_Wenshan2$sum_offline_scooter)
RMSE.test.Wenshan2 <- accuracy.test.Wenshan2[2]

#Xindian Dist
arima.extra.Xindian2 <- arima.extra.model(train_Xindian2)
arima.extra.Xindian2.pred <- forecast(arima.extra.Xindian2, xreg=test_Xindian2[,5], h=30)
accuracy.test.Xindian2 <- forecast::accuracy(arima.extra.Xindian2.pred$mean, test_Xindian2$sum_offline_scooter)
RMSE.test.Xindian2 <- accuracy.test.Xindian2[2]

#Xinyi Dist
arima.extra.Xinyi2 <- arima.extra.model(train_Xinyi2)
arima.extra.Xinyi2.pred <- forecast(arima.extra.Xinyi2, xreg=test_Xinyi2[,5], h=30)
accuracy.test.Xinyi2 <- forecast::accuracy(arima.extra.Xinyi2.pred$mean, test_Xinyi2$sum_offline_scooter)
RMSE.test.Xinyi2 <- accuracy.test.Xinyi2[2]

#Xinzhuang Dist
arima.extra.Xinzhuang2 <- arima.extra.model(train_Xinzhuang2)
arima.extra.Xinzhuang2.pred <- forecast(arima.extra.Xinzhuang2, xreg=test_Xinzhuang2[,5], h=30)
accuracy.test.Xinzhuang2 <- forecast::accuracy(arima.extra.Xinzhuang2.pred$mean, test_Xinzhuang2$sum_offline_scooter)
RMSE.test.Xinzhuang2 <- accuracy.test.Xinzhuang2[2]

#Yonghe Dist
arima.extra.Yonghe2 <- arima.extra.model(train_Yonghe2)
arima.extra.Yonghe2.pred <- forecast(arima.extra.Yonghe2, xreg=test_Yonghe2[,5], h=30)
accuracy.test.Yonghe2 <- forecast::accuracy(arima.extra.Yonghe2.pred$mean, test_Yonghe2$sum_offline_scooter)
RMSE.test.Yonghe2 <- accuracy.test.Yonghe2[2]

#Zhongshan Dist
arima.extra.Zhongshan2 <- arima.extra.model(train_Zhongshan2)
arima.extra.Zhongshan2.pred <- forecast(arima.extra.Zhongshan2, xreg=test_Zhongshan2[,5], h=30)
accuracy.test.Zhongshan2 <- forecast::accuracy(arima.extra.Zhongshan2.pred$mean, test_Zhongshan2$sum_offline_scooter)
RMSE.test.Zhongshan2 <- accuracy.test.Zhongshan2[2]

#Zhongzheng Dist
arima.extra.Zhongzheng2 <- arima.extra.model(train_Zhongzheng2)
arima.extra.Zhongzheng2.pred <- forecast(arima.extra.Zhongzheng2, xreg=test_Zhongzheng2[,5], h=30)
accuracy.test.Zhongzheng2 <- forecast::accuracy(arima.extra.Zhongzheng2.pred$mean, test_Zhongzheng2$sum_offline_scooter)
RMSE.test.Zhongzheng2 <- accuracy.test.Zhongzheng2[2]

# Calculate the average mean of 19 district
RMSE2 <- c(RMSE.test.Zhonghe2,RMSE.test.Banqiao2, RMSE.test.Beitou2,RMSE.test.Daan2,RMSE.test.Datong2,RMSE.test.Luzhou2,
           RMSE.test.Nangang2,RMSE.test.Neihu2,RMSE.test.Shilin2,RMSE.test.Songshan2,RMSE.test.Tamsui2,RMSE.test.Wanhua2 ,
           RMSE.test.Wenshan2,RMSE.test.Xindian2,RMSE.test.Xinyi2,RMSE.test.Xinzhuang2,RMSE.test.Yonghe2,
           RMSE.test.Zhongshan2 ,RMSE.test.Zhongzheng2)

RMSE2
# [1]  174.64078  274.37213  210.04739  394.29183 1183.15035   59.24672  115.46725  336.51948  200.95126  458.54617
# [11]   38.29923  122.97336  141.29995  146.23546  229.63630   82.11872   71.12753  339.22053  247.98846
mean(RMSE2)
#254.007

#########################################################
#shift3
#seperate by district- shift3
Zhonghe3.df <- shift_3%>% filter(admin_town_en == "Zhonghe Dist" )
Banqiao3.df <- shift_3%>% filter(admin_town_en == "Banqiao Dist" )
Beitou3.df <- shift_3%>% filter(admin_town_en == "Beitou Dist" )
Daan3.df <- shift_3%>% filter(admin_town_en == "Daan Dist" )
Datong3.df <- shift_3%>% filter(admin_town_en == "Datong Dist" )
Luzhou3.df <- shift_3%>% filter(admin_town_en == "Luzhou Dist" )
Nangang3.df <- shift_3%>% filter(admin_town_en == "Nangang Dist" )
Neihu3.df <- shift_3%>% filter(admin_town_en == "Neihu Dist" )
Shilin3.df <- shift_3%>% filter(admin_town_en == "Shilin Dist" )
Songshan3.df <- shift_3%>% filter(admin_town_en == "Songshan Dist" )
Tamsui3.df <- shift_3%>% filter(admin_town_en == "Tamsui Dist" )
Wanhua3.df <- shift_3%>% filter(admin_town_en == "Wanhua Dist" )
Wenshan3.df <- shift_3%>% filter(admin_town_en == "Wenshan Dist" )
Xindian3.df <- shift_3%>% filter(admin_town_en == "Xindian Dist" )
Xinyi3.df <- shift_3%>% filter(admin_town_en == "Xinyi Dist" )
Xinzhuang3.df <- shift_3%>% filter(admin_town_en == "Xinzhuang Dist" )
Yonghe3.df <- shift_3%>% filter(admin_town_en == "Yonghe Dist" )
Zhongshan3.df <- shift_3%>% filter(admin_town_en == "Zhongshan Dist" )
Zhongzheng3.df <- shift_3%>% filter(admin_town_en == "Zhongzheng Dist" )

# shift3
train_Zhonghe3 <- train_data(Zhonghe3.df, '2020-07-31')
test_Zhonghe3 <- test_data(Zhonghe3.df, '2020-07-31')

train_Banqiao3 <- train_data(Banqiao3.df, '2020-07-31')
test_Banqiao3 <- test_data(Banqiao3.df, '2020-07-31')

train_Beitou3 <- train_data(Beitou3.df, '2020-07-31')
test_Beitou3 <- test_data(Beitou3.df, '2020-07-31')

train_Daan3 <- train_data(Daan3.df, '2020-07-31')
test_Daan3 <- test_data(Daan3.df, '2020-07-31')

train_Datong3 <- train_data(Datong3.df, '2020-07-31')
test_Datong3 <- test_data(Datong3.df, '2020-07-31')

train_Luzhou3 <- train_data(Luzhou3.df, '2020-07-31')
test_Luzhou3 <- test_data(Luzhou3.df, '2020-07-31')

train_Nangang3 <- train_data(Nangang3.df, '2020-07-31')
test_Nangang3 <- test_data(Nangang3.df, '2020-07-31')

train_Neihu3 <- train_data(Neihu3.df, '2020-07-31')
test_Neihu3 <- test_data(Neihu3.df, '2020-07-31')

train_Shilin3 <- train_data(Shilin3.df, '2020-07-31')
test_Shilin3 <- test_data(Shilin3.df, '2020-07-31')

train_Songshan3 <- train_data(Songshan3.df, '2020-07-31')
test_Songshan3 <- test_data(Songshan3.df, '2020-07-31')

train_Tamsui3 <- train_data(Tamsui3.df, '2020-07-31')
test_Tamsui3 <- test_data(Tamsui3.df, '2020-07-31')

train_Wanhua3 <- train_data(Wanhua3.df, '2020-07-31')
test_Wanhua3 <- test_data(Wanhua3.df, '2020-07-31')

train_Wenshan3 <- train_data(Wenshan3.df, '2020-07-31')
test_Wenshan3 <- test_data(Wenshan3.df, '2020-07-31')

train_Xindian3 <- train_data(Xindian3.df, '2020-07-31')
test_Xindian3 <- test_data(Xindian3.df, '2020-07-31')

train_Xinyi3 <- train_data(Xinyi3.df, '2020-07-31')
test_Xinyi3 <- test_data(Xinyi3.df, '2020-07-31')

train_Xinzhuang3 <- train_data(Xinzhuang3.df, '2020-07-31')
test_Xinzhuang3 <- test_data(Xinzhuang3.df, '2020-07-31')

train_Yonghe3 <- train_data(Yonghe3.df, '2020-07-31')
test_Yonghe3 <- test_data(Yonghe3.df, '2020-07-31')

train_Zhongshan3 <- train_data(Zhongshan3.df, '2020-07-31')
test_Zhongshan3 <- test_data(Zhongshan3.df, '2020-07-31')

train_Zhongzheng3 <- train_data(Zhongzheng3.df, '2020-07-31')
test_Zhongzheng3 <- test_data(Zhongzheng3.df, '2020-07-31')


#auto arima with rain
arima.extra.model <- function(train_data){
  arima.extra<- auto.arima(train_data[,2], xreg=train_data[,5], stepwise = FALSE,
                           approximation = FALSE, biasadj= TRUE)
}

#Zhonghe Dist
arima.extra.Zhonghe3 <- arima.extra.model(train_Zhonghe3)
arima.extra.Zhonghe3.pred <- forecast(arima.extra.Zhonghe3, xreg=test_Zhonghe3[,5], h=30)
summary(arima.extra.Zhonghe3.pred)
checkresiduals(arima.extra.Zhonghe3.pred)
accuracy.test.Zhonghe3 <- forecast::accuracy(arima.extra.Zhonghe3.pred$mean, test_Zhonghe3$sum_offline_scooter)
RMSE.test.Zhonghe3 <- accuracy.test.Zhonghe3[2]

#Banqiao Dist
arima.extra.Banqiao3 <- arima.extra.model(train_Banqiao3)
arima.extra.Banqiao3.pred <- forecast(arima.extra.Banqiao3, xreg=test_Banqiao3[,5], h=30)
accuracy.test.Banqiao3 <- forecast::accuracy(arima.extra.Banqiao3.pred$mean, test_Banqiao3$sum_offline_scooter)
RMSE.test.Banqiao3 <- accuracy.test.Banqiao3[2]

#Beitou Dist
arima.extra.Beitou3 <- arima.extra.model(train_Beitou3)
arima.extra.Beitou3.pred <- forecast(arima.extra.Beitou3, xreg=test_Beitou3[,5], h=30)
accuracy.test.Beitou3 <- forecast::accuracy(arima.extra.Beitou3.pred$mean, test_Beitou3$sum_offline_scooter)
RMSE.test.Beitou3 <- accuracy.test.Beitou3[2]

#Daan Dist
arima.extra.Daan3 <- arima.extra.model(train_Daan3)
arima.extra.Daan3.pred <- forecast(arima.extra.Daan3, xreg=test_Daan3[,5], h=30)
accuracy.test.Daan3 <- forecast::accuracy(arima.extra.Daan3.pred$mean, test_Daan3$sum_offline_scooter)
RMSE.test.Daan3 <- accuracy.test.Daan3[2]

#Datong Dist
arima.extra.Datong3 <- arima.extra.model(train_Datong3)
arima.extra.Datong3.pred <- forecast(arima.extra.Daan3, xreg=test_Datong3[,5], h=30)
accuracy.test.Datong3 <- forecast::accuracy(arima.extra.Datong3.pred$mean, test_Datong3$sum_offline_scooter)
RMSE.test.Datong3 <- accuracy.test.Datong3[2]

#Luzhou Dist
arima.extra.Luzhou3 <- arima.extra.model(train_Luzhou3)
arima.extra.Luzhou3.pred <- forecast(arima.extra.Luzhou3, xreg=test_Luzhou3[,5], h=30)
accuracy.test.Luzhou3 <- forecast::accuracy(arima.extra.Luzhou3.pred$mean, test_Luzhou3$sum_offline_scooter)
RMSE.test.Luzhou3 <- accuracy.test.Luzhou3[2]

#Nangang Dist
arima.extra.Nangang3 <- arima.extra.model(train_Nangang3)
arima.extra.Nangang3.pred <- forecast(arima.extra.Nangang3, xreg=test_Nangang3[,5], h=30)
accuracy.test.Nangang3 <- forecast::accuracy(arima.extra.Nangang3.pred$mean, test_Nangang3$sum_offline_scooter)
RMSE.test.Nangang3 <- accuracy.test.Nangang3[2]

#Neihu Dist
arima.extra.Neihu3 <- arima.extra.model(train_Neihu3)
arima.extra.Neihu3.pred <- forecast(arima.extra.Neihu3, xreg=test_Neihu3[,5], h=30)
accuracy.test.Neihu3 <- forecast::accuracy(arima.extra.Neihu3.pred$mean, test_Neihu3$sum_offline_scooter)
RMSE.test.Neihu3 <- accuracy.test.Neihu3[2]

#Shilin Dist
arima.extra.Shilin3 <- arima.extra.model(train_Shilin3)
arima.extra.Shilin3.pred <- forecast(arima.extra.Shilin3, xreg=test_Shilin3[,5], h=30)
accuracy.test.Shilin3 <- forecast::accuracy(arima.extra.Shilin3.pred$mean, test_Shilin3$sum_offline_scooter)
RMSE.test.Shilin3 <- accuracy.test.Shilin3[2]

#Songshan Dist
arima.extra.Songshan3 <- arima.extra.model(train_Songshan3)
arima.extra.Songshan3.pred <- forecast(arima.extra.Songshan3, xreg=test_Songshan3[,5], h=30)
accuracy.test.Songshan3 <- forecast::accuracy(arima.extra.Songshan3.pred$mean, test_Songshan3$sum_offline_scooter)
RMSE.test.Songshan3 <- accuracy.test.Songshan3[2]

#Tamsui Dist
arima.extra.Tamsui3 <- arima.extra.model(train_Tamsui3)
arima.extra.Tamsui3.pred <- forecast(arima.extra.Tamsui3, xreg=test_Tamsui3[,5], h=30)
accuracy.test.Tamsui3 <- forecast::accuracy(arima.extra.Tamsui3.pred$mean, test_Tamsui3$sum_offline_scooter)
RMSE.test.Tamsui3 <- accuracy.test.Tamsui3[2]

#Wanhua Dist
arima.extra.Wanhua3 <- arima.extra.model(train_Wanhua3)
arima.extra.Wanhua3.pred <- forecast(arima.extra.Wanhua3, xreg=test_Wanhua3[,5], h=30)
accuracy.test.Wanhua3 <- forecast::accuracy(arima.extra.Wanhua3.pred$mean, test_Wanhua3$sum_offline_scooter)
RMSE.test.Wanhua3 <- accuracy.test.Wanhua3[2]

#Wenshan Dist
arima.extra.Wenshan3 <- arima.extra.model(train_Wenshan3)
arima.extra.Wenshan3.pred <- forecast(arima.extra.Wenshan3, xreg=test_Wenshan3[,5], h=30)
accuracy.test.Wenshan3 <- forecast::accuracy(arima.extra.Wenshan3.pred$mean, test_Wenshan3$sum_offline_scooter)
RMSE.test.Wenshan3 <- accuracy.test.Wenshan3[2]

#Xindian Dist
arima.extra.Xindian3 <- arima.extra.model(train_Xindian3)
arima.extra.Xindian3.pred <- forecast(arima.extra.Xindian3, xreg=test_Xindian3[,5], h=30)
accuracy.test.Xindian3 <- forecast::accuracy(arima.extra.Xindian3.pred$mean, test_Xindian3$sum_offline_scooter)
RMSE.test.Xindian3 <- accuracy.test.Xindian3[2]

#Xinyi Dist
arima.extra.Xinyi3 <- arima.extra.model(train_Xinyi3)
arima.extra.Xinyi3.pred <- forecast(arima.extra.Xinyi3, xreg=test_Xinyi3[,5], h=30)
accuracy.test.Xinyi3 <- forecast::accuracy(arima.extra.Xinyi3.pred$mean, test_Xinyi3$sum_offline_scooter)
RMSE.test.Xinyi3 <- accuracy.test.Xinyi3[2]

#Xinzhuang Dist
arima.extra.Xinzhuang3 <- arima.extra.model(train_Xinzhuang3)
arima.extra.Xinzhuang3.pred <- forecast(arima.extra.Xinzhuang3, xreg=test_Xinzhuang3[,5], h=30)
accuracy.test.Xinzhuang3 <- forecast::accuracy(arima.extra.Xinzhuang3.pred$mean, test_Xinzhuang3$sum_offline_scooter)
RMSE.test.Xinzhuang3 <- accuracy.test.Xinzhuang3[2]

#Yonghe Dist
arima.extra.Yonghe3 <- arima.extra.model(train_Yonghe3)
arima.extra.Yonghe3.pred <- forecast(arima.extra.Yonghe3, xreg=test_Yonghe3[,5], h=30)
accuracy.test.Yonghe3 <- forecast::accuracy(arima.extra.Yonghe3.pred$mean, test_Yonghe3$sum_offline_scooter)
RMSE.test.Yonghe3 <- accuracy.test.Yonghe3[2]

#Zhongshan Dist
arima.extra.Zhongshan3 <- arima.extra.model(train_Zhongshan3)
arima.extra.Zhongshan3.pred <- forecast(arima.extra.Zhongshan3, xreg=test_Zhongshan3[,5], h=30)
accuracy.test.Zhongshan3 <- forecast::accuracy(arima.extra.Zhongshan3.pred$mean, test_Zhongshan3$sum_offline_scooter)
RMSE.test.Zhongshan3 <- accuracy.test.Zhongshan3[2]

#Zhongzheng Dist
arima.extra.Zhongzheng3 <- arima.extra.model(train_Zhongzheng3)
arima.extra.Zhongzheng3.pred <- forecast(arima.extra.Zhongzheng3, xreg=test_Zhongzheng3[,5], h=30)
accuracy.test.Zhongzheng3 <- forecast::accuracy(arima.extra.Zhongzheng3.pred$mean, test_Zhongzheng3$sum_offline_scooter)
RMSE.test.Zhongzheng3 <- accuracy.test.Zhongzheng3[2]

# Calculate the average mean of 19 district
RMSE3 <- c(RMSE.test.Zhonghe3,RMSE.test.Banqiao3, RMSE.test.Beitou3,RMSE.test.Daan3,RMSE.test.Datong3,RMSE.test.Luzhou3,
           RMSE.test.Nangang3,RMSE.test.Neihu3,RMSE.test.Shilin3,RMSE.test.Songshan3,RMSE.test.Tamsui3,RMSE.test.Wanhua3 ,
           RMSE.test.Wenshan3,RMSE.test.Xindian3,RMSE.test.Xinyi3,RMSE.test.Xinzhuang3,RMSE.test.Yonghe3,
           RMSE.test.Zhongshan3 ,RMSE.test.Zhongzheng3)
RMSE3 
# [1]  149.34957  303.91439  190.31335  486.66998 1379.99010  103.93072  166.29007  364.60079  232.27046  516.94389
# [11]   37.68032  150.49674  158.66520  180.39700  370.04313   78.16751  108.25237  435.66433  285.42461

mean(RMSE3)
#299.9508


