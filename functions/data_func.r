library(tidyverse)
library(forecast)
library(timetk)
library(Metrics)
library(lubridate)
library(caret)
library(zoo)
library(sweep)

### Read data
# Read in data
wemo.df <- read.csv("data/wemo.csv")
# wemo.df$service_hour=as.POSIXct(paste(wemo.df$service_hour_date, wemo.df$shift), format="%Y-%m-%d %H:%M:%S")
wemo.df$service_hour=as.POSIXct(paste(wemo.df$date, wemo.df$shift), format="%Y-%m-%d %H:%M:%S", tz="UTC")
attributes(wemo.df$service_hour)$tzone <- "Asia/Taipei"

wemo.df$service_hour_date <- date(wemo.df$service_hour)
wemo.df$shift <- format(wemo.df$service_hour,format = '%T')

# Filter area & time (days without 3 shifts)
 wemo.df.new <- wemo.df%>%
   filter(town != "Sanchong Dist" & town != "out of boundary"& town != "Taishan Dist"
          & town != "Wugu Dist" & town != "Tucheng Dist" & town != "Shulin Dist" &
            town != "Xizhi Dist")

 wemo.df.new <- wemo.df.new%>%
     filter(town == "Da’an Dist" | town == "Neihu Dist" | town == "Xinzhuang Dist")
# wemo.df.new <- wemo.df%>%
#  filter(admin_town_zh == "大安區" | admin_town_zh == "內湖區" | admin_town_zh == "新莊區")

# Derived variable (Weekend or weekday)
wemo.df.new$weekday<-weekdays(wemo.df.new$service_hour)
wemo.df.new$weekend_or_weekday<-ifelse(wemo.df.new$weekday=="Saturday" | wemo.df.new$weekday == "Sunday", 1, 0)

# Change type to character
wemo.df.new$service_hour_date <- as.character(wemo.df.new$service_hour_date)

# Filter columns
wemo.df.new <- wemo.df.new %>% 
  select(town, sum_offline_scooter, service_hour_date, shift, weekend_or_weekday, total_rent, is_holiday)

### separate shift into three time series
# e.g.
# shift.time <- c("00:00:00", "08:00:00", "16:00:00")
# shift_1 <- separate_shift(wemo.df.new, shift.time[1])

shift.time <- c("00:00:00", "08:00:00", "16:00:00")

separate_shift <- function(data, time){
  shift <- data%>%
    filter(shift == time)
  return(shift)
}

### Add external info. rain and holiday

# external data function

external_data <- function(data){
  external.df <- read.csv(data)
  external.df$Date <- dmy(external.df$Date)
  external.df$Date  <- as.character(external.df$Date )
  return(external.df)
}

rain_df <- external_data("data/rain.csv")
holiday_df <- external_data("data/holiday.csv")

exter.df <- function(shift_df, rain_df, holiday_df){
  joined_tibble <- left_join(shift_df, rain_df,
                      by = c("service_hour_date" = "Date", "town" = "town"))
  
  joined_tibble <- left_join(joined_tibble, holiday_df, 
                             by = c("service_hour_date" = "Date"))
  return(joined_tibble)
}


### Plot the time series
# e.g.
# plot.ts(shift_1, shift.time[1])

plot.ts <- function(data, time){
  plot <- data %>%
    group_by(town)%>%
    ggplot(aes(as.Date(service_hour_date), sum_offline_scooter, color=town))+
    geom_line(size=.4)+
    guides(color=F)+
    facet_wrap(~town, nrow=5, scales='free_y')+
    labs(x='', title=paste("Time series for offline scooters in", time, "on training data in weekdays"))
  return(plot)
}

### roll forward data split
# e.g.
# train_s1 <- train_data(shift_1, '2020-07-31')
# test_s1 <- test_data(shift_1, '2020-07-31')

# roll forward 8 times
roll_forward <- c("2020-08-02", "2020-07-26", "2020-07-19", "2020-07-12", 
                  "2020-07-05", "2020-06-28", "2020-06-21", "2020-06-14")

districts <- c("Da’an Dist", "Neihu Dist", "Xindian Dist")



train_data <-function(data, date){
  train <- data%>%
    filter(service_hour_date < as.Date(date))
  return(train)
}

test_data <-function(data, date){
  test <- data%>%
    filter(service_hour_date >= as.Date(date))
  return(test)
}

nest_a <- function(data){
  nest_obj <- data%>%
    mutate(service_hour_date = ymd(service_hour_date))%>%
    group_by(town)%>%
    dplyr::select(-town, sum_offline_scooter)%>%
    tidyr::nest(.key = "dem_df")
  return(nest_obj)
}

nest_ts <- function(nest){
  nest_ts <- nest %>%
    mutate(dem_df = 
             map(.x = dem_df,
                 .f = tk_ts,
                 select = sum_offline_scooter, #select the outcome col
                 start= c(2020,31),
                 deltat= 1/365,
                 freq = 7))
  return(nest_ts)
}

