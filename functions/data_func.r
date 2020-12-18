library(tidyverse)
library(timetk)
library(Metrics)
library(lubridate)

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

### Plot the time series
# e.g.
# plot.ts(shift_1, shift.time[1])

plot.ts <- function(data, time){
  plot <- data %>%
    group_by(admin_town_en)%>%
    ggplot(aes(as.Date(service_hour_date), sum_offline_scooter, color=admin_town_en))+
    geom_line(size=.4)+
    guides(color=F)+
    facet_wrap(~admin_town_en, nrow=5, scales='free_y')+
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
