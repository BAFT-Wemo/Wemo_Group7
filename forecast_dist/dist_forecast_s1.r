library(tidyverse)
library(forecast)
library(timetk)
library(Metrics)
library(lubridate)
library(sweep)
library(zoo)
library(ModelMetrics)

# Read in data
wemo.df <- read.csv("data/Data_Jan_to_Aug.csv")
wemo.df$service_hour=as.POSIXct(paste(wemo.df$service_hour_date, wemo.df$shift), format="%Y-%m-%d %H:%M:%S")

wemo.df$service_hour=as.POSIXct(paste(wemo.df$service_hour_date, wemo.df$shift), format="%Y-%m-%d %H:%M:%S", tz="UTC")
attributes(wemo.df$service_hour)$tzone <- "Asia/Taipei"

wemo.df$service_hour_date <- date(wemo.df$service_hour)
wemo.df$shift <- format(wemo.df$service_hour,format = '%T')

# Filter area & time (days without 3 shifts)
# wemo.df.new <- wemo.df%>%
#   filter(admin_town_zh != "三重區" & admin_town_zh != "超出營運範圍"& admin_town_zh != "泰山區"
#          & admin_town_zh != "五股區" & admin_town_zh != "土城區" & admin_town_zh != "樹林區" &
#            admin_town_zh != "汐止區" & service_hour_date != "2020-01-31" & service_hour_date != "2020-08-31")

wemo.df.new <- wemo.df%>%
  filter(admin_town_zh == "大安區" | admin_town_zh == "內湖區" | admin_town_zh == "新莊區")%>%
  filter(service_hour_date != as.Date("2020-01-31") & service_hour_date != as.Date("2020-08-31"))

# Change type to character
wemo.df.new$service_hour_date <- as.character(wemo.df.new$service_hour_date)

# Shift1 + Whole week
shift_1 <- wemo.df.new%>%
  filter(shift == '00:00:00')

### Create train/validation sets
# Whole week
train_s1 <- shift_1%>%
  filter(service_hour_date <= as.Date('2020-08-29'))

train_s1_x <- train_s1 %>%
  mutate(sum_offline_scooter.x = sum_offline_scooter,
         sum_offline_scooter.y = NA)%>%
  select(admin_town_en, service_hour_date, sum_offline_scooter.x, sum_offline_scooter.y)

### nest
nest_s1 <- train_s1%>%
  mutate(service_hour_date = ymd(service_hour_date))%>%
  group_by(admin_town_en)%>%
  select(-admin_town_en, sum_offline_scooter)%>%
  nest(.key= 'dem_df')

nest_s1_ts <- nest_s1 %>%
  mutate(dem_df = 
           map(.x = dem_df,
               .f = tk_ts,
               select = sum_offline_scooter, 
               start= c(2020,31),
               deltat= 1/365,
               freq = 7)) 

#####################
# LINEAR REGRESSION FORECAST
## FORECAST in testing for 30 days
lm_models <- nest_s1_ts %>%
  mutate(lm_fit = map(.x=dem_df,
                      .f = function(x) tslm(x ~ trend + season)))

lm_forecast <- lm_models %>%
  mutate(fcast = map(lm_fit,
                     forecast,
                     h=28))%>%
  mutate(swp = map(fcast, sw_sweep, fitted=FALSE))%>%
  unnest(swp)%>%
  filter(key == 'forecast')%>%
  mutate(service_hour_date = seq(from = as.Date('2020-08-30'), by='day', length.out = 28))%>%
  select(admin_town_en, service_hour_date, value)

lm_forecast$service_hour_date <- as.character(lm_forecast$service_hour_date)

# change column names from lm model to match others. Value is pred, sum_offline_scooter is actual
lm_forecast_date <- lm_forecast %>%
  mutate(sum_offline_scooter.x = NA,
         sum_offline_scooter.y = value)

lm_forecast_date <- lm_forecast_date[,-3]

forecast_results <- rbind(train_s1_x, lm_forecast_date)

forecast_results$sum_offline_scooter.y <- ifelse(forecast_results$service_hour_date >= as.Date('2020-08-30') & forecast_results$service_hour_date <= as.Date('2020-09-12'), NA, forecast_results$sum_offline_scooter.y)

# train_valid_df  <- forecast_results %>%
#   filter(admin_town_en == "Da’an Dist" | admin_town_en == "Neihu Dist" | admin_town_en == "Xinzhuang Dist")

plot.ts <- function(data, time){
  plot <- data %>%
    group_by(admin_town_en)%>%
    ggplot(aes(as.Date(service_hour_date), sum_offline_scooter.x, col = I("black")))+
    geom_line(aes(as.Date(service_hour_date),sum_offline_scooter.y, col = I("red")))+
    geom_line(size=.4)+
    guides(color=F)+
    ylim(0, 2500)+
    scale_x_date(breaks = function(x) seq.Date(from = min(x), to = max(x), by = "2 month"))+
    facet_wrap(~admin_town_en, nrow=5, scales='free_y')+
    labs(x='', title=paste("Forecast Time series for offline scooters in", time))
  return(plot)
}

print(plot.ts(forecast_results, '00:00:00'))
# print(plot.ts(train_valid_df, '00:00:00'))
