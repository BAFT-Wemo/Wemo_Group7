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

# Filter area & time (days without 3 shifts)
wemo.df.new <- wemo.df%>%
  filter(admin_town_zh != "三重區" & admin_town_zh != "超出營運範圍"& admin_town_zh != "泰山區"
         & admin_town_zh != "五股區" & admin_town_zh != "土城區" & admin_town_zh != "樹林區" &
           admin_town_zh != "汐止區")

# Change type to character
wemo.df.new$service_hour_date <- as.character(wemo.df.new$service_hour_date)

# Shift1 + Whole week
shift_3 <- wemo.df.new%>%
  filter(shift == '00:00:00')

### Create train/validation sets
# Whole week
train_s3 <- shift_3%>%
  filter(service_hour_date <= as.Date('2020-08-29'))

train_s3_d1_d3 <- train_s3%>%
  filter(admin_town_zh != "信義區" & admin_town_zh != "松山區"& admin_town_zh != "內湖區" 
         & admin_town_zh != "文山區" & admin_town_zh != "士林區" & admin_town_zh != "中和區")

#####################
# Naive
#set up naive for comparison. final date training period sales: 2020-07-31
naive_pred_s3 <- train_s3_d1_d3%>%
  mutate(service_hour_date = as.Date(service_hour_date))%>%
  group_by(admin_town_en)%>%
  filter(service_hour_date == last(service_hour_date))%>%
  pull(sum_offline_scooter)

naive_towns_s3 <- train_s3_d1_d3%>%
  mutate(service_hour_date = as.Date(service_hour_date))%>%
  group_by(admin_town_en)%>%
  filter(service_hour_date == last(service_hour_date))%>%
  pull(admin_town_en)

#create a 'naive df' of final value repeated 30 days forward
naive_df_s3 <- data.frame(forecast = rep(naive_pred_s3, 28),
                          admin_town_en = rep(naive_towns_s3, 28))
naive_s3 <- naive_df_s3%>%
  group_by(admin_town_en)%>%
  mutate(service_hour_date = seq.Date(as.Date('2020-08-30'),
                                      by='day', length.out = 28))

naive_s3$service_hour_date <- as.character(naive_s3$service_hour_date)


# change column names from naive model to match others. forecast is pred, sum_offline_scooter is actual
naive_forecast_date <- naive_s3 %>%
  mutate(sum_offline_scooter.x = NA,
         sum_offline_scooter.y = forecast)
naive_forecast_date <- naive_forecast_date[, -1]

train_s3_x <- train_s3 %>%
  mutate(sum_offline_scooter.x = sum_offline_scooter,
         sum_offline_scooter.y = NA)%>%
  select(admin_town_en, service_hour_date, sum_offline_scooter.x, sum_offline_scooter.y)

forecast_results <- rbind(train_s3_x, naive_forecast_date)

train_s3_d2 <- train_s3%>%
  filter(admin_town_zh == "信義區" | admin_town_zh == "松山區" | admin_town_zh == "士林區"
         | admin_town_zh == "內湖區" | admin_town_zh == "文山區" | admin_town_zh == "中和區")

### nest
nest_s3 <- train_s3_d2%>%
  mutate(service_hour_date = ymd(service_hour_date))%>%
  group_by(admin_town_en)%>%
  select(-admin_town_en, sum_offline_scooter)%>%
  nest(.key= 'dem_df')

nest_s3_ts <- nest_s3 %>%
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
lm_models <- nest_s3_ts %>%
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

forecast_results <- rbind(forecast_results, lm_forecast_date)

forecast_results$sum_offline_scooter.y <- ifelse(forecast_results$service_hour_date >= as.Date('2020-08-30') & forecast_results$service_hour_date <= as.Date('2020-09-12'), NA, forecast_results$sum_offline_scooter.y)

plot.ts <- function(data, time){
  plot <- data %>%
    group_by(admin_town_en)%>%
    ggplot(aes(as.Date(service_hour_date), sum_offline_scooter.x, col = I("black")))+
    geom_line(aes(as.Date(service_hour_date),sum_offline_scooter.y, col = I("red")))+
    geom_line(size=.4)+
    guides(color=F)+
    ylim(0, 3000)+
    scale_x_date(breaks = function(x) seq.Date(from = min(x), to = max(x), by = "2 month"))+
    facet_wrap(~admin_town_en, nrow=5, scales='free_y')+
    labs(x='', title=paste("Forecast Time series for offline scooters in", time))
  return(plot)
}

print(plot.ts(forecast_results, '00:00:00'))
