library(tidyverse)
library(forecast)
library(timetk)
library(Metrics)
library(lubridate)
library(sweep)

# Read in data
wemo.df <- read.csv("Downloads/Data_Jan_to_Aug.csv")
wemo.df$service_hour=as.POSIXct(paste(wemo.df$service_hour_date, wemo.df$shift), format="%Y-%m-%d %H:%M:%S")

# Filter area & time (days without 3 shifts)
wemo.df.new <- wemo.df%>%
  filter(admin_town_zh != "三重區" & admin_town_zh != "超出營運範圍"& admin_town_zh != "泰山區"
         & admin_town_zh != "五股區" & admin_town_zh != "土城區" & admin_town_zh != "樹林區" & 
           admin_town_zh != "汐止區" & service_hour_date != "2020-01-31" & service_hour_date != "2020-08-31")

# Derived variable (Weekend or weekday)
wemo.df.new$weekday<-weekdays(wemo.df.new$service_hour)
wemo.df.new$weekend_or_weekday<-ifelse(wemo.df.new$weekday=="Saturday" | wemo.df.new$weekday == "Sunday", 1, 0)

# Change type to character
wemo.df.new$service_hour_date <- as.character(wemo.df.new$service_hour_date)

# Filter columns
wemo.df.new <- wemo.df.new %>% 
  select(admin_town_en, sum_offline_scooter, service_hour_date, shift, weekend_or_weekday)

# Plot time series for offline scooters (whole week & no shift)
wemo.df.new %>%
  group_by(admin_town_en)%>%
  ggplot(aes(as.Date(service_hour_date), sum_offline_scooter, color=admin_town_en))+
  geom_line(size=.4)+
  guides(color=F)+
  facet_wrap(~admin_town_en, nrow=5, scales='free_y')+
  labs(x='', title='Time series for offline scooters')

# Shifts + Whole week
shift_1 <- wemo.df.new%>%
  filter(shift == '00:00:00')
shift_2 <- wemo.df.new%>%
  filter(shift == '08:00:00')
shift_3 <- wemo.df.new%>%
  filter(shift == '16:00:00')

# Shift 1
shift_1_weekend <- wemo.df.new%>%
  filter(shift == '00:00:00'&weekend_or_weekday == 1)
shift_1_weekday <- wemo.df.new%>%
  filter(shift == '00:00:00'&weekend_or_weekday == 0)

shift_1_weekday %>%
  group_by(admin_town_en)%>%
  ggplot(aes(as.Date(service_hour_date), sum_offline_scooter, color=admin_town_en))+
  geom_line(size=.4)+
  guides(color=F)+
  facet_wrap(~admin_town_en, nrow=5, scales='free_y')+
  labs(x='', title='Time series for offline scooters in 00:00 weekday')

# Shift 2
shift_2_weekend <- wemo.df.new%>%
  filter(shift == '08:00:00'&weekend_or_weekday == 1)
shift_2_weekday <- wemo.df.new%>%
  filter(shift == '08:00:00'&weekend_or_weekday == 0)

shift_2_weekday %>%
  group_by(admin_town_en)%>%
  ggplot(aes(as.Date(service_hour_date), sum_offline_scooter, color=admin_town_en))+
  geom_line(size=.4)+
  guides(color=F)+
  facet_wrap(~admin_town_en, nrow=5, scales='free_y')+
  labs(x='', title='Time series for offline scooters in 08:00 weekday')

# Shift 3
shift_3_weekend <- wemo.df.new%>%
  filter(shift == '16:00:00'&weekend_or_weekday == 1)
shift_3_weekday <- wemo.df.new%>%
  filter(shift == '16:00:00'&weekend_or_weekday == 0)

shift_3_weekday %>%
  group_by(admin_town_en)%>%
  ggplot(aes(as.Date(service_hour_date), sum_offline_scooter, color=admin_town_en))+
  geom_line(size=.4)+
  guides(color=F)+
  facet_wrap(~admin_town_en, nrow=5, scales='free_y')+
  labs(x='', title='Time series for offline scooters in 16:00 weekday')

### Create train/validation sets
# Whole week
train_s1 <- shift_1%>%
  filter(service_hour_date <= as.Date('2020-07-31'))
test_s1 <- shift_1%>%
  filter(service_hour_date > as.Date('2020-07-31'))

train_s2 <- shift_2%>%
  filter(service_hour_date <= as.Date('2020-07-31'))
test_s2 <- shift_2%>%
  filter(service_hour_date > as.Date('2020-07-31'))

train_s3 <- shift_3%>%
  filter(service_hour_date <= as.Date('2020-07-31'))
test_s3 <- shift_3%>%
  filter(service_hour_date > as.Date('2020-07-31'))


# WEEKDAY
train_s1_wd <- shift_1_weekday%>%
  filter(service_hour_date <= as.Date('2020-07-31'))

test_s1_wd <- shift_1_weekday%>%
  filter(service_hour_date > as.Date('2020-07-31'))

train_s2_wd <- shift_2_weekday%>%
  filter(service_hour_date <= as.Date('2020-07-31'))

test_s2_wd <- shift_2_weekday%>%
  filter(service_hour_date > as.Date('2020-07-31'))

train_s3_wd <- shift_3_weekday%>%
  filter(service_hour_date <= as.Date('2020-07-31'))

test_s3_wd <- shift_3_weekday%>%
  filter(service_hour_date > as.Date('2020-07-31'))


# WEEKEND
train_s1_we <- shift_1_weekend%>%
  filter(service_hour_date <= as.Date('2020-07-31'))

test_s1_we <- shift_1_weekend%>%
  filter(service_hour_date > as.Date('2020-07-31'))

train_s2_we <- shift_2_weekend%>%
  filter(service_hour_date <= as.Date('2020-07-31'))

test_s2_we <- shift_2_weekend%>%
  filter(service_hour_date > as.Date('2020-07-31'))

train_s3_we <- shift_3_weekend%>%
  filter(service_hour_date <= as.Date('2020-07-31'))

test_s3_we <- shift_3_weekend%>%
  filter(service_hour_date > as.Date('2020-07-31'))


#plot original train series on weekdays
train_s1_wd %>%
  group_by(admin_town_en)%>%
  ggplot(aes(as.Date(service_hour_date), sum_offline_scooter, color=admin_town_en))+
  geom_line(size=.4)+
  guides(color=F)+
  facet_wrap(~admin_town_en, nrow=5, scales='free_y')+
  labs(x='', title='Time series for offline scooters in 00:00 on training data in weekdays')

#plot original test series on weekdays
test_s1_wd %>%
  group_by(admin_town_en)%>%
  ggplot(aes(as.Date(service_hour_date), sum_offline_scooter, color=admin_town_en))+
  geom_line(size=.4)+
  guides(color=F)+
  facet_wrap(~admin_town_en, nrow=5, scales='free_y')+
  labs(x='', title='Time series for offline scooters in 00:00 on testing data in weekdays')

#plot original train series on weekends
train_s1_we %>%
  group_by(admin_town_en)%>%
  ggplot(aes(as.Date(service_hour_date), sum_offline_scooter, color=admin_town_en))+
  geom_line(size=.4)+
  guides(color=F)+
  facet_wrap(~admin_town_en, nrow=5, scales='free_y')+
  labs(x='', title='Time series for offline scooters in 00:00 on training data in weekends')

#plot original test series on weekends
test_s1_we %>%
  group_by(admin_town_en)%>%
  ggplot(aes(as.Date(service_hour_date), sum_offline_scooter, color=admin_town_en))+
  geom_line(size=.4)+
  guides(color=F)+
  facet_wrap(~admin_town_en, nrow=5, scales='free_y')+
  labs(x='', title='Time series for offline scooters in 00:00 on testing data in weekends')

### naive prediction
naive_pred_wd <- train_s1_wd%>%
  mutate(date = as.Date(service_hour_date))%>%
  group_by(admin_town_en)%>%
  filter(date == last(date))%>%
  pull(sum_offline_scooter)

naive_pred_we <- train_s1_we%>%
  mutate(date = as.Date(service_hour_date))%>%
  group_by(admin_town_en)%>%
  filter(date == last(date))%>%
  pull(sum_offline_scooter)

naive_towns_wd <- train_s1_wd%>%
  mutate(date = as.Date(service_hour_date))%>%
  group_by(admin_town_en)%>%
  filter(date == last(date))%>%
  pull(admin_town_en)

naive_towns_we <- train_s1_we%>%
  mutate(date = as.Date(service_hour_date))%>%
  group_by(admin_town_en)%>%
  filter(date == last(date))%>%
  pull(admin_town_en)

naive_df_wd <- data.frame(forecast = rep(naive_pred_wd, 20),
                       admin_town_en = rep(naive_towns_wd, 20),
                       service_hour_date = test_s1_wd$service_hour_date)

naive_df_wd <- naive_df_wd %>%
  inner_join(test_s1_wd, by = c('admin_town_en', 'service_hour_date'))

# calculate forecast error i
naive_df_wd <- naive_df_wd %>%
  mutate(error = forecast-sum_offline_scooter)

#plot error series
naive_df_wd%>%
  ggplot(aes(as.Date(service_hour_date), error, color=admin_town_en))+
  geom_line()+
  guides(color=F)+
  facet_wrap(~admin_town_en, nrow=5)+
  labs(x='', title='Rediduals for offline scooters in 00:00 weekday in testing set')

# CHECK ACCURACY ON TEST SET: RMSE 231.9899
forecast::accuracy(naive_df_wd$forecast, naive_df_wd$sum_offline_scooter)

# calculate each rmse
naive_df_wd %>%
  group_by(admin_town_en)%>%
  summarize(rm = rmse(sum_offline_scooter, forecast))

################### BUILD TS OBJECTS
# CONVERT TO DATE GROUP AND THEN NEST EACH MATERIAL INTO LIST COLUMNS
nest_s1_wd <- train_s1_wd%>%
  mutate(service_hour_date = ymd(service_hour_date))%>%
  group_by(admin_town_en)%>%
  dplyr::select(-admin_town_en, sum_offline_scooter)%>%
  nest(.key= 'dem_df')

nest_s1 <- train_s1%>%
  mutate(service_hour_date = ymd(service_hour_date))%>%
  group_by(admin_town_en)%>%
  dplyr::select(-admin_town_en, sum_offline_scooter)%>%
  nest(.key= 'dem_df')

# FOR EACH LIST COLUMN, CONVERT IT TO TIME SERIES OBJECT
nest_s1_wd_ts <- nest_s1_wd %>%
  mutate(dem_df = 
           map(.x = dem_df,
               .f = tk_ts,
               select = sum_offline_scooter, #select the outcome col
               start= c(2020,31), #Jan 31th 20 (needs a check!!!)
               #end = c(2020,210),
               deltat= 1/365)) #daily data

nest_s1_ts <- nest_s1 %>%
  mutate(dem_df = 
           map(.x = dem_df,
               .f = tk_ts,
               select = sum_offline_scooter, #select the outcome col
               start= c(2020,31), #Jan 31th 2011 (needs a check!!!)
               #end = c(2020,210),
               deltat= 1/365)) #daily data




#####################
# AUTOARIMA. 
ar_models <- nest_s1_ts %>%
  mutate(ar_fit = map(.x=dem_df,
                      .f = auto.arima))


### TIDYING UP
# FORECAST in testing for 30 days
ar_forecast <- ar_models %>%
  mutate(fcast = map(ar_fit,
                     forecast,
                     h=30))%>%
  mutate(swp = map(fcast, sw_sweep, fitted=FALSE))%>%
  unnest(swp)%>%
  filter(key == 'forecast')%>%
  mutate(service_hour_date = seq(from = as.Date('2020-08-01'), by='day', length.out = 30))%>%
  select(admin_town_en, service_hour_date, sum_offline_scooter)

ar_forecast$service_hour_date <- as.character(ar_forecast$service_hour_date)
  
# join with actual values in validation
ar_forecast_date <- ar_forecast %>%
  left_join(test_s1, by = c('service_hour_date'='service_hour_date', 'admin_town_en'))


# label your model forecasts for later visualization
ar_forecast_date <- ar_forecast_date %>%
  mutate(model = 'arima')

# CHECK ACCURACY ON TEST SET. x is pred, y is actual. RMSE 232.4308
forecast::accuracy(ar_forecast_date$sum_offline_scooter.y, ar_forecast_date$sum_offline_scooter.x)

# plot forecasts to verify nothing insane happened
ar_forecast %>%
  group_by(admin_town_en)%>%
  ggplot(aes(service_hour_date, sum_offline_scooter, color=admin_town_en, group=1))+
  geom_line(size=1)



#####################
# LINEAR REGRESSION FORECAST
## FORECAST in testing for 30 days
lm_models <- nest_s1_ts %>%
  mutate(lm_fit = map(.x=dem_df,
                      .f = function(x) tslm(x ~ trend)))

lm_forecast <- lm_models %>%
  mutate(fcast = map(lm_fit,
                     forecast,
                     h=30))%>%
  mutate(swp = map(fcast, sw_sweep, fitted=FALSE))%>%
  unnest(swp)%>%
  filter(key == 'forecast')%>%
  mutate(service_hour_date = seq(from = as.Date('2020-08-01'), by='day', length.out = 30))%>%
  select(admin_town_en, service_hour_date, value)

lm_forecast$service_hour_date <- as.character(lm_forecast$service_hour_date)

# join with actual values in validation
lm_forecast_date <- lm_forecast %>%
  left_join(test_s1, by = c('service_hour_date'='service_hour_date', 'admin_town_en'))

# label your model forecasts for later visualization
lm_forecast_date <- lm_forecast_date %>%
  mutate(model = 'lm')

# CHECK ACCURACY ON TEST SET: RMSE 296.1717
forecast::accuracy(lm_forecast_date$sum_offline_scooter, lm_forecast_date$value) #sum_offline_scooter = actual, value = forecast value




################# 
# ETS FORECAST
ets_models <- nest_s1_ts %>%
  mutate(ets_fit = map(.x=dem_df,
                       .f = ets))

ets_forecast <- ets_models %>%
  mutate(fcast = map(ets_fit,
                     forecast,
                     h=30))%>%
  mutate(swp = map(fcast, sw_sweep, fitted=FALSE))%>%
  unnest(swp)%>%
  filter(key == 'forecast')%>%
  mutate(service_hour_date = seq(from = as.Date('2020-08-01'), by='day', length.out = 30))%>%
  select(admin_town_en, service_hour_date, sum_offline_scooter)

ets_forecast$service_hour_date <- as.character(ets_forecast$service_hour_date)

#join with actual values in validation
ets_forecast_date <- ets_forecast %>%
  left_join(test_s1, by = c('service_hour_date'='service_hour_date', 'admin_town_en'))

#label your model forecasts for later visualization
ets_forecast_date <- ets_forecast_date %>%
  mutate(model = 'ets')

#CHECK ACCURACY ON TEST SET. x is pred, y is actual. RMSE 626.9194
forecast::accuracy(ets_forecast_date$sum_offline_scooter.y, ets_forecast_date$sum_offline_scooter.x)




################# 
# Seasonal NAIVE FORECAST
naive_models <- nest_s1_ts %>%
  mutate(naive_fit = map(.x=dem_df,
                         .f = snaive))

naive_forecast <- naive_models %>%
  mutate(fcast = map(naive_fit,
                     forecast,
                     h=30))%>%
  mutate(swp = map(fcast, sw_sweep, fitted=FALSE))%>%
  unnest(swp)%>%
  filter(key == 'forecast')%>%
  mutate(service_hour_date = seq(from = as.Date('2020-08-01'), by='day', length.out = 30))%>%
  select(admin_town_en, service_hour_date, sum_offline_scooter)


naive_forecast$service_hour_date <- as.character(naive_forecast$service_hour_date)
# join with actual values in validation
naive_forecast_date <- naive_forecast %>%
  left_join(test_s1, by = c('service_hour_date'='service_hour_date', 'admin_town_en'))


# CHECK ACCURACY ON TEST SET. x is pred, y is actual. RMSE 363.9651
forecast::accuracy(naive_forecast_date$sum_offline_scooter.y, naive_forecast_date$sum_offline_scooter.x)


# label your model forecasts for later visualization
naive_forecast_date <- naive_forecast_date %>%
  mutate(model = 'snaive')



############ Combine all into one long DF
# LOOK AT PREDICTION ERROR FOR ALL MODELS

# change column names from lm model to match others. Value is pred, sum_offline_scooter is actual
lm_forecast_date <- lm_forecast_date %>%
  mutate(sum_offline_scooter.x = value,
         sum_offline_scooter.y = sum_offline_scooter,
         value = NULL,
         sum_offline_scooter = NULL)

# Combine all models into one long DF
full_df <- rbind(lm_forecast_date, 
                 ar_forecast_date,
                 ets_forecast_date,
                 naive_forecast_date)

full_df <- full_df%>%
  mutate(error = sum_offline_scooter.x - sum_offline_scooter.y)

# Conclude from the visual (error in all models)
full_df%>%
  ggplot(aes(service_hour_date, error, color=model, group=model))+
  geom_line()+
  facet_wrap(~admin_town_en, ncol =2, scale='free_y')

