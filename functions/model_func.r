library(tidyverse)
library(forecast)
library(timetk)
library(Metrics)
library(lubridate)
library(sweep)
library(caret)

### Calculate RMSE for each districts

dist.rmse <- function(model_result){
  model_rmse <- model_result%>%
    group_by(admin_town_en, model, roll_forward)%>%
    summarize(result.rmse = rmse(sum_offline_scooter.y,sum_offline_scooter.x), .groups = 'drop')
  return(model_rmse)
}

#####################
### Naive Forecast
# Create naive model
# e.g.
# Naive.model(train_s1, test_s1, 30, '2020-08-01')

Naive.model <- function(train_data, test_data, repeated_day, last_naive_day){
  # Create naive prediction items
  naive_pred <- train_data%>%
    mutate(service_hour_date = as.Date(service_hour_date))%>%
    group_by(admin_town_en)%>%
    filter(service_hour_date == last(service_hour_date))%>%
    pull(sum_offline_scooter)
  
  naive_towns <- train_data%>%
    mutate(service_hour_date = as.Date(service_hour_date))%>%
    group_by(admin_town_en)%>%
    filter(service_hour_date == last(service_hour_date))%>%
    pull(admin_town_en)
  
  #create a 'naive df' of final value repeated 30 days forward
  naive_df <- data.frame(forecast = rep(naive_pred, repeated_day),
                            admin_town_en = rep(naive_towns, repeated_day))
  naive <- naive_df%>%
    group_by(admin_town_en)%>%
    mutate(service_hour_date = seq.Date(as.Date(last_naive_day),
                                        by='day', length.out = repeated_day))
  
  naive$service_hour_date <- as.character(naive$service_hour_date)
  
  #Join test set
  naive_forecast_date <- naive %>%
    inner_join(test_data, by = c('service_hour_date', 'admin_town_en'))
  
  # label your model forecasts for later visualization
  navie_forecast_date <- naive_forecast_date %>%
    mutate(model = 'naive')
  
  #calculate forecast error i
  naive_forecast_date <- naive_forecast_date %>%
    mutate(error = forecast-sum_offline_scooter)
  
  naive_forecast_date <- navie_forecast_date %>%
    mutate(sum_offline_scooter.x = forecast,
           sum_offline_scooter.y = sum_offline_scooter,
           value = NULL,
           sum_offline_scooter = NULL)

  return(naive_forecast_date)
}

# Plot naive error series.
# e.g.
# naive_result <- Naive.model(train_s1, test_s1, 30, '2020-08-01')
# naive.plot(naive_result)

naive.plot <- function(naive_result){
  plot <- naive_result %>%
    ggplot(aes(service_hour_date, error, color=admin_town_en, group=admin_town_en))+
    geom_line()+
    guides(color=F)+
    facet_wrap(~admin_town_en, nrow=5)
  return(plot)
}

#####################
### AUTOARIMA. 
ARIMA.model <- function(nest_ts, test_data, repeated_day, last_day){
  ar_models <- nest_ts %>%
    mutate(ar_fit = map(.x=dem_df,
                        .f = auto.arima))
  ### TIDYING UP
  # FORECAST in testing for 30 days
  ar_forecast <- ar_models %>%
    mutate(fcast = map(ar_fit,
                       forecast,
                       h=repeated_day))%>%
    mutate(swp = map(fcast, sw_sweep, fitted=FALSE))%>%
    unnest(swp)%>%
    filter(key == 'forecast')%>%
    mutate(service_hour_date = seq(from = as.Date(last_day), by='day', length.out = repeated_day))%>%
    select(admin_town_en, service_hour_date, sum_offline_scooter)
  
  ar_forecast$service_hour_date <- as.character(ar_forecast$service_hour_date)
  
  # join with actual values in validation
  ar_forecast_date <- ar_forecast %>%
    left_join(test_data, by = c('service_hour_date'='service_hour_date', 'admin_town_en'))
  
  # label your model forecasts for later visualization
  ar_forecast_date <- ar_forecast_date %>%
    mutate(model = 'arima')
  
  return(ar_forecast_date)
}

# plot forecasts to verify nothing insane happened
ar.plot <- function(ar_result){
  plot <- ar_result %>%
    group_by(admin_town_en)%>%
    ggplot(aes(service_hour_date, sum_offline_scooter, color=admin_town_en, group=admin_town_en))+
    geom_line(size=1)+
    labs(x='', title='ARIMA plot for [shift1] in [whole week]')
  return(plot)
}

#####################
# LINEAR REGRESSION FORECAST
## FORECAST in testing for 30 days

lm.model <- function(nest_ts, test_data, repeated_day, last_day){
  lm_models <- nest_ts %>%
    mutate(lm_fit = map(.x=dem_df,
                        .f = function(x) tslm(x ~ trend)))
  
  lm_forecast <- lm_models %>%
    mutate(fcast = map(lm_fit,
                       forecast,
                       h=repeated_day))%>%
    mutate(swp = map(fcast, sw_sweep, fitted=FALSE))%>%
    unnest(swp)%>%
    filter(key == 'forecast')%>%
    mutate(service_hour_date = seq(from = as.Date(last_day), by='day', length.out = repeated_day))%>%
    select(admin_town_en, service_hour_date, value)
  
  lm_forecast$service_hour_date <- as.character(lm_forecast$service_hour_date)
  
  # join with actual values in validation
  lm_forecast_date <- lm_forecast %>%
    left_join(test_data, by = c('service_hour_date'='service_hour_date', 'admin_town_en'))
  
  # label your model forecasts for later visualization
  lm_forecast_date <- lm_forecast_date %>%
    mutate(model = 'lm')
  
  lm_forecast_date <- lm_forecast_date %>%
    mutate(sum_offline_scooter.x = value,
           sum_offline_scooter.y = sum_offline_scooter,
           value = NULL,
           sum_offline_scooter = NULL)
  
  return(lm_forecast_date)
}

################# 
# ETS FORECAST
ets.model <- function(nest_ts, test_data, repeated_day, last_day){
  ets_models <- nest_ts %>%
    mutate(ets_fit = map(.x=dem_df,
                         .f = ets))
  
  ets_forecast <- ets_models %>%
    mutate(fcast = map(ets_fit,
                       forecast,
                       h=repeated_day))%>%
    mutate(swp = map(fcast, sw_sweep, fitted=FALSE))%>%
    unnest(swp)%>%
    filter(key == 'forecast')%>%
    mutate(service_hour_date = seq(from = as.Date(last_day), by='day', length.out = repeated_day))%>%
    select(admin_town_en, service_hour_date, sum_offline_scooter)
  
  ets_forecast$service_hour_date <- as.character(ets_forecast$service_hour_date)
  
  #join with actual values in validation
  ets_forecast_date <- ets_forecast %>%
    left_join(test_data, by = c('service_hour_date'='service_hour_date', 'admin_town_en'))
  
  #label your model forecasts for later visualization
  ets_forecast_date <- ets_forecast_date %>%
    mutate(model = 'ets')
  return(ets_forecast_date)
}

################# 
# Seasonal NAIVE FORECAST

snaive.model <- function(nest_ts, test_data, repeated_day, last_day){
  snaive_models <- nest_ts %>%
    mutate(snaive_fit = map(.x=dem_df,
                            .f = snaive))
  
  snaive_forecast <- snaive_models %>%
    mutate(fcast = map(snaive_fit,
                       forecast,
                       h=repeated_day))%>%
    mutate(swp = map(fcast, sw_sweep, fitted=FALSE))%>%
    unnest(swp)%>%
    filter(key == 'forecast')%>%
    mutate(service_hour_date = seq(from = as.Date(last_day), by='day', length.out = repeated_day))%>%
    select(admin_town_en, service_hour_date, sum_offline_scooter)
  
  snaive_forecast$service_hour_date <- as.character(snaive_forecast$service_hour_date)
  # join with actual values in validation
  snaive_forecast_date <- snaive_forecast %>%
    left_join(test_data, by = c('service_hour_date'='service_hour_date', 'admin_town_en'))
  
  # CHECK ACCURACY ON TEST SET. x is pred, y is actual. RMSE 363.9651
  snaive_forecast_accuracy <- forecast::accuracy(snaive_forecast_date$sum_offline_scooter.y, snaive_forecast_date$sum_offline_scooter.x)
  
  # label your model forecasts for later visualization
  snaive_forecast_date <- snaive_forecast_date %>%
    mutate(model = 'snaive')
  
  return(snaive_forecast_date)
}



