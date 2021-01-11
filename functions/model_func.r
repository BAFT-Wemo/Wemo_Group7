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
    group_by(admin_town_en, model, roll_forward, shift)%>%
    summarize(result.rmse = rmse(sum_offline_scooter.y,sum_offline_scooter.x), .groups = 'drop')
  return(model_rmse)
}

# daan.rmse <- daan %>%
#   group_by(model) %>%
#   summarise(
#     RMSE = rmse(sum_offline_scooter.x,sum_offline_scooter.y)
#     ,R2 = cor(sum_offline_scooter.x, sum_offline_scooter.y)^2
#   )

rmse_boxplot <- function(rmse_data, shift_time){
  plot <- rmse_data %>%
    filter(shift == shift_time) %>%
    ggplot(aes(model, result.rmse,fill = model))+
    geom_boxplot()+
    facet_wrap(~admin_town_en)+
    labs(title = paste("rmse",shift_time))
  return(plot)
}

error_boxplot <- function(error_data, shift_time){
  plot <- error_data %>%
    filter(shift == shift_time) %>%
    ggplot(aes(model, error,fill = model))+
    geom_boxplot()+
    facet_wrap(~admin_town_en)+
    labs(title = paste("error", shift_time))
  return(plot)
}

model_name <- c("naive", "snaive", "ets", "lm", "arima", "mv", "nn")

#####################
### Naive Forecast
# Create naive model
# e.g.
# Naive.model(train_s1, test_data, 30, '2020-08-01')

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
# naive_result <- Naive.model(train_s1, test_data, 30, '2020-08-01')
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

ar_model_train <- function(train_df){
  # join with actual values in train
  for (i in 1:19) {
    #get fitted value
    ar_fitted <- data.frame(ar_models[[3]][[i]]$fitted)
    ar_fitted$admin_town_en <- ar_models$admin_town_en[i]
    
    # conbine fitted and actual
    dist_train <- train_df%>%
      filter(admin_town_en == ar_fitted$admin_town_en[i])
    dist_train$forecast <- ar_fitted$x
    
    # label your model forecasts for later visualization
    dist_train <- dist_train %>%
      mutate(model = "ar")
    
    #rbind to one dataframe
    full_df_train <- rbind(dist_train,full_df_train)
  }
  return(full_df_train)
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
                        .f = function(x) tslm(x ~ trend + season)))
  
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

snaive.model <- function(test_df, n){
  dist_valid_snaive <- data.frame(admin_town_en = test_df$admin_town_en, 
                                  #sum_offline_scooter = test_df$sum_offline_scooter, 
                                  service_hour_date = test_df$service_hour_date, 
                                  shift = test_df$shift, 
                                  weekend_or_weekday = test_df$weekend_or_weekday)
  
  dist_valid_snaive$sum_offline_scooter[8:length(test_df$sum_offline_scooter)] <- test_df$sum_offline_scooter[1:(length(test_df$sum_offline_scooter)-7)]
  
  # label your model forecasts for later visualization
  dist_valid_snaive <- dist_valid_snaive %>%
    mutate(model = "snaive")
  
  dist_valid_snaive <- as.data.frame(dist_valid_snaive)
  
  snaive_forecast_date <- dist_valid_snaive %>%
    select(admin_town_en, service_hour_date, sum_offline_scooter, model)
  
  #join with actual values in validation
  snaive_forecast_date <- snaive_forecast_date %>%
    left_join(test_df, by = c('service_hour_date'='service_hour_date', 'admin_town_en'))
  
  return(snaive_forecast_date)
}


################# 
#Nerual Net

nn.model <- function(nest_ts, test_data, repeated_day, last_day){
  nn_models <- nest_ts %>%
    mutate(nn_fit = map(.x=dem_df,
                        .f = function(x) nnetar(x, repeats = 5, size=10)))
  
  nn_forecast <- nn_models %>%
    mutate(fcast = map(nn_fit,
                       forecast,
                       h=repeated_day))%>%
    mutate(swp = map(fcast, sw_sweep, fitted=FALSE))%>%
    unnest(swp)%>%
    filter(key == 'forecast')%>%
    mutate(service_hour_date = seq(from = as.Date(last_day), by='day', length.out = repeated_day))%>%
    select(admin_town_en, service_hour_date, sum_offline_scooter)
  
  nn_forecast$service_hour_date <- as.character(nn_forecast$service_hour_date)
  
  #join with actual values in validation
  nn_forecast_date <- nn_forecast %>%
    left_join(test_data, by = c('service_hour_date'='service_hour_date', 'admin_town_en'))
  
  #label your model forecasts for later visualization
  nn_forecast_date <- nn_forecast_date %>%
    mutate(model = 'nn')
  
  return(nn_forecast_date)
}

#####################
# MOVING AVERAGE

mv.model <- function(nest_ts, test_data, repeated_day, last_day){
  mv_models <- nest_ts %>%
    mutate(mv_fit = map(.x=dem_df,
                        .f = function(x) rollmean(x, k = 12, align = "right")))
  
  mv_forecast <- mv_models %>%
    mutate(fcast = map(mv_fit,
                       forecast,
                       h=repeated_day))%>%
    mutate(swp = map(fcast, sw_sweep, fitted=FALSE))%>%
    unnest(swp)%>%
    filter(key == 'forecast')%>%
    mutate(service_hour_date = seq(from = as.Date(last_day), by='day', length.out = repeated_day))%>%
    select(admin_town_en, service_hour_date, sum_offline_scooter)
  
  mv_forecast$service_hour_date <- as.character(mv_forecast$service_hour_date)
  
  # join with actual values in validation
  mv_forecast_date <- mv_forecast %>%
    left_join(test_data, by = c('service_hour_date'='service_hour_date', 'admin_town_en'))
  
  # label your model forecasts for later visualization
  mv_forecast_date <- mv_forecast_date %>%
    mutate(model = 'mv')
}