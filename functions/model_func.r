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

dist.rmse.train <- function(model_result){
  model_rmse <- model_result%>%
    group_by(admin_town_en, model, roll_forward, shift)%>%
    summarize(result.rmse = rmse(forecast,sum_offline_scooter), .groups = 'drop')
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
  
  naive_forecast_date <- naive_forecast_date[,-1]
  
  dist_train_naive <- data.frame(admin_town_en = train_data$admin_town_en, 
                                 sum_offline_scooter = train_data$sum_offline_scooter, 
                                 service_hour_date = train_data$service_hour_date, 
                                 shift = train_data$shift, 
                                 weekend_or_weekday = train_data$weekend_or_weekday)
  dist_train_naive$forecast[2:nrow(train_data)] <- train_data$sum_offline_scooter[1:(nrow(train_data)-1)]
  dist_train_naive <- dist_train_naive%>%
    group_by(admin_town_en)%>%
    mutate(forecast = ifelse(service_hour_date == first(service_hour_date), NA, forecast ))
  dist_train_naive <- dist_train_naive %>%
    mutate(model = "naive")
  
  dist_train_naive <- as.data.frame(dist_train_naive)

  return(list(naive_forecast_date, dist_train_naive))
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
ARIMA.model <- function(nest_ts, train_data, test_data, repeated_day, last_day){
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
  
  ar_dist_train <- data.frame()
  
  for (k in 1:3) {
    #get fitted value
    ar_fitted <- data.frame(ar_models[[3]][[k]]$fitted)
    ar_fitted$admin_town_en <- ar_models$admin_town_en[k]
    
    # conbine fitted and actual
    dist_train <- train_data%>%
      filter(admin_town_en == ar_fitted$admin_town_en[k])
    dist_train$forecast <- ar_fitted$x
    
    # label your model forecasts for later visualization
    dist_train <- dist_train %>%
      mutate(model = "arima")
    ar_dist_train <- rbind(ar_dist_train,dist_train)
  }
  
  return(list(ar_forecast_date, ar_dist_train))
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

lm.model <- function(nest_ts, train_data, test_data, repeated_day, last_day){
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
  
  lm_dist_train <- data.frame()
  
  for (k in 1:3) {
    #get fitted value
    lm_fitted <- data.frame(lm_models[[3]][[k]]$fitted.values)
    lm_fitted$admin_town_en <- lm_models$admin_town_en[k]
    
    # conbine fitted and actual
    dist_train <- train_data%>%
      filter(admin_town_en == lm_fitted$admin_town_en[k])
    dist_train$forecast <- lm_fitted$lm_models..3....k...fitted.values
    
    # label your model forecasts for later visualization
    dist_train <- dist_train %>%
      mutate(model = "lm")
    dist_train$forecast <- as.numeric(dist_train$forecast)
    
    lm_dist_train <- rbind(lm_dist_train,dist_train)
  }
  
  return(list(lm_forecast_date, lm_dist_train))
}

################# 
# ETS FORECAST
ets.model <- function(nest_ts, train_data, test_data, repeated_day, last_day){
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
  
  ets_dist_train <- data.frame()
  
  for (k in 1:3) {
    #get fitted value
    ets_fitted <- data.frame(ets_models[[3]][[k]]$fitted)
    ets_fitted$admin_town_en <- ets_models$admin_town_en[k]
    
    # conbine fitted and actual
    dist_train <- train_data%>%
      filter(admin_town_en == ets_fitted$admin_town_en[k])
    dist_train$forecast <- ets_fitted$y
    
    # label your model forecasts for later visualization
    dist_train <- dist_train %>%
      mutate(model = "ets")
    ets_dist_train <- rbind(ets_dist_train,dist_train)
  }
  
  return(list(ets_forecast_date, ets_dist_train))
}

################# 
# Seasonal NAIVE FORECAST

snaive.model <- function(train_data, test_df, n){
  dist_valid_snaive <- data.frame(admin_town_en = test_df$admin_town_en, 
                                  #sum_offline_scooter = test_df$sum_offline_scooter, 
                                  service_hour_date = test_df$service_hour_date, 
                                  shift = test_df$shift, 
                                  weekend_or_weekday = test_df$weekend_or_weekday)
  
  dist_valid_snaive$sum_offline_scooter[8:length(test_df$sum_offline_scooter)] <- test_df$sum_offline_scooter[1:(length(test_df$sum_offline_scooter)-7)]
  
  # label your model forecasts for later visualization
  dist_valid_snaive <- dist_valid_snaive %>%
    mutate(model = "snaive")
  
  dist_valid_snaive <- dist_valid_snaive %>%
    filter(as.Date(service_hour_date) < as.Date('2020-08-31'))
  
  dist_valid_snaive <- as.data.frame(dist_valid_snaive)
  
  snaive_forecast_date <- dist_valid_snaive %>%
    select(admin_town_en, service_hour_date, sum_offline_scooter, model)
  
  #join with actual values in validation
  snaive_forecast_date <- snaive_forecast_date %>%
    left_join(test_df, by = c('service_hour_date'='service_hour_date', 'admin_town_en'))
  
  dist_train_snaive <- data.frame(admin_town_en = train_data$admin_town_en,
                                  sum_offline_scooter = train_data$sum_offline_scooter,
                                  service_hour_date = train_data$service_hour_date,
                                  shift = train_data$shift,
                                  weekend_or_weekday = train_data$weekend_or_weekday)
  
  dist_train_snaive$forecast[8:nrow(train_data)] <- train_data$sum_offline_scooter[1:(nrow(train_data)-7)]
  
  dist_train_snaive <- dist_train_snaive%>%
    group_by(admin_town_en)%>%
    mutate(forecast = ifelse(as.Date(service_hour_date) < as.Date('2020-02-08'),  NA, forecast ))
  
  # label your model forecasts for later visualization
  dist_train_snaive <- dist_train_snaive %>%
    mutate(model = "snaive")
  
  dist_train_snaive <- as.data.frame(dist_train_snaive)
  
  return(list(snaive_forecast_date, dist_train_snaive))
}


################# 
#Nerual Net

nn.model <- function(nest_ts, train_data, test_data, repeated_day, last_day){
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
  
  nn_dist_train <- data.frame()
  
  for (k in 1:3) {
    #get fitted value
    nn_fitted <- data.frame(nn_models[[3]][[k]][["fitted"]])
    nn_fitted$admin_town_en <- nn_models$admin_town_en[k]
    # conbine fitted and actual
    dist_train <- train_data%>%
      filter(admin_town_en == nn_fitted$admin_town_en[k])
    dist_train$forecast <- nn_fitted[1]
    
    # label your model forecasts for later visualization
    dist_train <- dist_train %>%
      mutate(model = "nn")

    dist_train$forecast <- data.matrix(dist_train$forecast)
    
    nn_dist_train <- rbind(nn_dist_train,dist_train)
  }
  
  return(list(nn_forecast_date, nn_dist_train))
}

#####################
# MOVING AVERAGE

mv.model <- function(nest_ts, train_data, test_data, repeated_day, last_day){
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
  
  mv_dist_train <- data.frame()
  
  for (k in 1:3) {
    #get fitted value
    mv_fitted <- data.frame(mv_models$mv_fit[[k]])
    mv_fitted$admin_town_en <- mv_models$admin_town_en[k]
    
    # conbine fitted and actual
    dist_train <- train_data%>%
      filter(admin_town_en == mv_fitted$admin_town_en[k])
    dist_train$forecast <- NA
    dist_train[12:nrow(dist_train),]$forecast <- mv_fitted$sum_offline_scooter
    
    # label your model forecasts for later visualization
    dist_train <- dist_train %>%
      mutate(model = "mv")
    
    mv_dist_train <- rbind(mv_dist_train,dist_train)
  }
  return(list(mv_forecast_date, mv_dist_train))
}
