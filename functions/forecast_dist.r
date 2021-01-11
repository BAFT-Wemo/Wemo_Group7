source("functions/data_func.r")
source("functions/model_func.r")

lm_fitted_model <- function(nest_ts, test_data, repeated_day, last_day){
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
  
  # join with actual values in train
  for (i in 1:length()) {
    #get fitted value
    lm_fitted <- data.frame(lm_models[[3]][[i]]$fitted.values)
    lm_fitted$admin_town_en <- lm_models$admin_town_en[i]
    
    # conbine fitted and actual
    dist_train <- train_s1%>%
      filter(admin_town_en == lm_fitted$admin_town_en[i])
    dist_train$forecast <- lm_fitted$lm_models..3....i...fitted.values
    
    # label your model forecasts for later visualization
    dist_train <- dist_train %>%
      mutate(model = "lm")
    dist_train$forecast <- as.numeric(dist_train$forecast)
    
    #rbind to one dataframe
    full_df_train <- rbind(dist_train, full_df_train)
  }
  
}



