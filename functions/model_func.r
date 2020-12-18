library(tidyverse)
library(forecast)
library(timetk)
library(Metrics)
library(lubridate)
library(sweep)

### Calculate RMSE for each districts

dist.rmse <- function(model_error){
  model_rmse <- model_error%>%
    group_by(admin_town_en)%>%
    summarize(rm = rmse(sum_offline_scooter,forecast))
  return(model_rmse)
}

### Naive Forecast
# Create naive model
# e.g.
# Naive.model(train_s1, test_s1, 30, '2020-08-01')

Naive.model <- function(train_data, test_data, repeated_day, last_naive_day){
  # Create naive prediction items
  naive_pred <- input_data%>%
    mutate(service_hour_date = as.Date(service_hour_date))%>%
    group_by(admin_town_en)%>%
    filter(service_hour_date == last(service_hour_date))%>%
    pull(sum_offline_scooter)
  
  naive_towns <- input_data%>%
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
  naive_forecast_results <- naive_forecast_date %>%
    mutate(error = forecast-sum_offline_scooter)
  
  return(naive_forecast_results)
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
