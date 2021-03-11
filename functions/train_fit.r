#Train fitted
naive_train <- function(train_data){
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
  return(dist_train_naive)
}

snaive_train <-function(train_data){
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
  
  return(dist_train_snaive)
}



