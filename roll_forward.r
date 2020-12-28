source("functions/data_func.r")
source("functions/model_func.r")

results_mse <- data.frame()

for (i in 1:8){
  for (j in 1:3){
    shift <- separate_shift(wemo.df.new, shift.time[j])
    train <- train_data(shift, roll_forward[i])
    test <- test_data(shift, roll_forward[i])
    naive_result <- Naive.model(train, test, 28, roll_forward[i])
    n_ts <- nest_ts(nest_a(train))
    ar_result <- ARIMA.model(n_ts, test, 28, roll_forward[i])
    lm_result <- lm.model(n_ts, test, 28, roll_forward[i])
    ets_result <- ets.model(n_ts, test, 28, roll_forward[i])
    snaive_result <- snaive.model(n_ts, test, 28, roll_forward[i])
    
    full_df <- rbind(lm_result, 
                     ar_result,
                     ets_result,
                     snaive_result,
                     naive_result)
    full_df <- full_df%>%
      mutate(error = sum_offline_scooter.x - sum_offline_scooter.y,
             roll_forward = paste(roll_forward[i], "_", shift.time[j]))
    full_df_forecast <- full_df%>%
      filter(service_hour_date >= (as.Date(roll_forward[i])+14))
    
    results_rmse <- rbind(results_rmse, dist.rmse(full_df_forecast))
  }
}

results_rmse %>%
    ggplot(aes(model, result.rmse,fill = model))+
    geom_boxplot()+
    facet_wrap(~admin_town_en)

full_df_forecast %>%
  ggplot(aes(model, error,fill = model))+
  geom_boxplot()+
  facet_wrap(~admin_town_en)
