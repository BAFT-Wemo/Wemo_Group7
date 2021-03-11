source("functions/data_func.r")
source("functions/model_func.r")
source("functions/ts_plot.r")

results_rmse <- data.frame()

for (j in 3:3){
  for (i in 1:8){
    shift <- separate_shift(wemo.df.new, shift.time[j])
    train <- train_data(shift, roll_forward[i])
    test <- test_data(shift, roll_forward[i])
    naive_result <- Naive.model(train, test, 28, roll_forward[i])
    n_ts <- nest_ts(nest_a(train))
    ar_result <- ARIMA.model(n_ts, test, 28, roll_forward[i])
    lm_result <- lm.model(n_ts, test, 28, roll_forward[i])
    ets_result <- ets.model(n_ts, test, 28, roll_forward[i])
    snaive_result <- snaive.model(test, i)
    mv_result <- mv.model(n_ts, test, 28, roll_forward[i])
    nn_result <- nn.model(n_ts, test, 28, roll_forward[i])
    
    full_df <- rbind(lm_result, 
                     ar_result,
                     ets_result,
                     mv_result,
                     nn_result,
                     snaive_result,
                     naive_result)
    full_df <- full_df%>%
      mutate(error = sum_offline_scooter.y - sum_offline_scooter.x,
             roll_forward = paste(roll_forward[i], "_", shift.time[j]))
    full_df_forecast <- full_df%>%
      filter(service_hour_date >= (as.Date(roll_forward[i])+14))

    results_rmse <- rbind(results_rmse, dist.rmse(full_df_forecast))
  }
  #print(rmse_boxplot(results_rmse, shift.time[j]))
  print(error_boxplot(full_df_forecast, shift.time[j]))
}







