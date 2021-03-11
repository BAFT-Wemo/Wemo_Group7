source("functions/data_func.r")
source("functions/model_func.r")
source("functions/ts_plot.r")
source("functions/train_fit.r")

results_rmse <- data.frame()
results_train_rmse <- data.frame()

for (j in 1:3){
  for (i in 1:8){
    shift <- separate_shift(wemo.df.new, shift.time[j])
    train <- train_data(shift, roll_forward[i])
    test <- test_data(shift, roll_forward[i])
    
    naive_model_result <- Naive.model(train, test, 28, roll_forward[i])
    naive_result <- data.frame(naive_model_result[1])
    naive_train_result1 <- data.frame(naive_model_result[2])
    naive_train_result <- naive_train(train)
    
    n_ts <- nest_ts(nest_a(train))
    ar_model_result <- ARIMA.model(n_ts, train, test, 28, roll_forward[i])
    ar_result <- data.frame(ar_model_result[1])
    ar_train_result <- data.frame(ar_model_result[2])
    
    lm_model_result <- lm.model(n_ts, train, test, 28, roll_forward[i])
    lm_result <- data.frame(lm_model_result[1])
    lm_train_result <- data.frame(lm_model_result[2])
    
    ets_model_result <- ets.model(n_ts, train, test, 28, roll_forward[i])
    ets_result <- data.frame(ets_model_result[1])
    ets_train_result <- data.frame(ets_model_result[2])
    
    snaive_model_result <- snaive.model(train, test, i)
    snaive_result <- data.frame(snaive_model_result[1])
    snaive_train_result1 <- data.frame(snaive_model_result[2])
    snaive_train_result <- snaive_train(train)
    
    mv_model_result <- mv.model(n_ts, train, test, 28, roll_forward[i])
    mv_result <- data.frame(mv_model_result[1])
    mv_train_result <- data.frame(mv_model_result[2])
    
    nn_model_result <- nn.model(n_ts, train, test, 28, roll_forward[i])
    nn_result <- data.frame(nn_model_result[1])
    nn_train_result <- data.frame(nn_model_result[2])
    
    full_df <- rbind(lm_result, 
                     ar_result,
                     ets_result,
                     mv_result,
                     nn_result,
                     snaive_result,
                     naive_result)
    
    full_df_train <- rbind(lm_train_result, 
                           ar_train_result,
                           ets_train_result,
                           mv_train_result,
                           nn_train_result,
                           snaive_train_result,
                           naive_train_result)
    full_df <- full_df%>%
      mutate(error = sum_offline_scooter.y - sum_offline_scooter.x,
             roll_forward = paste(roll_forward[i], "_", shift.time[j]))
    full_df_forecast <- full_df%>%
      filter(service_hour_date >= (as.Date(roll_forward[i])+14))
    
    full_df_train <- full_df_train%>%
      mutate(error = forecast - sum_offline_scooter,
             roll_forward = paste(roll_forward[i], "_", shift.time[j]))
    
    full_df_train <- full_df_train%>%
      filter(!is.na(forecast))

    results_rmse <- rbind(results_rmse, dist.rmse(full_df_forecast))
    results_train_rmse <- rbind(results_train_rmse, dist.rmse.train(full_df_train))
  }
  #print(rmse_boxplot(results_rmse, shift.time[j]))
  #print(error_boxplot(full_df_forecast, shift.time[j]))
}


mean_rmse <- results_rmse%>%
  group_by(shift, admin_town_en, model)%>%
  mutate(mean_rmse = mean(result.rmse))

mean_train_rmse <- results_train_rmse%>%
  group_by(shift, admin_town_en, model)%>%
  mutate(mean_rmse = mean(result.rmse))

three_mean_rmse <- mean_rmse%>%
  filter(admin_town_en == "Da’an Dist" | admin_town_en == "Neihu Dist" | admin_town_en == "Xinzhuang Dist")



