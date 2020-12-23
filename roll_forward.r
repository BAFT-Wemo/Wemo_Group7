source("functions/data_func.r")
source("functions/model_func.r")

for (i in 1:8){
  for (j in 1:3){
    shift <- separate_shift(wemo.df.new, shift.time[j])
    train <- train_data(shift, roll_forward[i])
    test <- test_data(shift, roll_forward[i])
    naive_result <- Naive.model(train, test, 30, roll_forward[i])
    nest_b <- nest_a(train)
    n_ts <- nest_ts(nest_a(train))
    ar_result <- ARIMA.model(n_ts, test, 30, roll_forward[i])
    lm_result <- lm.model(n_ts, test, 30, roll_forward[i])
    ets_result <- ets.model(n_ts, test, 30, roll_forward[i])
    snaive_result <- snaive.model(n_ts, test, 30, roll_forward[i])
    
    full_df <- rbind(lm_result, 
                     ar_result,
                     ets_result,
                     snaive_result,
                     naive_result)
    full_df <- full_df%>%
      mutate(error = sum_offline_scooter.x - sum_offline_scooter.y)
  }
}
