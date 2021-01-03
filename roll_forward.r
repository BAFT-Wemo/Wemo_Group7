source("functions/data_func.r")
source("functions/model_func.r")

results_rmse <- data.frame()

for (j in 1:3){
  for (i in 1:8){
    shift <- separate_shift(wemo.df.new, shift.time[j])
    extra <- exter.df(shift, rain_df, holiday_df)
    train <- train_data(extra, roll_forward[i])
    test <- test_data(extra, roll_forward[i])
    naive_result <- Naive.model(train, test, 28, roll_forward[i])
    n_ts <- nest_ts(nest_a(train))
    ar_result <- ARIMA.model(n_ts, test, 28, roll_forward[i])
    lm_result <- lm.model(n_ts, test, 28, roll_forward[i])
    ets_result <- ets.model(n_ts, test, 28, roll_forward[i])
    snaive_result <- snaive.model(n_ts, test, 28, roll_forward[i])
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
      mutate(error = sum_offline_scooter.x - sum_offline_scooter.y,
             roll_forward = paste(roll_forward[i], "_", shift.time[j]))
    full_df_forecast <- full_df%>%
      filter(service_hour_date >= (as.Date(roll_forward[i])+14))

    results_rmse <- rbind(results_rmse, dist.rmse(full_df_forecast))
  }
  print(rmse_boxplot(results_rmse, shift.time[i]))
  print(error_boxplot(full_df_forecast, shift.time[i]))
}

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

results_rmse %>%
  filter(shift == shift.time[1]) %>%
  ggplot(aes(model, result.rmse,fill = model))+
  geom_boxplot()+
  facet_wrap(~admin_town_en)+
  labs(title = shift.time[1])

results_rmse %>%
  filter(shift == shift.time[2]) %>%
  ggplot(aes(model, result.rmse,fill = model))+
  geom_boxplot()+
  facet_wrap(~admin_town_en)+
  labs(title = shift.time[2])

results_rmse %>%
  filter(shift == shift.time[3]) %>%
  ggplot(aes(model, result.rmse,fill = model))+
  geom_boxplot()+
  facet_wrap(~admin_town_en)+
  labs(title = shift.time[3])

full_df_forecast %>%
  filter(shift == shift.time[1]) %>%
  ggplot(aes(model, error,fill = model))+
  geom_boxplot()+
  facet_wrap(~admin_town_en)+
  #labs(title = paste("error", shift.time[1]))

full_df_forecast %>%
  filter(shift == shift.time[2]) %>%
  ggplot(aes(model, error,fill = model))+
  geom_boxplot()+
  facet_wrap(~admin_town_en)+
  labs(title = paste("error", shift.time[2]))

full_df_forecast %>%
  filter(shift == shift.time[3]) %>%
  ggplot(aes(model, error,fill = model))+
  geom_boxplot()+
  facet_wrap(~admin_town_en)+
  labs(title = paste("error", shift.time[3]))

model_name <- c("naive", "snaive", "ets", "lm", "arima", "mv", "nn")

ar_model_train

