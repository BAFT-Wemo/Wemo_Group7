source("functions/data_func.r")
library(scales)

town_length <- length(unique(wemo.df.new$town))
#origin_colname = c("town", "Date", "shift", "sum_offline_scooter", 
#                   "naive", "snaive", "ets", "lm", "lm+extra", "arima", "nn")
#origin_df = data.frame(matrix(vector(), 0, length(origin_colname),
#                       dimnames=list(c(), origin_colname)),
#                stringsAsFactors=F)


# for (i in 1:town_length){
for (i in 1:town_length){
  for (j in 1:1){
    shift <- separate_shift(wemo.df.new, shift.time[j])
    dist_data <- shift %>% filter(town == unique(town)[i])
    origin_data <- dist_data
    # ts objects
    ts_data <- ts(dist_data$sum_offline_scooter, start = c(2020,31), freq = 7) 
    
    # train and validation data
    valid.ts <- subset(ts_data, start = length(ts_data) - 28 +1)
    nValid <- length(valid.ts)
    train.ts <- subset(ts_data, end = length(ts_data) - nValid )
    
    # Train -2019/09/01 ~ 2020/09/02
    # dist_data$train <- " "
    # dist_data$train[1:368] <- train.ts
    # Valid 2020/09/03 ~ 2020/09/30
    
    #### Naive ###
    dist_data$forecast_naive <- " "
    dist_data$forecast_naive[1] <- NA
    dist_data$forecast_naive[2:368] <- dist_data$sum_offline_scooter[1:367]
    dist_data$forecast_naive[369:395] <- dist_data$forecast_naive[368]
    
    ## NAIVE prediction residuals
    dist_data$error_naive <- dist_data$sum_offline_scooter - as.numeric(dist_data$forecast_naive)
    
    
    ### Seasonal NAIVE FORECAST ###
    dist_data$forecast_snaive <- " "
    dist_data$forecast_snaive[1:7] <- NA
    dist_data$forecast_snaive[8:368] <- dist_data$sum_offline_scooter[1:361]
    dist_data$forecast_snaive[369:length(ts_data)] <- dist_data$forecast_naive[362]
    
    ## Seasonal NAIVE prediction residuals
    dist_data$error_snaive <- dist_data$sum_offline_scooter - as.numeric(dist_data$forecast_snaive)
    
    ### ETS ###
    hwin <- ets(train.ts, model = "ZZZ")
    hwin.pred <- forecast(hwin, h = 28, level = 0)
    length(hwin.pred$fitted)
    dist_data$forecast_ets <- " "
    dist_data$forecast_ets[1:368] <- hwin.pred$fitted
    dist_data$forecast_ets[369:length(ts_data)] <- hwin.pred$mean
    length(hwin.pred$mean)
    
    ## ETS prediction residuals
    dist_data$error_ets <- dist_data$sum_offline_scooter - as.numeric(dist_data$forecast_ets)
    
    
    ### Linear Regression ###
    # Linear Regression models with trend and seasonality
    lm.trend.season<- tslm(train.ts~trend+season)
    lm.trend.season.pred <- forecast(lm.trend.season, h=28, level = 0)
    dist_data$forecast_lm <- " "
    dist_data$forecast_lm[1:368] <- lm.trend.season.pred$fitted
    dist_data$forecast_lm[369:length(ts_data)] <- lm.trend.season.pred$mean
    
    ## Linear Regression prediction residuals
    dist_data$error_lm <- dist_data$sum_offline_scooter - as.numeric(dist_data$forecast_lm)
    
    
    ### Moving Average ###
    mv <- rollmean(ts_data, k = 12, align = "right")
    length(mv)
    dist_data$forecast_mv <- " "
    dist_data$forecast_mv[1:11] <- NA
    dist_data$forecast_mv[12:length(ts_data)] <- mv
    
    ## Moving Average prediction residuals
    dist_data$error_mv <- dist_data$sum_offline_scooter - as.numeric(dist_data$forecast_mv)
    
    
    ### Neural Net ###
    nn <- nnetar(ts_data, repeats = 5, size=10)
    dist_data$forecast_nn <- " "
    dist_data$forecast_nn <- nn$fitted
    
    ## Neural Net prediction residuals
    dist_data$error_nn <- dist_data$sum_offline_scooter - as.numeric(dist_data$forecast_nn)
    
    
    #### ARIMA ###
    ar <- auto.arima(ts_data)
    dist_data$forecast_ar <- " "
    dist_data$forecast_ar <- ar$fitted
    
    ## ARIMA prediction residuals
    dist_data$error_ar <- dist_data$sum_offline_scooter - as.numeric(dist_data$forecast_ar)
    
    # Only Forecast+Error
    fore_error <- dist_data[8:21]
    final_train <- dist_data[c(1:368),c(1:21)]
    
    # No Gap
    gap_no <- dist_data
    gap_no_valid <- dist_data[c((length(ts_data)-32+1):length(ts_data)),c(1:21)]
    
    # One week Gap
    fore_error_one <- fore_error
    fore_error_one[c(369:375),c(1:14)] <- NA
    gap_one <- cbind(origin_data, fore_error_one)
    gap_one_valid <- dist_data[c(376:396),c(1:21)]
    
    # Two week Gap
    fore_error_two <- fore_error
    fore_error_two[c(369:382),c(1:14)] <- NA
    gap_two <- cbind(origin_data, fore_error_two)
    gap_two_valid <- dist_data[c(383:396),c(1:21)]
    
    df_train <- dist_data %>%
      select(service_hour_date, shift, town, sum_offline_scooter, forecast_naive, forecast_snaive, forecast_lm,
             forecast_ets, forecast_ar, forecast_mv, forecast_nn) %>%
      gather(key = "variable", value = "value", -c(service_hour_date, shift, town))

    plot_forecast <- df_train %>%
      ggplot(aes(x = as.Date(service_hour_date), y = as.numeric(value),group=variable,colour=variable)) +
      geom_line() +
      geom_line(data=subset(df_train, variable == "sum_offline_scooter"), colour="black") +
      geom_vline(xintercept = as.Date("2020-08-30"), linetype="dashed", color = "blue") +
      geom_vline(xintercept = as.Date("2020-09-06"), linetype="dashed", color = "red") +
      geom_vline(xintercept = as.Date("2020-09-13"), linetype="dashed", color = "darkgreen") +
      scale_x_date(breaks = function(x) seq.Date(from = min(x), to = max(x), by = "1 month")) +
      guides(colour=guide_legend(override.aes=list(colour=c(hue_pal()(8)[1:7], "black")))) +
      ylim(0, 4000)+
      labs(x='Date', y="Low Battery Scooters' Amounts", title= paste("Forecast / Actual for scooters at", unique(df_train$shift), "in",  unique(df_train$town)))

    print(plot_forecast)

    df_residual <- dist_data %>%
      select(service_hour_date, shift, town, error_naive, error_snaive, error_lm,
             error_ets, error_ar, error_mv, error_nn) %>%
      gather(key = "variable", value = "value", -c(service_hour_date, shift, town))

    plot_residual <- df_residual %>%
      ggplot(aes(x = as.Date(service_hour_date), y = as.numeric(value),group=variable,colour=variable)) +
      geom_line() +
      geom_vline(xintercept = as.Date("2020-08-30"), linetype="dashed", color = "blue") +
      geom_vline(xintercept = as.Date("2020-09-06"), linetype="dashed", color = "red") +
      geom_vline(xintercept = as.Date("2020-09-13"), linetype="dashed", color = "darkgreen") +
      scale_x_date(breaks = function(x) seq.Date(from = min(x), to = max(x), by = "1 month")) +
      ylim(-1500,2500)+
      labs(x='Date', y="Residuals", title= paste("Residuals for scooters at", unique(df_residual$shift), "in",  unique(df_residual$town)))

    print(plot_residual)

    df_residual_no_gap <- gap_no_valid %>%
      select(service_hour_date, shift, town, error_naive, error_snaive, error_lm,
             error_ets, error_ar, error_mv, error_nn) %>%
      gather(key = "variable", value = "value", -c(service_hour_date, shift, town))
    
    plot_residual_no_gap <- df_residual_no_gap %>%
      ggplot(aes(x = as.Date(service_hour_date), y = as.numeric(value),group=variable,colour=variable)) + 
      geom_line() +
      xlim(as.Date(c("2020-08-30", "2020-09-30"), format="%Y-%m-%d") ) +
      geom_vline(xintercept = as.Date("2020-08-30"), linetype="dashed", color = "blue") +
      geom_vline(xintercept = as.Date("2020-09-06"), linetype="dashed", color = "red") +
      geom_vline(xintercept = as.Date("2020-09-13"), linetype="dashed", color = "darkgreen") +
      ylim(-2000,1500)+
      labs(x='Date', y="Residuals", title= paste("Residuals for scooters at", unique(df_residual$shift), "in",  unique(df_residual$town), "with gap"))
    
    print(plot_residual_no_gap)
    
    # df_residual_one_gap <- gap_one_valid %>%
    #   select(service_hour_date, shift, town, error_naive, error_snaive, error_lm,
    #          error_ets, error_ar, error_mv, error_nn) %>%
    #   gather(key = "variable", value = "value", -c(service_hour_date, shift, town))
    # 
    # plot_residual_one_gap <- df_residual_one_gap %>%
    #   ggplot(aes(x = as.Date(service_hour_date), y = as.numeric(value),group=variable,colour=variable)) + 
    #   geom_line() +
    #   xlim(as.Date(c("2020-08-30", "2020-09-30"), format="%Y-%m-%d") ) + 
    #   ylim(-1500,1500)+
    #   labs(x='Date', y="Residuals", title= paste("Residuals for scooters at", unique(df_residual$shift), "in",  unique(df_residual$town), "with one gap"))
    # 
    # print(plot_residual_one_gap)
    # 
    # df_residual_two_gap <- gap_two_valid %>%
    #   select(service_hour_date, shift, town, error_naive, error_snaive, error_lm,
    #          error_ets, error_ar, error_mv, error_nn) %>%
    #   gather(key = "variable", value = "value", -c(service_hour_date, shift, town))
    # 
    # plot_residual_two_gap <- df_residual_two_gap %>%
    #   ggplot(aes(x = as.Date(service_hour_date), y = as.numeric(value),group=variable,colour=variable)) + 
    #   geom_line() +
    #   xlim(as.Date(c("2020-08-30", "2020-09-30"), format="%Y-%m-%d") ) + 
    #   ylim(-1500,1500)+
    #   labs(x='Date', y="Residuals", title= paste("Residuals for scooters at", unique(df_residual$shift), "in",  unique(df_residual$town), "with two gap"))
    # 
    # print(plot_residual_two_gap)
  }
}
  

