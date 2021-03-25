source("functions/data_func.r")

town_length <- length(unique(wemo.df.new$town))

# for (i in 1:town_length){
for (i in 1:town_length){
  for (j in 1:1){
    shift <- separate_shift(wemo.df.new, shift.time[j])
    dist_data <- shift %>% filter(town == unique(town)[i])
    
    # ts objects
    ts_data <- ts(dist_data$sum_offline_scooter, start = c(2020,31), freq = 7) 
    
    # train and validation data
    valid.ts <- subset(ts_data, start = length(ts_data) - 28 +1)
    nValid <- length(valid.ts)
    train.ts <- subset(ts_data, end = length(ts_data) - nValid )
    
    #naive
    naive<- naive(train.ts, h=nValid)
    
    # ets
    hwin <- ets(train.ts, model = "MAM")
    hwin.pred <- forecast(hwin, h = 7, level = 0)
    
    # Linear Regression models with trend and seasonality
    lm.trend.season<- tslm(train.ts~trend+season)
    lm.trend.season.pred <- forecast(lm.trend.season, h=7, level = 0)
    
    ## Linear Regression prediction residuals
    lm.train.residual <- train.ts - lm.trend.season.pred$fitted
    lm.valid.residual <- valid.ts - lm.trend.season.pred$mean
    
    result <- data.frame(ets = hwin$fitted,
               lm = lm.trend.season$fitted,
               dist = dist_data$town)
    
  }
}
  

