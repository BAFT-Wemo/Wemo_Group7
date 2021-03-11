source("functions/data_func.r")
source("functions/model_func.r")


shift <- separate_shift(wemo.df.new, shift.time[1])
rain_df$admin_town_en <- ifelse(rain_df$admin_town_en == "Daan Dist", "Da’an Dist", rain_df$admin_town_en)
extra <- exter.df(shift, rain_df, holiday_df)
train <- train_data(extra, roll_forward[1])
test <- test_data(extra, roll_forward[1])
nest.df <- nest_a(train)

n_ts <- nest_ts(nest.df)

x <- as.data.frame(cbind(rain_df[,3], holiday_df[,2]))
formula <- as.formula(paste("n_ts", paste(c("trend", colnames()))))



