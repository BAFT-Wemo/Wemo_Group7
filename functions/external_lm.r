source("functions/data_func.r")
source("functions/model_func.r")

rain_df <- external_data("data/rain.csv")
holiday_df <- external_data("data/holiday.csv")

ext_df <- left_join(shift.df, rain_df, 
                             by = c("service_hour_date" = "Date", "admin_town_en" = "admin_town_en"))

joined_tibble <- left_join(joined_tibble, holiday_df, 
                           by = c("service_hour_date" = "Date"))

ext_nest <- nest_a(joined_tibble)
