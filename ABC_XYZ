library(tidyverse)
library(forecast)
library(timetk)
library(Metrics)
library(lubridate)
library(sweep)
library(caret) # used for avNNet

# Read in data
wemo.df <- read.csv("Data_Jan_to_Aug.csv")
wemo.df$service_hour=as.POSIXct(paste(wemo.df$service_hour_date, wemo.df$shift), format="%Y-%m-%d %H:%M:%S")

# Filter area & time (days without 3 shifts)
wemo.df.new <- wemo.df%>%
  filter(admin_town_en != "Sanchong Dist" & admin_town_en != "out of boundary"& admin_town_en != "Taishan Dist"
         & admin_town_en != "Wugu Dist" & admin_town_en != "Tucheng Dist" & admin_town_en != "Shulin Dist" & 
           admin_town_en != "Xizhi Dist" & service_hour_date != "2020-01-31" & service_hour_date != "2020-08-31")

# Derived variable (Weekend or weekday)
wemo.df.new$weekday<-weekdays(wemo.df.new$service_hour)
wemo.df.new$weekend_or_weekday<-ifelse(wemo.df.new$weekday=="Saturday" | wemo.df.new$weekday == "Sunday", 1, 0)

# Change type to character
wemo.df.new$service_hour_date <- as.character(wemo.df.new$service_hour_date)

# Filter columns
wemo.df.new <- wemo.df.new %>% 
  select(admin_town_en, sum_offline_scooter, service_hour_date, shift, weekend_or_weekday)

# Shifts + Whole week
shift_1 <- wemo.df.new%>%
  filter(shift == '00:00:00')
shift_2 <- wemo.df.new%>%
  filter(shift == '08:00:00')
shift_3 <- wemo.df.new%>%
  filter(shift == '16:00:00')

# Shift 1
shift_1_weekend <- wemo.df.new%>%
  filter(shift == '00:00:00'&weekend_or_weekday == 1)
shift_1_weekday <- wemo.df.new%>%
  filter(shift == '00:00:00'&weekend_or_weekday == 0)

# Shift 2
shift_2_weekend <- wemo.df.new%>%
 filter(shift == '08:00:00'&weekend_or_weekday == 1)
shift_2_weekday <- wemo.df.new%>%
  filter(shift == '08:00:00'&weekend_or_weekday == 0)

# Shift 3
shift_3_weekend <- wemo.df.new%>%
  filter(shift == '16:00:00'&weekend_or_weekday == 1)
shift_3_weekday <- wemo.df.new%>%
  filter(shift == '16:00:00'&weekend_or_weekday == 0)


############## ABC XYZ ANALYSIS 
#functions to use: abc(), xyz() and abcxyz()
library(tsutils)

shift1_clean = subset(shift_1, select = -c(shift,weekend_or_weekday))
shift2_clean = subset(shift_2, select = -c(shift,weekend_or_weekday))
shift3_clean = subset(shift_3, select = -c(shift,weekend_or_weekday))
#shift1_weekend_clean = subset(shift_1_weekend, select = -c(shift,weekend_or_weekday))
#shift2_weekend_clean = subset(shift_2_weekend, select = -c(shift,weekend_or_weekday))
#shift3_weekend_clean = subset(shift_3_weekend, select = -c(shift,weekend_or_weekday))
#shift1_weekday_clean = subset(shift_1_weekday, select = -c(shift,weekend_or_weekday))
#shift2_weekday_clean = subset(shift_2_weekday, select = -c(shift,weekend_or_weekday))
#shift3_weekday_clean = subset(shift_3_weekday, select = -c(shift,weekend_or_weekday))

#need wide format: date rows, material columns
wide_df_shift1 <- shift1_clean %>%
  tidyr::spread(admin_town_en, sum_offline_scooter)

wide_df_shift2 <- shift2_clean %>%
  tidyr::spread(admin_town_en, sum_offline_scooter)

wide_df_shift3 <- shift3_clean %>%
  tidyr::spread(admin_town_en, sum_offline_scooter)

######### shift1 wholeweek
#Get % total sales ABC analysis
abc(wide_df_shift1 %>% select(-service_hour_date))
abc_plot <-  abc(wide_df_shift1 %>% select(-service_hour_date))
plot(abc_plot)
class <- t(data.frame(abc_plot$class))
abc_plot$value
results <- cbind(data.frame(mean_value = abc_plot$value), importance = abc_plot$class[1,])
results <- results%>%
  rownames_to_column(var='item_id')

#plot mean values by item
results%>%
  ggplot(aes(reorder(item_id, mean_value), mean_value, fill=importance))+
  geom_col()+
  scale_fill_brewer(type='qual', direction = -1)+
  coord_flip()+
  labs(x='Items')

#XYZ: estimate forecastibility by coefficient of variation:
# sd(series)/mean(series
xyz(wide_df_shift1 %>% select(-service_hour_date), type='cv')
xyz_plot <- xyz(wide_df_shift1 %>% select(-service_hour_date), type='cv')
plot(xyz_plot)
xyz_plot$value
xyz_plot$class
xyzresults <- cbind(data.frame(item_id = results$item_id, 
                               cv_value = xyz_plot$value[1,]), 
                    forecastability = xyz_plot$class[1,])

#Plot results
xyzresults%>%
  ggplot(aes(reorder(item_id, cv_value), 
             cv_value, fill=forecastability))+
  geom_col()+
  scale_fill_brewer(type='qual', direction = 1)+
  guides(fill = guide_legend(reverse=T))+
  coord_flip()+
  labs(x='Items')

#Use this to guide your forecasting strategy
x <- abcxyz(abc(wide_df_shift1 %>% select(-service_hour_date)),
            xyz(wide_df_shift1 %>% select(-service_hour_date), type='cv'))


######### shift2 wholeweek
#Get % total sales ABC analysis
abc(wide_df_shift2 %>% select(-service_hour_date))
abc_plot <-  abc(wide_df_shift2 %>% select(-service_hour_date))
plot(abc_plot)
class <- t(data.frame(abc_plot$class))
abc_plot$value
results <- cbind(data.frame(mean_value = abc_plot$value), importance = abc_plot$class[1,])
results <- results%>%
  rownames_to_column(var='item_id')

#plot mean values by item
results%>%
  ggplot(aes(reorder(item_id, mean_value), mean_value, fill=importance))+
  geom_col()+
  scale_fill_brewer(type='qual', direction = -1)+
  coord_flip()+
  labs(x='Items')

#XYZ: estimate forecastibility by coefficient of variation:
# sd(series)/mean(series
xyz(wide_df_shift2 %>% select(-service_hour_date), type='cv')
xyz_plot <- xyz(wide_df_shift2 %>% select(-service_hour_date), type='cv')
plot(xyz_plot)
xyz_plot$value
xyz_plot$class
xyzresults <- cbind(data.frame(item_id = results$item_id,  cv_value = xyz_plot$value[1,]), forecastability = xyz_plot$class[1,])
#Plot results
xyzresults%>%
  ggplot(aes(reorder(item_id, cv_value), 
             cv_value, fill=forecastability))+
  geom_col()+
  scale_fill_brewer(type='qual', direction = 1)+
  guides(fill = guide_legend(reverse=T))+
  coord_flip()+
  labs(x='Items')

#Use this to guide your forecasting strategy
x <- abcxyz(abc(wide_df_shift2 %>% select(-service_hour_date)),
            xyz(wide_df_shift2 %>% select(-service_hour_date), type='cv'))


######### shift3 wholeweek
#Get % total sales ABC analysis
abc(wide_df_shift3 %>% select(-service_hour_date))
abc_plot <-  abc(wide_df_shift3 %>% select(-service_hour_date))
plot(abc_plot)
class <- t(data.frame(abc_plot$class))
abc_plot$value
results <- cbind(data.frame(mean_value = abc_plot$value), importance = abc_plot$class[1,])
results <- results%>%
  rownames_to_column(var='item_id')

#plot mean values by item
results%>%
  ggplot(aes(reorder(item_id, mean_value), mean_value, fill=importance))+
  geom_col()+
  scale_fill_brewer(type='qual', direction = -1)+
  coord_flip()+
  labs(x='Items')

#XYZ: estimate forecastibility by coefficient of variation:
# sd(series)/mean(series
xyz(wide_df_shift3 %>% select(-service_hour_date), type='cv')
xyz_plot <- xyz(wide_df_shift3 %>% select(-service_hour_date), type='cv')
plot(xyz_plot)
xyz_plot$value
xyz_plot$class
xyzresults <- cbind(data.frame(item_id = results$item_id,  cv_value = xyz_plot$value[1,]), forecastability = xyz_plot$class[1,])
#Plot results
xyzresults%>%
  ggplot(aes(reorder(item_id, cv_value), 
             cv_value, fill=forecastability))+
  geom_col()+
  scale_fill_brewer(type='qual', direction = 1)+
  guides(fill = guide_legend(reverse=T))+
  coord_flip()+
  labs(x='Items')

#Use this to guide your forecasting strategy
x <- abcxyz(abc(wide_df_shift3 %>% select(-service_hour_date)),
            xyz(wide_df_shift3 %>% select(-service_hour_date), type='cv'))
