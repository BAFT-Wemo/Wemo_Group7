source("functions/data_func.r")
source("functions/model_func.r")

plot.ts <- function(data, time){
  plot <- data %>%
    group_by(admin_town_en)%>%
    ggplot(aes(as.Date(service_hour_date), sum_offline_scooter, color=admin_town_en))+
    geom_line(size=.4)+
    guides(color=F)+
    facet_wrap(~admin_town_en, nrow=5, scales='free_y')+
    labs(x='', title=paste("Time series for offline scooters in", time, "on training data in weekdays"))
  return(plot)
}

err_boxplot <- function(var_data, shift_time, var_name, name){
  plot <- var_data %>%
    filter(shift == shift_time) %>%
    ggplot(aes(model, var,fill = model))+
    geom_boxplot()+
    facet_wrap(~admin_town_en)+
    labs(title = paste(name,shift_time))
  return(plot)
}

plot.forecast <- function(full_data){
  plot <- full_data%>%
    ggplot(aes(service_hour_date,sum_offline_scooter.x, color=model, group=model))+
    geom_line()+
    facet_wrap(~admin_town_en, ncol =2, scale='free_y')+
    labs(x='', title='Residuals for offline scooters in [shift1] on testing data in [whole week]')
  #plot <- plot + geom_line(aes(x=service_hour_date, y=sum_offline_scooter), data = df)
  return(plot)
}


plot.residual <- function(full_data){
  plot <- full_data%>%
    ggplot(aes(service_hour_date, error, color=model, group=model))+
    geom_line()+
    facet_wrap(~admin_town_en, ncol =2, scale='free_y')+
    labs(x='', title='Residuals for offline scooters in [shift1] on testing data in [whole week]')
  return(plot)
}
