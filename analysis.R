library(ggplot2)
energy_df <-
  read.csv(file = "energydata_complete.csv", stringsAsFactors = FALSE)
energy_df$date <- as.Date(energy_df$date)
selected_energy_df <- energy_df[43:nrow(energy_df), ]


ws = 144
smoothed_selected_appliances <-
  filter(selected_energy_df$Appliances, rep(1 / ws, ws))

selected_energy_df$smoothed_selected_appliances = c(smoothed_selected_appliances)
ggplot(data = selected_energy_df, aes(x = date, y = smoothed_selected_appliances)) +
  geom_line(na.rm = TRUE, size = 0) + geom_smooth() +
  geom_hline(yintercept = mean(smoothed_selected_appliances, na.rm = TRUE))

plot(smoothed_selected_appliances, type = "l")
# abline(h=mean(smoothed_selected_appliances, na.rm = TRUE))
#
#
# lines(filter(energy_df[43:nrow(energy_df), ]$T1, rep(1 / ws, ws)), col="red")


ws = 144
smoothed_kitchen_temp <-
  filter(selected_energy_df$T1, rep(1 / ws, ws))
plot(smoothed_kitchen_temp, type = "l")

ws = 144
smoothed_kitchen_hum <-
  filter(selected_energy_df$RH_1, rep(1 / ws, ws))
plot(smoothed_kitchen_hum, type = "l")


ws = 144
smoothed_lroom_temp <-
  filter(selected_energy_df$T2, rep(1 / ws, ws))
plot(smoothed_lroom_temp, type = "l")
X <- cbind(smoothed_kitchen_temp[72:(length(smoothed_selected_appliances) -
                                       72)],          smoothed_kitchen_hum[72:(length(smoothed_selected_appliances) - 72)])

zooed_X<-zoo(X)
lagged_x<-lag(zooed_X,k = 10, na.pad = TRUE)
armam <-
  arima(
    smoothed_selected_appliances [72:(length(smoothed_selected_appliances) -
                                        82)] ,
    order = c(10, 0, 10),
    xreg =lagged_x[1:(nrow(lagged_x)-10),]
  )
    