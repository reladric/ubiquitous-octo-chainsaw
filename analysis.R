library(zoo)
library(ggplot2)
library(pracma)
energy_df <-
    read.csv(file = "energydata_complete.csv", stringsAsFactors = FALSE)
energy_df$date <- as.Date(energy_df$date)
selected_energy_df <- energy_df[43:nrow(energy_df),]
selected_energy_df$timescale <- seq(1,nrow(selected_energy_df))

selected_energy_df$T_out <- detrend(selected_energy_df$T_out)
selected_energy_df$T1 <- detrend(selected_energy_df$T1)
selected_energy_df$T2 <- detrend(selected_energy_df$T2)
selected_energy_df$T3 <- detrend(selected_energy_df$T3)
selected_energy_df$T4 <- detrend(selected_energy_df$T4)
selected_energy_df$T5 <- detrend(selected_energy_df$T5)
selected_energy_df$T6 <- detrend(selected_energy_df$T6)
selected_energy_df$T7 <- detrend(selected_energy_df$T7)
selected_energy_df$T8 <- detrend(selected_energy_df$T8)
selected_energy_df$T9 <- detrend(selected_energy_df$T9)

selected_energy_df$Press_mm_hg <-
    detrend(selected_energy_df$Press_mm_hg)
selected_energy_df$Tdewpoint <-
    detrend(selected_energy_df$Tdewpoint)
selected_energy_df$Windspeed <-
    detrend(selected_energy_df$Windspeed)


library(MASS)
ind <- sapply(selected_energy_df, is.numeric)
selected_energy_df[ind] <- lapply(selected_energy_df[ind], scale, center=TRUE, scale=TRUE)

ws = 144




smoothed_appliances <-
    filter(x = selected_energy_df$Appliances, filter = rep(1 / ws,ws))
smoothed_appliances<-smoothed_appliances[!is.na(smoothed_appliances)]
smoothed_T_out <-
    filter(x = selected_energy_df$T_out, filter = rep(1 / ws,ws))
smoothed_RH_out <-
    filter(x = selected_energy_df$RH_out, filter = rep(1 / ws,ws))
smoothed_Windspeed <-
        filter(x = selected_energy_df$Windspeed, filter = rep(1 / ws,ws))
smoothed_Tdewpoint <-
    filter(x = selected_energy_df$Tdewpoint, filter = rep(1 / ws,ws))
smoothed_Press_mm_hg <-
    filter(x = selected_energy_df$Press_mm_hg, filter = rep(1 / ws,ws))

smoothed_T1 <-
    filter(x = selected_energy_df$T1, filter = rep(1 / ws,ws))
smoothed_T2 <-
    filter(x = selected_energy_df$T2, filter = rep(1 / ws,ws))
smoothed_T3 <-
    filter(x = selected_energy_df$T3, filter = rep(1 / ws,ws))
smoothed_T4 <-
    filter(x = selected_energy_df$T4, filter = rep(1 / ws,ws))
smoothed_T5 <-
    filter(x = selected_energy_df$T2, filter = rep(1 / ws,ws))
smoothed_T6 <-
    filter(x = selected_energy_df$T6, filter = rep(1 / ws,ws))
smoothed_T7 <-
    filter(x = selected_energy_df$T3, filter = rep(1 / ws,ws))
smoothed_T8 <-
    filter(x = selected_energy_df$T4, filter = rep(1 / ws,ws))
smoothed_T9 <-
    filter(x = selected_energy_df$T4, filter = rep(1 / ws,ws))

smoothed_RH_1 <-
    filter(x = selected_energy_df$RH_1, filter = rep(1 / ws,ws))
smoothed_RH_2 <-
    filter(x = selected_energy_df$RH_2, filter = rep(1 / ws,ws))
smoothed_RH_3 <-
    filter(x = selected_energy_df$RH_3, filter = rep(1 / ws,ws))
smoothed_RH_4 <-
    filter(x = selected_energy_df$RH_4, filter = rep(1 / ws,ws))
smoothed_RH_5 <-
    filter(x = selected_energy_df$RH_5, filter = rep(1 / ws,ws))
smoothed_RH_6 <-
    filter(x = selected_energy_df$RH_6, filter = rep(1 / ws,ws))
smoothed_RH_7 <-
    filter(x = selected_energy_df$RH_7, filter = rep(1 / ws,ws))
smoothed_RH_8 <-
    filter(x = selected_energy_df$RH_8, filter = rep(1 / ws,ws))
smoothed_RH_9 <-
    filter(x = selected_energy_df$RH_9, filter = rep(1 / ws,ws))


smoothed_T_out <-
    filter(x = selected_energy_df$T_out, filter = rep(1 / ws,ws))
smoothed_RH_out <-
    filter(x = selected_energy_df$RH_out, filter = rep(1 / ws,ws))
smoothed_Windspeed <-
    filter(x = selected_energy_df$Windspeed, filter = rep(1 / ws,ws))
smoothed_Tdewpoint <-
    filter(x = selected_energy_df$Tdewpoint, filter = rep(1 / ws,ws))
smoothed_Press_mm_hg <-
    filter(x = selected_energy_df$Press_mm_hg, filter = rep(1 / ws,ws))


nester_inner_X = cbind(smoothed_T_out,smoothed_RH_out,smoothed_Press_mm_hg)
nester_inner_X<-nester_inner_X[complete.cases(nester_inner_X),]

colnames(nester_inner_X)<-c("smoothed_T_out","smoothed_RH_out","smoothed_Press_mm_hg")

nested_inner_model_lag<-2
smoothed_appliances <-
    filter(x = selected_energy_df$Appliances, filter = rep(1 / ws,ws))
smoothed_appliances<-smoothed_appliances[!is.na(smoothed_appliances)]
smoothed_appliances <-
    smoothed_appliances[(nested_inner_model_lag + 1):length(smoothed_appliances)]

nester_inner_zooed_X <- zoo(nester_inner_X)
nested_lagged_x <- lag(nester_inner_zooed_X, k = nested_inner_model_lag, na.pad = TRUE)
nested_lagged_x<-nested_lagged_x[complete.cases(nested_lagged_x),]

nested_inner_model <-
    arima(
        smoothed_appliances,
        order = c(nested_inner_model_lag, 0, 1),
        seasonal = list(order = c(1, 0, 1)),
        xreg = nested_lagged_x
    )

nested_inner_sig_v <- (1 - pnorm(abs(nested_inner_model$coef) / sqrt(diag(nested_inner_model$var.coef)))) * 2





nester_outer_X = cbind(
    smoothed_T_out,smoothed_RH_out,smoothed_Windspeed,smoothed_Press_mm_hg, smoothed_T1, smoothed_T2, smoothed_T4, smoothed_RH_1,smoothed_RH_2,smoothed_RH_4
)
nester_outer_X<-nester_outer_X[complete.cases(nester_outer_X),]


colnames(nester_outer_X) <-
    c(
        "smoothed_T_out","smoothed_RH_out","smoothed_Windspeed","smoothed_Press_mm_hg","Kitchen Temperature","Living room temperature", "Office Temperature","Kitchen Humidity","Living room Humidity", "Office Humidity"
    )
nested_outer_model_lag<-3


nester_outer_zooed_X <- zoo(nester_outer_X)
nester_outer_lagged_x <- lag(nester_outer_zooed_X, k = nested_outer_model_lag, na.pad = TRUE)
nester_outer_lagged_x<-nester_outer_lagged_x[complete.cases(nester_outer_lagged_x),]
smoothed_appliances <-
    filter(x = selected_energy_df$Appliances, filter = rep(1 / ws,ws))
smoothed_appliances<-smoothed_appliances[!is.na(smoothed_appliances)]
smoothed_appliances <-
    smoothed_appliances[(nested_outer_model_lag + 1):length(smoothed_appliances)]
nested_outer_model <-
    arima(
        smoothed_appliances,
        order = c(nested_outer_model_lag, 0, 2),
        seasonal = list(order = c(1, 0,1)),
        xreg = nester_outer_lagged_x
    )

sig_v <- (1 - pnorm(abs(nested_outer_model$coef) / sqrt(diag(nested_outer_model$var.coef)))) * 2




clear_X = cbind(
    smoothed_T_out,smoothed_RH_out, smoothed_Press_mm_hg, smoothed_T6, smoothed_RH_6,smoothed_T3,smoothed_RH_3, smoothed_T5, smoothed_RH_5
)
clear_X<-clear_X[complete.cases(clear_X),]


colnames(clear_X) <-
    c(
        "smoothed_T_out","smoothed_RH_out","Pressure","Outside Temperature", "Outside  Humidity","Laundry room Temperature","Laundry Room Humidity","Bathroom Temperature","Bathroom Humidity"
    )
clear_lag<-2


clear_zooed_X <- zoo(clear_X)
clear_lagged_x <- lag(clear_zooed_X, k = clear_lag, na.pad = TRUE)
clear_lagged_x<-clear_lagged_x[complete.cases(clear_lagged_x),]
smoothed_appliances <-
    filter(x = selected_energy_df$Appliances, filter = rep(1 / ws,ws))
smoothed_appliances<-smoothed_appliances[!is.na(smoothed_appliances)]
smoothed_appliances <-
    smoothed_appliances[(clear_lag + 1):length(smoothed_appliances)]
clear_model <-
    arima(
        smoothed_appliances,
        order = c(clear_lag, 0, 1),
        seasonal = list(order = c(1, 0,1)),
        xreg = clear_lagged_x
    )

clear_sig_v <- (1 - pnorm(abs(clear_model$coef) / sqrt(diag(clear_model$var.coef)))) * 2

png(filename = "appliances.png")
plot(smoothed_appliances, type = "l", xlab = "Time", ylab = "Energy consumed by appliance")
dev.off()


png(filename = "acf.png")
plot(acff)
dev.off()



png(filename = "residuals.png")
plot(nested_outer_model$residuals, type="l")
dev.off()
