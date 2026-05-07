#packages----
library(readxl)
library(lmtest)
library(forecast)
library(Metrics)
library(mgcv)
library(DiceKriging)
options(scipen=999)


#reading in the data----
Austria <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Austria.xlsx")[,c('Year', 'material_footprint')])
Belgium <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Belgium.xlsx")[,c('Year', 'material_footprint')])
Denmark<- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Denmark.xlsx")[,c('Year', 'material_footprint')])
Finland <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Finland.xlsx")[,c('Year', 'material_footprint')])
France <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/France.xlsx")[,c('Year', 'material_footprint')])
Germany <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Germany.xlsx")[,c('Year', 'material_footprint')])
Ireland <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Ireland.xlsx")[,c('Year', 'material_footprint')])
Italy <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Italy.xlsx")[,c('Year', 'material_footprint')])
Luxembourg <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Luxembourg.xlsx")[,c('Year', 'material_footprint')])
Netherlands <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Netherlands.xlsx")[,c('Year', 'material_footprint')])
Portugal <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Portugal.xlsx")[,c('Year', 'material_footprint')])
Spain <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Spain.xlsx")[,c('Year', 'material_footprint')])
Sweden <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Sweden.xlsx")[,c('Year', 'material_footprint')])

# Functions for PI diagnostics----
coverage_prob <- function(y_test, pi_lo, pi_hi) {
  mean(y_test >= pi_lo & y_test <= pi_hi, na.rm = TRUE)
}

relative_pi_width <- function(y_test, pi_lo, pi_hi) {
  rel_width <- (pi_hi - pi_lo) / abs(y_test)
  rel_width[abs(y_test) == 0] <- NA
  mean(rel_width, na.rm = TRUE)
}

relative_pi_width_last <- function(y_test, pi_lo, pi_hi) {
  h <- length(y_test)
  if(h == 0) return(NA_real_)
  if(is.na(y_test[h]) || is.na(pi_lo[h]) || is.na(pi_hi[h])) return(NA_real_)
  if(abs(y_test[h]) == 0) return(NA_real_)
  
  (pi_hi[h] - pi_lo[h]) / abs(y_test[h])
}

relative_pi_width_h <- function(y_test, pi_lo, pi_hi, h) {
  if(h > length(y_test)) return(NA_real_)
  if(is.na(y_test[h]) || is.na(pi_lo[h]) || is.na(pi_hi[h])) return(NA_real_)
  if(abs(y_test[h]) == 0) return(NA_real_)
  
  (pi_hi[h] - pi_lo[h]) / abs(y_test[h])
}

pi_diagnostics <- function(country, y_test, y_hat, pi_lo, pi_hi){
  data.frame(
    country = country,
    n_test = length(y_test), 
    coverage = coverage_prob(y_test, pi_lo, pi_hi),
    relative_width_h1 = relative_pi_width_h(y_test, pi_lo, pi_hi, 1),
    relative_width_h5 = relative_pi_width_h(y_test, pi_lo, pi_hi, 5),
    relative_width_last = relative_pi_width_last(y_test, pi_lo, pi_hi)
  )
}

results_df <- data.frame(
  country = character(),
  n_test = numeric(), 
  coverage = numeric(),
  relative_width_h1 = numeric(),
  relative_width_h5 = numeric(), 
  relative_width_last = numeric(),
  stringsAsFactors = FALSE
)




#Austria----
austria_train <- Austria[1:42,]
austria_test <- Austria[43:nrow(Austria),]

austria_arima <- auto.arima(austria_train$material_footprint, seasonal=FALSE, max.p=7, 
                            max.q=7)
forecast_austria_arima <- forecast(austria_arima, h=nrow(austria_test), level=95)

plot(y=austria_test$material_footprint, x=austria_test$Year, type='b', 
     xlab='Year', ylab='Material Footprint', main='Austria', 
     ylim=c(min(forecast_austria_arima$lower), max(forecast_austria_arima$upper)))
lines(y=forecast_austria_arima$mean, x=austria_test$Year, type='b', col='red')
polygon(c(austria_test$Year, rev(austria_test$Year)), 
        c(forecast_austria_arima$upper, rev(forecast_austria_arima$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Austria",
    y_test = austria_test$material_footprint,
    y_hat = as.numeric(forecast_austria_arima$mean),
    pi_lo = as.numeric(forecast_austria_arima$lower[,1]),
    pi_hi = as.numeric(forecast_austria_arima$upper[,1])
  )
)

#Belgium----
belgium_train <- Belgium[1:42,]
belgium_test <- Belgium[43:nrow(Belgium),]

belgium_arima <- auto.arima(belgium_train$material_footprint, seasonal=FALSE, max.p=7, 
                            max.q=7)
forecast_belgium_arima <- forecast(belgium_arima, h=nrow(belgium_test), level=95)

plot(y=belgium_test$material_footprint, x=belgium_test$Year, type='b', 
     xlab='Year', ylab='Material Footprint', main='belgium', 
     ylim=c(min(forecast_belgium_arima$lower), max(forecast_belgium_arima$upper)))
lines(y=forecast_belgium_arima$mean, x=belgium_test$Year, type='b', col='red')
polygon(c(belgium_test$Year, rev(belgium_test$Year)), 
        c(forecast_belgium_arima$upper, rev(forecast_belgium_arima$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Belgium",
    y_test = belgium_test$material_footprint,
    y_hat = as.numeric(forecast_belgium_arima$mean),
    pi_lo = as.numeric(forecast_belgium_arima$lower[,1]),
    pi_hi = as.numeric(forecast_belgium_arima$upper[,1])
  )
)

#Denmark----
denmark_train <- Denmark[1:42,]
denmark_test <- Denmark[43:nrow(Denmark),]

denmark_arima <- auto.arima(denmark_train$material_footprint, seasonal=FALSE, max.p=7, 
                            max.q=7)
forecast_denmark_arima <- forecast(denmark_arima, h=nrow(denmark_test), level=95)

plot(y=denmark_test$material_footprint, x=denmark_test$Year, type='b', 
     xlab='Year', ylab='Material Footprint', main='denmark', 
     ylim=c(min(forecast_denmark_arima$lower), max(forecast_denmark_arima$upper)))
lines(y=forecast_denmark_arima$mean, x=denmark_test$Year, type='b', col='red')
polygon(c(denmark_test$Year, rev(denmark_test$Year)), 
        c(forecast_denmark_arima$upper, rev(forecast_denmark_arima$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Denmark",
    y_test = denmark_test$material_footprint,
    y_hat = as.numeric(forecast_denmark_arima$mean),
    pi_lo = as.numeric(forecast_denmark_arima$lower[,1]),
    pi_hi = as.numeric(forecast_denmark_arima$upper[,1])
  )
)

#Finland----
finland_train <- Finland[1:42,]
finland_test <- Finland[43:nrow(Finland),]

finland_arima <- auto.arima(finland_train$material_footprint, seasonal=FALSE, max.p=7, 
                            max.q=7)
forecast_finland_arima <- forecast(finland_arima, h=nrow(finland_test), level=95)

plot(y=finland_test$material_footprint, x=finland_test$Year, type='b', 
     xlab='Year', ylab='Material Footprint', main='finland', 
     ylim=c(min(forecast_finland_arima$lower), max(forecast_finland_arima$upper)))
lines(y=forecast_finland_arima$mean, x=finland_test$Year, type='b', col='red')
polygon(c(finland_test$Year, rev(finland_test$Year)), 
        c(forecast_finland_arima$upper, rev(forecast_finland_arima$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Finland",
    y_test = finland_test$material_footprint,
    y_hat = as.numeric(forecast_finland_arima$mean),
    pi_lo = as.numeric(forecast_finland_arima$lower[,1]),
    pi_hi = as.numeric(forecast_finland_arima$upper[,1])
  )
)

#France----
france_train <- France[1:42,]
france_test <- France[43:nrow(France),]

france_arima <- auto.arima(france_train$material_footprint, seasonal=FALSE, max.p=7, 
                            max.q=7)
forecast_france_arima <- forecast(france_arima, h=nrow(france_test), level=95)

plot(y=france_test$material_footprint, x=france_test$Year, type='b', 
     xlab='Year', ylab='Material Footprint', main='france', 
     ylim=c(min(forecast_france_arima$lower), max(forecast_france_arima$upper)))
lines(y=forecast_france_arima$mean, x=france_test$Year, type='b', col='red')
polygon(c(france_test$Year, rev(france_test$Year)), 
        c(forecast_france_arima$upper, rev(forecast_france_arima$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "France",
    y_test = france_test$material_footprint,
    y_hat = as.numeric(forecast_france_arima$mean),
    pi_lo = as.numeric(forecast_france_arima$lower[,1]),
    pi_hi = as.numeric(forecast_france_arima$upper[,1])
  )
)

#Germany----
germany_train <- Germany[1:42,]
germany_test <- Germany[43:nrow(Germany),]

germany_arima <- auto.arima(germany_train$material_footprint, seasonal=FALSE, max.p=7, 
                            max.q=7)
forecast_germany_arima <- forecast(germany_arima, h=nrow(germany_test), level=95)

plot(y=germany_test$material_footprint, x=germany_test$Year, type='b', 
     xlab='Year', ylab='Material Footprint', main='germany', 
     ylim=c(min(forecast_germany_arima$lower), max(forecast_germany_arima$upper)))
lines(y=forecast_germany_arima$mean, x=germany_test$Year, type='b', col='red')
polygon(c(germany_test$Year, rev(germany_test$Year)), 
        c(forecast_germany_arima$upper, rev(forecast_germany_arima$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Germany",
    y_test = germany_test$material_footprint,
    y_hat = as.numeric(forecast_germany_arima$mean),
    pi_lo = as.numeric(forecast_germany_arima$lower[,1]),
    pi_hi = as.numeric(forecast_germany_arima$upper[,1])
  )
)

#Ireland----
ireland_train <- Ireland[1:42,]
ireland_test <- Ireland[43:nrow(Ireland),]

ireland_arima <- auto.arima(ireland_train$material_footprint, seasonal=FALSE, max.p=7, 
                            max.q=7)
forecast_ireland_arima <- forecast(ireland_arima, h=nrow(ireland_test), level=95)

plot(y=ireland_test$material_footprint, x=ireland_test$Year, type='b', 
     xlab='Year', ylab='Material Footprint', main='ireland', 
     ylim=c(min(forecast_ireland_arima$lower), max(forecast_ireland_arima$upper)))
lines(y=forecast_ireland_arima$mean, x=ireland_test$Year, type='b', col='red')
polygon(c(ireland_test$Year, rev(ireland_test$Year)), 
        c(forecast_ireland_arima$upper, rev(forecast_ireland_arima$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Ireland",
    y_test = ireland_test$material_footprint,
    y_hat = as.numeric(forecast_ireland_arima$mean),
    pi_lo = as.numeric(forecast_ireland_arima$lower[,1]),
    pi_hi = as.numeric(forecast_ireland_arima$upper[,1])
  )
)

#Italy----
italy_train <- Italy[1:42,]
italy_test <- Italy[43:nrow(Italy),]

italy_arima <- auto.arima(italy_train$material_footprint, seasonal=FALSE, max.p=7, 
                            max.q=7)
forecast_italy_arima <- forecast(italy_arima, h=nrow(italy_test), level=95)

plot(y=italy_test$material_footprint, x=italy_test$Year, type='b', 
     xlab='Year', ylab='Material Footprint', main='italy', 
     ylim=c(min(forecast_italy_arima$lower), max(forecast_italy_arima$upper)))
lines(y=forecast_italy_arima$mean, x=italy_test$Year, type='b', col='red')
polygon(c(italy_test$Year, rev(italy_test$Year)), 
        c(forecast_italy_arima$upper, rev(forecast_italy_arima$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Italy",
    y_test = italy_test$material_footprint,
    y_hat = as.numeric(forecast_italy_arima$mean),
    pi_lo = as.numeric(forecast_italy_arima$lower[,1]),
    pi_hi = as.numeric(forecast_italy_arima$upper[,1])
  )
)

#Luxembourg----
luxembourg_train <- Luxembourg[1:42,]
luxembourg_test <- Luxembourg[43:nrow(Luxembourg),]

luxembourg_arima <- auto.arima(luxembourg_train$material_footprint, seasonal=FALSE, max.p=7, 
                            max.q=7)
forecast_luxembourg_arima <- forecast(luxembourg_arima, h=nrow(luxembourg_test), level=95)

plot(y=luxembourg_test$material_footprint, x=luxembourg_test$Year, type='b', 
     xlab='Year', ylab='Material Footprint', main='luxembourg', 
     ylim=c(min(forecast_luxembourg_arima$lower), max(forecast_luxembourg_arima$upper)))
lines(y=forecast_luxembourg_arima$mean, x=luxembourg_test$Year, type='b', col='red')
polygon(c(luxembourg_test$Year, rev(luxembourg_test$Year)), 
        c(forecast_luxembourg_arima$upper, rev(forecast_luxembourg_arima$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Luxembourg",
    y_test = luxembourg_test$material_footprint,
    y_hat = as.numeric(forecast_luxembourg_arima$mean),
    pi_lo = as.numeric(forecast_luxembourg_arima$lower[,1]),
    pi_hi = as.numeric(forecast_luxembourg_arima$upper[,1])
  )
)

#Netherlands----
netherlands_train <- Netherlands[1:42,]
netherlands_test <- Netherlands[43:nrow(Netherlands),]

netherlands_arima <- auto.arima(netherlands_train$material_footprint, seasonal=FALSE, max.p=7, 
                            max.q=7)
forecast_netherlands_arima <- forecast(netherlands_arima, h=nrow(netherlands_test), level=95)

plot(y=netherlands_test$material_footprint, x=netherlands_test$Year, type='b', 
     xlab='Year', ylab='Material Footprint', main='netherlands', 
     ylim=c(min(forecast_netherlands_arima$lower), max(forecast_netherlands_arima$upper)))
lines(y=forecast_netherlands_arima$mean, x=netherlands_test$Year, type='b', col='red')
polygon(c(netherlands_test$Year, rev(netherlands_test$Year)), 
        c(forecast_netherlands_arima$upper, rev(forecast_netherlands_arima$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Netherlands",
    y_test = netherlands_test$material_footprint,
    y_hat = as.numeric(forecast_netherlands_arima$mean),
    pi_lo = as.numeric(forecast_netherlands_arima$lower[,1]),
    pi_hi = as.numeric(forecast_netherlands_arima$upper[,1])
  )
)

#Portugal----
portugal_train <- Portugal[1:42,]
portugal_test <- Portugal[43:nrow(Portugal),]

portugal_arima <- auto.arima(portugal_train$material_footprint, seasonal=FALSE, max.p=7, 
                            max.q=7)
forecast_portugal_arima <- forecast(portugal_arima, h=nrow(portugal_test), level=95)

plot(y=portugal_test$material_footprint, x=portugal_test$Year, type='b', 
     xlab='Year', ylab='Material Footprint', main='portugal', 
     ylim=c(min(forecast_portugal_arima$lower), max(forecast_portugal_arima$upper)))
lines(y=forecast_portugal_arima$mean, x=portugal_test$Year, type='b', col='red')
polygon(c(portugal_test$Year, rev(portugal_test$Year)), 
        c(forecast_portugal_arima$upper, rev(forecast_portugal_arima$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Portugal",
    y_test = portugal_test$material_footprint,
    y_hat = as.numeric(forecast_portugal_arima$mean),
    pi_lo = as.numeric(forecast_portugal_arima$lower[,1]),
    pi_hi = as.numeric(forecast_portugal_arima$upper[,1])
  )
)

#Spain----
spain_train <- Spain[1:42,]
spain_test <- Spain[43:nrow(Spain),]

spain_arima <- auto.arima(spain_train$material_footprint, seasonal=FALSE, max.p=7, 
                            max.q=7)
forecast_spain_arima <- forecast(spain_arima, h=nrow(spain_test), level=95)

plot(y=spain_test$material_footprint, x=spain_test$Year, type='b', 
     xlab='Year', ylab='Material Footprint', main='spain', 
     ylim=c(min(forecast_spain_arima$lower), max(forecast_spain_arima$upper)))
lines(y=forecast_spain_arima$mean, x=spain_test$Year, type='b', col='red')
polygon(c(spain_test$Year, rev(spain_test$Year)), 
        c(forecast_spain_arima$upper, rev(forecast_spain_arima$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Spain",
    y_test = spain_test$material_footprint,
    y_hat = as.numeric(forecast_spain_arima$mean),
    pi_lo = as.numeric(forecast_spain_arima$lower[,1]),
    pi_hi = as.numeric(forecast_spain_arima$upper[,1])
  )
)

#Sweden----
sweden_train <- Sweden[1:42,]
sweden_test <- Sweden[43:nrow(Sweden),]

sweden_arima <- auto.arima(sweden_train$material_footprint, seasonal=FALSE, max.p=7, 
                            max.q=7)
forecast_sweden_arima <- forecast(sweden_arima, h=nrow(sweden_test), level=95)

plot(y=sweden_test$material_footprint, x=sweden_test$Year, type='b', 
     xlab='Year', ylab='Material Footprint', main='sweden', 
     ylim=c(min(forecast_sweden_arima$lower), max(forecast_sweden_arima$upper)))
lines(y=forecast_sweden_arima$mean, x=sweden_test$Year, type='b', col='red')
polygon(c(sweden_test$Year, rev(sweden_test$Year)), 
        c(forecast_sweden_arima$upper, rev(forecast_sweden_arima$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Sweden",
    y_test = sweden_test$material_footprint,
    y_hat = as.numeric(forecast_sweden_arima$mean),
    pi_lo = as.numeric(forecast_austria_arima$lower[,1]),
    pi_hi = as.numeric(forecast_austria_arima$upper[,1])
  )
)
#analysing PIs----
median(results_df$coverage)
median(results_df$relative_width_h1)
median(results_df$relative_width_h5)
median(results_df$relative_width_last)
