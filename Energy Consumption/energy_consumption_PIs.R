#Load in packages----
library(readxl)
library(lmtest)
library(forecast)
library(Metrics)
library(mgcv)
library(DiceKriging)
options(scipen=999)

#Read in the data----
Austria <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Austria.xlsx")[,c('Year', 'energy_consumption')])
Belgium <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Belgium.xlsx")[,c('Year', 'energy_consumption')])
Denmark<- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Denmark.xlsx")[,c('Year', 'energy_consumption')])
Finland <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Finland.xlsx")[,c('Year', 'energy_consumption')])
France <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/France.xlsx")[,c('Year', 'energy_consumption')])
Germany <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Germany.xlsx")[,c('Year', 'energy_consumption')])
Ireland <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Ireland.xlsx")[,c('Year', 'energy_consumption')])
Italy <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Italy.xlsx")[,c('Year', 'energy_consumption')])
Luxembourg <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Luxembourg.xlsx")[,c('Year', 'energy_consumption')])
Netherlands <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Netherlands.xlsx")[,c('Year', 'energy_consumption')])
Portugal <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Portugal.xlsx")[,c('Year', 'energy_consumption')])
Spain <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Spain.xlsx")[,c('Year', 'energy_consumption')])
Sweden <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Sweden.xlsx")[,c('Year', 'energy_consumption')])

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
austria_train <- Austria[1:24,]
austria_test <- Austria[25:nrow(Austria),]

austria_gam <- mgcv::gam(energy_consumption~s(Year, k=8), data=austria_train, method="REML")
forecast_austria_gam <- predict(austria_gam, newdata=austria_test, se.fit=TRUE)

pred_sd <- sqrt(forecast_austria_gam$se.fit^2 + austria_gam$sig2)
pi_lo <- forecast_austria_gam$fit-qnorm(1-0.05/2)*pred_sd
pi_hi <- forecast_austria_gam$fit+qnorm(1-0.05/2)*pred_sd
out_austria <- data.frame(Year=austria_test$Year, lo95=pi_lo,hi95=pi_hi)

plot(y=forecast_austria_gam$fit, x=austria_test$Year, type='b',
     ylim=c(min(austria_test$energy_consumption), max(austria_test$energy_consumption)+2),
     main='GAM', xlab='Year', ylab='energy_consumption', col="red")
lines(x=austria_test$Year, y=austria_test$energy_consumption, type="b")
polygon(c(out_austria$Year, rev(out_austria$Year)),
        c(out_austria$hi95, rev(out_austria$lo95)),
        border=NA, col=rgb(0,0,0,0.15))


results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Austria",
    y_test = austria_test$energy_consumption,
    y_hat = as.numeric(forecast_austria_gam$fit),
    pi_lo = as.numeric(out_austria$lo95),
    pi_hi = as.numeric(out_austria$hi95)
  )
)

#Belgium----
belgium_train <- Belgium[1:24,]
belgium_test <- Belgium[25:nrow(Belgium),]

belgium_gam <- mgcv::gam(energy_consumption~s(Year, k=8), data=belgium_train, method="REML")
forecast_belgium_gam <- predict(belgium_gam, newdata=belgium_test, se.fit=TRUE)

pred_sd <- sqrt(forecast_belgium_gam$se.fit^2 + belgium_gam$sig2)
pi_lo <- forecast_belgium_gam$fit-qnorm(1-0.05/2)*pred_sd
pi_hi <- forecast_belgium_gam$fit+qnorm(1-0.05/2)*pred_sd
out_belgium <- data.frame(Year=belgium_test$Year, lo95=pi_lo,hi95=pi_hi)

plot(y=forecast_belgium_gam$fit, x=belgium_test$Year, type='b',
     ylim=c(min(out_belgium$lo95), max(out_belgium$hi95)),
     main='GAM', xlab='Year', ylab='energy_consumption', col="red")
lines(x=belgium_test$Year, y=belgium_test$energy_consumption, type="b")
polygon(c(out_belgium$Year, rev(out_belgium$Year)),
        c(out_belgium$hi95, rev(out_belgium$lo95)),
        border=NA, col=rgb(0,0,0,0.15))

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Belgium",
    y_test = belgium_test$energy_consumption,
    y_hat = as.numeric(forecast_belgium_gam$fit),
    pi_lo = as.numeric(out_belgium$lo95),
    pi_hi = as.numeric(out_belgium$hi95)
  )
)

#Denmark----
denmark_train <- Denmark[1:24,]
denmark_test <- Denmark[25:nrow(Denmark),]

denmark_gam <- mgcv::gam(energy_consumption~s(Year, k=8), data=denmark_train, method="REML")
forecast_denmark_gam <- predict(denmark_gam, newdata=denmark_test, se.fit=TRUE)

pred_sd <- sqrt(forecast_denmark_gam$se.fit^2 + denmark_gam$sig2)
pi_lo <- forecast_denmark_gam$fit-qnorm(1-0.05/2)*pred_sd
pi_hi <- forecast_denmark_gam$fit+qnorm(1-0.05/2)*pred_sd
out_denmark <- data.frame(Year=denmark_test$Year, lo95=pi_lo,hi95=pi_hi)

plot(y=forecast_denmark_gam$fit, x=denmark_test$Year, type='b',
     ylim=c(min(denmark_test$energy_consumption), max(out_denmark$hi95)),
     main='GAM', xlab='Year', ylab='energy_consumption', col="red")
lines(x=denmark_test$Year, y=denmark_test$energy_consumption, type="b")
polygon(c(out_denmark$Year, rev(out_denmark$Year)),
        c(out_denmark$hi95, rev(out_denmark$lo95)),
        border=NA, col=rgb(0,0,0,0.15))

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Denmark",
    y_test = denmark_test$energy_consumption,
    y_hat = as.numeric(forecast_denmark_gam$fit),
    pi_lo = as.numeric(out_denmark$lo95),
    pi_hi = as.numeric(out_denmark$hi95)
  )
)

#Finland----
finland_train <- Finland[1:24,]
finland_test <- Finland[25:nrow(Finland),]

finland_gam <- mgcv::gam(energy_consumption~s(Year, k=8), data=finland_train, method="REML")
forecast_finland_gam <- predict(finland_gam, newdata=finland_test, se.fit=TRUE)

pred_sd <- sqrt(forecast_finland_gam$se.fit^2 + finland_gam$sig2)
pi_lo <- forecast_finland_gam$fit-qnorm(1-0.05/2)*pred_sd
pi_hi <- forecast_finland_gam$fit+qnorm(1-0.05/2)*pred_sd
out_finland <- data.frame(Year=finland_test$Year, lo95=pi_lo,hi95=pi_hi)

plot(y=forecast_finland_gam$fit, x=finland_test$Year, type='b',
     ylim=c(min(finland_test$energy_consumption), max(finland_test$energy_consumption)+2),
     main='GAM', xlab='Year', ylab='energy_consumption', col="red")
lines(x=finland_test$Year, y=finland_test$energy_consumption, type="b")
polygon(c(out_finland$Year, rev(out_finland$Year)),
        c(out_finland$hi95, rev(out_finland$lo95)),
        border=NA, col=rgb(0,0,0,0.15))

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Finland",
    y_test = finland_test$energy_consumption,
    y_hat = as.numeric(forecast_finland_gam$fit),
    pi_lo = as.numeric(out_finland$lo95),
    pi_hi = as.numeric(out_finland$hi95)
  )
)

#France----
france_train <- France[1:24,]
france_test <- France[25:nrow(France),]

france_gam <- mgcv::gam(energy_consumption~s(Year, k=8), data=france_train, method="REML")
forecast_france_gam <- predict(france_gam, newdata=france_test, se.fit=TRUE)

pred_sd <- sqrt(forecast_france_gam$se.fit^2 + france_gam$sig2)
pi_lo <- forecast_france_gam$fit-qnorm(1-0.05/2)*pred_sd
pi_hi <- forecast_france_gam$fit+qnorm(1-0.05/2)*pred_sd
out_france <- data.frame(Year=france_test$Year, lo95=pi_lo,hi95=pi_hi)

plot(y=forecast_france_gam$fit, x=france_test$Year, type='b',
     ylim=c(min(france_test$energy_consumption), max(out_france$hi95)),
     main='GAM', xlab='Year', ylab='energy_consumption', col="red")
lines(x=france_test$Year, y=france_test$energy_consumption, type="b")
polygon(c(out_france$Year, rev(out_france$Year)),
        c(out_france$hi95, rev(out_france$lo95)),
        border=NA, col=rgb(0,0,0,0.15))

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "France",
    y_test = france_test$energy_consumption,
    y_hat = as.numeric(forecast_france_gam$fit),
    pi_lo = as.numeric(out_france$lo95),
    pi_hi = as.numeric(out_france$hi95)
  )
)

#Germany----
germany_train <- Germany[1:24,]
germany_test <- Germany[25:nrow(Germany),]

germany_gam <- mgcv::gam(energy_consumption~s(Year, k=8), data=germany_train, method="REML")
forecast_germany_gam <- predict(germany_gam, newdata=germany_test, se.fit=TRUE)

pred_sd <- sqrt(forecast_germany_gam$se.fit^2 + germany_gam$sig2)
pi_lo <- forecast_germany_gam$fit-qnorm(1-0.05/2)*pred_sd
pi_hi <- forecast_germany_gam$fit+qnorm(1-0.05/2)*pred_sd
out_germany <- data.frame(Year=germany_test$Year, lo95=pi_lo,hi95=pi_hi)

plot(y=forecast_germany_gam$fit, x=germany_test$Year, type='b',
     ylim=c(min(out_germany$lo95), max(out_germany$hi95)+2),
     main='GAM', xlab='Year', ylab='energy_consumption', col="red")
lines(x=germany_test$Year, y=germany_test$energy_consumption, type="b")
polygon(c(out_germany$Year, rev(out_germany$Year)),
        c(out_germany$hi95, rev(out_germany$lo95)),
        border=NA, col=rgb(0,0,0,0.15))

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Germany",
    y_test = germany_test$energy_consumption,
    y_hat = as.numeric(forecast_germany_gam$fit),
    pi_lo = as.numeric(out_germany$lo95),
    pi_hi = as.numeric(out_germany$hi95)
  )
)

#Ireland----
ireland_train <- Ireland[1:24,]
ireland_test <- Ireland[25:nrow(Ireland),]

ireland_gam <- mgcv::gam(energy_consumption~s(Year, k=8), data=ireland_train, method="REML")
forecast_ireland_gam <- predict(ireland_gam, newdata=ireland_test, se.fit=TRUE)

pred_sd <- sqrt(forecast_ireland_gam$se.fit^2 + ireland_gam$sig2)
pi_lo <- forecast_ireland_gam$fit-qnorm(1-0.05/2)*pred_sd
pi_hi <- forecast_ireland_gam$fit+qnorm(1-0.05/2)*pred_sd
out_ireland <- data.frame(Year=ireland_test$Year, lo95=pi_lo,hi95=pi_hi)

plot(y=forecast_ireland_gam$fit, x=ireland_test$Year, type='b',
     ylim=c(min(out_ireland$lo95), max(ireland_test$energy_consumption)+2),
     main='GAM', xlab='Year', ylab='energy_consumption', col="red")
lines(x=ireland_test$Year, y=ireland_test$energy_consumption, type="b")
polygon(c(out_ireland$Year, rev(out_ireland$Year)),
        c(out_ireland$hi95, rev(out_ireland$lo95)),
        border=NA, col=rgb(0,0,0,0.15))

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Ireland",
    y_test = ireland_test$energy_consumption,
    y_hat = as.numeric(forecast_ireland_gam$fit),
    pi_lo = as.numeric(out_ireland$lo95),
    pi_hi = as.numeric(out_ireland$hi95)
  )
)

#Italy----
italy_train <- Italy[1:24,]
italy_test <- Italy[25:nrow(Italy),]

italy_gam <- mgcv::gam(energy_consumption~s(Year, k=8), data=italy_train, method="REML")
forecast_italy_gam <- predict(italy_gam, newdata=italy_test, se.fit=TRUE)

pred_sd <- sqrt(forecast_italy_gam$se.fit^2 + italy_gam$sig2)
pi_lo <- forecast_italy_gam$fit-qnorm(1-0.05/2)*pred_sd
pi_hi <- forecast_italy_gam$fit+qnorm(1-0.05/2)*pred_sd
out_italy <- data.frame(Year=italy_test$Year, lo95=pi_lo,hi95=pi_hi)

plot(y=forecast_italy_gam$fit, x=italy_test$Year, type='b',
     ylim=c(min(italy_test$energy_consumption), max(italy_test$energy_consumption)+2),
     main='GAM', xlab='Year', ylab='energy_consumption', col="red")
lines(x=italy_test$Year, y=italy_test$energy_consumption, type="b")
polygon(c(out_italy$Year, rev(out_italy$Year)),
        c(out_italy$hi95, rev(out_italy$lo95)),
        border=NA, col=rgb(0,0,0,0.15))

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Italy",
    y_test = italy_test$energy_consumption,
    y_hat = as.numeric(forecast_italy_gam$fit),
    pi_lo = as.numeric(out_italy$lo95),
    pi_hi = as.numeric(out_italy$hi95)
  )
)

#Luxembourg----
luxembourg_train <- Luxembourg[1:24,]
luxembourg_test <- Luxembourg[25:nrow(Luxembourg),]

luxembourg_gam <- mgcv::gam(energy_consumption~s(Year, k=8), data=luxembourg_train, method="REML")
forecast_luxembourg_gam <- predict(luxembourg_gam, newdata=luxembourg_test, se.fit=TRUE)

pred_sd <- sqrt(forecast_luxembourg_gam$se.fit^2 + luxembourg_gam$sig2)
pi_lo <- forecast_luxembourg_gam$fit-qnorm(1-0.05/2)*pred_sd
pi_hi <- forecast_luxembourg_gam$fit+qnorm(1-0.05/2)*pred_sd
out_luxembourg <- data.frame(Year=luxembourg_test$Year, lo95=pi_lo,hi95=pi_hi)

plot(y=forecast_luxembourg_gam$fit, x=luxembourg_test$Year, type='b',
     ylim=c(min(luxembourg_test$energy_consumption), max(luxembourg_test$energy_consumption)+2),
     main='GAM', xlab='Year', ylab='energy_consumption', col="red")
lines(x=luxembourg_test$Year, y=luxembourg_test$energy_consumption, type="b")
polygon(c(out_luxembourg$Year, rev(out_luxembourg$Year)),
        c(out_luxembourg$hi95, rev(out_luxembourg$lo95)),
        border=NA, col=rgb(0,0,0,0.15))

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Luxembourg",
    y_test = luxembourg_test$energy_consumption,
    y_hat = as.numeric(forecast_luxembourg_gam$fit),
    pi_lo = as.numeric(out_luxembourg$lo95),
    pi_hi = as.numeric(out_luxembourg$hi95)
  )
)

#Netherlands----
netherlands_train <- Netherlands[1:24,]
netherlands_test <- Netherlands[25:nrow(Netherlands),]

netherlands_gam <- mgcv::gam(energy_consumption~s(Year, k=8), data=netherlands_train, method="REML")
forecast_netherlands_gam <- predict(netherlands_gam, newdata=netherlands_test, se.fit=TRUE)

pred_sd <- sqrt(forecast_netherlands_gam$se.fit^2 + netherlands_gam$sig2)
pi_lo <- forecast_netherlands_gam$fit-qnorm(1-0.05/2)*pred_sd
pi_hi <- forecast_netherlands_gam$fit+qnorm(1-0.05/2)*pred_sd
out_netherlands <- data.frame(Year=netherlands_test$Year, lo95=pi_lo,hi95=pi_hi)

plot(y=forecast_netherlands_gam$fit, x=netherlands_test$Year, type='b',
     ylim=c(min(netherlands_test$energy_consumption), max(out_netherlands$hi95)+2),
     main='GAM', xlab='Year', ylab='energy_consumption', col="red")
lines(x=netherlands_test$Year, y=netherlands_test$energy_consumption, type="b")
polygon(c(out_netherlands$Year, rev(out_netherlands$Year)),
        c(out_netherlands$hi95, rev(out_netherlands$lo95)),
        border=NA, col=rgb(0,0,0,0.15))

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Netherlands",
    y_test = netherlands_test$energy_consumption,
    y_hat = as.numeric(forecast_netherlands_gam$fit),
    pi_lo = as.numeric(out_netherlands$lo95),
    pi_hi = as.numeric(out_netherlands$hi95)
  )
)

#Portugal----
portugal_train <- Portugal[1:24,]
portugal_test <- Portugal[25:nrow(Portugal),]

portugal_gam <- mgcv::gam(energy_consumption~s(Year, k=8), data=portugal_train, method="REML")
forecast_portugal_gam <- predict(portugal_gam, newdata=portugal_test, se.fit=TRUE)

pred_sd <- sqrt(forecast_portugal_gam$se.fit^2 + portugal_gam$sig2)
pi_lo <- forecast_portugal_gam$fit-qnorm(1-0.05/2)*pred_sd
pi_hi <- forecast_portugal_gam$fit+qnorm(1-0.05/2)*pred_sd
out_portugal <- data.frame(Year=portugal_test$Year, lo95=pi_lo,hi95=pi_hi)

plot(y=forecast_portugal_gam$fit, x=portugal_test$Year, type='b',
     ylim=c(min(out_portugal$lo95), max(portugal_test$energy_consumption)+2),
     main='GAM', xlab='Year', ylab='energy_consumption', col="red")
lines(x=portugal_test$Year, y=portugal_test$energy_consumption, type="b")
polygon(c(out_portugal$Year, rev(out_portugal$Year)),
        c(out_portugal$hi95, rev(out_portugal$lo95)),
        border=NA, col=rgb(0,0,0,0.15))

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Portugal",
    y_test = portugal_test$energy_consumption,
    y_hat = as.numeric(forecast_portugal_gam$fit),
    pi_lo = as.numeric(out_portugal$lo95),
    pi_hi = as.numeric(out_portugal$hi95)
  )
)

#Spain----
spain_train <- Spain[1:24,]
spain_test <- Spain[25:nrow(Spain),]

spain_gam <- mgcv::gam(energy_consumption~s(Year, k=8), data=spain_train, method="REML")
forecast_spain_gam <- predict(spain_gam, newdata=spain_test, se.fit=TRUE)

pred_sd <- sqrt(forecast_spain_gam$se.fit^2 + spain_gam$sig2)
pi_lo <- forecast_spain_gam$fit-qnorm(1-0.05/2)*pred_sd
pi_hi <- forecast_spain_gam$fit+qnorm(1-0.05/2)*pred_sd
out_spain <- data.frame(Year=spain_test$Year, lo95=pi_lo,hi95=pi_hi)

plot(y=forecast_spain_gam$fit, x=spain_test$Year, type='b',
     ylim=c(min(out_spain$lo95), max(out_spain$hi95)+2),
     main='GAM', xlab='Year', ylab='energy_consumption', col="red")
lines(x=spain_test$Year, y=spain_test$energy_consumption, type="b")
polygon(c(out_spain$Year, rev(out_spain$Year)),
        c(out_spain$hi95, rev(out_spain$lo95)),
        border=NA, col=rgb(0,0,0,0.15))

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Spain",
    y_test = spain_test$energy_consumption,
    y_hat = as.numeric(forecast_spain_gam$fit),
    pi_lo = as.numeric(out_spain$lo95),
    pi_hi = as.numeric(out_spain$hi95)
  )
)

#Sweden----
sweden_train <- Sweden[1:24,]
sweden_test <- Sweden[25:nrow(Sweden),]

sweden_gam <- mgcv::gam(energy_consumption~s(Year, k=8), data=sweden_train, method="REML")
forecast_sweden_gam <- predict(sweden_gam, newdata=sweden_test, se.fit=TRUE)

pred_sd <- sqrt(forecast_sweden_gam$se.fit^2 + sweden_gam$sig2)
pi_lo <- forecast_sweden_gam$fit-qnorm(1-0.05/2)*pred_sd
pi_hi <- forecast_sweden_gam$fit+qnorm(1-0.05/2)*pred_sd
out_sweden <- data.frame(Year=sweden_test$Year, lo95=pi_lo,hi95=pi_hi)

plot(y=forecast_sweden_gam$fit, x=sweden_test$Year, type='b',
     ylim=c(min(out_sweden$lo95), max(out_sweden$hi95)+2),
     main='GAM', xlab='Year', ylab='energy_consumption', col="red")
lines(x=sweden_test$Year, y=sweden_test$energy_consumption, type="b")
polygon(c(out_sweden$Year, rev(out_sweden$Year)),
        c(out_sweden$hi95, rev(out_sweden$lo95)),
        border=NA, col=rgb(0,0,0,0.15))

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Sweden",
    y_test = sweden_test$energy_consumption,
    y_hat = as.numeric(forecast_sweden_gam$fit),
    pi_lo = as.numeric(out_sweden$lo95),
    pi_hi = as.numeric(out_sweden$hi95)
  )
)
#analysing PIs----
median(results_df$coverage)
median(results_df$relative_width_h1)
median(results_df$relative_width_h5)
median(results_df$relative_width_last)

