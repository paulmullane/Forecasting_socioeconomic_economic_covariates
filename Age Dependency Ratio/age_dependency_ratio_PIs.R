#packages----
library(readxl)
library(lmtest)
library(forecast)
library(Metrics)
library(mgcv)
library(DiceKriging)
options(scipen=999)

#Load the data----
Austria <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Austria.xlsx")[,c('Year', 'age_dependency_ratio')])
Belgium <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Belgium.xlsx")[,c('Year', 'age_dependency_ratio')])
Denmark<- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Denmark.xlsx")[,c('Year', 'age_dependency_ratio')])
Finland <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Finland.xlsx")[,c('Year', 'age_dependency_ratio')])
France <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/France.xlsx")[,c('Year', 'age_dependency_ratio')])
Germany <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Germany.xlsx")[,c('Year', 'age_dependency_ratio')])
Ireland <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Ireland.xlsx")[,c('Year', 'age_dependency_ratio')])
Italy <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Italy.xlsx")[,c('Year', 'age_dependency_ratio')])
Luxembourg <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Luxembourg.xlsx")[,c('Year', 'age_dependency_ratio')])
Netherlands <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Netherlands.xlsx")[,c('Year', 'age_dependency_ratio')])
Portugal <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Portugal.xlsx")[,c('Year', 'age_dependency_ratio')])
Spain <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Spain.xlsx")[,c('Year', 'age_dependency_ratio')])
Sweden <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Sweden.xlsx")[,c('Year', 'age_dependency_ratio')])

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
austria_train <- Austria[1:52,]
austria_test <- Austria[53:nrow(Austria),]

austria_ets <- ets(austria_train$age_dependency_ratio, model="ZZN")
forecast_austria_ets <- forecast(austria_ets, h=nrow(austria_test), level=95)

plot(y=forecast_austria_ets$mean, x=austria_test$Year, type='b',
     ylim=c(min(forecast_austria_ets$lower), max(forecast_austria_ets$upper)),
     main='Austria', xlab='Year', ylab='age dependency ratio', col='red')
lines(y=austria_test$age_dependency_ratio, x=austria_test$Year, type='b')
polygon(c(austria_test$Year, rev(austria_test$Year)), 
        c(forecast_austria_ets$upper, rev(forecast_austria_ets$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Austria",
    y_test = austria_test$age_dependency_ratio,
    y_hat = as.numeric(forecast_austria_ets$mean),
    pi_lo = as.numeric(forecast_austria_ets$lower[,1]),
    pi_hi = as.numeric(forecast_austria_ets$upper[,1])
  )
)

#Belgium----
belgium_train <- Belgium[1:52,]
belgium_test <- Belgium[53:nrow(Belgium),]

belgium_ets <- ets(belgium_train$age_dependency_ratio, model="ZZN")
forecast_belgium_ets <- forecast(belgium_ets, h=nrow(belgium_test), level=95)

plot(y=forecast_belgium_ets$mean, x=belgium_test$Year, type='b',
     ylim=c(min(forecast_belgium_ets$lower), max(forecast_belgium_ets$upper)),
     main='belgium', xlab='Year', ylab='age dependency ratio', col='red')
lines(y=belgium_test$age_dependency_ratio, x=belgium_test$Year, type='b')
polygon(c(belgium_test$Year, rev(belgium_test$Year)), 
        c(forecast_belgium_ets$upper, rev(forecast_belgium_ets$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Belgium",
    y_test = belgium_test$age_dependency_ratio,
    y_hat = as.numeric(forecast_belgium_ets$mean),
    pi_lo = as.numeric(forecast_belgium_ets$lower[,1]),
    pi_hi = as.numeric(forecast_belgium_ets$upper[,1])
  )
)

#Denmark----
denmark_train <- Denmark[1:52,]
denmark_test <- Denmark[53:nrow(Denmark),]

denmark_ets <- ets(denmark_train$age_dependency_ratio, model="ZZN")
forecast_denmark_ets <- forecast(denmark_ets, h=nrow(denmark_test), level=95)

plot(y=forecast_denmark_ets$mean, x=denmark_test$Year, type='b',
     ylim=c(min(forecast_denmark_ets$lower), max(forecast_denmark_ets$upper)),
     main='denmark', xlab='Year', ylab='age dependency ratio', col='red')
lines(y=denmark_test$age_dependency_ratio, x=denmark_test$Year, type='b')
polygon(c(denmark_test$Year, rev(denmark_test$Year)), 
        c(forecast_denmark_ets$upper, rev(forecast_denmark_ets$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Denmark",
    y_test = denmark_test$age_dependency_ratio,
    y_hat = as.numeric(forecast_denmark_ets$mean),
    pi_lo = as.numeric(forecast_denmark_ets$lower[,1]),
    pi_hi = as.numeric(forecast_denmark_ets$upper[,1])
  )
)

#Finland----
finland_train <- Finland[1:52,]
finland_test <- Finland[53:nrow(Finland),]

finland_ets <- ets(finland_train$age_dependency_ratio, model="ZZN")
forecast_finland_ets <- forecast(finland_ets, h=nrow(finland_test), level=95)

plot(y=forecast_finland_ets$mean, x=finland_test$Year, type='b',
     ylim=c(min(forecast_finland_ets$lower), max(forecast_finland_ets$upper)),
     main='finland', xlab='Year', ylab='age dependency ratio', col='red')
lines(y=finland_test$age_dependency_ratio, x=finland_test$Year, type='b')
polygon(c(finland_test$Year, rev(finland_test$Year)), 
        c(forecast_finland_ets$upper, rev(forecast_finland_ets$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Finland",
    y_test = finland_test$age_dependency_ratio,
    y_hat = as.numeric(forecast_finland_ets$mean),
    pi_lo = as.numeric(forecast_finland_ets$lower[,1]),
    pi_hi = as.numeric(forecast_finland_ets$upper[,1])
  )
)

#France----
france_train <- France[1:52,]
france_test <- France[53:nrow(France),]

france_ets <- ets(france_train$age_dependency_ratio, model="ZZN")
forecast_france_ets <- forecast(france_ets, h=nrow(france_test), level=95)

plot(y=forecast_france_ets$mean, x=france_test$Year, type='b',
     ylim=c(min(forecast_france_ets$lower), max(forecast_france_ets$upper)),
     main='france', xlab='Year', ylab='age dependency ratio', col='red')
lines(y=france_test$age_dependency_ratio, x=france_test$Year, type='b')
polygon(c(france_test$Year, rev(france_test$Year)), 
        c(forecast_france_ets$upper, rev(forecast_france_ets$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "France",
    y_test = france_test$age_dependency_ratio,
    y_hat = as.numeric(forecast_france_ets$mean),
    pi_lo = as.numeric(forecast_france_ets$lower[,1]),
    pi_hi = as.numeric(forecast_france_ets$upper[,1])
  )
)

#Germany----
germany_train <- Germany[1:52,]
germany_test <- Germany[53:nrow(Germany),]

germany_ets <- ets(germany_train$age_dependency_ratio, model="ZZN")
forecast_germany_ets <- forecast(germany_ets, h=nrow(germany_test), level=95)

plot(y=forecast_germany_ets$mean, x=germany_test$Year, type='b',
     ylim=c(min(forecast_germany_ets$lower), max(forecast_germany_ets$upper)),
     main='germany', xlab='Year', ylab='age dependency ratio', col='red')
lines(y=germany_test$age_dependency_ratio, x=germany_test$Year, type='b')
polygon(c(germany_test$Year, rev(germany_test$Year)), 
        c(forecast_germany_ets$upper, rev(forecast_germany_ets$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Germany",
    y_test = germany_test$age_dependency_ratio,
    y_hat = as.numeric(forecast_germany_ets$mean),
    pi_lo = as.numeric(forecast_germany_ets$lower[,1]),
    pi_hi = as.numeric(forecast_germany_ets$upper[,1])
  )
)

#Ireland----
ireland_train <- Ireland[1:52,]
ireland_test <- Ireland[53:nrow(Ireland),]

ireland_ets <- ets(ireland_train$age_dependency_ratio, model="ZZN")
forecast_ireland_ets <- forecast(ireland_ets, h=nrow(ireland_test), level=95)

plot(y=forecast_ireland_ets$mean, x=ireland_test$Year, type='b',
     ylim=c(min(forecast_ireland_ets$lower), max(forecast_ireland_ets$upper)),
     main='ireland', xlab='Year', ylab='age dependency ratio', col='red')
lines(y=ireland_test$age_dependency_ratio, x=ireland_test$Year, type='b')
polygon(c(ireland_test$Year, rev(ireland_test$Year)), 
        c(forecast_ireland_ets$upper, rev(forecast_ireland_ets$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Ireland",
    y_test = ireland_test$age_dependency_ratio,
    y_hat = as.numeric(forecast_ireland_ets$mean),
    pi_lo = as.numeric(forecast_ireland_ets$lower[,1]),
    pi_hi = as.numeric(forecast_ireland_ets$upper[,1])
  )
)

#Italy----
italy_train <- Italy[1:52,]
italy_test <- Italy[53:nrow(Italy),]

italy_ets <- ets(italy_train$age_dependency_ratio, model="ZZN")
forecast_italy_ets <- forecast(italy_ets, h=nrow(italy_test), level=95)

plot(y=forecast_italy_ets$mean, x=italy_test$Year, type='b',
     ylim=c(min(forecast_italy_ets$lower), max(forecast_italy_ets$upper)),
     main='italy', xlab='Year', ylab='age dependency ratio', col='red')
lines(y=italy_test$age_dependency_ratio, x=italy_test$Year, type='b')
polygon(c(italy_test$Year, rev(italy_test$Year)), 
        c(forecast_italy_ets$upper, rev(forecast_italy_ets$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Italy",
    y_test = italy_test$age_dependency_ratio,
    y_hat = as.numeric(forecast_italy_ets$mean),
    pi_lo = as.numeric(forecast_italy_ets$lower[,1]),
    pi_hi = as.numeric(forecast_italy_ets$upper[,1])
  )
)

#Luxembourg----
luxembourg_train <- Luxembourg[1:52,]
luxembourg_test <- Luxembourg[53:nrow(Luxembourg),]

luxembourg_ets <- ets(luxembourg_train$age_dependency_ratio, model="ZZN")
forecast_luxembourg_ets <- forecast(luxembourg_ets, h=nrow(luxembourg_test), level=95)

plot(y=forecast_luxembourg_ets$mean, x=luxembourg_test$Year, type='b',
     ylim=c(min(forecast_luxembourg_ets$lower), max(forecast_luxembourg_ets$upper)),
     main='luxembourg', xlab='Year', ylab='age dependency ratio', col='red')
lines(y=luxembourg_test$age_dependency_ratio, x=luxembourg_test$Year, type='b')
polygon(c(luxembourg_test$Year, rev(luxembourg_test$Year)), 
        c(forecast_luxembourg_ets$upper, rev(forecast_luxembourg_ets$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Luxembourg",
    y_test = luxembourg_test$age_dependency_ratio,
    y_hat = as.numeric(forecast_luxembourg_ets$mean),
    pi_lo = as.numeric(forecast_luxembourg_ets$lower[,1]),
    pi_hi = as.numeric(forecast_luxembourg_ets$upper[,1])
  )
)

#Netherlands----
netherlands_train <- Netherlands[1:52,]
netherlands_test <- Netherlands[53:nrow(Netherlands),]

netherlands_ets <- ets(netherlands_train$age_dependency_ratio, model="ZZN")
forecast_netherlands_ets <- forecast(netherlands_ets, h=nrow(netherlands_test), level=95)

plot(y=forecast_netherlands_ets$mean, x=netherlands_test$Year, type='b',
     ylim=c(min(forecast_netherlands_ets$lower), max(forecast_netherlands_ets$upper)),
     main='netherlands', xlab='Year', ylab='age dependency ratio', col='red')
lines(y=netherlands_test$age_dependency_ratio, x=netherlands_test$Year, type='b')
polygon(c(netherlands_test$Year, rev(netherlands_test$Year)), 
        c(forecast_netherlands_ets$upper, rev(forecast_netherlands_ets$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Netherlands",
    y_test = netherlands_test$age_dependency_ratio,
    y_hat = as.numeric(forecast_netherlands_ets$mean),
    pi_lo = as.numeric(forecast_netherlands_ets$lower[,1]),
    pi_hi = as.numeric(forecast_netherlands_ets$upper[,1])
  )
)

#Portugal----
portugal_train <- Portugal[1:52,]
portugal_test <- Portugal[53:nrow(Portugal),]

portugal_ets <- ets(portugal_train$age_dependency_ratio, model="ZZN")
forecast_portugal_ets <- forecast(portugal_ets, h=nrow(portugal_test), level=95)

plot(y=forecast_portugal_ets$mean, x=portugal_test$Year, type='b',
     ylim=c(min(forecast_portugal_ets$lower), max(forecast_portugal_ets$upper)),
     main='portugal', xlab='Year', ylab='age dependency ratio', col='red')
lines(y=portugal_test$age_dependency_ratio, x=portugal_test$Year, type='b')
polygon(c(portugal_test$Year, rev(portugal_test$Year)), 
        c(forecast_portugal_ets$upper, rev(forecast_portugal_ets$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Portugal",
    y_test = portugal_test$age_dependency_ratio,
    y_hat = as.numeric(forecast_portugal_ets$mean),
    pi_lo = as.numeric(forecast_portugal_ets$lower[,1]),
    pi_hi = as.numeric(forecast_portugal_ets$upper[,1])
  )
)

#Spain----
spain_train <- Spain[1:52,]
spain_test <- Spain[53:nrow(Spain),]

spain_ets <- ets(spain_train$age_dependency_ratio, model="ZZN")
forecast_spain_ets <- forecast(spain_ets, h=nrow(spain_test), level=95)

plot(y=forecast_spain_ets$mean, x=spain_test$Year, type='b',
     ylim=c(min(forecast_spain_ets$lower), max(forecast_spain_ets$upper)),
     main='spain', xlab='Year', ylab='age dependency ratio', col='red')
lines(y=spain_test$age_dependency_ratio, x=spain_test$Year, type='b')
polygon(c(spain_test$Year, rev(spain_test$Year)), 
        c(forecast_spain_ets$upper, rev(forecast_spain_ets$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Spain",
    y_test = spain_test$age_dependency_ratio,
    y_hat = as.numeric(forecast_spain_ets$mean),
    pi_lo = as.numeric(forecast_spain_ets$lower[,1]),
    pi_hi = as.numeric(forecast_spain_ets$upper[,1])
  )
)

#Sweden----
sweden_train <- Sweden[1:52,]
sweden_test <- Sweden[53:nrow(Sweden),]

sweden_ets <- ets(sweden_train$age_dependency_ratio, model="ZZN")
forecast_sweden_ets <- forecast(sweden_ets, h=nrow(sweden_test), level=95)

plot(y=forecast_sweden_ets$mean, x=sweden_test$Year, type='b',
     ylim=c(min(forecast_sweden_ets$lower), max(forecast_sweden_ets$upper)),
     main='sweden', xlab='Year', ylab='age dependency ratio', col='red')
lines(y=sweden_test$age_dependency_ratio, x=sweden_test$Year, type='b')
polygon(c(sweden_test$Year, rev(sweden_test$Year)), 
        c(forecast_sweden_ets$upper, rev(forecast_sweden_ets$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Sweden",
    y_test = sweden_test$age_dependency_ratio,
    y_hat = as.numeric(forecast_sweden_ets$mean),
    pi_lo = as.numeric(forecast_sweden_ets$lower[,1]),
    pi_hi = as.numeric(forecast_sweden_ets$upper[,1])
  )
)

#analysing PIs----
median(results_df$coverage)
median(results_df$relative_width_h1)
median(results_df$relative_width_h5)
median(results_df$relative_width_last)


