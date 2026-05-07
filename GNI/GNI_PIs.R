#packages----
library(readxl)
library(lmtest)
library(forecast)
library(Metrics)
library(mgcv)
library(DiceKriging)
options(scipen=999)

validation_errors <- function(train_df, p_optimal, size_optimal){
  stopifnot("gni" %in% names(train_df))
  
  y <- train_df$gni
  freq <- if (is.ts(y)) frequency(y) else 1
  y_num <- as.numeric(y)
  
  n <- length(y_num)
  n_val  <- ceiling(0.2 * n)
  n_core <- n - n_val
  
  y_core <- ts(y_num[1:n_core], frequency = freq)
  y_val  <- y_num[(n_core + 1):n]
  
  set.seed(20229798)
  fit <- nnetar(y_core, p = p_optimal, size = size_optimal)
  
  # Forecast the whole validation block in one go
  fc_val <- forecast(fit, h = length(y_val))
  
  # Validation residuals (actual - forecast)
  e_valid <- y_val - as.numeric(fc_val$mean)
  
  e_valid
}

#loading in the data----
Austria <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Austria.xlsx")[,c('Year', 'gni')])
Belgium <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Belgium.xlsx")[,c('Year', 'gni')])
Denmark<- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Denmark.xlsx")[,c('Year', 'gni')])
Finland <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Finland.xlsx")[,c('Year', 'gni')])
France <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/France.xlsx")[,c('Year', 'gni')])
Germany <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Germany.xlsx")[,c('Year', 'gni')])
Ireland <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Ireland.xlsx")[,c('Year', 'gni')])
Italy <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Italy.xlsx")[,c('Year', 'gni')])
Luxembourg <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Luxembourg.xlsx")[,c('Year', 'gni')])
Netherlands <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Netherlands.xlsx")[,c('Year', 'gni')])
Portugal <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Portugal.xlsx")[,c('Year', 'gni')])
Spain <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Spain.xlsx")[,c('Year', 'gni')])
Sweden <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Sweden.xlsx")[,c('Year', 'gni')])

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

relative_pi_width_h <- function(y_test, pi_lo, pi_hi, h) {
  if(h > length(y_test)) return(NA_real_)
  if(is.na(y_test[h]) || is.na(pi_lo[h]) || is.na(pi_hi[h])) return(NA_real_)
  if(abs(y_test[h]) == 0) return(NA_real_)
  
  (pi_hi[h] - pi_lo[h]) / abs(y_test[h])
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
austria_train <- Austria[1:50,]
austria_test <- Austria[51:nrow(Austria),]

austria_nnar <- nnetar(austria_train$gni, p=4, size=2)

austria_val_errors <- validation_errors(austria_train, 4, 2)
austria_innov <- matrix(rnorm(nrow(austria_test)*1000, mean=0, 
                              sd=sd(austria_val_errors, na.rm=TRUE)), 
                        nrow=nrow(austria_test), ncol=1000)


forecast_austria_nnar <- forecast(austria_nnar, h=nrow(austria_test), PI=TRUE, 
                                  level=c(95), npaths=1000, innov=austria_innov)

plot(y=forecast_austria_nnar$mean, x=austria_test$Year, type='b',
     ylim=c(min(forecast_austria_nnar$lower), max(forecast_austria_nnar$upper)),
     main='Austria', xlab='Year', ylab='gni', col='red')
lines(y=austria_test$gni, x=austria_test$Year, type='b')
polygon(c(austria_test$Year, rev(austria_test$Year)), 
        c(forecast_austria_nnar$upper, rev(forecast_austria_nnar$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Austria",
    y_test = austria_test$gni,
    y_hat = as.numeric(forecast_austria_nnar$mean),
    pi_lo = as.numeric(forecast_austria_nnar$lower[,1]),
    pi_hi = as.numeric(forecast_austria_nnar$upper[,1])
  )
)


#Belgium----
belgium_train <- Belgium[1:50,]
belgium_test <- Belgium[51:nrow(Belgium),]

belgium_nnar <- nnetar(belgium_train$gni, p=4, size=5)

belgium_val_errors <- validation_errors(belgium_train, 4, 5)
belgium_innov <- matrix(rnorm(nrow(belgium_test)*1000, mean=0, 
                              sd=sd(belgium_val_errors, na.rm=TRUE)), 
                        nrow=nrow(belgium_test), ncol=1000)


forecast_belgium_nnar <- forecast(belgium_nnar, h=nrow(belgium_test), PI=TRUE, 
                                  level=c(95), npaths=1000, innov=belgium_innov)

plot(y=forecast_belgium_nnar$mean, x=belgium_test$Year, type='b',
     ylim=c(min(forecast_belgium_nnar$lower), max(forecast_belgium_nnar$upper)),
     main='belgium', xlab='Year', ylab='gni', col='red')
lines(y=belgium_test$gni, x=belgium_test$Year, type='b')
polygon(c(belgium_test$Year, rev(belgium_test$Year)), 
        c(forecast_belgium_nnar$upper, rev(forecast_belgium_nnar$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Belgium",
    y_test = belgium_test$gni,
    y_hat = as.numeric(forecast_belgium_nnar$mean),
    pi_lo = as.numeric(forecast_belgium_nnar$lower[,1]),
    pi_hi = as.numeric(forecast_belgium_nnar$upper[,1])
  )
)

#Denmark----
denmark_train <- Denmark[1:50,]
denmark_test <- Denmark[51:nrow(Denmark),]

denmark_nnar <- nnetar(denmark_train$gni, p=3, size=2)

denmark_val_errors <- validation_errors(denmark_train, 3, 2)
denmark_innov <- matrix(rnorm(nrow(denmark_test)*1000, mean=0, 
                              sd=sd(denmark_val_errors, na.rm=TRUE)), 
                        nrow=nrow(denmark_test), ncol=1000)


forecast_denmark_nnar <- forecast(denmark_nnar, h=nrow(denmark_test), PI=TRUE, 
                                  level=c(95), npaths=1000, innov=denmark_innov)

plot(y=forecast_denmark_nnar$mean, x=denmark_test$Year, type='b',
     ylim=c(min(forecast_denmark_nnar$lower), max(forecast_denmark_nnar$upper)),
     main='denmark', xlab='Year', ylab='gni', col='red')
lines(y=denmark_test$gni, x=denmark_test$Year, type='b')
polygon(c(denmark_test$Year, rev(denmark_test$Year)), 
        c(forecast_denmark_nnar$upper, rev(forecast_denmark_nnar$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Denmark",
    y_test = denmark_test$gni,
    y_hat = as.numeric(forecast_denmark_nnar$mean),
    pi_lo = as.numeric(forecast_denmark_nnar$lower[,1]),
    pi_hi = as.numeric(forecast_denmark_nnar$upper[,1])
  )
)

#Finland----
finland_train <- Finland[1:50,]
finland_test <- Finland[51:nrow(Finland),]

finland_nnar <- nnetar(finland_train$gni, p=5, size=5)

finland_val_errors <- validation_errors(finland_train, 5, 5)
finland_innov <- matrix(rnorm(nrow(finland_test)*1000, mean=0, 
                              sd=sd(finland_val_errors, na.rm=TRUE)), 
                        nrow=nrow(finland_test), ncol=1000)


forecast_finland_nnar <- forecast(finland_nnar, h=nrow(finland_test), PI=TRUE, 
                                  level=c(95), npaths=1000, innov=finland_innov)

plot(y=forecast_finland_nnar$mean, x=finland_test$Year, type='b',
     ylim=c(min(forecast_finland_nnar$lower), max(forecast_finland_nnar$upper)),
     main='finland', xlab='Year', ylab='gni', col='red')
lines(y=finland_test$gni, x=finland_test$Year, type='b')
polygon(c(finland_test$Year, rev(finland_test$Year)), 
        c(forecast_finland_nnar$upper, rev(forecast_finland_nnar$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Finland",
    y_test = finland_test$gni,
    y_hat = as.numeric(forecast_finland_nnar$mean),
    pi_lo = as.numeric(forecast_finland_nnar$lower[,1]),
    pi_hi = as.numeric(forecast_finland_nnar$upper[,1])
  )
)


#France----
france_train <- France[1:50,]
france_test <- France[51:nrow(France),]

france_nnar <- nnetar(france_train$gni, p=4, size=4)

france_val_errors <- validation_errors(france_train, 4, 4)
france_innov <- matrix(rnorm(nrow(france_test)*1000, mean=0, 
                             sd=sd(france_val_errors, na.rm=TRUE)), 
                       nrow=nrow(france_test), ncol=1000)


forecast_france_nnar <- forecast(france_nnar, h=nrow(france_test), PI=TRUE, 
                                 level=c(95), npaths=1000, innov=france_innov)

plot(y=forecast_france_nnar$mean, x=france_test$Year, type='b',
     ylim=c(min(forecast_france_nnar$lower), max(forecast_france_nnar$upper)),
     main='france', xlab='Year', ylab='gni', col='red')
lines(y=france_test$gni, x=france_test$Year, type='b')
polygon(c(france_test$Year, rev(france_test$Year)), 
        c(forecast_france_nnar$upper, rev(forecast_france_nnar$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "France",
    y_test = france_test$gni,
    y_hat = as.numeric(forecast_france_nnar$mean),
    pi_lo = as.numeric(forecast_france_nnar$lower[,1]),
    pi_hi = as.numeric(forecast_france_nnar$upper[,1])
  )
)


#Germany----
germany_train <- Germany[1:50,]
germany_test <- Germany[51:nrow(Germany),]

germany_nnar <- nnetar(germany_train$gni, p=4, size=5)

germany_val_errors <- validation_errors(germany_train, 4, 5)
germany_innov <- matrix(rnorm(nrow(germany_test)*1000, mean=0, 
                              sd=sd(germany_val_errors, na.rm=TRUE)), 
                        nrow=nrow(germany_test), ncol=1000)


forecast_germany_nnar <- forecast(germany_nnar, h=nrow(germany_test), PI=TRUE, 
                                  level=c(95), npaths=1000, innov=germany_innov)

plot(y=forecast_germany_nnar$mean, x=germany_test$Year, type='b',
     ylim=c(min(forecast_germany_nnar$lower), max(forecast_germany_nnar$upper)),
     main='germany', xlab='Year', ylab='gni', col='red')
lines(y=germany_test$gni, x=germany_test$Year, type='b')
polygon(c(germany_test$Year, rev(germany_test$Year)), 
        c(forecast_germany_nnar$upper, rev(forecast_germany_nnar$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Germany",
    y_test = germany_test$gni,
    y_hat = as.numeric(forecast_germany_nnar$mean),
    pi_lo = as.numeric(forecast_germany_nnar$lower[,1]),
    pi_hi = as.numeric(forecast_germany_nnar$upper[,1])
  )
)


#Ireland----
ireland_train <- Ireland[1:50,]
ireland_test <- Ireland[51:nrow(Ireland),]

ireland_nnar <- nnetar(ireland_train$gni, p=5, size=1)

ireland_val_errors <- validation_errors(ireland_train, 2, 2)
ireland_innov <- matrix(rnorm(nrow(ireland_test)*1000, mean=0, 
                              sd=sd(ireland_val_errors, na.rm=TRUE)), 
                        nrow=nrow(ireland_test), ncol=1000)


forecast_ireland_nnar <- forecast(ireland_nnar, h=nrow(ireland_test), PI=TRUE, 
                                  level=c(95), npaths=1000, innov=ireland_innov)

plot(y=forecast_ireland_nnar$mean, x=ireland_test$Year, type='b',
     ylim=c(min(forecast_ireland_nnar$lower), max(ireland_test$gni)),
     main='ireland', xlab='Year', ylab='gni', col='red')
lines(y=ireland_test$gni, x=ireland_test$Year, type='b')
polygon(c(ireland_test$Year, rev(ireland_test$Year)), 
        c(forecast_ireland_nnar$upper, rev(forecast_ireland_nnar$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Ireland",
    y_test = ireland_test$gni,
    y_hat = as.numeric(forecast_ireland_nnar$mean),
    pi_lo = as.numeric(forecast_ireland_nnar$lower[,1]),
    pi_hi = as.numeric(forecast_ireland_nnar$upper[,1])
  )
)


#Italy----
italy_train <- Italy[1:50,]
italy_test <- Italy[51:nrow(Italy),]

italy_nnar <- nnetar(italy_train$gni, p=2, size=5)

italy_val_errors <- validation_errors(italy_train, 2, 5)
italy_innov <- matrix(rnorm(nrow(italy_test)*1000, mean=0, 
                            sd=sd(italy_val_errors, na.rm=TRUE)), 
                      nrow=nrow(italy_test), ncol=1000)


forecast_italy_nnar <- forecast(italy_nnar, h=nrow(italy_test), PI=TRUE, 
                                level=c(95), npaths=1000, innov=italy_innov)

plot(y=forecast_italy_nnar$mean, x=italy_test$Year, type='b',
     ylim=c(min(forecast_italy_nnar$lower), max(forecast_italy_nnar$upper)),
     main='italy', xlab='Year', ylab='gni', col='red')
lines(y=italy_test$gni, x=italy_test$Year, type='b')
polygon(c(italy_test$Year, rev(italy_test$Year)), 
        c(forecast_italy_nnar$upper, rev(forecast_italy_nnar$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Italy",
    y_test = italy_test$gni,
    y_hat = as.numeric(forecast_italy_nnar$mean),
    pi_lo = as.numeric(forecast_italy_nnar$lower[,1]),
    pi_hi = as.numeric(forecast_italy_nnar$upper[,1])
  )
)


#Luxembourg----
luxembourg_train <- Luxembourg[1:50,]
luxembourg_test <- Luxembourg[51:nrow(Luxembourg),]

luxembourg_nnar <- nnetar(luxembourg_train$gni, p=4, size=4)

luxembourg_val_errors <- validation_errors(luxembourg_train, 4, 4)
luxembourg_innov <- matrix(rnorm(nrow(luxembourg_test)*1000, mean=0, 
                                 sd=sd(luxembourg_val_errors, na.rm=TRUE)), 
                           nrow=nrow(luxembourg_test), ncol=1000)


forecast_luxembourg_nnar <- forecast(luxembourg_nnar, h=nrow(luxembourg_test), PI=TRUE, 
                                     level=c(95), npaths=1000, innov=luxembourg_innov)

plot(y=forecast_luxembourg_nnar$mean, x=luxembourg_test$Year, type='b',
     ylim=c(min(forecast_luxembourg_nnar$lower), max(forecast_luxembourg_nnar$upper)),
     main='luxembourg', xlab='Year', ylab='gni', col='red')
lines(y=luxembourg_test$gni, x=luxembourg_test$Year, type='b')
polygon(c(luxembourg_test$Year, rev(luxembourg_test$Year)), 
        c(forecast_luxembourg_nnar$upper, rev(forecast_luxembourg_nnar$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Luxembourg",
    y_test = luxembourg_test$gni,
    y_hat = as.numeric(forecast_luxembourg_nnar$mean),
    pi_lo = as.numeric(forecast_luxembourg_nnar$lower[,1]),
    pi_hi = as.numeric(forecast_luxembourg_nnar$upper[,1])
  )
)



#Netherlands----
netherlands_train <- Netherlands[1:50,]
netherlands_test <- Netherlands[51:nrow(Netherlands),]

netherlands_nnar <- nnetar(netherlands_train$gni, p=4, size=5)

netherlands_val_errors <- validation_errors(netherlands_train, 4, 5)
netherlands_innov <- matrix(rnorm(nrow(netherlands_test)*1000, mean=0, 
                                  sd=sd(netherlands_val_errors, na.rm=TRUE)), 
                            nrow=nrow(netherlands_test), ncol=1000)


forecast_netherlands_nnar <- forecast(netherlands_nnar, h=nrow(netherlands_test), PI=TRUE, 
                                      level=c(95), npaths=1000, innov=netherlands_innov)

plot(y=forecast_netherlands_nnar$mean, x=netherlands_test$Year, type='b',
     ylim=c(min(forecast_netherlands_nnar$lower), max(forecast_netherlands_nnar$upper)),
     main='netherlands', xlab='Year', ylab='gni', col='red')
lines(y=netherlands_test$gni, x=netherlands_test$Year, type='b')
polygon(c(netherlands_test$Year, rev(netherlands_test$Year)), 
        c(forecast_netherlands_nnar$upper, rev(forecast_netherlands_nnar$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Netherlands",
    y_test = netherlands_test$gni,
    y_hat = as.numeric(forecast_netherlands_nnar$mean),
    pi_lo = as.numeric(forecast_netherlands_nnar$lower[,1]),
    pi_hi = as.numeric(forecast_netherlands_nnar$upper[,1])
  )
)

#Portugal----
portugal_train <- Portugal[1:50,]
portugal_test <- Portugal[51:nrow(Portugal),]

portugal_nnar <- nnetar(portugal_train$gni, p=4, size=1)

portugal_val_errors <- validation_errors(portugal_train, 4, 1)
portugal_innov <- matrix(rnorm(nrow(portugal_test)*1000, mean=0, 
                               sd=sd(portugal_val_errors, na.rm=TRUE)), 
                         nrow=nrow(portugal_test), ncol=1000)


forecast_portugal_nnar <- forecast(portugal_nnar, h=nrow(portugal_test), PI=TRUE, 
                                   level=c(95), npaths=1000, innov=portugal_innov)

plot(y=forecast_portugal_nnar$mean, x=portugal_test$Year, type='b',
     ylim=c(min(forecast_portugal_nnar$lower), max(forecast_portugal_nnar$upper)),
     main='portugal', xlab='Year', ylab='gni', col='red')
lines(y=portugal_test$gni, x=portugal_test$Year, type='b')
polygon(c(portugal_test$Year, rev(portugal_test$Year)), 
        c(forecast_portugal_nnar$upper, rev(forecast_portugal_nnar$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Portugal",
    y_test = portugal_test$gni,
    y_hat = as.numeric(forecast_portugal_nnar$mean),
    pi_lo = as.numeric(forecast_portugal_nnar$lower[,1]),
    pi_hi = as.numeric(forecast_portugal_nnar$upper[,1])
  )
)

#Spain----
spain_train <- Spain[1:50,]
spain_test <- Spain[51:nrow(Spain),]

spain_nnar <- nnetar(spain_train$gni, p=5, size=5)

spain_val_errors <- validation_errors(spain_train, 5, 5)
spain_innov <- matrix(rnorm(nrow(spain_test)*1000, mean=0, 
                            sd=sd(spain_val_errors, na.rm=TRUE)), 
                      nrow=nrow(spain_test), ncol=1000)


forecast_spain_nnar <- forecast(spain_nnar, h=nrow(spain_test), PI=TRUE, 
                                level=c(95), npaths=1000, innov=spain_innov)

plot(y=forecast_spain_nnar$mean, x=spain_test$Year, type='b',
     ylim=c(min(forecast_spain_nnar$lower), max(forecast_spain_nnar$upper)),
     main='spain', xlab='Year', ylab='gni', col='red')
lines(y=spain_test$gni, x=spain_test$Year, type='b')
polygon(c(spain_test$Year, rev(spain_test$Year)), 
        c(forecast_spain_nnar$upper, rev(forecast_spain_nnar$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Spain",
    y_test = spain_test$gni,
    y_hat = as.numeric(forecast_spain_nnar$mean),
    pi_lo = as.numeric(forecast_spain_nnar$lower[,1]),
    pi_hi = as.numeric(forecast_spain_nnar$upper[,1])
  )
)

#Sweden----
sweden_train <- Sweden[1:50,]
sweden_test <- Sweden[51:nrow(Sweden),]

sweden_nnar <- nnetar(sweden_train$gni, p=2, size=5)

sweden_val_errors <- validation_errors(sweden_train, 2, 5)
sweden_innov <- matrix(rnorm(nrow(sweden_test)*1000, mean=0, 
                             sd=sd(sweden_val_errors, na.rm=TRUE)), 
                       nrow=nrow(sweden_test), ncol=1000)


forecast_sweden_nnar <- forecast(sweden_nnar, h=nrow(sweden_test), PI=TRUE, 
                                 level=c(95), npaths=1000, innov=sweden_innov)

plot(y=forecast_sweden_nnar$mean, x=sweden_test$Year, type='b',
     ylim=c(min(forecast_sweden_nnar$lower), max(forecast_sweden_nnar$upper)),
     main='sweden', xlab='Year', ylab='gni', col='red')
lines(y=sweden_test$gni, x=sweden_test$Year, type='b')
polygon(c(sweden_test$Year, rev(sweden_test$Year)), 
        c(forecast_sweden_nnar$upper, rev(forecast_sweden_nnar$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Sweden",
    y_test = sweden_test$gni,
    y_hat = as.numeric(forecast_sweden_nnar$mean),
    pi_lo = as.numeric(forecast_sweden_nnar$lower[,1]),
    pi_hi = as.numeric(forecast_sweden_nnar$upper[,1])
  )
)

#analysing PIs----
median(results_df$coverage)
median(results_df$relative_width_h1)
median(results_df$relative_width_h5)
median(results_df$relative_width_last)
