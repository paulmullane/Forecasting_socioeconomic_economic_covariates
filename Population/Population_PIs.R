#packages----
library(readxl)
library(lmtest)
library(forecast)
library(Metrics)
library(mgcv)
library(DiceKriging)
options(scipen=999)

validation_errors <- function(train_df, p_optimal, size_optimal){
  stopifnot("Population" %in% names(train_df))
  
  y <- train_df$Population
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
Austria <- read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Austria.xlsx")[,c('Year', 'Population')]
Belgium <- read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Belgium.xlsx")[,c('Year', 'Population')]
Denmark<- read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Denmark.xlsx")[,c('Year', 'Population')]
Finland <- read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Finland.xlsx")[,c('Year', 'Population')]
France <- read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/France.xlsx")[,c('Year', 'Population')]
Germany <- read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Germany.xlsx")[,c('Year', 'Population')]
Ireland <- read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Ireland.xlsx")[,c('Year', 'Population')]
Italy <- read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Italy.xlsx")[,c('Year', 'Population')]
Luxembourg <- read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Luxembourg.xlsx")[,c('Year', 'Population')]
Netherlands <- read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Netherlands.xlsx")[,c('Year', 'Population')]
Portugal <- read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Portugal.xlsx")[,c('Year', 'Population')]
Spain <- read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Spain.xlsx")[,c('Year', 'Population')]
Sweden <- read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Sweden.xlsx")[,c('Year', 'Population')]

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

austria_nnar <- nnetar(austria_train$Population, p=1, size=1)

austria_val_errors <- validation_errors(austria_train, 1, 1)
austria_innov <- matrix(rnorm(nrow(austria_test)*1000, mean=0, 
                      sd=sd(austria_val_errors, na.rm=TRUE)), 
                nrow=nrow(austria_test), ncol=1000)


forecast_austria_nnar <- forecast(austria_nnar, h=nrow(austria_test), PI=TRUE, 
                                  level=c(95), npaths=1000, innov=austria_innov)

plot(y=forecast_austria_nnar$mean, x=austria_test$Year, type='b',
     ylim=c(min(forecast_austria_nnar$lower), max(austria_test$Population)),
     main='Austria', xlab='Year', ylab='Population', col='red')
lines(y=austria_test$Population, x=austria_test$Year, type='b')
polygon(c(austria_test$Year, rev(austria_test$Year)), 
        c(forecast_austria_nnar$upper, rev(forecast_austria_nnar$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Austria",
    y_test = austria_test$Population,
    y_hat = as.numeric(forecast_austria_nnar$mean),
    pi_lo = as.numeric(forecast_austria_nnar$lower[,1]),
    pi_hi = as.numeric(forecast_austria_nnar$upper[,1])
  )
)


#Belgium----
belgium_train <- Belgium[1:52,]
belgium_test <- Belgium[53:nrow(Belgium),]

belgium_nnar <- nnetar(belgium_train$Population, p=1, size=1)

belgium_val_errors <- validation_errors(belgium_train, 1, 1)
belgium_innov <- matrix(rnorm(nrow(belgium_test)*1000, mean=0, 
                              sd=sd(belgium_val_errors, na.rm=TRUE)), 
                        nrow=nrow(belgium_test), ncol=1000)


forecast_belgium_nnar <- forecast(belgium_nnar, h=nrow(belgium_test), PI=TRUE, 
                                  level=c(95), npaths=1000, innov=belgium_innov)

plot(y=forecast_belgium_nnar$mean, x=belgium_test$Year, type='b',
     ylim=c(min(forecast_belgium_nnar$lower), max(forecast_belgium_nnar$upper)),
     main='belgium', xlab='Year', ylab='Population', col='red')
lines(y=belgium_test$Population, x=belgium_test$Year, type='b')
polygon(c(belgium_test$Year, rev(belgium_test$Year)), 
        c(forecast_belgium_nnar$upper, rev(forecast_belgium_nnar$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Belgium",
    y_test = belgium_test$Population,
    y_hat = as.numeric(forecast_belgium_nnar$mean),
    pi_lo = as.numeric(forecast_belgium_nnar$lower[,1]),
    pi_hi = as.numeric(forecast_belgium_nnar$upper[,1])
  )
)



#Denmark----
denmark_train <- Denmark[1:52,]
denmark_test <- Denmark[53:nrow(Denmark),]

denmark_nnar <- nnetar(denmark_train$Population, p=1, size=2)

denmark_val_errors <- validation_errors(denmark_train, 1, 2)
denmark_innov <- matrix(rnorm(nrow(denmark_test)*1000, mean=0, 
                              sd=sd(denmark_val_errors, na.rm=TRUE)), 
                        nrow=nrow(denmark_test), ncol=1000)


forecast_denmark_nnar <- forecast(denmark_nnar, h=nrow(denmark_test), PI=TRUE, 
                                  level=c(95), npaths=1000, innov=denmark_innov)

plot(y=forecast_denmark_nnar$mean, x=denmark_test$Year, type='b',
     ylim=c(min(forecast_denmark_nnar$lower), max(denmark_test$Population)),
     main='denmark', xlab='Year', ylab='Population', col='red')
lines(y=denmark_test$Population, x=denmark_test$Year, type='b')
polygon(c(denmark_test$Year, rev(denmark_test$Year)), 
        c(forecast_denmark_nnar$upper, rev(forecast_denmark_nnar$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Denmark",
    y_test = denmark_test$Population,
    y_hat = as.numeric(forecast_denmark_nnar$mean),
    pi_lo = as.numeric(forecast_denmark_nnar$lower[,1]),
    pi_hi = as.numeric(forecast_denmark_nnar$upper[,1])
  )
)


#Finland----
finland_train <- Finland[1:52,]
finland_test <- Finland[53:nrow(Finland),]

finland_nnar <- nnetar(finland_train$Population, p=5, size=2)

finland_val_errors <- validation_errors(finland_train, 5, 2)
finland_innov <- matrix(rnorm(nrow(finland_test)*1000, mean=0, 
                              sd=sd(finland_val_errors, na.rm=TRUE)), 
                        nrow=nrow(finland_test), ncol=1000)


forecast_finland_nnar <- forecast(finland_nnar, h=nrow(finland_test), PI=TRUE, 
                                  level=c(95), npaths=1000, innov=finland_innov)

plot(y=forecast_finland_nnar$mean, x=finland_test$Year, type='b',
     ylim=c(min(forecast_finland_nnar$lower), max(forecast_finland_nnar$upper)),
     main='finland', xlab='Year', ylab='Population', col='red')
lines(y=finland_test$Population, x=finland_test$Year, type='b')
polygon(c(finland_test$Year, rev(finland_test$Year)), 
        c(forecast_finland_nnar$upper, rev(forecast_finland_nnar$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Finland",
    y_test = finland_test$Population,
    y_hat = as.numeric(forecast_finland_nnar$mean),
    pi_lo = as.numeric(forecast_finland_nnar$lower[,1]),
    pi_hi = as.numeric(forecast_finland_nnar$upper[,1])
  )
)


#France----
france_train <- France[1:52,]
france_test <- France[53:nrow(France),]

france_nnar <- nnetar(france_train$Population, p=1, size=5)

france_val_errors <- validation_errors(france_train, 1, 5)
france_innov <- matrix(rnorm(nrow(france_test)*1000, mean=0, 
                              sd=sd(france_val_errors, na.rm=TRUE)), 
                        nrow=nrow(france_test), ncol=1000)


forecast_france_nnar <- forecast(france_nnar, h=nrow(france_test), PI=TRUE, 
                                  level=c(95), npaths=1000, innov=france_innov)

plot(y=forecast_france_nnar$mean, x=france_test$Year, type='b',
     ylim=c(min(forecast_france_nnar$lower), max(france_test$Population)),
     main='france', xlab='Year', ylab='Population', col='red')
lines(y=france_test$Population, x=france_test$Year, type='b')
polygon(c(france_test$Year, rev(france_test$Year)), 
        c(forecast_france_nnar$upper, rev(forecast_france_nnar$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "France",
    y_test = france_test$Population,
    y_hat = as.numeric(forecast_france_nnar$mean),
    pi_lo = as.numeric(forecast_france_nnar$lower[,1]),
    pi_hi = as.numeric(forecast_france_nnar$upper[,1])
  )
)


#Germany----
germany_train <- Germany[1:52,]
germany_test <- Germany[53:nrow(Germany),]

germany_nnar <- nnetar(germany_train$Population, p=2, size=5)

germany_val_errors <- validation_errors(germany_train, 2, 5)
germany_innov <- matrix(rnorm(nrow(germany_test)*1000, mean=0, 
                             sd=sd(germany_val_errors, na.rm=TRUE)), 
                       nrow=nrow(germany_test), ncol=1000)


forecast_germany_nnar <- forecast(germany_nnar, h=nrow(germany_test), PI=TRUE, 
                                 level=c(95), npaths=1000, innov=germany_innov)

plot(y=forecast_germany_nnar$mean, x=germany_test$Year, type='b',
     ylim=c(min(forecast_germany_nnar$lower), max(germany_test$Population)),
     main='germany', xlab='Year', ylab='Population', col='red')
lines(y=germany_test$Population, x=germany_test$Year, type='b')
polygon(c(germany_test$Year, rev(germany_test$Year)), 
        c(forecast_germany_nnar$upper, rev(forecast_germany_nnar$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Germany",
    y_test = germany_test$Population,
    y_hat = as.numeric(forecast_germany_nnar$mean),
    pi_lo = as.numeric(forecast_germany_nnar$lower[,1]),
    pi_hi = as.numeric(forecast_germany_nnar$upper[,1])
  )
)


#Ireland----
ireland_train <- Ireland[1:52,]
ireland_test <- Ireland[53:nrow(Ireland),]

ireland_nnar <- nnetar(ireland_train$Population, p=2, size=2)

ireland_val_errors <- validation_errors(ireland_train, 2, 2)
ireland_innov <- matrix(rnorm(nrow(ireland_test)*1000, mean=0, 
                              sd=sd(ireland_val_errors, na.rm=TRUE)), 
                        nrow=nrow(ireland_test), ncol=1000)


forecast_ireland_nnar <- forecast(ireland_nnar, h=nrow(ireland_test), PI=TRUE, 
                                  level=c(95), npaths=1000, innov=ireland_innov)

plot(y=forecast_ireland_nnar$mean, x=ireland_test$Year, type='b',
     ylim=c(min(forecast_ireland_nnar$lower), max(ireland_test$Population)),
     main='ireland', xlab='Year', ylab='Population', col='red')
lines(y=ireland_test$Population, x=ireland_test$Year, type='b')
polygon(c(ireland_test$Year, rev(ireland_test$Year)), 
        c(forecast_ireland_nnar$upper, rev(forecast_ireland_nnar$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Ireland",
    y_test = ireland_test$Population,
    y_hat = as.numeric(forecast_ireland_nnar$mean),
    pi_lo = as.numeric(forecast_ireland_nnar$lower[,1]),
    pi_hi = as.numeric(forecast_ireland_nnar$upper[,1])
  )
)


#Italy----
italy_train <- Italy[1:52,]
italy_test <- Italy[53:nrow(Italy),]

italy_nnar <- nnetar(italy_train$Population, p=3, size=5)

italy_val_errors <- validation_errors(italy_train, 3, 5)
italy_innov <- matrix(rnorm(nrow(italy_test)*1000, mean=0, 
                              sd=sd(italy_val_errors, na.rm=TRUE)), 
                        nrow=nrow(italy_test), ncol=1000)


forecast_italy_nnar <- forecast(italy_nnar, h=nrow(italy_test), PI=TRUE, 
                                  level=c(95), npaths=1000, innov=italy_innov)

plot(y=forecast_italy_nnar$mean, x=italy_test$Year, type='b',
     ylim=c(min(forecast_italy_nnar$lower), max(forecast_italy_nnar$upper)),
     main='italy', xlab='Year', ylab='Population', col='red')
lines(y=italy_test$Population, x=italy_test$Year, type='b')
polygon(c(italy_test$Year, rev(italy_test$Year)), 
        c(forecast_italy_nnar$upper, rev(forecast_italy_nnar$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Italy",
    y_test = italy_test$Population,
    y_hat = as.numeric(forecast_italy_nnar$mean),
    pi_lo = as.numeric(forecast_italy_nnar$lower[,1]),
    pi_hi = as.numeric(forecast_italy_nnar$upper[,1])
  )
)


#Luxembourg----
luxembourg_train <- Luxembourg[1:52,]
luxembourg_test <- Luxembourg[53:nrow(Luxembourg),]

luxembourg_nnar <- nnetar(luxembourg_train$Population, p=3, size=3)

luxembourg_val_errors <- validation_errors(luxembourg_train, 3, 3)
luxembourg_innov <- matrix(rnorm(nrow(luxembourg_test)*1000, mean=0, 
                            sd=sd(luxembourg_val_errors, na.rm=TRUE)), 
                      nrow=nrow(luxembourg_test), ncol=1000)


forecast_luxembourg_nnar <- forecast(luxembourg_nnar, h=nrow(luxembourg_test), PI=TRUE, 
                                level=c(95), npaths=1000, innov=luxembourg_innov)

plot(y=forecast_luxembourg_nnar$mean, x=luxembourg_test$Year, type='b',
     ylim=c(min(forecast_luxembourg_nnar$lower), max(luxembourg_test$Population)),
     main='luxembourg', xlab='Year', ylab='Population', col='red')
lines(y=luxembourg_test$Population, x=luxembourg_test$Year, type='b')
polygon(c(luxembourg_test$Year, rev(luxembourg_test$Year)), 
        c(forecast_luxembourg_nnar$upper, rev(forecast_luxembourg_nnar$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Luxembourg",
    y_test = luxembourg_test$Population,
    y_hat = as.numeric(forecast_luxembourg_nnar$mean),
    pi_lo = as.numeric(forecast_luxembourg_nnar$lower[,1]),
    pi_hi = as.numeric(forecast_luxembourg_nnar$upper[,1])
  )
)



#Netherlands----
netherlands_train <- Netherlands[1:52,]
netherlands_test <- Netherlands[53:nrow(Netherlands),]

netherlands_nnar <- nnetar(netherlands_train$Population, p=2, size=5)

netherlands_val_errors <- validation_errors(netherlands_train, 2, 5)
netherlands_innov <- matrix(rnorm(nrow(netherlands_test)*1000, mean=0, 
                                 sd=sd(netherlands_val_errors, na.rm=TRUE)), 
                           nrow=nrow(netherlands_test), ncol=1000)


forecast_netherlands_nnar <- forecast(netherlands_nnar, h=nrow(netherlands_test), PI=TRUE, 
                                     level=c(95), npaths=1000, innov=netherlands_innov)

plot(y=forecast_netherlands_nnar$mean, x=netherlands_test$Year, type='b',
     ylim=c(min(forecast_netherlands_nnar$lower), max(forecast_netherlands_nnar$upper)),
     main='netherlands', xlab='Year', ylab='Population', col='red')
lines(y=netherlands_test$Population, x=netherlands_test$Year, type='b')
polygon(c(netherlands_test$Year, rev(netherlands_test$Year)), 
        c(forecast_netherlands_nnar$upper, rev(forecast_netherlands_nnar$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Netherlands",
    y_test = netherlands_test$Population,
    y_hat = as.numeric(forecast_netherlands_nnar$mean),
    pi_lo = as.numeric(forecast_netherlands_nnar$lower[,1]),
    pi_hi = as.numeric(forecast_netherlands_nnar$upper[,1])
  )
)

#Portugal----
portugal_train <- Portugal[1:52,]
portugal_test <- Portugal[53:nrow(Portugal),]

portugal_nnar <- nnetar(portugal_train$Population, p=5, size=1)

portugal_val_errors <- validation_errors(portugal_train, 5, 1)
portugal_innov <- matrix(rnorm(nrow(portugal_test)*1000, mean=0, 
                                  sd=sd(portugal_val_errors, na.rm=TRUE)), 
                            nrow=nrow(portugal_test), ncol=1000)


forecast_portugal_nnar <- forecast(portugal_nnar, h=nrow(portugal_test), PI=TRUE, 
                                      level=c(95), npaths=1000, innov=portugal_innov)

plot(y=forecast_portugal_nnar$mean, x=portugal_test$Year, type='b',
     ylim=c(min(forecast_portugal_nnar$lower), max(forecast_portugal_nnar$upper)),
     main='portugal', xlab='Year', ylab='Population', col='red')
lines(y=portugal_test$Population, x=portugal_test$Year, type='b')
polygon(c(portugal_test$Year, rev(portugal_test$Year)), 
        c(forecast_portugal_nnar$upper, rev(forecast_portugal_nnar$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Portugal",
    y_test = portugal_test$Population,
    y_hat = as.numeric(forecast_portugal_nnar$mean),
    pi_lo = as.numeric(forecast_portugal_nnar$lower[,1]),
    pi_hi = as.numeric(forecast_portugal_nnar$upper[,1])
  )
)

#Spain----
spain_train <- Spain[1:52,]
spain_test <- Spain[53:nrow(Spain),]

spain_nnar <- nnetar(spain_train$Population, p=1, size=4)

spain_val_errors <- validation_errors(spain_train, 1, 4)
spain_innov <- matrix(rnorm(nrow(spain_test)*1000, mean=0, 
                               sd=sd(spain_val_errors, na.rm=TRUE)), 
                         nrow=nrow(spain_test), ncol=1000)


forecast_spain_nnar <- forecast(spain_nnar, h=nrow(spain_test), PI=TRUE, 
                                   level=c(95), npaths=1000, innov=spain_innov)

plot(y=forecast_spain_nnar$mean, x=spain_test$Year, type='b',
     ylim=c(min(forecast_spain_nnar$lower), max(forecast_spain_nnar$upper)),
     main='spain', xlab='Year', ylab='Population', col='red')
lines(y=spain_test$Population, x=spain_test$Year, type='b')
polygon(c(spain_test$Year, rev(spain_test$Year)), 
        c(forecast_spain_nnar$upper, rev(forecast_spain_nnar$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Spain",
    y_test = spain_test$Population,
    y_hat = as.numeric(forecast_spain_nnar$mean),
    pi_lo = as.numeric(forecast_spain_nnar$lower[,1]),
    pi_hi = as.numeric(forecast_spain_nnar$upper[,1])
  )
)

#Sweden----
sweden_train <- Sweden[1:52,]
sweden_test <- Sweden[53:nrow(Sweden),]

sweden_nnar <- nnetar(sweden_train$Population, p=1, size=1)

sweden_val_errors <- validation_errors(sweden_train, 1, 1)
sweden_innov <- matrix(rnorm(nrow(sweden_test)*1000, mean=0, 
                            sd=sd(sweden_val_errors, na.rm=TRUE)), 
                      nrow=nrow(sweden_test), ncol=1000)


forecast_sweden_nnar <- forecast(sweden_nnar, h=nrow(sweden_test), PI=TRUE, 
                                level=c(95), npaths=1000, innov=sweden_innov)

plot(y=forecast_sweden_nnar$mean, x=sweden_test$Year, type='b',
     ylim=c(min(forecast_sweden_nnar$lower), max(forecast_sweden_nnar$upper)),
     main='sweden', xlab='Year', ylab='Population', col='red')
lines(y=sweden_test$Population, x=sweden_test$Year, type='b')
polygon(c(sweden_test$Year, rev(sweden_test$Year)), 
        c(forecast_sweden_nnar$upper, rev(forecast_sweden_nnar$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Sweden",
    y_test = sweden_test$Population,
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
