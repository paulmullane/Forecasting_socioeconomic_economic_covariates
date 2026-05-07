#packages----
library(readxl)
library(forecast)
options(scipen=999)

#theta was found to be the best model for exports. The level parameter in the thetaf
#function specifies the level to be used for prediction intervals.

#reading in the data----
Austria <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Austria.xlsx")[,c('Year', 'exports')])
Belgium <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Belgium.xlsx")[,c('Year', 'exports')])
Denmark<- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Denmark.xlsx")[,c('Year', 'exports')])
Finland <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Finland.xlsx")[,c('Year', 'exports')])
France <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/France.xlsx")[,c('Year', 'exports')])
Germany <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Germany.xlsx")[,c('Year', 'exports')])
Ireland <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Ireland.xlsx")[,c('Year', 'exports')])
Italy <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Italy.xlsx")[,c('Year', 'exports')])
Luxembourg <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Luxembourg.xlsx")[,c('Year', 'exports')])
Netherlands <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Netherlands.xlsx")[,c('Year', 'exports')])
Portugal <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Portugal.xlsx")[,c('Year', 'exports')])
Spain <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Spain.xlsx")[,c('Year', 'exports')])
Sweden <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Sweden.xlsx")[,c('Year', 'exports')])

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

#austria----
austria_train <- Austria[1:42,]
austria_test <- Austria[43:nrow(Austria),]

austria_theta <- thetaf(austria_train$exports, h=nrow(austria_test), level=95)

plot(y=austria_test$exports, x=austria_test$Year, type='b', xlab='Year', ylab='exports', 
     ylim=c(min(austria_theta$lower), max(austria_theta$upper)), main='Austria')
lines(y=austria_theta$mean, x=austria_test$Year, type='b', col='red')
polygon(c(austria_test$Year, rev(austria_test$Year)), 
        c(austria_theta$upper, rev(austria_theta$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Austria",
    y_test = austria_test$exports,
    y_hat = as.numeric(austria_theta$mean),
    pi_lo = as.numeric(austria_theta$lower[,1]),
    pi_hi = as.numeric(austria_theta$upper[,1])
  )
)
#Belgium----
belgium_train <- Belgium[1:42,]
belgium_test <- Belgium[43:nrow(Belgium),]

belgium_theta <- thetaf(belgium_train$exports, h=nrow(belgium_test), level=95)

plot(y=belgium_test$exports, x=belgium_test$Year, type='b', xlab='Year', ylab='exports', 
     ylim=c(min(belgium_theta$lower), max(belgium_theta$upper)), main='Belgium')
lines(y=belgium_theta$mean, x=belgium_test$Year, type='b', col='red')
polygon(c(belgium_test$Year, rev(belgium_test$Year)), 
        c(belgium_theta$upper, rev(belgium_theta$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)


results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Belgium",
    y_test = belgium_test$exports,
    y_hat = as.numeric(belgium_theta$mean),
    pi_lo = as.numeric(belgium_theta$lower[,1]),
    pi_hi = as.numeric(belgium_theta$upper[,1])
  )
)

#Denmark----
denmark_train <- Denmark[1:46,]
denmark_test <- Denmark[47:nrow(Denmark),]

denmark_theta <- thetaf(denmark_train$exports, h=nrow(denmark_test), level=95) 

plot(y=denmark_test$exports, x=denmark_test$Year, type='b', xlab='Year', ylab='exports', 
     ylim=c(min(denmark_theta$lower), max(denmark_theta$upper)), main='denmark')
lines(y=denmark_theta$mean, x=denmark_test$Year, type='b', col='red')
polygon(c(denmark_test$Year, rev(denmark_test$Year)), 
        c(denmark_theta$upper, rev(denmark_theta$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Denmark",
    y_test = denmark_test$exports,
    y_hat = as.numeric(denmark_theta$mean),
    pi_lo = as.numeric(denmark_theta$lower[,1]),
    pi_hi = as.numeric(denmark_theta$upper[,1])
  )
)

#Finland----
finland_train <- Finland[1:42,]
finland_test <- Finland[43:nrow(Finland),]

finland_theta <- thetaf(finland_train$exports, h=nrow(finland_test), level=95) 

plot(y=finland_test$exports, x=finland_test$Year, type='b', xlab='Year', ylab='exports', 
     ylim=c(min(finland_theta$lower), max(finland_theta$upper)), main='finland')
lines(y=finland_theta$mean, x=finland_test$Year, type='b', col='red')
polygon(c(finland_test$Year, rev(finland_test$Year)), 
        c(finland_theta$upper, rev(finland_theta$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Finland",
    y_test = finland_test$exports,
    y_hat = as.numeric(finland_theta$mean),
    pi_lo = as.numeric(finland_theta$lower[,1]),
    pi_hi = as.numeric(finland_theta$upper[,1])
  )
)


#France----
france_train <- France[1:52,]
france_test <- France[53:nrow(France),]

france_theta <- thetaf(france_train$exports, h=nrow(france_test), level=95) 

plot(y=france_test$exports, x=france_test$Year, type='b', xlab='Year', ylab='exports', 
     ylim=c(min(france_theta$lower), max(france_theta$upper)), main='france')
lines(y=france_theta$mean, x=france_test$Year, type='b', col='red')
polygon(c(france_test$Year, rev(france_test$Year)), 
        c(france_theta$upper, rev(france_theta$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "France",
    y_test = france_test$exports,
    y_hat = as.numeric(france_theta$mean),
    pi_lo = as.numeric(france_theta$lower[,1]),
    pi_hi = as.numeric(france_theta$upper[,1])
  )
)

#Germany----
germany_train <- Germany[1:42,]
germany_test <- Germany[43:nrow(Germany),]

germany_theta <- thetaf(germany_train$exports, h=nrow(germany_test), level=95)

plot(y=germany_test$exports, x=germany_test$Year, type='b', xlab='Year', ylab='exports', 
     ylim=c(min(germany_theta$lower), max(germany_theta$upper)), main='germany')
lines(y=germany_theta$mean, x=germany_test$Year, type='b', col='red')
polygon(c(germany_test$Year, rev(germany_test$Year)), 
        c(germany_theta$upper, rev(germany_theta$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Germany",
    y_test = germany_test$exports,
    y_hat = as.numeric(germany_theta$mean),
    pi_lo = as.numeric(germany_theta$lower[,1]),
    pi_hi = as.numeric(germany_theta$upper[,1])
  )
)

#Ireland----
ireland_train <- Ireland[1:42,]
ireland_test <- Ireland[43:nrow(Ireland),]

ireland_theta <- thetaf(ireland_train$exports, h=nrow(ireland_test), level=95)

plot(y=ireland_test$exports, x=ireland_test$Year, type='b', xlab='Year', ylab='exports', 
     ylim=c(min(ireland_theta$lower), max(ireland_test$exports)), main='ireland')
lines(y=ireland_theta$mean, x=ireland_test$Year, type='b', col='red')
polygon(c(ireland_test$Year, rev(ireland_test$Year)), 
        c(ireland_theta$upper, rev(ireland_theta$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Ireland",
    y_test = ireland_test$exports,
    y_hat = as.numeric(ireland_theta$mean),
    pi_lo = as.numeric(ireland_theta$lower[,1]),
    pi_hi = as.numeric(ireland_theta$upper[,1])
  )
)


#Italy----
italy_train <- Italy[1:42,]
italy_test <- Italy[43:nrow(Italy),]

italy_theta <- thetaf(italy_train$exports, h=nrow(italy_test), level=95)

plot(y=italy_test$exports, x=italy_test$Year, type='b', xlab='Year', ylab='exports', 
     ylim=c(min(italy_theta$lower), max(italy_theta$upper)), main='italy')
lines(y=italy_theta$mean, x=italy_test$Year, type='b', col='red')
polygon(c(italy_test$Year, rev(italy_test$Year)), 
        c(italy_theta$upper, rev(italy_theta$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Italy",
    y_test = italy_test$exports,
    y_hat = as.numeric(italy_theta$mean),
    pi_lo = as.numeric(italy_theta$lower[,1]),
    pi_hi = as.numeric(italy_theta$upper[,1])
  )
)

#Luxembourg----
luxembourg_train <- Luxembourg[1:42,]
luxembourg_test <- Luxembourg[43:nrow(Luxembourg),]

luxembourg_theta <- thetaf(luxembourg_train$exports, h=nrow(luxembourg_test), level=95)

plot(y=luxembourg_test$exports, x=luxembourg_test$Year, type='b', xlab='Year', ylab='exports', 
     ylim=c(min(luxembourg_theta$lower), max(luxembourg_theta$upper)), main='luxembourg')
lines(y=luxembourg_theta$mean, x=luxembourg_test$Year, type='b', col='red')
polygon(c(luxembourg_test$Year, rev(luxembourg_test$Year)), 
        c(luxembourg_theta$upper, rev(luxembourg_theta$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Luxembourg",
    y_test = luxembourg_test$exports,
    y_hat = as.numeric(luxembourg_theta$mean),
    pi_lo = as.numeric(luxembourg_theta$lower[,1]),
    pi_hi = as.numeric(luxembourg_theta$upper[,1])
  )
)

#Netherlands----
netherlands_train <- Netherlands[1:43,]
netherlands_test <- Netherlands[44:nrow(Netherlands),]

netherlands_theta <- thetaf(netherlands_train$exports, h=nrow(netherlands_test), level=95)

plot(y=netherlands_test$exports, x=netherlands_test$Year, type='b', xlab='Year', ylab='exports', 
     ylim=c(min(netherlands_theta$lower), max(netherlands_theta$upper)), main='netherlands')
lines(y=netherlands_theta$mean, x=netherlands_test$Year, type='b', col='red')
polygon(c(netherlands_test$Year, rev(netherlands_test$Year)), 
        c(netherlands_theta$upper, rev(netherlands_theta$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Netherlands",
    y_test = netherlands_test$exports,
    y_hat = as.numeric(netherlands_theta$mean),
    pi_lo = as.numeric(netherlands_theta$lower[,1]),
    pi_hi = as.numeric(netherlands_theta$upper[,1])
  )
)

#Portugal----
portugal_train <- Portugal[1:42,]
portugal_test <- Portugal[43:nrow(Portugal),]

portugal_theta <- thetaf(portugal_train$exports, h=nrow(portugal_test), level=95)

plot(y=portugal_test$exports, x=portugal_test$Year, type='b', xlab='Year', ylab='exports', 
     ylim=c(min(portugal_theta$lower), max(portugal_theta$upper)), main='portugal')
lines(y=portugal_theta$mean, x=portugal_test$Year, type='b', col='red')
polygon(c(portugal_test$Year, rev(portugal_test$Year)), 
        c(portugal_theta$upper, rev(portugal_theta$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Portugal",
    y_test = portugal_test$exports,
    y_hat = as.numeric(portugal_theta$mean),
    pi_lo = as.numeric(portugal_theta$lower[,1]),
    pi_hi = as.numeric(portugal_theta$upper[,1])
  )
)

#Spain----
spain_train <- Spain[1:42,]
spain_test <- Spain[43:nrow(Spain),]

spain_theta <- thetaf(spain_train$exports, h=nrow(spain_test), level=95)

plot(y=spain_test$exports, x=spain_test$Year, type='b', xlab='Year', ylab='exports', 
     ylim=c(min(spain_theta$lower), max(spain_theta$upper)), main='spain')
lines(y=spain_theta$mean, x=spain_test$Year, type='b', col='red')
polygon(c(spain_test$Year, rev(spain_test$Year)), 
        c(spain_theta$upper, rev(spain_theta$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Spain",
    y_test = spain_test$exports,
    y_hat = as.numeric(spain_theta$mean),
    pi_lo = as.numeric(spain_theta$lower[,1]),
    pi_hi = as.numeric(spain_theta$upper[,1])
  )
)


#Sweden----
sweden_train <- Sweden[1:52,]
sweden_test <- Sweden[53:nrow(Sweden),]

sweden_theta <- thetaf(sweden_train$exports, h=nrow(sweden_test), level=95) 

plot(y=sweden_test$exports, x=sweden_test$Year, type='b', xlab='Year', ylab='exports', 
     ylim=c(min(sweden_theta$lower), max(sweden_theta$upper)), main='sweden')
lines(y=sweden_theta$mean, x=sweden_test$Year, type='b', col='red')
polygon(c(sweden_test$Year, rev(sweden_test$Year)), 
        c(sweden_theta$upper, rev(sweden_theta$lower)),
        col=rgb(0, 0, 1, 0.2), border=NA)

results_df <- rbind(
  results_df,
  pi_diagnostics(
    country = "Sweden",
    y_test = sweden_test$exports,
    y_hat = as.numeric(sweden_theta$mean),
    pi_lo = as.numeric(sweden_theta$lower[,1]),
    pi_hi = as.numeric(sweden_theta$upper[,1])
  )
)

#analysing PIs----
median(results_df$coverage)
median(results_df$relative_width_h1)
median(results_df$relative_width_h5)
median(results_df$relative_width_last)
