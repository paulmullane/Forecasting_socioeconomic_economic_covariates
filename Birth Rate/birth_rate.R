#packages----
library(readxl)
library(lmtest)
library(forecast)
library(Metrics)
library(mgcv)
library(DiceKriging)
options(scipen=999)

#function for optimising the nnar----
select_nnar <- function(train_series, p_grid=1:5, size_grid=1:5,
                        metric=c("rmse", "mae", "mape"), val_frac=0.20){
  
  metric <- match.arg(metric)
  set.seed(20229798)
  
  train_series <- as.numeric(train_series)
  n <- length(train_series)
  
  #define validation split (last val_frac of train_series)
  n_val <- ceiling(val_frac*n)
  n_core <- n-n_val
  
  if(n_core<=max(p_grid)){
    stop("Not enough observations after validation split for largest p.")
  }
  
  train_core<-train_series[1:n_core]
  val_series<-train_series[(n_core+1):n]
  
  results_list <- list()
  idx <- 1
  
  best_score <- Inf
  best_params <- NULL
  
  for(p in p_grid){
    for(size in size_grid){
      fit <- try(nnetar(train_core, p=p, size=size), silent=TRUE)
      if (inherits(fit, "try-error")) next
      
      # in-sample (training core) fit score
      fitted_vals <- fitted(fit)
      valid <- !is.na(fitted_vals)
      
      train_score <- switch(metric, 
                            rmse=rmse(train_core[valid], fitted_vals[valid]),
                            mae=mae(train_core[valid], fitted_vals[valid]),
                            mape=mape(train_core[valid], fitted_vals[valid])
      )
      
      # validation forecast score
      fc <- forecast(fit, h=length(val_series))
      fc_vals <- as.numeric(fc$mean)
      
      val_score <- switch(metric, rmse=rmse(val_series, fc_vals),
                          mae=mae(val_series, fc_vals), mape=mape(val_series, fc_vals)
      )
      
      results_list[[idx]] <- data.frame(p=p, size=size, train_score=train_score,
                                        val_score=val_score)
      idx <- idx + 1
      
      if(is.finite(val_score) && val_score < best_score){
        best_score <- val_score
        best_params <- list(p = p, size = size)
      }
    }
  }
  
  if(is.null(best_params)){
    stop("No valid NNAR model could be fitted.")
  }
  
  results <- do.call(rbind, results_list)
  results <- results[order(results$val_score),]
  
  list(best_params=best_params, results=results, metric=metric, 
       val_frac=val_frac)
}
#reading in the data----
Austria <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Austria.xlsx")[,c('Year', 'birth_rate')])
Belgium <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Belgium.xlsx")[,c('Year', 'birth_rate')])
Denmark<- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Denmark.xlsx")[,c('Year', 'birth_rate')])
Finland <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Finland.xlsx")[,c('Year', 'birth_rate')])
France <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/France.xlsx")[,c('Year', 'birth_rate')])
Germany <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Germany.xlsx")[,c('Year', 'birth_rate')])
Ireland <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Ireland.xlsx")[,c('Year', 'birth_rate')])
Italy <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Italy.xlsx")[,c('Year', 'birth_rate')])
Luxembourg <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Luxembourg.xlsx")[,c('Year', 'birth_rate')])
Netherlands <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Netherlands.xlsx")[,c('Year', 'birth_rate')])
Portugal <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Portugal.xlsx")[,c('Year', 'birth_rate')])
Spain <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Spain.xlsx")[,c('Year', 'birth_rate')])
Sweden <- na.omit(read_excel("C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Country data/Sweden.xlsx")[,c('Year', 'birth_rate')])

#Austria----
#train/test split - 1960-2011/2012-2024
austria_train <- Austria[1:52,]
austria_test <- Austria[53:nrow(Austria),]

plot(y=Austria$birth_rate, x=Austria$Year, type ='b', xlab='Year',  
     ylab='birth_rate')
abline(v=2011, col="red")

#ARIMA
austria_arima <- auto.arima(austria_train$birth_rate, seasonal=FALSE, max.p=7, 
                            max.q=7)
plot(y=austria_train$birth_rate, x=austria_train$Year, type='b', xlab='Year', 
     ylab='birth_rate')
lines(y=fitted(austria_arima), x=austria_train$Year, col='red')

rmse(austria_train$birth_rate, fitted(austria_arima))
mae(austria_train$birth_rate, fitted(austria_arima))
mape(austria_train$birth_rate, fitted(austria_arima))

checkresiduals(austria_arima) #look okay

#test set
forecast_austria_arima <- forecast::forecast(austria_arima, h=nrow(austria_test))

plot(y=as.numeric(forecast_austria_arima$mean), x=austria_test$Year, type='b',
     ylim=c(min(austria_test$birth_rate)-1, max(austria_test$birth_rate)),
     main='ARIMA', xlab='Year',ylab='birth_rate', col="red")
lines(x=austria_test$Year, y=austria_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(austria_test$birth_rate, as.numeric(forecast_austria_arima$mean))
mae(austria_test$birth_rate, as.numeric(forecast_austria_arima$mean))
mape(austria_test$birth_rate, as.numeric(forecast_austria_arima$mean))


#ETS
austria_ets <- ets(austria_train$birth_rate, model="ZZN") #zzn allows error and trend, but not seasonality

plot(y=austria_train$birth_rate, x=austria_train$Year, type='b', xlab='Year', 
     ylab='birth_rate')
lines(y=fitted(austria_ets), x=austria_train$Year, col='red')

rmse(austria_train$birth_rate, fitted(austria_ets))
mae(austria_train$birth_rate, fitted(austria_ets))
mape(austria_train$birth_rate, fitted(austria_ets))

checkresiduals(austria_ets) #look okay


forecast_austria_ets <- forecast::forecast(austria_ets, h = nrow(austria_test))

plot(y=as.numeric(forecast_austria_ets$mean), x=austria_test$Year, type='b',
     ylim=c(min(austria_test$birth_rate), max(austria_test$birth_rate)), 
     main='ETS', xlab='Year', ylab='birth_rate', col = "red")
lines(x=austria_test$Year, y = austria_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(austria_test$birth_rate, as.numeric(forecast_austria_ets$mean))
mae(austria_test$birth_rate, as.numeric(forecast_austria_ets$mean))
mape(austria_test$birth_rate, as.numeric(forecast_austria_ets$mean))


#theta
austria_theta <- thetaf(austria_train$birth_rate, h=nrow(austria_test))

plot(y=austria_train$birth_rate, x=austria_train$Year, type='b', xlab='Year', 
     ylab='birth_rate')
lines(y=fitted(austria_theta), x=austria_train$Year, col='red')

rmse(austria_train$birth_rate, fitted(austria_theta))
mae(austria_train$birth_rate, fitted(austria_theta))
mape(austria_train$birth_rate, fitted(austria_theta))

checkresiduals(austria_theta) #look okay



plot(y=as.numeric(austria_theta$mean), x=austria_test$Year, type='b',
     ylim=c(min(austria_test$birth_rate)-1, max(austria_test$birth_rate)),
     main='Theta', xlab='Year', ylab='birth_rate', col = "red")
lines(x=austria_test$Year, y = austria_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(austria_test$birth_rate, as.numeric(austria_theta$mean))
mae(austria_test$birth_rate, as.numeric(austria_theta$mean))
mape(austria_test$birth_rate, as.numeric(austria_theta$mean))

#nnar
select_nnar(austria_train$birth_rate)$best_params
austria_nnar <- nnetar(austria_train$birth_rate, p=1, size=5)

plot(y=austria_train$birth_rate, x=austria_train$Year, type='b', xlab='Year', 
     ylab='birth_rate')
lines(y=fitted(austria_nnar), x=austria_train$Year, col='red')

rmse(austria_train$birth_rate, fitted(austria_nnar))
mae(austria_train$birth_rate, fitted(austria_nnar))
mape(austria_train$birth_rate, fitted(austria_nnar))

checkresiduals(austria_nnar) #look okay


forecast_austria_nnar <- forecast::forecast(austria_nnar, h = nrow(austria_test))

plot(y=as.numeric(forecast_austria_nnar$mean), x=austria_test$Year, type='b',
     ylim=c(min(austria_test$birth_rate), max(austria_test$birth_rate)), 
     main='NNAR', xlab='Year', ylab='birth_rate', col = "red")
lines(x=austria_test$Year, y = austria_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(austria_test$birth_rate, as.numeric(forecast_austria_nnar$mean))
mae(austria_test$birth_rate, as.numeric(forecast_austria_nnar$mean))
mape(austria_test$birth_rate, as.numeric(forecast_austria_nnar$mean))

#ARFIMA
austria_arfima <- forecast::arfima(austria_train$birth_rate)

plot(y=austria_train$birth_rate, x=austria_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(y=fitted(austria_arfima), x=austria_train$Year, col='red')

rmse(austria_train$birth_rate, fitted(austria_arfima))
mae(austria_train$birth_rate, fitted(austria_arfima))
mape(austria_train$birth_rate, fitted(austria_arfima))

checkresiduals(austria_arfima)

forecast_austria_arfima <- forecast::forecast(austria_arfima, h = nrow(austria_test))

plot(y=as.numeric(forecast_austria_arfima$mean), x=austria_test$Year, type='b',
     ylim=c(min(austria_test$birth_rate), max(austria_test$birth_rate)+4),
     main='ARFIMA', xlab='Year', ylab='birth_rate', col="red")
lines(x=austria_test$Year, y=austria_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(austria_test$birth_rate, as.numeric(forecast_austria_arfima$mean))
mae(austria_test$birth_rate, as.numeric(forecast_austria_arfima$mean))
mape(austria_test$birth_rate, as.numeric(forecast_austria_arfima$mean))



# Gaussian Process (trend + GP residual)
y_train <- as.numeric(austria_train$birth_rate)
h <- nrow(austria_test)

t_train <- seq_along(y_train)
X_train <- matrix(t_train, ncol=1)

#deterministic trend
trend_lm <- lm(y_train~t_train)
trend_fit <- as.numeric(fitted(trend_lm))
resid_train <- y_train-trend_fit

#GP on residuals
austria_gp <- DiceKriging::km(design=X_train, response=resid_train,
                              covtype="gauss", nugget.estim=TRUE,
                              control=list(trace=FALSE))

#training fitted values (trend + GP fitted residual mean)
gp_resid_fit <- as.numeric(predict(austria_gp, newdata=X_train, type="UK")$mean)
austria_gp_fitted_train <- trend_fit+gp_resid_fit

plot(y=austria_train$birth_rate, x=austria_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(y=austria_gp_fitted_train, x=austria_train$Year, col='red')

rmse(austria_train$birth_rate, austria_gp_fitted_train)
mae(austria_train$birth_rate, austria_gp_fitted_train)
mape(austria_train$birth_rate, austria_gp_fitted_train)

# 3) forecast h steps
t_future <- (max(t_train)+1):(max(t_train)+h)
X_future <- matrix(t_future, ncol=1)

gp_pred <- predict(austria_gp, newdata=X_future, type="UK", se.compute = TRUE)

trend_future <- predict(trend_lm, newdata=data.frame(t_train=t_future))

austria_forecast_mean_gp <- as.numeric(trend_future)+as.numeric(gp_pred$mean)

plot(y=austria_forecast_mean_gp, x=austria_test$Year, type='b',
     ylim=c(min(austria_test$birth_rate)-3, max(austria_test$birth_rate)+2),
     main='Gaussian Process', xlab='Year', ylab='birth_rate', col="red")
lines(x=austria_test$Year, y=austria_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(austria_test$birth_rate, austria_forecast_mean_gp)
mae(austria_test$birth_rate, austria_forecast_mean_gp)
mape(austria_test$birth_rate, austria_forecast_mean_gp)



#GAM (smooth trend only)
#smooth trend over Year (k kept small to avoid overfitting short annual series)
austria_gam <- mgcv::gam(birth_rate~s(Year, k=8), data=austria_train, method="REML")

# fitted values (train)
plot(y=austria_train$birth_rate, x=austria_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(x=austria_train$Year, y=fitted(austria_gam), col='red')

rmse(austria_train$birth_rate, fitted(austria_gam))
mae(austria_train$birth_rate, fitted(austria_gam))
mape(austria_train$birth_rate, fitted(austria_gam))

# forecast on test years
forecast_austria_gam <- predict(austria_gam, newdata=austria_test, se.fit=TRUE)
forecast_mean <- as.numeric(forecast_austria_gam$fit)

plot(y=forecast_mean, x=austria_test$Year, type='b',
     ylim=c(min(austria_test$birth_rate), max(austria_test$birth_rate)+2),
     main='GAM', xlab='Year', ylab='birth_rate', col="red")
lines(x=austria_test$Year, y=austria_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(austria_test$birth_rate, forecast_mean)
mae(austria_test$birth_rate, forecast_mean)
mape(austria_test$birth_rate, forecast_mean)


#Belgium----
#train/test split - 1960-2011/2012-2024
belgium_train <- Belgium[1:52,]
belgium_test <- Belgium[53:nrow(Belgium),]

plot(y=Belgium$birth_rate, x=Belgium$Year, type='b', xlab='Year', ylab='birth_rate')
abline(v=2011, col="red")

#ARIMA
belgium_arima <- auto.arima(belgium_train$birth_rate, seasonal=FALSE, max.p=7, 
                            max.q=7)
plot(y=belgium_train$birth_rate, x=belgium_train$Year, type='b', xlab='Year', 
     ylab='birth_rate')
lines(y=fitted(belgium_arima), x=belgium_train$Year, col='red')

rmse(belgium_train$birth_rate, fitted(belgium_arima))
mae(belgium_train$birth_rate, fitted(belgium_arima))
mape(belgium_train$birth_rate, fitted(belgium_arima))

checkresiduals(belgium_arima) #look okay

#test set
forecast_belgium_arima <- forecast::forecast(belgium_arima, h = nrow(belgium_test))

plot(y=as.numeric(forecast_belgium_arima$mean), x=belgium_test$Year, type='b',
     ylim=c(min(belgium_test$birth_rate), max(belgium_test$birth_rate)+1), 
     xlab='Year', ylab='birth_rate', col = "red", main='ARIMA')
lines(x=belgium_test$Year, y = belgium_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(belgium_test$birth_rate, as.numeric(forecast_belgium_arima$mean))
mae(belgium_test$birth_rate, as.numeric(forecast_belgium_arima$mean))
mape(belgium_test$birth_rate, as.numeric(forecast_belgium_arima$mean))




#ETS
belgium_ets <- ets(belgium_train$birth_rate, model="ZZN") #zzn allows error and trend, but not seasonality

plot(y=belgium_train$birth_rate, x=belgium_train$Year, type='b', xlab='Year', 
     ylab='birth_rate')
lines(y=fitted(belgium_ets), x=belgium_train$Year, col='red')

rmse(belgium_train$birth_rate, fitted(belgium_ets))
mae(belgium_train$birth_rate, fitted(belgium_ets))
mape(belgium_train$birth_rate, fitted(belgium_ets))

checkresiduals(belgium_ets) #look okay


forecast_belgium_ets <- forecast::forecast(belgium_ets, h = nrow(belgium_test))

plot(y=as.numeric(forecast_belgium_ets$mean), x=belgium_test$Year, type='b',
     ylim=c(min(belgium_test$birth_rate), max(belgium_test$birth_rate)+1), 
     xlab='Year', ylab='birth_rate', col = "red", main='ETS')
lines(x=belgium_test$Year, y = belgium_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(belgium_test$birth_rate, as.numeric(forecast_belgium_ets$mean))
mae(belgium_test$birth_rate, as.numeric(forecast_belgium_ets$mean))
mape(belgium_test$birth_rate, as.numeric(forecast_belgium_ets$mean))


#theta
belgium_theta <- thetaf(belgium_train$birth_rate, h=nrow(belgium_test))

plot(y=belgium_train$birth_rate, x=belgium_train$Year, type='b', xlab='Year', 
     ylab='birth_rate')
lines(y=fitted(belgium_theta), x=belgium_train$Year, col='red')

rmse(belgium_train$birth_rate, fitted(belgium_theta))
mae(belgium_train$birth_rate, fitted(belgium_theta))
mape(belgium_train$birth_rate, fitted(belgium_theta))

checkresiduals(belgium_theta) #look okay



plot(y=as.numeric(belgium_theta$mean), x=belgium_test$Year, type='b',
     ylim=c(min(belgium_test$birth_rate), max(belgium_test$birth_rate)+1),
     xlab='Year', ylab='birth_rate', col = "red", main='Theta')
lines(x=belgium_test$Year, y = belgium_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(belgium_test$birth_rate, as.numeric(belgium_theta$mean))
mae(belgium_test$birth_rate, as.numeric(belgium_theta$mean))
mape(belgium_test$birth_rate, as.numeric(belgium_theta$mean))



#NNAR
select_nnar(belgium_train$birth_rate)$best_params
belgium_nnar <- nnetar(belgium_train$birth_rate, p=1, size=2)

plot(y=belgium_train$birth_rate, x=belgium_train$Year, type='b', xlab='Year', 
     ylab='birth_rate')
lines(y=fitted(belgium_nnar), x=belgium_train$Year, col='red')

rmse(belgium_train$birth_rate, fitted(belgium_nnar))
mae(belgium_train$birth_rate, fitted(belgium_nnar))
mape(belgium_train$birth_rate, fitted(belgium_nnar))

checkresiduals(belgium_nnar) #look okay


forecast_belgium_nnar <- forecast::forecast(belgium_nnar, h = nrow(belgium_test))

plot(y=as.numeric(forecast_belgium_nnar$mean), x=belgium_test$Year, type='b',
     ylim=c(min(belgium_test$birth_rate), max(belgium_test$birth_rate)+1), 
     xlab='Year', ylab='birth_rate', col = "red", main='NNAR')
lines(x=belgium_test$Year, y = belgium_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(belgium_test$birth_rate, as.numeric(forecast_belgium_nnar$mean))
mae(belgium_test$birth_rate, as.numeric(forecast_belgium_nnar$mean))
mape(belgium_test$birth_rate, as.numeric(forecast_belgium_nnar$mean))

#ARFIMA
belgium_arfima <- forecast::arfima(belgium_train$birth_rate)

plot(y=belgium_train$birth_rate, x=belgium_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(y=fitted(belgium_arfima), x=belgium_train$Year, col='red')

rmse(belgium_train$birth_rate, fitted(belgium_arfima))
mae(belgium_train$birth_rate, fitted(belgium_arfima))
mape(belgium_train$birth_rate, fitted(belgium_arfima))

checkresiduals(belgium_arfima)

forecast_belgium_arfima <- forecast::forecast(belgium_arfima, h = nrow(belgium_test))

plot(y=as.numeric(forecast_belgium_arfima$mean), x=belgium_test$Year, type='b',
     ylim=c(min(belgium_test$birth_rate), max(belgium_test$birth_rate)+2),
     main='ARFIMA', xlab='Year', ylab='birth_rate', col="red")
lines(x=belgium_test$Year, y=belgium_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(belgium_test$birth_rate, as.numeric(forecast_belgium_arfima$mean))
mae(belgium_test$birth_rate, as.numeric(forecast_belgium_arfima$mean))
mape(belgium_test$birth_rate, as.numeric(forecast_belgium_arfima$mean))



# Gaussian Process (trend + GP residual)
y_train <- as.numeric(belgium_train$birth_rate)
h <- nrow(belgium_test)

t_train <- seq_along(y_train)
X_train <- matrix(t_train, ncol=1)

#deterministic trend
trend_lm <- lm(y_train~t_train)
trend_fit <- as.numeric(fitted(trend_lm))
resid_train <- y_train-trend_fit

#GP on residuals
belgium_gp <- DiceKriging::km(design=X_train, response=resid_train,
                              covtype="gauss", nugget.estim=TRUE,
                              control=list(trace=FALSE))

#training fitted values (trend + GP fitted residual mean)
gp_resid_fit <- as.numeric(predict(belgium_gp, newdata=X_train, type="UK")$mean)
belgium_gp_fitted_train <- trend_fit+gp_resid_fit

plot(y=belgium_train$birth_rate, x=belgium_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(y=belgium_gp_fitted_train, x=belgium_train$Year, col='red')

rmse(belgium_train$birth_rate, belgium_gp_fitted_train)
mae(belgium_train$birth_rate, belgium_gp_fitted_train)
mape(belgium_train$birth_rate, belgium_gp_fitted_train)

# 3) forecast h steps
t_future <- (max(t_train)+1):(max(t_train)+h)
X_future <- matrix(t_future, ncol=1)

gp_pred <- predict(belgium_gp, newdata=X_future, type="UK", se.compute = TRUE)

trend_future <- predict(trend_lm, newdata=data.frame(t_train=t_future))

belgium_forecast_mean_gp <- as.numeric(trend_future)+as.numeric(gp_pred$mean)

plot(y=belgium_forecast_mean_gp, x=belgium_test$Year, type='b',
     ylim=c(min(belgium_test$birth_rate)-2, max(belgium_test$birth_rate)+2),
     main='Gaussian Process', xlab='Year', ylab='birth_rate', col="red")
lines(x=belgium_test$Year, y=belgium_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(belgium_test$birth_rate, belgium_forecast_mean_gp)
mae(belgium_test$birth_rate, belgium_forecast_mean_gp)
mape(belgium_test$birth_rate, belgium_forecast_mean_gp)

#GAM (smooth trend only)
#smooth trend over Year (k kept small to avoid overfitting short annual series)
belgium_gam <- mgcv::gam(birth_rate~s(Year, k=8), data=belgium_train, method="REML")

# fitted values (train)
plot(y=belgium_train$birth_rate, x=belgium_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(x=belgium_train$Year, y=fitted(belgium_gam), col='red')

rmse(belgium_train$birth_rate, fitted(belgium_gam))
mae(belgium_train$birth_rate, fitted(belgium_gam))
mape(belgium_train$birth_rate, fitted(belgium_gam))

# forecast on test years
forecast_belgium_gam <- predict(belgium_gam, newdata=belgium_test, se.fit=TRUE)
forecast_mean <- as.numeric(forecast_belgium_gam$fit)

plot(y=forecast_mean, x=belgium_test$Year, type='b',
     ylim=c(min(belgium_test$birth_rate), max(belgium_test$birth_rate)+2),
     main='GAM', xlab='Year', ylab='birth_rate', col="red")
lines(x=belgium_test$Year, y=belgium_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(belgium_test$birth_rate, forecast_mean)
mae(belgium_test$birth_rate, forecast_mean)
mape(belgium_test$birth_rate, forecast_mean)

#Denmark----
#train/test split - 1960-2011/2012-2024
denmark_train <- Denmark[1:52,]
denmark_test <- Denmark[53:nrow(Denmark),]

plot(y=Denmark$birth_rate, x=Denmark$Year, type='b',  xlab='Year',  ylab='birth_rate')
abline(v=2011, col="red")

#ARIMA
denmark_arima <- auto.arima(denmark_train$birth_rate, seasonal=FALSE, max.p=7, 
                            max.q=7)
plot(y=denmark_train$birth_rate, x=denmark_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(denmark_arima), x=denmark_train$Year, col='red')

rmse(denmark_train$birth_rate, fitted(denmark_arima))
mae(denmark_train$birth_rate, fitted(denmark_arima))
mape(denmark_train$birth_rate, fitted(denmark_arima))

checkresiduals(denmark_arima) #look okay


forecast_denmark_arima <- forecast::forecast(denmark_arima, h = nrow(denmark_test))

plot(y=as.numeric(forecast_denmark_arima$mean), x=denmark_test$Year, type='b',
     ylim=c(min(denmark_test$birth_rate), max(denmark_test$birth_rate)),  
     xlab='Year',  ylab='birth_rate', col = "red", main='ARIMA')
lines(x=denmark_test$Year, y = denmark_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(denmark_test$birth_rate, as.numeric(forecast_denmark_arima$mean))
mae(denmark_test$birth_rate, as.numeric(forecast_denmark_arima$mean))
mape(denmark_test$birth_rate, as.numeric(forecast_denmark_arima$mean))


#ETS
denmark_ets <- ets(denmark_train$birth_rate, model="ZZN") #zzn allows error and trend, but not seasonality

plot(y=denmark_train$birth_rate, x=denmark_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(denmark_ets), x=denmark_train$Year, col='red')

rmse(denmark_train$birth_rate, fitted(denmark_ets))
mae(denmark_train$birth_rate, fitted(denmark_ets))
mape(denmark_train$birth_rate, fitted(denmark_ets))

checkresiduals(denmark_ets) #look okay


forecast_denmark_ets <- forecast::forecast(denmark_ets, h = nrow(denmark_test))

plot(y=as.numeric(forecast_denmark_ets$mean), x=denmark_test$Year, type='b',
     ylim=c(min(denmark_test$birth_rate)-2, max(denmark_test$birth_rate)),  
     xlab='Year',  ylab='birth_rate',  col = "red", main='ETS')
lines(x=denmark_test$Year, y = denmark_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(denmark_test$birth_rate, as.numeric(forecast_denmark_ets$mean))
mae(denmark_test$birth_rate, as.numeric(forecast_denmark_ets$mean))
mape(denmark_test$birth_rate, as.numeric(forecast_denmark_ets$mean))


#theta
denmark_theta <- thetaf(denmark_train$birth_rate, h=nrow(denmark_test))

plot(y=denmark_train$birth_rate, x=denmark_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(denmark_theta), x=denmark_train$Year, col='red')

rmse(denmark_train$birth_rate, fitted(denmark_theta))
mae(denmark_train$birth_rate, fitted(denmark_theta))
mape(denmark_train$birth_rate, fitted(denmark_theta))

checkresiduals(denmark_theta) #look okay

plot(y=as.numeric(denmark_theta$mean), x=denmark_test$Year, type='b',
     ylim=c(min(denmark_test$birth_rate), max(denmark_test$birth_rate)), 
     xlab='Year',  ylab='birth_rate', col = "red", main='Theta')
lines(x=denmark_test$Year, y = denmark_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(denmark_test$birth_rate, as.numeric(denmark_theta$mean))
mae(denmark_test$birth_rate, as.numeric(denmark_theta$mean))
mape(denmark_test$birth_rate, as.numeric(denmark_theta$mean))



#NNAR
select_nnar(denmark_train$birth_rate)$best_params
denmark_nnar <- nnetar(denmark_train$birth_rate, p=5, size=2)

plot(y=denmark_train$birth_rate, x=denmark_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(denmark_nnar), x=denmark_train$Year, col='red')

rmse(denmark_train$birth_rate, fitted(denmark_nnar))
mae(denmark_train$birth_rate, fitted(denmark_nnar))
mape(denmark_train$birth_rate, fitted(denmark_nnar))

checkresiduals(denmark_nnar) #look okay


forecast_denmark_nnar <- forecast::forecast(denmark_nnar, h = nrow(denmark_test))

plot(y=as.numeric(forecast_denmark_nnar$mean), x=denmark_test$Year, type='b',
     ylim=c(min(denmark_test$birth_rate), max(denmark_test$birth_rate)+2),  
     xlab='Year',  ylab='birth_rate', col = "red", main='NNAR')
lines(x=denmark_test$Year, y = denmark_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(denmark_test$birth_rate, as.numeric(forecast_denmark_nnar$mean))
mae(denmark_test$birth_rate, as.numeric(forecast_denmark_nnar$mean))
mape(denmark_test$birth_rate, as.numeric(forecast_denmark_nnar$mean))

#ARFIMA
denmark_arfima <- forecast::arfima(denmark_train$birth_rate)

plot(y=denmark_train$birth_rate, x=denmark_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(y=fitted(denmark_arfima), x=denmark_train$Year, col='red')

rmse(denmark_train$birth_rate, fitted(denmark_arfima))
mae(denmark_train$birth_rate, fitted(denmark_arfima))
mape(denmark_train$birth_rate, fitted(denmark_arfima))

checkresiduals(denmark_arfima)

forecast_denmark_arfima <- forecast::forecast(denmark_arfima, h = nrow(denmark_test))

plot(y=as.numeric(forecast_denmark_arfima$mean), x=denmark_test$Year, type='b',
     ylim=c(min(denmark_test$birth_rate), max(denmark_test$birth_rate)+2),
     main='ARFIMA', xlab='Year', ylab='birth_rate', col="red")
lines(x=denmark_test$Year, y=denmark_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(denmark_test$birth_rate, as.numeric(forecast_denmark_arfima$mean))
mae(denmark_test$birth_rate, as.numeric(forecast_denmark_arfima$mean))
mape(denmark_test$birth_rate, as.numeric(forecast_denmark_arfima$mean))



# Gaussian Process (trend + GP residual)
y_train <- as.numeric(denmark_train$birth_rate)
h <- nrow(denmark_test)

t_train <- seq_along(y_train)
X_train <- matrix(t_train, ncol=1)

#deterministic trend
trend_lm <- lm(y_train~t_train)
trend_fit <- as.numeric(fitted(trend_lm))
resid_train <- y_train-trend_fit

#GP on residuals
denmark_gp <- DiceKriging::km(design=X_train, response=resid_train,
                              covtype="gauss", nugget.estim=TRUE,
                              control=list(trace=FALSE))

#training fitted values (trend + GP fitted residual mean)
gp_resid_fit <- as.numeric(predict(denmark_gp, newdata=X_train, type="UK")$mean)
denmark_gp_fitted_train <- trend_fit+gp_resid_fit

plot(y=denmark_train$birth_rate, x=denmark_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(y=denmark_gp_fitted_train, x=denmark_train$Year, col='red')

rmse(denmark_train$birth_rate, denmark_gp_fitted_train)
mae(denmark_train$birth_rate, denmark_gp_fitted_train)
mape(denmark_train$birth_rate, denmark_gp_fitted_train)

# 3) forecast h steps
t_future <- (max(t_train)+1):(max(t_train)+h)
X_future <- matrix(t_future, ncol=1)

gp_pred <- predict(denmark_gp, newdata=X_future, type="UK", se.compute = TRUE)

trend_future <- predict(trend_lm, newdata=data.frame(t_train=t_future))

denmark_forecast_mean_gp <- as.numeric(trend_future)+as.numeric(gp_pred$mean)

plot(y=denmark_forecast_mean_gp, x=denmark_test$Year, type='b',
     ylim=c(min(denmark_test$birth_rate)-3, max(denmark_test$birth_rate)),
     main='Gaussian Process', xlab='Year', ylab='birth_rate', col="red")
lines(x=denmark_test$Year, y=denmark_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(denmark_test$birth_rate, denmark_forecast_mean_gp)
mae(denmark_test$birth_rate, denmark_forecast_mean_gp)
mape(denmark_test$birth_rate, denmark_forecast_mean_gp)



#GAM (smooth trend only)
#smooth trend over Year (k kept small to avoid overfitting short annual series)
denmark_gam <- mgcv::gam(birth_rate~s(Year, k=8), data=denmark_train, method="REML")

# fitted values (train)
plot(y=denmark_train$birth_rate, x=denmark_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(x=denmark_train$Year, y=fitted(denmark_gam), col='red')

rmse(denmark_train$birth_rate, fitted(denmark_gam))
mae(denmark_train$birth_rate, fitted(denmark_gam))
mape(denmark_train$birth_rate, fitted(denmark_gam))

# forecast on test years
forecast_denmark_gam <- predict(denmark_gam, newdata=denmark_test, se.fit=TRUE)
forecast_mean <- as.numeric(forecast_denmark_gam$fit)

plot(y=forecast_mean, x=denmark_test$Year, type='b',
     ylim=c(min(denmark_test$birth_rate), max(denmark_test$birth_rate)+1),
     main='GAM', xlab='Year', ylab='birth_rate', col="red")
lines(x=denmark_test$Year, y=denmark_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(denmark_test$birth_rate, forecast_mean)
mae(denmark_test$birth_rate, forecast_mean)
mape(denmark_test$birth_rate, forecast_mean)


#finland----
#train/test split - 1960-2011/2012-2024
finland_train <- Finland[1:52,]
finland_test <- Finland[53:nrow(Finland),]

plot(y=Finland$birth_rate, x=Finland$Year, type='b',  xlab='Year',  ylab='birth_rate')
abline(v=2011, col="red")

#ARIMA
finland_arima <- auto.arima(finland_train$birth_rate, seasonal=FALSE, max.p=7, 
                            max.q=7)
plot(y=finland_train$birth_rate, x=finland_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(finland_arima), x=finland_train$Year, col='red')

rmse(finland_train$birth_rate, fitted(finland_arima))
mae(finland_train$birth_rate, fitted(finland_arima))
mape(finland_train$birth_rate, fitted(finland_arima))

checkresiduals(finland_arima) #look okay


#test set
forecast_finland_arima <- forecast::forecast(finland_arima, h = nrow(finland_test))

plot(y=as.numeric(forecast_finland_arima$mean), x=finland_test$Year, type='b',
     ylim=c(min(finland_test$birth_rate), max(finland_test$birth_rate)),  
     xlab='Year',  ylab='birth_rate', col = "red", main='ARIMA')
lines(x=finland_test$Year, y = finland_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(finland_test$birth_rate, as.numeric(forecast_finland_arima$mean))
mae(finland_test$birth_rate, as.numeric(forecast_finland_arima$mean))
mape(finland_test$birth_rate, as.numeric(forecast_finland_arima$mean))




#ETS
finland_ets <- ets(finland_train$birth_rate, model="ZZN") #zzn allows error and trend, but not seasonality

plot(y=finland_train$birth_rate, x=finland_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(finland_ets), x=finland_train$Year, col='red')

rmse(finland_train$birth_rate, fitted(finland_ets))
mae(finland_train$birth_rate, fitted(finland_ets))
mape(finland_train$birth_rate, fitted(finland_ets))

checkresiduals(finland_ets) #look okay


forecast_finland_ets <- forecast::forecast(finland_ets, h = nrow(finland_test))

plot(y=as.numeric(forecast_finland_ets$mean), x=finland_test$Year, type='b',
     ylim=c(min(finland_test$birth_rate), max(finland_test$birth_rate)+0.5), 
     xlab='Year',  ylab='birth_rate', col = "red", main='ETA')
lines(x=finland_test$Year, y = finland_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(finland_test$birth_rate, as.numeric(forecast_finland_ets$mean))
mae(finland_test$birth_rate, as.numeric(forecast_finland_ets$mean))
mape(finland_test$birth_rate, as.numeric(forecast_finland_ets$mean))


#theta
finland_theta <- thetaf(finland_train$birth_rate, h=nrow(finland_test))

plot(y=finland_train$birth_rate, x=finland_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(finland_theta), x=finland_train$Year, col='red')

rmse(finland_train$birth_rate, fitted(finland_theta))
mae(finland_train$birth_rate, fitted(finland_theta))
mape(finland_train$birth_rate, fitted(finland_theta))

checkresiduals(finland_theta) #look okay



plot(y=as.numeric(finland_theta$mean), x=finland_test$Year, type='b',
     ylim=c(min(finland_test$birth_rate), max(finland_test$birth_rate)),  
     xlab='Year',  ylab='birth_rate', col = "red", main='Theta')
lines(x=finland_test$Year, y = finland_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(finland_test$birth_rate, as.numeric(finland_theta$mean))
mae(finland_test$birth_rate, as.numeric(finland_theta$mean))
mape(finland_test$birth_rate, as.numeric(finland_theta$mean))



#NNAR
select_nnar(finland_train$birth_rate)$best_params
finland_nnar <- nnetar(finland_train$birth_rate, p=4, size=5)

plot(y=finland_train$birth_rate, x=finland_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(finland_nnar), x=finland_train$Year, col='red')

rmse(finland_train$birth_rate, fitted(finland_nnar))
mae(finland_train$birth_rate, fitted(finland_nnar))
mape(finland_train$birth_rate, fitted(finland_nnar))

checkresiduals(finland_nnar) #look okay


forecast_finland_nnar <- forecast::forecast(finland_nnar, h = nrow(finland_test))

plot(y=as.numeric(forecast_finland_nnar$mean), x=finland_test$Year, type='b',
     ylim=c(min(finland_test$birth_rate), max(finland_test$birth_rate)+2),  
     xlab='Year',  ylab='birth_rate', col = "red", main='NNAR')
lines(x=finland_test$Year, y = finland_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(finland_test$birth_rate, as.numeric(forecast_finland_nnar$mean))
mae(finland_test$birth_rate, as.numeric(forecast_finland_nnar$mean))
mape(finland_test$birth_rate, as.numeric(forecast_finland_nnar$mean))

#ARFIMA
finland_arfima <- forecast::arfima(finland_train$birth_rate)

plot(y=finland_train$birth_rate, x=finland_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(y=fitted(finland_arfima), x=finland_train$Year, col='red')

rmse(finland_train$birth_rate, fitted(finland_arfima))
mae(finland_train$birth_rate, fitted(finland_arfima))
mape(finland_train$birth_rate, fitted(finland_arfima))

checkresiduals(finland_arfima)

forecast_finland_arfima <- forecast::forecast(finland_arfima, h = nrow(finland_test))

plot(y=as.numeric(forecast_finland_arfima$mean), x=finland_test$Year, type='b',
     ylim=c(min(finland_test$birth_rate), max(finland_test$birth_rate)+2),
     main='ARFIMA', xlab='Year', ylab='birth_rate', col="red")
lines(x=finland_test$Year, y=finland_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(finland_test$birth_rate, as.numeric(forecast_finland_arfima$mean))
mae(finland_test$birth_rate, as.numeric(forecast_finland_arfima$mean))
mape(finland_test$birth_rate, as.numeric(forecast_finland_arfima$mean))



# Gaussian Process (trend + GP residual)
y_train <- as.numeric(finland_train$birth_rate)
h <- nrow(finland_test)

t_train <- seq_along(y_train)
X_train <- matrix(t_train, ncol=1)

#deterministic trend
trend_lm <- lm(y_train~t_train)
trend_fit <- as.numeric(fitted(trend_lm))
resid_train <- y_train-trend_fit

#GP on residuals
finland_gp <- DiceKriging::km(design=X_train, response=resid_train,
                              covtype="gauss", nugget.estim=TRUE,
                              control=list(trace=FALSE))

#training fitted values (trend + GP fitted residual mean)
gp_resid_fit <- as.numeric(predict(finland_gp, newdata=X_train, type="UK")$mean)
finland_gp_fitted_train <- trend_fit+gp_resid_fit

plot(y=finland_train$birth_rate, x=finland_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(y=finland_gp_fitted_train, x=finland_train$Year, col='red')

rmse(finland_train$birth_rate, finland_gp_fitted_train)
mae(finland_train$birth_rate, finland_gp_fitted_train)
mape(finland_train$birth_rate, finland_gp_fitted_train)

# 3) forecast h steps
t_future <- (max(t_train)+1):(max(t_train)+h)
X_future <- matrix(t_future, ncol=1)

gp_pred <- predict(finland_gp, newdata=X_future, type="UK", se.compute = TRUE)

trend_future <- predict(trend_lm, newdata=data.frame(t_train=t_future))

finland_forecast_mean_gp <- as.numeric(trend_future)+as.numeric(gp_pred$mean)

plot(y=finland_forecast_mean_gp, x=finland_test$Year, type='b',
     ylim=c(min(finland_test$birth_rate), max(finland_test$birth_rate)+2),
     main='Gaussian Process', xlab='Year', ylab='birth_rate', col="red")
lines(x=finland_test$Year, y=finland_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(finland_test$birth_rate, finland_forecast_mean_gp)
mae(finland_test$birth_rate, finland_forecast_mean_gp)
mape(finland_test$birth_rate, finland_forecast_mean_gp)

#GAM (smooth trend only)
#smooth trend over Year (k kept small to avoid overfitting short annual series)
finland_gam <- mgcv::gam(birth_rate~s(Year, k=8), data=finland_train, method="REML")

# fitted values (train)
plot(y=finland_train$birth_rate, x=finland_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(x=finland_train$Year, y=fitted(finland_gam), col='red')

rmse(finland_train$birth_rate, fitted(finland_gam))
mae(finland_train$birth_rate, fitted(finland_gam))
mape(finland_train$birth_rate, fitted(finland_gam))

# forecast on test years
forecast_finland_gam <- predict(finland_gam, newdata=finland_test, se.fit=TRUE)
forecast_mean <- as.numeric(forecast_finland_gam$fit)

plot(y=forecast_mean, x=finland_test$Year, type='b',
     ylim=c(min(finland_test$birth_rate), max(finland_test$birth_rate)+3),
     main='GAM', xlab='Year', ylab='birth_rate', col="red")
lines(x=finland_test$Year, y=finland_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(finland_test$birth_rate, forecast_mean)
mae(finland_test$birth_rate, forecast_mean)
mape(finland_test$birth_rate, forecast_mean)

#france----
#train/test split - 1960-2011/2012-2024
france_train <- France[1:52,]
france_test <- France[53:nrow(France),]

plot(y=France$birth_rate, x=France$Year, type='b',  xlab='Year',  ylab='birth_rate')
abline(v=2011, col="red")

#ARIMA
france_arima <- auto.arima(france_train$birth_rate, seasonal=FALSE, max.p=7, 
                           max.q=7)
plot(y=france_train$birth_rate, x=france_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(france_arima), x=france_train$Year, col='red')

rmse(france_train$birth_rate, fitted(france_arima))
mae(france_train$birth_rate, fitted(france_arima))
mape(france_train$birth_rate, fitted(france_arima))

checkresiduals(france_arima) #look okay

#test set
forecast_france_arima <- forecast::forecast(france_arima, h = nrow(france_test))

plot(y=as.numeric(forecast_france_arima$mean), x=france_test$Year, type='b',
     ylim=c(min(france_test$birth_rate), max(france_test$birth_rate)+1),  
     xlab='Year',  ylab='birth_rate', col = "red", main='ARIMA')
lines(x=france_test$Year, y = france_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(france_test$birth_rate, as.numeric(forecast_france_arima$mean))
mae(france_test$birth_rate, as.numeric(forecast_france_arima$mean))
mape(france_test$birth_rate, as.numeric(forecast_france_arima$mean))




#ETS
france_ets <- ets(france_train$birth_rate, model="ZZN") #zzn allows error and trend, but not seasonality

plot(y=france_train$birth_rate, x=france_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(france_ets), x=france_train$Year, col='red')

rmse(france_train$birth_rate, fitted(france_ets))
mae(france_train$birth_rate, fitted(france_ets))
mape(france_train$birth_rate, fitted(france_ets))

checkresiduals(france_ets) #look okay


forecast_france_ets <- forecast::forecast(france_ets, h = nrow(france_test))

plot(y=as.numeric(forecast_france_ets$mean), x=france_test$Year, type='b',
     ylim=c(min(france_test$birth_rate)-3, max(france_test$birth_rate)+3),  
     xlab='Year',  ylab='birth_rate', col = "red", main='ETS')
lines(x=france_test$Year, y = france_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(france_test$birth_rate, as.numeric(forecast_france_ets$mean))
mae(france_test$birth_rate, as.numeric(forecast_france_ets$mean))
mape(france_test$birth_rate, as.numeric(forecast_france_ets$mean))


#theta
france_theta <- thetaf(france_train$birth_rate, h=nrow(france_test))

plot(y=france_train$birth_rate, x=france_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(france_theta), x=france_train$Year, col='red')

rmse(france_train$birth_rate, fitted(france_theta))
mae(france_train$birth_rate, fitted(france_theta))
mape(france_train$birth_rate, fitted(france_theta))

checkresiduals(france_theta) #look okay



plot(y=as.numeric(france_theta$mean), x=france_test$Year, type='b',
     ylim=c(min(france_test$birth_rate), max(france_test$birth_rate)+2),  
     xlab='Year',  ylab='birth_rate', col = "red", main='Theta')
lines(x=france_test$Year, y = france_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(france_test$birth_rate, as.numeric(france_theta$mean))
mae(france_test$birth_rate, as.numeric(france_theta$mean))
mape(france_test$birth_rate, as.numeric(france_theta$mean))



#NNAR
select_nnar(france_train$birth_rate)$best_params
france_nnar <- nnetar(france_train$birth_rate, p=1, size=5)

plot(y=france_train$birth_rate, x=france_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(france_nnar), x=france_train$Year, col='red')

rmse(france_train$birth_rate, fitted(france_nnar))
mae(france_train$birth_rate, fitted(france_nnar))
mape(france_train$birth_rate, fitted(france_nnar))

checkresiduals(france_nnar) #look okay


forecast_france_nnar <- forecast::forecast(france_nnar, h = nrow(france_test))

plot(y=as.numeric(forecast_france_nnar$mean), x=france_test$Year, type='b',
     ylim=c(min(france_test$birth_rate), max(france_test$birth_rate)+2),  
     xlab='Year',  ylab='birth_rate', col = "red", main='NNAR')
lines(x=france_test$Year, y = france_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(france_test$birth_rate, as.numeric(forecast_france_nnar$mean))
mae(france_test$birth_rate, as.numeric(forecast_france_nnar$mean))
mape(france_test$birth_rate, as.numeric(forecast_france_nnar$mean))

#ARFIMA
france_arfima <- forecast::arfima(france_train$birth_rate)

plot(y=france_train$birth_rate, x=france_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(y=fitted(france_arfima), x=france_train$Year, col='red')

rmse(france_train$birth_rate, fitted(france_arfima))
mae(france_train$birth_rate, fitted(france_arfima))
mape(france_train$birth_rate, fitted(france_arfima))

checkresiduals(france_arfima)

forecast_france_arfima <- forecast::forecast(france_arfima, h = nrow(france_test))

plot(y=as.numeric(forecast_france_arfima$mean), x=france_test$Year, type='b',
     ylim=c(min(france_test$birth_rate), max(france_test$birth_rate)+2),
     main='ARFIMA', xlab='Year', ylab='birth_rate', col="red")
lines(x=france_test$Year, y=france_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(france_test$birth_rate, as.numeric(forecast_france_arfima$mean))
mae(france_test$birth_rate, as.numeric(forecast_france_arfima$mean))
mape(france_test$birth_rate, as.numeric(forecast_france_arfima$mean))



# Gaussian Process (trend + GP residual)
y_train <- as.numeric(france_train$birth_rate)
h <- nrow(france_test)

t_train <- seq_along(y_train)
X_train <- matrix(t_train, ncol=1)

#deterministic trend
trend_lm <- lm(y_train~t_train)
trend_fit <- as.numeric(fitted(trend_lm))
resid_train <- y_train-trend_fit

#GP on residuals
france_gp <- DiceKriging::km(design=X_train, response=resid_train,
                             covtype="gauss", nugget.estim=TRUE,
                             control=list(trace=FALSE))

#training fitted values (trend + GP fitted residual mean)
gp_resid_fit <- as.numeric(predict(france_gp, newdata=X_train, type="UK")$mean)
france_gp_fitted_train <- trend_fit+gp_resid_fit

plot(y=france_train$birth_rate, x=france_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(y=france_gp_fitted_train, x=france_train$Year, col='red')

rmse(france_train$birth_rate, france_gp_fitted_train)
mae(france_train$birth_rate, france_gp_fitted_train)
mape(france_train$birth_rate, france_gp_fitted_train)

# 3) forecast h steps
t_future <- (max(t_train)+1):(max(t_train)+h)
X_future <- matrix(t_future, ncol=1)

gp_pred <- predict(france_gp, newdata=X_future, type="UK", se.compute = TRUE)

trend_future <- predict(trend_lm, newdata=data.frame(t_train=t_future))

france_forecast_mean_gp <- as.numeric(trend_future)+as.numeric(gp_pred$mean)

plot(y=france_forecast_mean_gp, x=france_test$Year, type='b',
     ylim=c(min(france_test$birth_rate), max(france_test$birth_rate)+1),
     main='Gaussian Process', xlab='Year', ylab='birth_rate', col="red")
lines(x=france_test$Year, y=france_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(france_test$birth_rate, france_forecast_mean_gp)
mae(france_test$birth_rate, france_forecast_mean_gp)
mape(france_test$birth_rate, france_forecast_mean_gp)


#GAM (smooth trend only)
#smooth trend over Year (k kept small to avoid overfitting short annual series)
france_gam <- mgcv::gam(birth_rate~s(Year, k=8), data=france_train, method="REML")

# fitted values (train)
plot(y=france_train$birth_rate, x=france_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(x=france_train$Year, y=fitted(france_gam), col='red')

rmse(france_train$birth_rate, fitted(france_gam))
mae(france_train$birth_rate, fitted(france_gam))
mape(france_train$birth_rate, fitted(france_gam))

# forecast on test years
forecast_france_gam <- predict(france_gam, newdata=france_test, se.fit=TRUE)
forecast_mean <- as.numeric(forecast_france_gam$fit)

plot(y=forecast_mean, x=france_test$Year, type='b',
     ylim=c(min(france_test$birth_rate), max(france_test$birth_rate)+1),
     main='GAM', xlab='Year', ylab='birth_rate', col="red")
lines(x=france_test$Year, y=france_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(france_test$birth_rate, forecast_mean)
mae(france_test$birth_rate, forecast_mean)
mape(france_test$birth_rate, forecast_mean)


#germany----
#train/test split - 1960-2011/2012-2024
germany_train <- Germany[1:52,]
germany_test <- Germany[53:nrow(Germany),]

plot(y=Germany$birth_rate, x=Germany$Year, type='b',  xlab='Year',  ylab='birth_rate')
abline(v=2011, col="red")

#ARIMA
germany_arima <- auto.arima(germany_train$birth_rate, seasonal=FALSE, max.p=7, 
                            max.q=7)
plot(y=germany_train$birth_rate, x=germany_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(germany_arima), x=germany_train$Year, col='red')

rmse(germany_train$birth_rate, fitted(germany_arima))
mae(germany_train$birth_rate, fitted(germany_arima))
mape(germany_train$birth_rate, fitted(germany_arima))

checkresiduals(germany_arima) #look okay

#test set
forecast_germany_arima <- forecast::forecast(germany_arima, h = nrow(germany_test))

plot(y=as.numeric(forecast_germany_arima$mean), x=germany_test$Year, type='b',
     ylim=c(min(germany_test$birth_rate), max(germany_test$birth_rate)),  
     xlab='Year',  ylab='birth_rate', col = "red", main='ARIMA')
lines(x=germany_test$Year, y = germany_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(germany_test$birth_rate, as.numeric(forecast_germany_arima$mean))
mae(germany_test$birth_rate, as.numeric(forecast_germany_arima$mean))
mape(germany_test$birth_rate, as.numeric(forecast_germany_arima$mean))




#ETS
germany_ets <- ets(germany_train$birth_rate, model="ZZN") #zzn allows error and trend, but not seasonality

plot(y=germany_train$birth_rate, x=germany_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(germany_ets), x=germany_train$Year, col='red')

rmse(germany_train$birth_rate, fitted(germany_ets))
mae(germany_train$birth_rate, fitted(germany_ets))
mape(germany_train$birth_rate, fitted(germany_ets))

checkresiduals(germany_ets) #look okay


forecast_germany_ets <- forecast::forecast(germany_ets, h = nrow(germany_test))

plot(y=as.numeric(forecast_germany_ets$mean), x=germany_test$Year, type='b',
     ylim=c(min(germany_test$birth_rate), max(germany_test$birth_rate)),  
     xlab='Year',  ylab='birth_rate', col = "red", main='ETS')
lines(x=germany_test$Year, y = germany_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(germany_test$birth_rate, as.numeric(forecast_germany_ets$mean))
mae(germany_test$birth_rate, as.numeric(forecast_germany_ets$mean))
mape(germany_test$birth_rate, as.numeric(forecast_germany_ets$mean))


#theta
germany_theta <- thetaf(germany_train$birth_rate, h=nrow(germany_test))

plot(y=germany_train$birth_rate, x=germany_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(germany_theta), x=germany_train$Year, col='red')

rmse(germany_train$birth_rate, fitted(germany_theta))
mae(germany_train$birth_rate, fitted(germany_theta))
mape(germany_train$birth_rate, fitted(germany_theta))

checkresiduals(germany_theta) #look okay



plot(y=as.numeric(germany_theta$mean), x=germany_test$Year, type='b',
     ylim=c(min(germany_test$birth_rate)-1, max(germany_test$birth_rate)),  
     xlab='Year',  ylab='birth_rate', col = "red", main='Theta')
lines(x=germany_test$Year, y = germany_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(germany_test$birth_rate, as.numeric(germany_theta$mean))
mae(germany_test$birth_rate, as.numeric(germany_theta$mean))
mape(germany_test$birth_rate, as.numeric(germany_theta$mean))



#NNAR
select_nnar(germany_train$birth_rate)$best_params
germany_nnar <- nnetar(germany_train$birth_rate, p=4, size=5)

plot(y=germany_train$birth_rate, x=germany_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(germany_nnar), x=germany_train$Year, col='red')

rmse(germany_train$birth_rate, fitted(germany_nnar))
mae(germany_train$birth_rate, fitted(germany_nnar))
mape(germany_train$birth_rate, fitted(germany_nnar))

checkresiduals(germany_nnar) #look okay


forecast_germany_nnar <- forecast::forecast(germany_nnar, h = nrow(germany_test))

plot(y=as.numeric(forecast_germany_nnar$mean), x=germany_test$Year, type='b',
     ylim=c(min(germany_test$birth_rate)-1, max(germany_test$birth_rate)),  
     xlab='Year',  ylab='birth_rate', col = "red", main='NNAR')
lines(x=germany_test$Year, y = germany_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(germany_test$birth_rate, as.numeric(forecast_germany_nnar$mean))
mae(germany_test$birth_rate, as.numeric(forecast_germany_nnar$mean))
mape(germany_test$birth_rate, as.numeric(forecast_germany_nnar$mean))

#ARFIMA
germany_arfima <- forecast::arfima(germany_train$birth_rate)

plot(y=germany_train$birth_rate, x=germany_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(y=fitted(germany_arfima), x=germany_train$Year, col='red')

rmse(germany_train$birth_rate, fitted(germany_arfima))
mae(germany_train$birth_rate, fitted(germany_arfima))
mape(germany_train$birth_rate, fitted(germany_arfima))

checkresiduals(germany_arfima)

forecast_germany_arfima <- forecast::forecast(germany_arfima, h = nrow(germany_test))

plot(y=as.numeric(forecast_germany_arfima$mean), x=germany_test$Year, type='b',
     ylim=c(min(germany_test$birth_rate), max(germany_test$birth_rate)+2),
     main='ARFIMA', xlab='Year', ylab='birth_rate', col="red")
lines(x=germany_test$Year, y=germany_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(germany_test$birth_rate, as.numeric(forecast_germany_arfima$mean))
mae(germany_test$birth_rate, as.numeric(forecast_germany_arfima$mean))
mape(germany_test$birth_rate, as.numeric(forecast_germany_arfima$mean))


# Gaussian Process (trend + GP residual)
y_train <- as.numeric(germany_train$birth_rate)
h <- nrow(germany_test)

t_train <- seq_along(y_train)
X_train <- matrix(t_train, ncol=1)

#deterministic trend
trend_lm <- lm(y_train~t_train)
trend_fit <- as.numeric(fitted(trend_lm))
resid_train <- y_train-trend_fit

#GP on residuals
germany_gp <- DiceKriging::km(design=X_train, response=resid_train,
                              covtype="gauss", nugget.estim=TRUE,
                              control=list(trace=FALSE))

#training fitted values (trend + GP fitted residual mean)
gp_resid_fit <- as.numeric(predict(germany_gp, newdata=X_train, type="UK")$mean)
germany_gp_fitted_train <- trend_fit+gp_resid_fit

plot(y=germany_train$birth_rate, x=germany_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(y=germany_gp_fitted_train, x=germany_train$Year, col='red')

rmse(germany_train$birth_rate, germany_gp_fitted_train)
mae(germany_train$birth_rate, germany_gp_fitted_train)
mape(germany_train$birth_rate, germany_gp_fitted_train)

# 3) forecast h steps
t_future <- (max(t_train)+1):(max(t_train)+h)
X_future <- matrix(t_future, ncol=1)

gp_pred <- predict(germany_gp, newdata=X_future, type="UK", se.compute = TRUE)

trend_future <- predict(trend_lm, newdata=data.frame(t_train=t_future))

germany_forecast_mean_gp <- as.numeric(trend_future)+as.numeric(gp_pred$mean)

plot(y=germany_forecast_mean_gp, x=germany_test$Year, type='b',
     ylim=c(min(germany_test$birth_rate)-4, max(germany_test$birth_rate)),
     main='Gaussian Process', xlab='Year', ylab='birth_rate', col="red")
lines(x=germany_test$Year, y=germany_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(germany_test$birth_rate, germany_forecast_mean_gp)
mae(germany_test$birth_rate, germany_forecast_mean_gp)
mape(germany_test$birth_rate, germany_forecast_mean_gp)



#GAM (smooth trend only)
#smooth trend over Year (k kept small to avoid overfitting short annual series)
germany_gam <- mgcv::gam(birth_rate~s(Year, k=8), data=germany_train, method="REML")

# fitted values (train)
plot(y=germany_train$birth_rate, x=germany_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(x=germany_train$Year, y=fitted(germany_gam), col='red')

rmse(germany_train$birth_rate, fitted(germany_gam))
mae(germany_train$birth_rate, fitted(germany_gam))
mape(germany_train$birth_rate, fitted(germany_gam))

# forecast on test years
forecast_germany_gam <- predict(germany_gam, newdata=germany_test, se.fit=TRUE)
forecast_mean <- as.numeric(forecast_germany_gam$fit)

plot(y=forecast_mean, x=germany_test$Year, type='b',
     ylim=c(min(germany_test$birth_rate)-2, max(germany_test$birth_rate)+2),
     main='GAM', xlab='Year', ylab='birth_rate', col="red")
lines(x=germany_test$Year, y=germany_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(germany_test$birth_rate, forecast_mean)
mae(germany_test$birth_rate, forecast_mean)
mape(germany_test$birth_rate, forecast_mean)

#Ireland----
#train/test split - 1960-2011/2012-2024
ireland_train <- Ireland[1:52,]
ireland_test <- Ireland[53:nrow(Ireland),]

plot(y=Ireland$birth_rate, x=Ireland$Year, type='b',  xlab='Year',  ylab='birth_rate')
abline(v=2011, col="red")

#ARIMA
ireland_arima <- auto.arima(ireland_train$birth_rate, seasonal=FALSE, max.p=7, 
                            max.q=7)
plot(y=ireland_train$birth_rate, x=ireland_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(ireland_arima), x=ireland_train$Year, col='red')

rmse(ireland_train$birth_rate, fitted(ireland_arima))
mae(ireland_train$birth_rate, fitted(ireland_arima))
mape(ireland_train$birth_rate, fitted(ireland_arima))

checkresiduals(ireland_arima) #look okay

#test set
forecast_ireland_arima <- forecast::forecast(ireland_arima, h = nrow(ireland_test))

plot(y=as.numeric(forecast_ireland_arima$mean), x=ireland_test$Year, type='b',
     ylim=c(min(ireland_test$birth_rate), max(ireland_test$birth_rate)+3),  
     xlab='Year',  ylab='birth_rate', col = "red", main='ARIMA')
lines(x=ireland_test$Year, y = ireland_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(ireland_test$birth_rate, as.numeric(forecast_ireland_arima$mean))
mae(ireland_test$birth_rate, as.numeric(forecast_ireland_arima$mean))
mape(ireland_test$birth_rate, as.numeric(forecast_ireland_arima$mean))




#ETS
ireland_ets <- ets(ireland_train$birth_rate, model="ZZN") #zzn allows error and trend, but not seasonality

plot(y=ireland_train$birth_rate, x=ireland_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(ireland_ets), x=ireland_train$Year, col='red')

rmse(ireland_train$birth_rate, fitted(ireland_ets))
mae(ireland_train$birth_rate, fitted(ireland_ets))
mape(ireland_train$birth_rate, fitted(ireland_ets))

checkresiduals(ireland_ets) #look okay


forecast_ireland_ets <- forecast::forecast(ireland_ets, h = nrow(ireland_test))

plot(y=as.numeric(forecast_ireland_ets$mean), x=ireland_test$Year, type='b',
     ylim=c(min(ireland_test$birth_rate), max(ireland_test$birth_rate)+3),  
     xlab='Year',  ylab='birth_rate', col = "red", main='ETS')
lines(x=ireland_test$Year, y = ireland_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(ireland_test$birth_rate, as.numeric(forecast_ireland_ets$mean))
mae(ireland_test$birth_rate, as.numeric(forecast_ireland_ets$mean))
mape(ireland_test$birth_rate, as.numeric(forecast_ireland_ets$mean))


#theta
ireland_theta <- thetaf(ireland_train$birth_rate, h=nrow(ireland_test))

plot(y=ireland_train$birth_rate, x=ireland_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(ireland_theta), x=ireland_train$Year, col='red')

rmse(ireland_train$birth_rate, fitted(ireland_theta))
mae(ireland_train$birth_rate, fitted(ireland_theta))
mape(ireland_train$birth_rate, fitted(ireland_theta))

checkresiduals(ireland_theta) #look okay



plot(y=as.numeric(ireland_theta$mean), x=ireland_test$Year, type='b',
     ylim=c(min(ireland_test$birth_rate), max(ireland_test$birth_rate)+4),  
     xlab='Year',  ylab='birth_rate', col = "red", main='Theta')
lines(x=ireland_test$Year, y = ireland_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(ireland_test$birth_rate, as.numeric(ireland_theta$mean))
mae(ireland_test$birth_rate, as.numeric(ireland_theta$mean))
mape(ireland_test$birth_rate, as.numeric(ireland_theta$mean))



#NNAR
select_nnar(ireland_train$birth_rate)$best_params
ireland_nnar <- nnetar(ireland_train$birth_rate, p=2, size=1)

plot(y=ireland_train$birth_rate, x=ireland_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(ireland_nnar), x=ireland_train$Year, col='red')

rmse(ireland_train$birth_rate, fitted(ireland_nnar))
mae(ireland_train$birth_rate, fitted(ireland_nnar))
mape(ireland_train$birth_rate, fitted(ireland_nnar))

checkresiduals(ireland_nnar) #look okay


forecast_ireland_nnar <- forecast::forecast(ireland_nnar, h = nrow(ireland_test))

plot(y=as.numeric(forecast_ireland_nnar$mean), x=ireland_test$Year, type='b',
     ylim=c(min(ireland_test$birth_rate), max(ireland_test$birth_rate)+3), 
     xlab='Year',  ylab='birth_rate', col = "red", main='NNAR')
lines(x=ireland_test$Year, y = ireland_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(ireland_test$birth_rate, as.numeric(forecast_ireland_nnar$mean))
mae(ireland_test$birth_rate, as.numeric(forecast_ireland_nnar$mean))
mape(ireland_test$birth_rate, as.numeric(forecast_ireland_nnar$mean))

#ARFIMA
ireland_arfima <- forecast::arfima(ireland_train$birth_rate)

plot(y=ireland_train$birth_rate, x=ireland_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(y=fitted(ireland_arfima), x=ireland_train$Year, col='red')

rmse(ireland_train$birth_rate, fitted(ireland_arfima))
mae(ireland_train$birth_rate, fitted(ireland_arfima))
mape(ireland_train$birth_rate, fitted(ireland_arfima))

checkresiduals(ireland_arfima)

forecast_ireland_arfima <- forecast::forecast(ireland_arfima, h = nrow(ireland_test))

plot(y=as.numeric(forecast_ireland_arfima$mean), x=ireland_test$Year, type='b',
     ylim=c(min(ireland_test$birth_rate), max(ireland_test$birth_rate)+4),
     main='ARFIMA', xlab='Year', ylab='birth_rate', col="red")
lines(x=ireland_test$Year, y=ireland_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(ireland_test$birth_rate, as.numeric(forecast_ireland_arfima$mean))
mae(ireland_test$birth_rate, as.numeric(forecast_ireland_arfima$mean))
mape(ireland_test$birth_rate, as.numeric(forecast_ireland_arfima$mean))



# Gaussian Process (trend + GP residual)
y_train <- as.numeric(ireland_train$birth_rate)
h <- nrow(ireland_test)

t_train <- seq_along(y_train)
X_train <- matrix(t_train, ncol=1)

#deterministic trend
trend_lm <- lm(y_train~t_train)
trend_fit <- as.numeric(fitted(trend_lm))
resid_train <- y_train-trend_fit

#GP on residuals
ireland_gp <- DiceKriging::km(design=X_train, response=resid_train,
                              covtype="gauss", nugget.estim=TRUE,
                              control=list(trace=FALSE))

#training fitted values (trend + GP fitted residual mean)
gp_resid_fit <- as.numeric(predict(ireland_gp, newdata=X_train, type="UK")$mean)
ireland_gp_fitted_train <- trend_fit+gp_resid_fit

plot(y=ireland_train$birth_rate, x=ireland_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(y=ireland_gp_fitted_train, x=ireland_train$Year, col='red')

rmse(ireland_train$birth_rate, ireland_gp_fitted_train)
mae(ireland_train$birth_rate, ireland_gp_fitted_train)
mape(ireland_train$birth_rate, ireland_gp_fitted_train)

# 3) forecast h steps
t_future <- (max(t_train)+1):(max(t_train)+h)
X_future <- matrix(t_future, ncol=1)

gp_pred <- predict(ireland_gp, newdata=X_future, type="UK", se.compute = TRUE)

trend_future <- predict(trend_lm, newdata=data.frame(t_train=t_future))

ireland_forecast_mean_gp <- as.numeric(trend_future)+as.numeric(gp_pred$mean)

plot(y=ireland_forecast_mean_gp, x=ireland_test$Year, type='b',
     ylim=c(min(ireland_test$birth_rate), max(ireland_test$birth_rate)+2),
     main='Gaussian Process', xlab='Year', ylab='birth_rate', col="red")
lines(x=ireland_test$Year, y=ireland_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(ireland_test$birth_rate, ireland_forecast_mean_gp)
mae(ireland_test$birth_rate, ireland_forecast_mean_gp)
mape(ireland_test$birth_rate, ireland_forecast_mean_gp)


#GAM (smooth trend only)
#smooth trend over Year (k kept small to avoid overfitting short annual series)
ireland_gam <- mgcv::gam(birth_rate~s(Year, k=8), data=ireland_train, method="REML")

# fitted values (train)
plot(y=ireland_train$birth_rate, x=ireland_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(x=ireland_train$Year, y=fitted(ireland_gam), col='red')

rmse(ireland_train$birth_rate, fitted(ireland_gam))
mae(ireland_train$birth_rate, fitted(ireland_gam))
mape(ireland_train$birth_rate, fitted(ireland_gam))

# forecast on test years
forecast_ireland_gam <- predict(ireland_gam, newdata=ireland_test, se.fit=TRUE)
forecast_mean <- as.numeric(forecast_ireland_gam$fit)

plot(y=forecast_mean, x=ireland_test$Year, type='b',
     ylim=c(min(ireland_test$birth_rate), max(ireland_test$birth_rate)+4),
     main='GAM', xlab='Year', ylab='birth_rate', col="red")
lines(x=ireland_test$Year, y=ireland_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(ireland_test$birth_rate, forecast_mean)
mae(ireland_test$birth_rate, forecast_mean)
mape(ireland_test$birth_rate, forecast_mean)

#Italy----
#train/test split - 1960-2011/2012-2024
italy_train <- Italy[1:52,]
italy_test <- Italy[53:nrow(Italy),]

plot(y=Italy$birth_rate, x=Italy$Year, type='b',  xlab='Year',  ylab='birth_rate')
abline(v=2011, col="red")

#ARIMA
italy_arima <- auto.arima(italy_train$birth_rate, seasonal=FALSE, max.p=7, 
                          max.q=7)
plot(y=italy_train$birth_rate, x=italy_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(italy_arima), x=italy_train$Year, col='red')

rmse(italy_train$birth_rate, fitted(italy_arima))
mae(italy_train$birth_rate, fitted(italy_arima))
mape(italy_train$birth_rate, fitted(italy_arima))

checkresiduals(italy_arima) #look okay

#test set
forecast_italy_arima <- forecast::forecast(italy_arima, h = nrow(italy_test))

plot(y=as.numeric(forecast_italy_arima$mean), x=italy_test$Year, type='b',
     ylim=c(min(italy_test$birth_rate), max(italy_test$birth_rate)+4),  
     xlab='Year',  ylab='birth_rate', col = "red", main='ARIMA')
lines(x=italy_test$Year, y = italy_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(italy_test$birth_rate, as.numeric(forecast_italy_arima$mean))
mae(italy_test$birth_rate, as.numeric(forecast_italy_arima$mean))
mape(italy_test$birth_rate, as.numeric(forecast_italy_arima$mean))




#ETS
italy_ets <- ets(italy_train$birth_rate, model="ZZN") #zzn allows error and trend, but not seasonality

plot(y=italy_train$birth_rate, x=italy_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(italy_ets), x=italy_train$Year, col='red')

rmse(italy_train$birth_rate, fitted(italy_ets))
mae(italy_train$birth_rate, fitted(italy_ets))
mape(italy_train$birth_rate, fitted(italy_ets))

checkresiduals(italy_ets) #look okay


forecast_italy_ets <- forecast::forecast(italy_ets, h = nrow(italy_test))

plot(y=as.numeric(forecast_italy_ets$mean), x=italy_test$Year, type='b',
     ylim=c(min(italy_test$birth_rate)-1, max(italy_test$birth_rate)+3),  
     xlab='Year',  ylab='birth_rate', col = "red", main='ETS')
lines(x=italy_test$Year, y = italy_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(italy_test$birth_rate, as.numeric(forecast_italy_ets$mean))
mae(italy_test$birth_rate, as.numeric(forecast_italy_ets$mean))
mape(italy_test$birth_rate, as.numeric(forecast_italy_ets$mean))


#theta
italy_theta <- thetaf(italy_train$birth_rate, h=nrow(italy_test))

plot(y=italy_train$birth_rate, x=italy_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(italy_theta), x=italy_train$Year, col='red')

rmse(italy_train$birth_rate, fitted(italy_theta))
mae(italy_train$birth_rate, fitted(italy_theta))
mape(italy_train$birth_rate, fitted(italy_theta))

checkresiduals(italy_theta) #look okay



plot(y=as.numeric(italy_theta$mean), x=italy_test$Year,
     ylim=c(min(italy_test$birth_rate), max(italy_test$birth_rate)+2),
     col = "red", type = "b",  xlab='Year',  ylab='birth_rate', main='Theta')
lines(x=italy_test$Year, y = italy_test$birth_rate, type='b' )
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(italy_test$birth_rate, as.numeric(italy_theta$mean))
mae(italy_test$birth_rate, as.numeric(italy_theta$mean))
mape(italy_test$birth_rate, as.numeric(italy_theta$mean))



#NNAR
select_nnar(italy_train$birth_rate)$best_params
italy_nnar <- nnetar(italy_train$birth_rate, p=5, size=5)

plot(y=italy_train$birth_rate, x=italy_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(italy_nnar), x=italy_train$Year, col='red')

rmse(italy_train$birth_rate, fitted(italy_nnar))
mae(italy_train$birth_rate, fitted(italy_nnar))
mape(italy_train$birth_rate, fitted(italy_nnar))

checkresiduals(italy_nnar) #look okay


forecast_italy_nnar <- forecast::forecast(italy_nnar, h = nrow(italy_test))

plot(y=as.numeric(forecast_italy_nnar$mean), x=italy_test$Year, type='b',
     ylim=c(min(italy_test$birth_rate), max(italy_test$birth_rate)+3),  
     xlab='Year',  ylab='birth_rate', col = "red", main='NNAR')
lines(x=italy_test$Year, y = italy_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(italy_test$birth_rate, as.numeric(forecast_italy_nnar$mean))
mae(italy_test$birth_rate, as.numeric(forecast_italy_nnar$mean))
mape(italy_test$birth_rate, as.numeric(forecast_italy_nnar$mean))

#ARFIMA
italy_arfima <- forecast::arfima(italy_train$birth_rate)

plot(y=italy_train$birth_rate, x=italy_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(y=fitted(italy_arfima), x=italy_train$Year, col='red')

rmse(italy_train$birth_rate, fitted(italy_arfima))
mae(italy_train$birth_rate, fitted(italy_arfima))
mape(italy_train$birth_rate, fitted(italy_arfima))

checkresiduals(italy_arfima)

forecast_italy_arfima <- forecast::forecast(italy_arfima, h = nrow(italy_test))

plot(y=as.numeric(forecast_italy_arfima$mean), x=italy_test$Year, type='b',
     ylim=c(min(italy_test$birth_rate)-5, max(italy_test$birth_rate)+2),
     main='ARFIMA', xlab='Year', ylab='birth_rate', col="red")
lines(x=italy_test$Year, y=italy_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(italy_test$birth_rate, as.numeric(forecast_italy_arfima$mean))
mae(italy_test$birth_rate, as.numeric(forecast_italy_arfima$mean))
mape(italy_test$birth_rate, as.numeric(forecast_italy_arfima$mean))



# Gaussian Process (trend + GP residual)
y_train <- as.numeric(italy_train$birth_rate)
h <- nrow(italy_test)

t_train <- seq_along(y_train)
X_train <- matrix(t_train, ncol=1)

#deterministic trend
trend_lm <- lm(y_train~t_train)
trend_fit <- as.numeric(fitted(trend_lm))
resid_train <- y_train-trend_fit

#GP on residuals
italy_gp <- DiceKriging::km(design=X_train, response=resid_train,
                            covtype="gauss", nugget.estim=TRUE,
                            control=list(trace=FALSE))

#training fitted values (trend + GP fitted residual mean)
gp_resid_fit <- as.numeric(predict(italy_gp, newdata=X_train, type="UK")$mean)
italy_gp_fitted_train <- trend_fit+gp_resid_fit

plot(y=italy_train$birth_rate, x=italy_train$Year, type='b', xlab='Year', ylab='birth_rate')

lines(y=italy_gp_fitted_train, x=italy_train$Year, col='red')

rmse(italy_train$birth_rate, italy_gp_fitted_train)
mae(italy_train$birth_rate, italy_gp_fitted_train)
mape(italy_train$birth_rate, italy_gp_fitted_train)

# 3) forecast h steps
t_future <- (max(t_train)+1):(max(t_train)+h)
X_future <- matrix(t_future, ncol=1)

gp_pred <- predict(italy_gp, newdata=X_future, type="UK", se.compute = TRUE)

trend_future <- predict(trend_lm, newdata=data.frame(t_train=t_future))

italy_forecast_mean_gp <- as.numeric(trend_future)+as.numeric(gp_pred$mean)

plot(y=italy_forecast_mean_gp, x=italy_test$Year, type='b',
     ylim=c(min(italy_test$birth_rate)-3, max(italy_test$birth_rate)),
     main='Gaussian Process', xlab='Year', ylab='birth_rate', col="red")
lines(x=italy_test$Year, y=italy_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(italy_test$birth_rate, italy_forecast_mean_gp)
mae(italy_test$birth_rate, italy_forecast_mean_gp)
mape(italy_test$birth_rate, italy_forecast_mean_gp)


#GAM (smooth trend only)
#smooth trend over Year (k kept small to avoid overfitting short annual series)
italy_gam <- mgcv::gam(birth_rate~s(Year, k=8), data=italy_train, method="REML")

# fitted values (train)
plot(y=italy_train$birth_rate, x=italy_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(x=italy_train$Year, y=fitted(italy_gam), col='red')

rmse(italy_train$birth_rate, fitted(italy_gam))
mae(italy_train$birth_rate, fitted(italy_gam))
mape(italy_train$birth_rate, fitted(italy_gam))

# forecast on test years
forecast_italy_gam <- predict(italy_gam, newdata=italy_test, se.fit=TRUE)
forecast_mean <- as.numeric(forecast_italy_gam$fit)

plot(y=forecast_mean, x=italy_test$Year, type='b',
     ylim=c(min(italy_test$birth_rate), max(italy_test$birth_rate)+2),
     main='GAM', xlab='Year', ylab='birth_rate', col="red")
lines(x=italy_test$Year, y=italy_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(italy_test$birth_rate, forecast_mean)
mae(italy_test$birth_rate, forecast_mean)
mape(italy_test$birth_rate, forecast_mean)

#Luxembourg----
#train/test split - 1960-2011/2012-2024
luxembourg_train <- Luxembourg[1:52,]
luxembourg_test <- Luxembourg[53:nrow(Luxembourg),]

plot(y=Luxembourg$birth_rate, x=Luxembourg$Year, type='b',  xlab='Year',  ylab='birth_rate')
abline(v=2011, col="red")

#ARIMA
luxembourg_arima <- auto.arima(luxembourg_train$birth_rate, seasonal=FALSE, max.p=7, 
                               max.q=7)
plot(y=luxembourg_train$birth_rate, x=luxembourg_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(luxembourg_arima), x=luxembourg_train$Year, col='red')

rmse(luxembourg_train$birth_rate, fitted(luxembourg_arima))
mae(luxembourg_train$birth_rate, fitted(luxembourg_arima))
mape(luxembourg_train$birth_rate, fitted(luxembourg_arima))

checkresiduals(luxembourg_arima) #look okay

#test set
forecast_luxembourg_arima <- forecast::forecast(luxembourg_arima, h = nrow(luxembourg_test))

plot(y=as.numeric(forecast_luxembourg_arima$mean), x=luxembourg_test$Year, type='b',
     ylim=c(min(luxembourg_test$birth_rate)-5, max(luxembourg_test$birth_rate)),  
     xlab='Year',  ylab='birth_rate', col = "red", main='ARIMA')
lines(x=luxembourg_test$Year, y = luxembourg_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(luxembourg_test$birth_rate, as.numeric(forecast_luxembourg_arima$mean))
mae(luxembourg_test$birth_rate, as.numeric(forecast_luxembourg_arima$mean))
mape(luxembourg_test$birth_rate, as.numeric(forecast_luxembourg_arima$mean))




#ETS
luxembourg_ets <- ets(luxembourg_train$birth_rate, model="ZZN") #zzn allows error and trend, but not seasonality

plot(y=luxembourg_train$birth_rate, x=luxembourg_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(luxembourg_ets), x=luxembourg_train$Year, col='red')

rmse(luxembourg_train$birth_rate, fitted(luxembourg_ets))
mae(luxembourg_train$birth_rate, fitted(luxembourg_ets))
mape(luxembourg_train$birth_rate, fitted(luxembourg_ets))

checkresiduals(luxembourg_ets) #look okay


forecast_luxembourg_ets <- forecast::forecast(luxembourg_ets, h = nrow(luxembourg_test))

plot(y=as.numeric(forecast_luxembourg_ets$mean), x=luxembourg_test$Year, type='b',
     ylim=c(min(luxembourg_test$birth_rate), max(luxembourg_test$birth_rate)+1), 
     xlab='Year',  ylab='birth_rate', col = "red", main='ETS')
lines(x=luxembourg_test$Year, y = luxembourg_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(luxembourg_test$birth_rate, as.numeric(forecast_luxembourg_ets$mean))
mae(luxembourg_test$birth_rate, as.numeric(forecast_luxembourg_ets$mean))
mape(luxembourg_test$birth_rate, as.numeric(forecast_luxembourg_ets$mean))


#theta
luxembourg_theta <- thetaf(luxembourg_train$birth_rate, h=nrow(luxembourg_test))

plot(y=luxembourg_train$birth_rate, x=luxembourg_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(luxembourg_theta), x=luxembourg_train$Year, col='red')

rmse(luxembourg_train$birth_rate, fitted(luxembourg_theta))
mae(luxembourg_train$birth_rate, fitted(luxembourg_theta))
mape(luxembourg_train$birth_rate, fitted(luxembourg_theta))

checkresiduals(luxembourg_theta) #look okay



plot(y=as.numeric(luxembourg_theta$mean), x=luxembourg_test$Year, type='b',
     ylim=c(min(luxembourg_test$birth_rate), max(luxembourg_test$birth_rate)+1),  
     xlab='Year',  ylab='birth_rate', col = "red", main='Theta')
lines(x=luxembourg_test$Year, y = luxembourg_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(luxembourg_test$birth_rate, as.numeric(luxembourg_theta$mean))
mae(luxembourg_test$birth_rate, as.numeric(luxembourg_theta$mean))
mape(luxembourg_test$birth_rate, as.numeric(luxembourg_theta$mean))



#NNAR
select_nnar(luxembourg_train$birth_rate)$best_params
luxembourg_nnar <- nnetar(luxembourg_train$birth_rate, p=5, size=2)

plot(y=luxembourg_train$birth_rate, x=luxembourg_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(luxembourg_nnar), x=luxembourg_train$Year, col='red')

rmse(luxembourg_train$birth_rate, fitted(luxembourg_nnar))
mae(luxembourg_train$birth_rate, fitted(luxembourg_nnar))
mape(luxembourg_train$birth_rate, fitted(luxembourg_nnar))

checkresiduals(luxembourg_nnar) #look okay


forecast_luxembourg_nnar <- forecast::forecast(luxembourg_nnar, h = nrow(luxembourg_test))

plot(y=as.numeric(forecast_luxembourg_nnar$mean), x=luxembourg_test$Year, type='b',
     ylim=c(min(luxembourg_test$birth_rate), max(luxembourg_test$birth_rate)+2),  
     xlab='Year',  ylab='birth_rate', col = "red", main='NNAR')
lines(x=luxembourg_test$Year, y = luxembourg_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(luxembourg_test$birth_rate, as.numeric(forecast_luxembourg_nnar$mean))
mae(luxembourg_test$birth_rate, as.numeric(forecast_luxembourg_nnar$mean))
mape(luxembourg_test$birth_rate, as.numeric(forecast_luxembourg_nnar$mean))

#ARFIMA
luxembourg_arfima <- forecast::arfima(luxembourg_train$birth_rate)

plot(y=luxembourg_train$birth_rate, x=luxembourg_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(y=fitted(luxembourg_arfima), x=luxembourg_train$Year, col='red')

rmse(luxembourg_train$birth_rate, fitted(luxembourg_arfima))
mae(luxembourg_train$birth_rate, fitted(luxembourg_arfima))
mape(luxembourg_train$birth_rate, fitted(luxembourg_arfima))

checkresiduals(luxembourg_arfima)

forecast_luxembourg_arfima <- forecast::forecast(luxembourg_arfima, h = nrow(luxembourg_test))

plot(y=as.numeric(forecast_luxembourg_arfima$mean), x=luxembourg_test$Year, type='b',
     ylim=c(min(luxembourg_test$birth_rate), max(luxembourg_test$birth_rate)+1),
     main='ARFIMA', xlab='Year', ylab='birth_rate', col="red")
lines(x=luxembourg_test$Year, y=luxembourg_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(luxembourg_test$birth_rate, as.numeric(forecast_luxembourg_arfima$mean))
mae(luxembourg_test$birth_rate, as.numeric(forecast_luxembourg_arfima$mean))
mape(luxembourg_test$birth_rate, as.numeric(forecast_luxembourg_arfima$mean))



# Gaussian Process (trend + GP residual)
y_train <- as.numeric(luxembourg_train$birth_rate)
h <- nrow(luxembourg_test)

t_train <- seq_along(y_train)
X_train <- matrix(t_train, ncol=1)

#deterministic trend
trend_lm <- lm(y_train~t_train)
trend_fit <- as.numeric(fitted(trend_lm))
resid_train <- y_train-trend_fit

#GP on residuals
luxembourg_gp <- DiceKriging::km(design=X_train, response=resid_train,
                                 covtype="gauss", nugget.estim=TRUE,
                                 control=list(trace=FALSE))

#training fitted values (trend + GP fitted residual mean)
gp_resid_fit <- as.numeric(predict(luxembourg_gp, newdata=X_train, type="UK")$mean)
luxembourg_gp_fitted_train <- trend_fit+gp_resid_fit

plot(y=luxembourg_train$birth_rate, x=luxembourg_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(y=luxembourg_gp_fitted_train, x=luxembourg_train$Year, col='red')

rmse(luxembourg_train$birth_rate, luxembourg_gp_fitted_train)
mae(luxembourg_train$birth_rate, luxembourg_gp_fitted_train)
mape(luxembourg_train$birth_rate, luxembourg_gp_fitted_train)

# 3) forecast h steps
t_future <- (max(t_train)+1):(max(t_train)+h)
X_future <- matrix(t_future, ncol=1)

gp_pred <- predict(luxembourg_gp, newdata=X_future, type="UK", se.compute = TRUE)

trend_future <- predict(trend_lm, newdata=data.frame(t_train=t_future))

luxembourg_forecast_mean_gp <- as.numeric(trend_future)+as.numeric(gp_pred$mean)

plot(y=luxembourg_forecast_mean_gp, x=luxembourg_test$Year, type='b',
     ylim=c(min(luxembourg_test$birth_rate), max(luxembourg_test$birth_rate)+2),
     main='Gaussian Process', xlab='Year', ylab='birth_rate', col="red")
lines(x=luxembourg_test$Year, y=luxembourg_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(luxembourg_test$birth_rate, luxembourg_forecast_mean_gp)
mae(luxembourg_test$birth_rate, luxembourg_forecast_mean_gp)
mape(luxembourg_test$birth_rate, luxembourg_forecast_mean_gp)

#GAM (smooth trend only)
#smooth trend over Year (k kept small to avoid overfitting short annual series)
luxembourg_gam <- mgcv::gam(birth_rate~s(Year, k=8), data=luxembourg_train, method="REML")

# fitted values (train)
plot(y=luxembourg_train$birth_rate, x=luxembourg_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(x=luxembourg_train$Year, y=fitted(luxembourg_gam), col='red')

rmse(luxembourg_train$birth_rate, fitted(luxembourg_gam))
mae(luxembourg_train$birth_rate, fitted(luxembourg_gam))
mape(luxembourg_train$birth_rate, fitted(luxembourg_gam))

# forecast on test years
forecast_luxembourg_gam <- predict(luxembourg_gam, newdata=luxembourg_test, se.fit=TRUE)
forecast_mean <- as.numeric(forecast_luxembourg_gam$fit)

plot(y=forecast_mean, x=luxembourg_test$Year, type='b',
     ylim=c(min(luxembourg_test$birth_rate), max(luxembourg_test$birth_rate)+2),
     main='GAM', xlab='Year', ylab='birth_rate', col="red")
lines(x=luxembourg_test$Year, y=luxembourg_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(luxembourg_test$birth_rate, forecast_mean)
mae(luxembourg_test$birth_rate, forecast_mean)
mape(luxembourg_test$birth_rate, forecast_mean)

#Netherlands----
#train/test split - 1960-2011/2012-2024
netherlands_train <- Netherlands[1:52,]
netherlands_test <- Netherlands[53:nrow(Netherlands),]

plot(y=Netherlands$birth_rate, x=Netherlands$Year, type='b',  xlab='Year',  ylab='birth_rate')
abline(v=2011, col="red")

#ARIMA
netherlands_arima <- auto.arima(netherlands_train$birth_rate, seasonal=FALSE, max.p=7, 
                                max.q=7)
plot(y=netherlands_train$birth_rate, x=netherlands_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(netherlands_arima), x=netherlands_train$Year, col='red')

rmse(netherlands_train$birth_rate, fitted(netherlands_arima))
mae(netherlands_train$birth_rate, fitted(netherlands_arima))
mape(netherlands_train$birth_rate, fitted(netherlands_arima))

checkresiduals(netherlands_arima) #look okay

#test set
forecast_netherlands_arima <- forecast::forecast(netherlands_arima, h = nrow(netherlands_test))

plot(y=as.numeric(forecast_netherlands_arima$mean), x=netherlands_test$Year, type='b',
     ylim=c(min(netherlands_test$birth_rate)-1, max(netherlands_test$birth_rate)+2),  
     xlab='Year',  ylab='birth_rate', col = "red", main='ARIMA')
lines(x=netherlands_test$Year, y = netherlands_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(netherlands_test$birth_rate, as.numeric(forecast_netherlands_arima$mean))
mae(netherlands_test$birth_rate, as.numeric(forecast_netherlands_arima$mean))
mape(netherlands_test$birth_rate, as.numeric(forecast_netherlands_arima$mean))




#ETS
netherlands_ets <- ets(netherlands_train$birth_rate, model="ZZN") #zzn allows error and trend, but not seasonality

plot(y=netherlands_train$birth_rate, x=netherlands_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(netherlands_ets), x=netherlands_train$Year, col='red')

rmse(netherlands_train$birth_rate, fitted(netherlands_ets))
mae(netherlands_train$birth_rate, fitted(netherlands_ets))
mape(netherlands_train$birth_rate, fitted(netherlands_ets))

checkresiduals(netherlands_ets) #look okay


forecast_netherlands_ets <- forecast::forecast(netherlands_ets, h = nrow(netherlands_test))

plot(y=as.numeric(forecast_netherlands_ets$mean), x=netherlands_test$Year, type='b',
     ylim=c(min(netherlands_test$birth_rate), max(netherlands_test$birth_rate)+1),  
     xlab='Year',  ylab='birth_rate', col = "red", main='ETS')
lines(x=netherlands_test$Year, y = netherlands_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(netherlands_test$birth_rate, as.numeric(forecast_netherlands_ets$mean))
mae(netherlands_test$birth_rate, as.numeric(forecast_netherlands_ets$mean))
mape(netherlands_test$birth_rate, as.numeric(forecast_netherlands_ets$mean))


#theta
netherlands_theta <- thetaf(netherlands_train$birth_rate, h=nrow(netherlands_test))

plot(y=netherlands_train$birth_rate, x=netherlands_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(netherlands_theta), x=netherlands_train$Year, col='red')

rmse(netherlands_train$birth_rate, fitted(netherlands_theta))
mae(netherlands_train$birth_rate, fitted(netherlands_theta))
mape(netherlands_train$birth_rate, fitted(netherlands_theta))

checkresiduals(netherlands_theta) #look okay



plot(y=as.numeric(netherlands_theta$mean), x=netherlands_test$Year, type='b',
     ylim=c(min(netherlands_test$birth_rate), max(netherlands_test$birth_rate)+2),
     xlab='Year',  ylab='birth_rate', col = "red", main='Theta')
lines(x=netherlands_test$Year, y = netherlands_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(netherlands_test$birth_rate, as.numeric(netherlands_theta$mean))
mae(netherlands_test$birth_rate, as.numeric(netherlands_theta$mean))
mape(netherlands_test$birth_rate, as.numeric(netherlands_theta$mean))



#NNAR
select_nnar(netherlands_train$birth_rate)$best_params
netherlands_nnar <- nnetar(netherlands_train$birth_rate, p=4, size=1)

plot(y=netherlands_train$birth_rate, x=netherlands_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(netherlands_nnar), x=netherlands_train$Year, col='red')

rmse(netherlands_train$birth_rate, fitted(netherlands_nnar))
mae(netherlands_train$birth_rate, fitted(netherlands_nnar))
mape(netherlands_train$birth_rate, fitted(netherlands_nnar))

checkresiduals(netherlands_nnar) #look okay


forecast_netherlands_nnar <- forecast::forecast(netherlands_nnar, h = nrow(netherlands_test))

plot(y=as.numeric(forecast_netherlands_nnar$mean), x=netherlands_test$Year, type='b',
     ylim=c(min(netherlands_test$birth_rate), max(netherlands_test$birth_rate)+2),  
     xlab='Year',  ylab='birth_rate', col = "red", main='NNAR')
lines(x=netherlands_test$Year, y = netherlands_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(netherlands_test$birth_rate, as.numeric(forecast_netherlands_nnar$mean))
mae(netherlands_test$birth_rate, as.numeric(forecast_netherlands_nnar$mean))
mape(netherlands_test$birth_rate, as.numeric(forecast_netherlands_nnar$mean))

#ARFIMA
netherlands_arfima <- forecast::arfima(netherlands_train$birth_rate)

plot(y=netherlands_train$birth_rate, x=netherlands_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(y=fitted(netherlands_arfima), x=netherlands_train$Year, col='red')

rmse(netherlands_train$birth_rate, fitted(netherlands_arfima))
mae(netherlands_train$birth_rate, fitted(netherlands_arfima))
mape(netherlands_train$birth_rate, fitted(netherlands_arfima))

checkresiduals(netherlands_arfima)

forecast_netherlands_arfima <- forecast::forecast(netherlands_arfima, h = nrow(netherlands_test))

plot(y=as.numeric(forecast_netherlands_arfima$mean), x=netherlands_test$Year, type='b',
     ylim=c(min(netherlands_test$birth_rate), max(netherlands_test$birth_rate)+2),
     main='ARFIMA', xlab='Year', ylab='birth_rate', col="red")
lines(x=netherlands_test$Year, y=netherlands_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(netherlands_test$birth_rate, as.numeric(forecast_netherlands_arfima$mean))
mae(netherlands_test$birth_rate, as.numeric(forecast_netherlands_arfima$mean))
mape(netherlands_test$birth_rate, as.numeric(forecast_netherlands_arfima$mean))



# Gaussian Process (trend + GP residual)
y_train <- as.numeric(netherlands_train$birth_rate)
h <- nrow(netherlands_test)

t_train <- seq_along(y_train)
X_train <- matrix(t_train, ncol=1)

#deterministic trend
trend_lm <- lm(y_train~t_train)
trend_fit <- as.numeric(fitted(trend_lm))
resid_train <- y_train-trend_fit

#GP on residuals
netherlands_gp <- DiceKriging::km(design=X_train, response=resid_train,
                                  covtype="gauss", nugget.estim=TRUE,
                                  control=list(trace=FALSE))

#training fitted values (trend + GP fitted residual mean)
gp_resid_fit <- as.numeric(predict(netherlands_gp, newdata=X_train, type="UK")$mean)
netherlands_gp_fitted_train <- trend_fit+gp_resid_fit

plot(y=netherlands_train$birth_rate, x=netherlands_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(y=netherlands_gp_fitted_train, x=netherlands_train$Year, col='red')

rmse(netherlands_train$birth_rate, netherlands_gp_fitted_train)
mae(netherlands_train$birth_rate, netherlands_gp_fitted_train)
mape(netherlands_train$birth_rate, netherlands_gp_fitted_train)

# 3) forecast h steps
t_future <- (max(t_train)+1):(max(t_train)+h)
X_future <- matrix(t_future, ncol=1)

gp_pred <- predict(netherlands_gp, newdata=X_future, type="UK", se.compute = TRUE)

trend_future <- predict(trend_lm, newdata=data.frame(t_train=t_future))

netherlands_forecast_mean_gp <- as.numeric(trend_future)+as.numeric(gp_pred$mean)

plot(y=netherlands_forecast_mean_gp, x=netherlands_test$Year, type='b',
     ylim=c(min(netherlands_test$birth_rate)-2, max(netherlands_test$birth_rate)),
     main='Gaussian Process', xlab='Year', ylab='birth_rate', col="red")
lines(x=netherlands_test$Year, y=netherlands_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(netherlands_test$birth_rate, netherlands_forecast_mean_gp)
mae(netherlands_test$birth_rate, netherlands_forecast_mean_gp)
mape(netherlands_test$birth_rate, netherlands_forecast_mean_gp)


#GAM (smooth trend only)
#smooth trend over Year (k kept small to avoid overfitting short annual series)
netherlands_gam <- mgcv::gam(birth_rate~s(Year, k=8), data=netherlands_train, method="REML")

# fitted values (train)
plot(y=netherlands_train$birth_rate, x=netherlands_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(x=netherlands_train$Year, y=fitted(netherlands_gam), col='red')

rmse(netherlands_train$birth_rate, fitted(netherlands_gam))
mae(netherlands_train$birth_rate, fitted(netherlands_gam))
mape(netherlands_train$birth_rate, fitted(netherlands_gam))

# forecast on test years
forecast_netherlands_gam <- predict(netherlands_gam, newdata=netherlands_test, se.fit=TRUE)
forecast_mean <- as.numeric(forecast_netherlands_gam$fit)

plot(y=forecast_mean, x=netherlands_test$Year, type='b',
     ylim=c(min(netherlands_test$birth_rate)-2, max(netherlands_test$birth_rate)),
     main='GAM', xlab='Year', ylab='birth_rate', col="red")
lines(x=netherlands_test$Year, y=netherlands_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(netherlands_test$birth_rate, forecast_mean)
mae(netherlands_test$birth_rate, forecast_mean)
mape(netherlands_test$birth_rate, forecast_mean)


#Portugal----
#train/test split - 1960-2011/2012-2024
portugal_train <- Portugal[1:52,]
portugal_test <- Portugal[53:nrow(Portugal),]

plot(y=Portugal$birth_rate, x=Portugal$Year, type='b',  xlab='Year',  ylab='birth_rate')
abline(v=2011, col="red")

#ARIMA
portugal_arima <- auto.arima(portugal_train$birth_rate, seasonal=FALSE, max.p=7, 
                             max.q=7)
plot(y=portugal_train$birth_rate, x=portugal_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(portugal_arima), x=portugal_train$Year, col='red')

rmse(portugal_train$birth_rate, fitted(portugal_arima))
mae(portugal_train$birth_rate, fitted(portugal_arima))
mape(portugal_train$birth_rate, fitted(portugal_arima))

checkresiduals(portugal_arima) #look okay

#test set
forecast_portugal_arima <- forecast::forecast(portugal_arima, h = nrow(portugal_test))

plot(y=as.numeric(forecast_portugal_arima$mean), x=portugal_test$Year, type='b',
     ylim=c(min(portugal_test$birth_rate)-3, max(portugal_test$birth_rate)+2),  
     xlab='Year',  ylab='birth_rate', col = "red", main='ARIMA')
lines(x=portugal_test$Year, y = portugal_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(portugal_test$birth_rate, as.numeric(forecast_portugal_arima$mean))
mae(portugal_test$birth_rate, as.numeric(forecast_portugal_arima$mean))
mape(portugal_test$birth_rate, as.numeric(forecast_portugal_arima$mean))




#ETS
portugal_ets <- ets(portugal_train$birth_rate, model="ZZN") #zzn allows error and trend, but not seasonality

plot(y=portugal_train$birth_rate, x=portugal_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(portugal_ets), x=portugal_train$Year, col='red')

rmse(portugal_train$birth_rate, fitted(portugal_ets))
mae(portugal_train$birth_rate, fitted(portugal_ets))
mape(portugal_train$birth_rate, fitted(portugal_ets))

checkresiduals(portugal_ets) #look okay


forecast_portugal_ets <- forecast::forecast(portugal_ets, h = nrow(portugal_test))

plot(y=as.numeric(forecast_portugal_ets$mean), x=portugal_test$Year, type='b',
     ylim=c(min(portugal_test$birth_rate), max(portugal_test$birth_rate)+2),  
     xlab='Year',  ylab='birth_rate', col = "red", main='ETS')
lines(x=portugal_test$Year, y = portugal_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(portugal_test$birth_rate, as.numeric(forecast_portugal_ets$mean))
mae(portugal_test$birth_rate, as.numeric(forecast_portugal_ets$mean))
mape(portugal_test$birth_rate, as.numeric(forecast_portugal_ets$mean))


#theta
portugal_theta <- thetaf(portugal_train$birth_rate, h=nrow(portugal_test))

plot(y=portugal_train$birth_rate, x=portugal_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(portugal_theta), x=portugal_train$Year, col='red')

rmse(portugal_train$birth_rate, fitted(portugal_theta))
mae(portugal_train$birth_rate, fitted(portugal_theta))
mape(portugal_train$birth_rate, fitted(portugal_theta))

checkresiduals(portugal_theta) #look okay



plot(y=as.numeric(portugal_theta$mean), x=portugal_test$Year, type='b',
     ylim=c(min(portugal_test$birth_rate)-2, max(portugal_test$birth_rate)+2),  
     xlab='Year',  ylab='birth_rate', col = "red", main='Theta')
lines(x=portugal_test$Year, y = portugal_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(portugal_test$birth_rate, as.numeric(portugal_theta$mean))
mae(portugal_test$birth_rate, as.numeric(portugal_theta$mean))
mape(portugal_test$birth_rate, as.numeric(portugal_theta$mean))



#NNAR
select_nnar(portugal_train$birth_rate)$best_params
portugal_nnar <- nnetar(portugal_train$birth_rate, p=5, size=1)

plot(y=portugal_train$birth_rate, x=portugal_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(portugal_nnar), x=portugal_train$Year, col='red')

rmse(portugal_train$birth_rate, fitted(portugal_nnar))
mae(portugal_train$birth_rate, fitted(portugal_nnar))
mape(portugal_train$birth_rate, fitted(portugal_nnar))

checkresiduals(portugal_nnar) #look okay


forecast_portugal_nnar <- forecast::forecast(portugal_nnar, h = nrow(portugal_test))

plot(y=as.numeric(forecast_portugal_nnar$mean), x=portugal_test$Year, type='b',
     ylim=c(min(portugal_test$birth_rate), max(portugal_test$birth_rate)+2),  
     xlab='Year',  ylab='birth_rate', col = "red", main='NNAR')
lines(x=portugal_test$Year, y = portugal_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(portugal_test$birth_rate, as.numeric(forecast_portugal_nnar$mean))
mae(portugal_test$birth_rate, as.numeric(forecast_portugal_nnar$mean))
mape(portugal_test$birth_rate, as.numeric(forecast_portugal_nnar$mean))

#ARFIMA
portugal_arfima <- forecast::arfima(portugal_train$birth_rate)

plot(y=portugal_train$birth_rate, x=portugal_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(y=fitted(portugal_arfima), x=portugal_train$Year, col='red')

rmse(portugal_train$birth_rate, fitted(portugal_arfima))
mae(portugal_train$birth_rate, fitted(portugal_arfima))
mape(portugal_train$birth_rate, fitted(portugal_arfima))

checkresiduals(portugal_arfima)

forecast_portugal_arfima <- forecast::forecast(portugal_arfima, h = nrow(portugal_test))

plot(y=as.numeric(forecast_portugal_arfima$mean), x=portugal_test$Year, type='b',
     ylim=c(min(portugal_test$birth_rate), max(portugal_test$birth_rate)+4),
     main='ARFIMA', xlab='Year', ylab='birth_rate', col="red")
lines(x=portugal_test$Year, y=portugal_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(portugal_test$birth_rate, as.numeric(forecast_portugal_arfima$mean))
mae(portugal_test$birth_rate, as.numeric(forecast_portugal_arfima$mean))
mape(portugal_test$birth_rate, as.numeric(forecast_portugal_arfima$mean))



# Gaussian Process (trend + GP residual)
y_train <- as.numeric(portugal_train$birth_rate)
h <- nrow(portugal_test)

t_train <- seq_along(y_train)
X_train <- matrix(t_train, ncol=1)

#deterministic trend
trend_lm <- lm(y_train~t_train)
trend_fit <- as.numeric(fitted(trend_lm))
resid_train <- y_train-trend_fit

#GP on residuals
portugal_gp <- DiceKriging::km(design=X_train, response=resid_train,
                               covtype="gauss", nugget.estim=TRUE,
                               control=list(trace=FALSE))

#training fitted values (trend + GP fitted residual mean)
gp_resid_fit <- as.numeric(predict(portugal_gp, newdata=X_train, type="UK")$mean)
portugal_gp_fitted_train <- trend_fit+gp_resid_fit

plot(y=portugal_train$birth_rate, x=portugal_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(y=portugal_gp_fitted_train, x=portugal_train$Year, col='red')

rmse(portugal_train$birth_rate, portugal_gp_fitted_train)
mae(portugal_train$birth_rate, portugal_gp_fitted_train)
mape(portugal_train$birth_rate, portugal_gp_fitted_train)

# 3) forecast h steps
t_future <- (max(t_train)+1):(max(t_train)+h)
X_future <- matrix(t_future, ncol=1)

gp_pred <- predict(portugal_gp, newdata=X_future, type="UK", se.compute = TRUE)

trend_future <- predict(trend_lm, newdata=data.frame(t_train=t_future))

portugal_forecast_mean_gp <- as.numeric(trend_future)+as.numeric(gp_pred$mean)

plot(y=portugal_forecast_mean_gp, x=portugal_test$Year, type='b',
     ylim=c(min(portugal_test$birth_rate)-8, max(portugal_test$birth_rate)+2),
     main='Gaussian Process', xlab='Year', ylab='birth_rate', col="red")
lines(x=portugal_test$Year, y=portugal_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(portugal_test$birth_rate, portugal_forecast_mean_gp)
mae(portugal_test$birth_rate, portugal_forecast_mean_gp)
mape(portugal_test$birth_rate, portugal_forecast_mean_gp)


#GAM (smooth trend only)
#smooth trend over Year (k kept small to avoid overfitting short annual series)
portugal_gam <- mgcv::gam(birth_rate~s(Year, k=8), data=portugal_train, method="REML")

# fitted values (train)
plot(y=portugal_train$birth_rate, x=portugal_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(x=portugal_train$Year, y=fitted(portugal_gam), col='red')

rmse(portugal_train$birth_rate, fitted(portugal_gam))
mae(portugal_train$birth_rate, fitted(portugal_gam))
mape(portugal_train$birth_rate, fitted(portugal_gam))

# forecast on test years
forecast_portugal_gam <- predict(portugal_gam, newdata=portugal_test, se.fit=TRUE)
forecast_mean <- as.numeric(forecast_portugal_gam$fit)

plot(y=forecast_mean, x=portugal_test$Year, type='b',
     ylim=c(min(portugal_test$birth_rate)-1, max(portugal_test$birth_rate)+1),
     main='GAM', xlab='Year', ylab='birth_rate', col="red")
lines(x=portugal_test$Year, y=portugal_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(portugal_test$birth_rate, forecast_mean)
mae(portugal_test$birth_rate, forecast_mean)
mape(portugal_test$birth_rate, forecast_mean)

#Spain----
#train/test split - 1960-2011/2012-2024
spain_train <- Spain[1:52,]
spain_test <- Spain[53:nrow(Spain),]

plot(y=Spain$birth_rate, x=Spain$Year, type='b',  xlab='Year',  ylab='birth_rate')
abline(v=2011, col="red")

#ARIMA
spain_arima <- auto.arima(spain_train$birth_rate, seasonal=FALSE, max.p=7, 
                          max.q=7)
plot(y=spain_train$birth_rate, x=spain_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(spain_arima), x=spain_train$Year, col='red')

rmse(spain_train$birth_rate, fitted(spain_arima))
mae(spain_train$birth_rate, fitted(spain_arima))
mape(spain_train$birth_rate, fitted(spain_arima))

checkresiduals(spain_arima) #look okay

#test set
forecast_spain_arima <- forecast::forecast(spain_arima, h = nrow(spain_test))

plot(y=as.numeric(forecast_spain_arima$mean), x=spain_test$Year, type='b',
     ylim=c(min(spain_test$birth_rate), max(spain_test$birth_rate)+1),  
     xlab='Year',  ylab='birth_rate', col = "red", main='ARIMA')
lines(x=spain_test$Year, y = spain_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(spain_test$birth_rate, as.numeric(forecast_spain_arima$mean))
mae(spain_test$birth_rate, as.numeric(forecast_spain_arima$mean))
mape(spain_test$birth_rate, as.numeric(forecast_spain_arima$mean))




#ETS
spain_ets <- ets(spain_train$birth_rate, model="ZZN") #zzn allows error and trend, but not seasonality

plot(y=spain_train$birth_rate, x=spain_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(spain_ets), x=spain_train$Year, col='red')

rmse(spain_train$birth_rate, fitted(spain_ets))
mae(spain_train$birth_rate, fitted(spain_ets))
mape(spain_train$birth_rate, fitted(spain_ets))

checkresiduals(spain_ets) #look okay


forecast_spain_ets <- forecast::forecast(spain_ets, h = nrow(spain_test))

plot(y=as.numeric(forecast_spain_ets$mean), x=spain_test$Year, type='b',
     ylim=c(min(spain_test$birth_rate), max(spain_test$birth_rate)+1),  
     xlab='Year',  ylab='birth_rate', col = "red", main='ETS')
lines(x=spain_test$Year, y = spain_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(spain_test$birth_rate, as.numeric(forecast_spain_ets$mean))
mae(spain_test$birth_rate, as.numeric(forecast_spain_ets$mean))
mape(spain_test$birth_rate, as.numeric(forecast_spain_ets$mean))


#theta
spain_theta <- thetaf(spain_train$birth_rate, h=nrow(spain_test))

plot(y=spain_train$birth_rate, x=spain_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(spain_theta), x=spain_train$Year, col='red')

rmse(spain_train$birth_rate, fitted(spain_theta))
mae(spain_train$birth_rate, fitted(spain_theta))
mape(spain_train$birth_rate, fitted(spain_theta))

checkresiduals(spain_theta) #look okay



plot(y=as.numeric(spain_theta$mean), x=spain_test$Year, type='b',
     ylim=c(min(spain_test$birth_rate), max(spain_test$birth_rate)+1),  
     xlab='Year',  ylab='birth_rate', col = "red", main='Theta')
lines(x=spain_test$Year, y = spain_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(spain_test$birth_rate, as.numeric(spain_theta$mean))
mae(spain_test$birth_rate, as.numeric(spain_theta$mean))
mape(spain_test$birth_rate, as.numeric(spain_theta$mean))



#NNAR
select_nnar(spain_train$birth_rate)$best_params
spain_nnar <- nnetar(spain_train$birth_rate, p=4, size=5)

plot(y=spain_train$birth_rate, x=spain_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(spain_nnar), x=spain_train$Year, col='red')

rmse(spain_train$birth_rate, fitted(spain_nnar))
mae(spain_train$birth_rate, fitted(spain_nnar))
mape(spain_train$birth_rate, fitted(spain_nnar))

checkresiduals(spain_nnar) #look okay


forecast_spain_nnar <- forecast::forecast(spain_nnar, h = nrow(spain_test))

plot(y=as.numeric(forecast_spain_nnar$mean), x=spain_test$Year, type='b',
     ylim=c(min(spain_test$birth_rate), max(spain_test$birth_rate)+2),  
     xlab='Year',  ylab='birth_rate', col="red", main='NNAR')
lines(x=spain_test$Year, y = spain_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(spain_test$birth_rate, as.numeric(forecast_spain_nnar$mean))
mae(spain_test$birth_rate, as.numeric(forecast_spain_nnar$mean))
mape(spain_test$birth_rate, as.numeric(forecast_spain_nnar$mean))


#ARFIMA
spain_arfima <- forecast::arfima(spain_train$birth_rate)

plot(y=spain_train$birth_rate, x=spain_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(y=fitted(spain_arfima), x=spain_train$Year, col='red')

rmse(spain_train$birth_rate, fitted(spain_arfima))
mae(spain_train$birth_rate, fitted(spain_arfima))
mape(spain_train$birth_rate, fitted(spain_arfima))

checkresiduals(spain_arfima)

forecast_spain_arfima <- forecast::forecast(spain_arfima, h = nrow(spain_test))

plot(y=as.numeric(forecast_spain_arfima$mean), x=spain_test$Year, type='b',
     ylim=c(min(spain_test$birth_rate)-2, max(spain_test$birth_rate)+2),
     main='ARFIMA', xlab='Year', ylab='birth_rate', col="red")
lines(x=spain_test$Year, y=spain_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(spain_test$birth_rate, as.numeric(forecast_spain_arfima$mean))
mae(spain_test$birth_rate, as.numeric(forecast_spain_arfima$mean))
mape(spain_test$birth_rate, as.numeric(forecast_spain_arfima$mean))



# Gaussian Process (trend + GP residual)
y_train <- as.numeric(spain_train$birth_rate)
h <- nrow(spain_test)

t_train <- seq_along(y_train)
X_train <- matrix(t_train, ncol=1)

#deterministic trend
trend_lm <- lm(y_train~t_train)
trend_fit <- as.numeric(fitted(trend_lm))
resid_train <- y_train-trend_fit

#GP on residuals
spain_gp <- DiceKriging::km(design=X_train, response=resid_train,
                            covtype="gauss", nugget.estim=TRUE,
                            control=list(trace=FALSE))

#training fitted values (trend + GP fitted residual mean)
gp_resid_fit <- as.numeric(predict(spain_gp, newdata=X_train, type="UK")$mean)
spain_gp_fitted_train <- trend_fit+gp_resid_fit

plot(y=spain_train$birth_rate, x=spain_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(y=spain_gp_fitted_train, x=spain_train$Year, col='red')

rmse(spain_train$birth_rate, spain_gp_fitted_train)
mae(spain_train$birth_rate, spain_gp_fitted_train)
mape(spain_train$birth_rate, spain_gp_fitted_train)

# 3) forecast h steps
t_future <- (max(t_train)+1):(max(t_train)+h)
X_future <- matrix(t_future, ncol=1)

gp_pred <- predict(spain_gp, newdata=X_future, type="UK", se.compute = TRUE)

trend_future <- predict(trend_lm, newdata=data.frame(t_train=t_future))

spain_forecast_mean_gp <- as.numeric(trend_future)+as.numeric(gp_pred$mean)

plot(y=spain_forecast_mean_gp, x=spain_test$Year, type='b',
     ylim=c(min(spain_test$birth_rate)-4, max(spain_test$birth_rate)),
     main='Gaussian Process', xlab='Year', ylab='birth_rate', col="red")
lines(x=spain_test$Year, y=spain_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(spain_test$birth_rate, spain_forecast_mean_gp)
mae(spain_test$birth_rate, spain_forecast_mean_gp)
mape(spain_test$birth_rate, spain_forecast_mean_gp)

#GAM (smooth trend only)
#smooth trend over Year (k kept small to avoid overfitting short annual series)
spain_gam <- mgcv::gam(birth_rate~s(Year, k=8), data=spain_train, method="REML")

# fitted values (train)
plot(y=spain_train$birth_rate, x=spain_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(x=spain_train$Year, y=fitted(spain_gam), col='red')

rmse(spain_train$birth_rate, fitted(spain_gam))
mae(spain_train$birth_rate, fitted(spain_gam))
mape(spain_train$birth_rate, fitted(spain_gam))

# forecast on test years
forecast_spain_gam <- predict(spain_gam, newdata=spain_test, se.fit=TRUE)
forecast_mean <- as.numeric(forecast_spain_gam$fit)

plot(y=forecast_mean, x=spain_test$Year, type='b',
     ylim=c(min(spain_test$birth_rate), max(spain_test$birth_rate)+2),
     main='GAM', xlab='Year', ylab='birth_rate', col="red")
lines(x=spain_test$Year, y=spain_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(spain_test$birth_rate, forecast_mean)
mae(spain_test$birth_rate, forecast_mean)
mape(spain_test$birth_rate, forecast_mean)

#Sweden----
#train/test split - 1960-2011/2012-2024
sweden_train <- Sweden[1:52,]
sweden_test <- Sweden[53:nrow(Sweden),]

plot(y=Sweden$birth_rate, x=Sweden$Year, type='b',  xlab='Year',  ylab='birth_rate')
abline(v=2011, col="red")

#ARIMA
sweden_arima <- auto.arima(sweden_train$birth_rate, seasonal=FALSE, max.p=7, 
                           max.q=7)
plot(y=sweden_train$birth_rate, x=sweden_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(sweden_arima), x=sweden_train$Year, col='red')

rmse(sweden_train$birth_rate, fitted(sweden_arima))
mae(sweden_train$birth_rate, fitted(sweden_arima))
mape(sweden_train$birth_rate, fitted(sweden_arima))

checkresiduals(sweden_arima) #look okay

#test set
forecast_sweden_arima <- forecast::forecast(sweden_arima, h = nrow(sweden_test))

plot(y=as.numeric(forecast_sweden_arima$mean), x=sweden_test$Year, type='b',
     ylim=c(min(sweden_test$birth_rate), max(sweden_test$birth_rate)),  
     xlab='Year',  ylab='birth_rate', col = "red", main='ARIMA')
lines(x=sweden_test$Year, y = sweden_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(sweden_test$birth_rate, as.numeric(forecast_sweden_arima$mean))
mae(sweden_test$birth_rate, as.numeric(forecast_sweden_arima$mean))
mape(sweden_test$birth_rate, as.numeric(forecast_sweden_arima$mean))




#ETS
sweden_ets <- ets(sweden_train$birth_rate, model="ZZN") #zzn allows error and trend, but not seasonality

plot(y=sweden_train$birth_rate, x=sweden_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(sweden_ets), x=sweden_train$Year, col='red')

rmse(sweden_train$birth_rate, fitted(sweden_ets))
mae(sweden_train$birth_rate, fitted(sweden_ets))
mape(sweden_train$birth_rate, fitted(sweden_ets))

checkresiduals(sweden_ets) #look okay


forecast_sweden_ets <- forecast::forecast(sweden_ets, h = nrow(sweden_test))

plot(y=as.numeric(forecast_sweden_ets$mean), x=sweden_test$Year, type='b',
     ylim=c(min(sweden_test$birth_rate), max(sweden_test$birth_rate)),  
     xlab='Year',  ylab='birth_rate', col = "red", main='ETS')
lines(x=sweden_test$Year, y = sweden_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(sweden_test$birth_rate, as.numeric(forecast_sweden_ets$mean))
mae(sweden_test$birth_rate, as.numeric(forecast_sweden_ets$mean))
mape(sweden_test$birth_rate, as.numeric(forecast_sweden_ets$mean))


#theta
sweden_theta <- thetaf(sweden_train$birth_rate, h=nrow(sweden_test))

plot(y=sweden_train$birth_rate, x=sweden_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(sweden_theta), x=sweden_train$Year, col='red')

rmse(sweden_train$birth_rate, fitted(sweden_theta))
mae(sweden_train$birth_rate, fitted(sweden_theta))
mape(sweden_train$birth_rate, fitted(sweden_theta))

checkresiduals(sweden_theta) #look okay



plot(y=as.numeric(sweden_theta$mean), x=sweden_test$Year, type='b',
     ylim=c(min(sweden_test$birth_rate), max(sweden_test$birth_rate)),
     xlab='Year',  ylab='birth_rate', col = "red", main='Theta')
lines(x=sweden_test$Year, y = sweden_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(sweden_test$birth_rate, as.numeric(sweden_theta$mean))
mae(sweden_test$birth_rate, as.numeric(sweden_theta$mean))
mape(sweden_test$birth_rate, as.numeric(sweden_theta$mean))



#NNAR
select_nnar(sweden_train$birth_rate)$best_params
sweden_nnar <- nnetar(sweden_train$birth_rate, p=2, size=4)

plot(y=sweden_train$birth_rate, x=sweden_train$Year, type='b',  xlab='Year',  ylab='birth_rate')
lines(y=fitted(sweden_nnar), x=sweden_train$Year, col='red')

rmse(sweden_train$birth_rate, fitted(sweden_nnar))
mae(sweden_train$birth_rate, fitted(sweden_nnar))
mape(sweden_train$birth_rate, fitted(sweden_nnar))

checkresiduals(sweden_nnar) #look okay


forecast_sweden_nnar <- forecast::forecast(sweden_nnar, h = nrow(sweden_test))

plot(y=as.numeric(forecast_sweden_nnar$mean), x=sweden_test$Year, type='b',
     ylim=c(min(sweden_test$birth_rate), max(sweden_test$birth_rate)+3),  
     xlab='Year',  ylab='birth_rate', col = "red", main='NNAR')
lines(x=sweden_test$Year, y = sweden_test$birth_rate, type = "b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)
rmse(sweden_test$birth_rate, as.numeric(forecast_sweden_nnar$mean))
mae(sweden_test$birth_rate, as.numeric(forecast_sweden_nnar$mean))
mape(sweden_test$birth_rate, as.numeric(forecast_sweden_nnar$mean))

#ARFIMA
sweden_arfima <- forecast::arfima(sweden_train$birth_rate)

plot(y=sweden_train$birth_rate, x=sweden_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(y=fitted(sweden_arfima), x=sweden_train$Year, col='red')

rmse(sweden_train$birth_rate, fitted(sweden_arfima))
mae(sweden_train$birth_rate, fitted(sweden_arfima))
mape(sweden_train$birth_rate, fitted(sweden_arfima))

checkresiduals(sweden_arfima)

forecast_sweden_arfima <- forecast::forecast(sweden_arfima, h = nrow(sweden_test))

plot(y=as.numeric(forecast_sweden_arfima$mean), x=sweden_test$Year, type='b',
     ylim=c(min(sweden_test$birth_rate)-3, max(sweden_test$birth_rate)+2),
     main='ARFIMA', xlab='Year', ylab='birth_rate', col="red")
lines(x=sweden_test$Year, y=sweden_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(sweden_test$birth_rate, as.numeric(forecast_sweden_arfima$mean))
mae(sweden_test$birth_rate, as.numeric(forecast_sweden_arfima$mean))
mape(sweden_test$birth_rate, as.numeric(forecast_sweden_arfima$mean))


# Gaussian Process (trend + GP residual)
y_train <- as.numeric(sweden_train$birth_rate)
h <- nrow(sweden_test)

t_train <- seq_along(y_train)
X_train <- matrix(t_train, ncol=1)

#deterministic trend
trend_lm <- lm(y_train~t_train)
trend_fit <- as.numeric(fitted(trend_lm))
resid_train <- y_train-trend_fit

#GP on residuals
sweden_gp <- DiceKriging::km(design=X_train, response=resid_train,
                             covtype="gauss", nugget.estim=TRUE,
                             control=list(trace=FALSE))

#training fitted values (trend + GP fitted residual mean)
gp_resid_fit <- as.numeric(predict(sweden_gp, newdata=X_train, type="UK")$mean)
sweden_gp_fitted_train <- trend_fit+gp_resid_fit

plot(y=sweden_train$birth_rate, x=sweden_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(y=sweden_gp_fitted_train, x=sweden_train$Year, col='red')

rmse(sweden_train$birth_rate, sweden_gp_fitted_train)
mae(sweden_train$birth_rate, sweden_gp_fitted_train)
mape(sweden_train$birth_rate, sweden_gp_fitted_train)

# 3) forecast h steps
t_future <- (max(t_train)+1):(max(t_train)+h)
X_future <- matrix(t_future, ncol=1)

gp_pred <- predict(sweden_gp, newdata=X_future, type="UK", se.compute = TRUE)

trend_future <- predict(trend_lm, newdata=data.frame(t_train=t_future))

sweden_forecast_mean_gp <- as.numeric(trend_future)+as.numeric(gp_pred$mean)

plot(y=sweden_forecast_mean_gp, x=sweden_test$Year, type='b',
     ylim=c(min(sweden_test$birth_rate), max(sweden_test$birth_rate)+2),
     main='Gaussian Process', xlab='Year', ylab='birth_rate', col="red")
lines(x=sweden_test$Year, y=sweden_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(sweden_test$birth_rate, sweden_forecast_mean_gp)
mae(sweden_test$birth_rate, sweden_forecast_mean_gp)
mape(sweden_test$birth_rate, sweden_forecast_mean_gp)


#GAM (smooth trend only)
#smooth trend over Year (k kept small to avoid overfitting short annual series)
sweden_gam <- mgcv::gam(birth_rate~s(Year, k=8), data=sweden_train, method="REML")

# fitted values (train)
plot(y=sweden_train$birth_rate, x=sweden_train$Year, type='b', xlab='Year', ylab='birth_rate')
lines(x=sweden_train$Year, y=fitted(sweden_gam), col='red')

rmse(sweden_train$birth_rate, fitted(sweden_gam))
mae(sweden_train$birth_rate, fitted(sweden_gam))
mape(sweden_train$birth_rate, fitted(sweden_gam))

# forecast on test years
forecast_sweden_gam <- predict(sweden_gam, newdata=sweden_test, se.fit=TRUE)
forecast_mean <- as.numeric(forecast_sweden_gam$fit)

plot(y=forecast_mean, x=sweden_test$Year, type='b',
     ylim=c(min(sweden_test$birth_rate), max(sweden_test$birth_rate)+3),
     main='GAM', xlab='Year', ylab='birth_rate', col="red")
lines(x=sweden_test$Year, y=sweden_test$birth_rate, type="b")
legend("topleft", legend=c("Forecast", "Actual"), col=c("red", "black"), lty=1)

rmse(sweden_test$birth_rate, forecast_mean)
mae(sweden_test$birth_rate, forecast_mean)
mape(sweden_test$birth_rate, forecast_mean)


#compile results----
results <- data.frame(Country=character(), Model=character(), 
                      Train_MAPE=numeric(), Horizon_1_3_MAPE=numeric(),
                      Horizon_1_5_MAPE=numeric(),Horizon_Full_MAPE=numeric(),
                      stringsAsFactors=FALSE)

results <- rbind(results, data.frame(Country = "Austria", Model = "ARIMA",
                                     Train_MAPE = mape(austria_train$birth_rate, fitted(austria_arima)),
                                     Horizon_1_3_MAPE = mape(austria_test$birth_rate[1:3], as.numeric(forecast_austria_arima$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(austria_test$birth_rate[1:5], as.numeric(forecast_austria_arima$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(austria_test$birth_rate, as.numeric(forecast_austria_arima$mean))
))

results <- rbind(results, data.frame(Country = "Austria", Model = "ETS",
                                     Train_MAPE = mape(austria_train$birth_rate, fitted(austria_ets)),
                                     Horizon_1_3_MAPE = mape(austria_test$birth_rate[1:3], as.numeric(forecast_austria_ets$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(austria_test$birth_rate[1:5], as.numeric(forecast_austria_ets$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(austria_test$birth_rate, as.numeric(forecast_austria_ets$mean))
))

results <- rbind(results, data.frame(Country = "Austria", Model = "Theta",
                                     Train_MAPE = mape(austria_train$birth_rate, fitted(austria_theta)),
                                     Horizon_1_3_MAPE = mape(austria_test$birth_rate[1:3], as.numeric(austria_theta$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(austria_test$birth_rate[1:5], as.numeric(austria_theta$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(austria_test$birth_rate, as.numeric(austria_theta$mean))
))

results <- rbind(results, data.frame(Country = "Austria", Model = "NNAR",
                                     Train_MAPE = mape(austria_train$birth_rate[(austria_nnar$p+1):24], fitted(austria_nnar)[(austria_nnar$p+1):24]),
                                     Horizon_1_3_MAPE = mape(austria_test$birth_rate[1:3], as.numeric(forecast_austria_nnar$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(austria_test$birth_rate[1:5], as.numeric(forecast_austria_nnar$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(austria_test$birth_rate, as.numeric(forecast_austria_nnar$mean))
))

results <- rbind(results, data.frame(Country = "Austria", Model = "ARFIMA",
                                     Train_MAPE = mape(austria_train$birth_rate, fitted(austria_arfima)),
                                     Horizon_1_3_MAPE = mape(austria_test$birth_rate[1:3], as.numeric(forecast_austria_arfima$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(austria_test$birth_rate[1:5], as.numeric(forecast_austria_arfima$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(austria_test$birth_rate, as.numeric(forecast_austria_arfima$mean))
))

results <- rbind(results, data.frame(Country = "Austria", Model = "GP",
                                     Train_MAPE = mape(austria_train$birth_rate, austria_gp_fitted_train),
                                     Horizon_1_3_MAPE = mape(austria_test$birth_rate[1:3], as.numeric(austria_forecast_mean_gp)[1:3]),
                                     Horizon_1_5_MAPE = mape(austria_test$birth_rate[1:5], as.numeric(austria_forecast_mean_gp)[1:5]),
                                     Horizon_Full_MAPE = mape(austria_test$birth_rate, as.numeric(austria_forecast_mean_gp))
))
results <- rbind(results, data.frame(Country = "Austria", Model = "GAM",
                                     Train_MAPE = mape(austria_train$birth_rate, fitted(austria_gam)),
                                     Horizon_1_3_MAPE = mape(austria_test$birth_rate[1:3], as.numeric(forecast_austria_gam$fit)[1:3]),
                                     Horizon_1_5_MAPE = mape(austria_test$birth_rate[1:5], as.numeric(forecast_austria_gam$fit)[1:5]),
                                     Horizon_Full_MAPE = mape(austria_test$birth_rate, as.numeric(forecast_austria_gam$fit))
))


results <- rbind(results, data.frame(Country = "Belgium", Model = "ARIMA",
                                     Train_MAPE = mape(belgium_train$birth_rate, fitted(belgium_arima)),
                                     Horizon_1_3_MAPE = mape(belgium_test$birth_rate[1:3], as.numeric(forecast_belgium_arima$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(belgium_test$birth_rate[1:5], as.numeric(forecast_belgium_arima$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(belgium_test$birth_rate, as.numeric(forecast_belgium_arima$mean))
))

results <- rbind(results, data.frame(Country = "Belgium", Model = "ETS",
                                     Train_MAPE = mape(belgium_train$birth_rate, fitted(belgium_ets)),
                                     Horizon_1_3_MAPE = mape(belgium_test$birth_rate[1:3], as.numeric(forecast_belgium_ets$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(belgium_test$birth_rate[1:5], as.numeric(forecast_belgium_ets$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(belgium_test$birth_rate, as.numeric(forecast_belgium_ets$mean))
))

results <- rbind(results, data.frame(Country = "Belgium", Model = "Theta",
                                     Train_MAPE = mape(belgium_train$birth_rate, fitted(belgium_theta)),
                                     Horizon_1_3_MAPE = mape(belgium_test$birth_rate[1:3], as.numeric(belgium_theta$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(belgium_test$birth_rate[1:5], as.numeric(belgium_theta$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(belgium_test$birth_rate, as.numeric(belgium_theta$mean))
))

results <- rbind(results, data.frame(Country = "Belgium", Model = "NNAR",
                                     Train_MAPE = mape(belgium_train$birth_rate[(belgium_nnar$p+1):24], fitted(belgium_nnar)[(belgium_nnar$p+1):24]),
                                     Horizon_1_3_MAPE = mape(belgium_test$birth_rate[1:3], as.numeric(forecast_belgium_nnar$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(belgium_test$birth_rate[1:5], as.numeric(forecast_belgium_nnar$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(belgium_test$birth_rate, as.numeric(forecast_belgium_nnar$mean))
))

results <- rbind(results, data.frame(Country = "Belgium", Model = "ARFIMA",
                                     Train_MAPE = mape(belgium_train$birth_rate, fitted(belgium_arfima)),
                                     Horizon_1_3_MAPE = mape(belgium_test$birth_rate[1:3], as.numeric(forecast_belgium_arfima$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(belgium_test$birth_rate[1:5], as.numeric(forecast_belgium_arfima$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(belgium_test$birth_rate, as.numeric(forecast_belgium_arfima$mean))
))

results <- rbind(results, data.frame(Country = "Belgium", Model = "GP",
                                     Train_MAPE = mape(belgium_train$birth_rate, belgium_gp_fitted_train),
                                     Horizon_1_3_MAPE = mape(belgium_test$birth_rate[1:3], as.numeric(belgium_forecast_mean_gp)[1:3]),
                                     Horizon_1_5_MAPE = mape(belgium_test$birth_rate[1:5], as.numeric(belgium_forecast_mean_gp)[1:5]),
                                     Horizon_Full_MAPE = mape(belgium_test$birth_rate, as.numeric(belgium_forecast_mean_gp))
))

results <- rbind(results, data.frame(Country = "Belgium", Model = "GAM",
                                     Train_MAPE = mape(belgium_train$birth_rate, fitted(belgium_gam)),
                                     Horizon_1_3_MAPE = mape(belgium_test$birth_rate[1:3], as.numeric(forecast_belgium_gam$fit)[1:3]),
                                     Horizon_1_5_MAPE = mape(belgium_test$birth_rate[1:5], as.numeric(forecast_belgium_gam$fit)[1:5]),
                                     Horizon_Full_MAPE = mape(belgium_test$birth_rate, as.numeric(forecast_belgium_gam$fit))
))

results <- rbind(results, data.frame(Country = "Denmark", Model = "ARIMA",
                                     Train_MAPE = mape(denmark_train$birth_rate, fitted(denmark_arima)),
                                     Horizon_1_3_MAPE = mape(denmark_test$birth_rate[1:3], as.numeric(forecast_denmark_arima$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(denmark_test$birth_rate[1:5], as.numeric(forecast_denmark_arima$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(denmark_test$birth_rate, as.numeric(forecast_denmark_arima$mean))
))

results <- rbind(results, data.frame(Country = "Denmark", Model = "ETS",
                                     Train_MAPE = mape(denmark_train$birth_rate, fitted(denmark_ets)),
                                     Horizon_1_3_MAPE = mape(denmark_test$birth_rate[1:3], as.numeric(forecast_denmark_ets$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(denmark_test$birth_rate[1:5], as.numeric(forecast_denmark_ets$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(denmark_test$birth_rate, as.numeric(forecast_denmark_ets$mean))
))

results <- rbind(results, data.frame(Country = "Denmark", Model = "Theta",
                                     Train_MAPE = mape(denmark_train$birth_rate, fitted(denmark_theta)),
                                     Horizon_1_3_MAPE = mape(denmark_test$birth_rate[1:3], as.numeric(denmark_theta$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(denmark_test$birth_rate[1:5], as.numeric(denmark_theta$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(denmark_test$birth_rate, as.numeric(denmark_theta$mean))
))

results <- rbind(results, data.frame(Country = "Denmark", Model = "NNAR",
                                     Train_MAPE = mape(denmark_train$birth_rate[(denmark_nnar$p+1):24], fitted(denmark_nnar)[(denmark_nnar$p+1):24]),
                                     Horizon_1_3_MAPE = mape(denmark_test$birth_rate[1:3], as.numeric(forecast_denmark_nnar$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(denmark_test$birth_rate[1:5], as.numeric(forecast_denmark_nnar$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(denmark_test$birth_rate, as.numeric(forecast_denmark_nnar$mean))
))

results <- rbind(results, data.frame(Country = "Denmark", Model = "ARFIMA",
                                     Train_MAPE = mape(denmark_train$birth_rate, fitted(denmark_arfima)),
                                     Horizon_1_3_MAPE = mape(denmark_test$birth_rate[1:3], as.numeric(forecast_denmark_arfima$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(denmark_test$birth_rate[1:5], as.numeric(forecast_denmark_arfima$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(denmark_test$birth_rate, as.numeric(forecast_denmark_arfima$mean))
))

results <- rbind(results, data.frame(Country = "Denmark", Model = "GP",
                                     Train_MAPE = mape(denmark_train$birth_rate, denmark_gp_fitted_train),
                                     Horizon_1_3_MAPE = mape(denmark_test$birth_rate[1:3], as.numeric(denmark_forecast_mean_gp)[1:3]),
                                     Horizon_1_5_MAPE = mape(denmark_test$birth_rate[1:5], as.numeric(denmark_forecast_mean_gp)[1:5]),
                                     Horizon_Full_MAPE = mape(denmark_test$birth_rate, as.numeric(denmark_forecast_mean_gp))
))

results <- rbind(results, data.frame(Country = "Denmark", Model = "GAM",
                                     Train_MAPE = mape(denmark_train$birth_rate, fitted(denmark_gam)),
                                     Horizon_1_3_MAPE = mape(denmark_test$birth_rate[1:3], as.numeric(forecast_denmark_gam$fit)[1:3]),
                                     Horizon_1_5_MAPE = mape(denmark_test$birth_rate[1:5], as.numeric(forecast_denmark_gam$fit)[1:5]),
                                     Horizon_Full_MAPE = mape(denmark_test$birth_rate, as.numeric(forecast_denmark_gam$fit))
))

results <- rbind(results, data.frame(Country = "Finland", Model = "ARIMA",
                                     Train_MAPE = mape(finland_train$birth_rate, fitted(finland_arima)),
                                     Horizon_1_3_MAPE = mape(finland_test$birth_rate[1:3], as.numeric(forecast_finland_arima$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(finland_test$birth_rate[1:5], as.numeric(forecast_finland_arima$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(finland_test$birth_rate, as.numeric(forecast_finland_arima$mean))
))

results <- rbind(results, data.frame(Country = "Finland", Model = "ETS",
                                     Train_MAPE = mape(finland_train$birth_rate, fitted(finland_ets)),
                                     Horizon_1_3_MAPE = mape(finland_test$birth_rate[1:3], as.numeric(forecast_finland_ets$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(finland_test$birth_rate[1:5], as.numeric(forecast_finland_ets$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(finland_test$birth_rate, as.numeric(forecast_finland_ets$mean))
))

results <- rbind(results, data.frame(Country = "Finland", Model = "Theta",
                                     Train_MAPE = mape(finland_train$birth_rate, fitted(finland_theta)),
                                     Horizon_1_3_MAPE = mape(finland_test$birth_rate[1:3], as.numeric(finland_theta$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(finland_test$birth_rate[1:5], as.numeric(finland_theta$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(finland_test$birth_rate, as.numeric(finland_theta$mean))
))

results <- rbind(results, data.frame(Country = "Finland", Model = "NNAR",
                                     Train_MAPE = mape(finland_train$birth_rate[(finland_nnar$p+1):24], fitted(finland_nnar)[(finland_nnar$p+1):24]),
                                     Horizon_1_3_MAPE = mape(finland_test$birth_rate[1:3], as.numeric(forecast_finland_nnar$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(finland_test$birth_rate[1:5], as.numeric(forecast_finland_nnar$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(finland_test$birth_rate, as.numeric(forecast_finland_nnar$mean))
))

results <- rbind(results, data.frame(Country = "Finlad", Model = "ARFIMA",
                                     Train_MAPE = mape(finland_train$birth_rate, fitted(finland_arfima)),
                                     Horizon_1_3_MAPE = mape(finland_test$birth_rate[1:3], as.numeric(forecast_finland_arfima$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(finland_test$birth_rate[1:5], as.numeric(forecast_finland_arfima$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(finland_test$birth_rate, as.numeric(forecast_finland_arfima$mean))
))

results <- rbind(results, data.frame(Country = "Finland", Model = "GP",
                                     Train_MAPE = mape(finland_train$birth_rate, finland_gp_fitted_train),
                                     Horizon_1_3_MAPE = mape(finland_test$birth_rate[1:3], as.numeric(finland_forecast_mean_gp)[1:3]),
                                     Horizon_1_5_MAPE = mape(finland_test$birth_rate[1:5], as.numeric(finland_forecast_mean_gp)[1:5]),
                                     Horizon_Full_MAPE = mape(finland_test$birth_rate, as.numeric(finland_forecast_mean_gp))
))

results <- rbind(results, data.frame(Country = "Finland", Model = "GAM",
                                     Train_MAPE = mape(finland_train$birth_rate, fitted(finland_gam)),
                                     Horizon_1_3_MAPE = mape(finland_test$birth_rate[1:3], as.numeric(forecast_finland_gam$fit)[1:3]),
                                     Horizon_1_5_MAPE = mape(finland_test$birth_rate[1:5], as.numeric(forecast_finland_gam$fit)[1:5]),
                                     Horizon_Full_MAPE = mape(finland_test$birth_rate, as.numeric(forecast_finland_gam$fit))
))

results <- rbind(results, data.frame(Country = "France", Model = "ARIMA",
                                     Train_MAPE = mape(france_train$birth_rate, fitted(france_arima)),
                                     Horizon_1_3_MAPE = mape(france_test$birth_rate[1:3], as.numeric(forecast_france_arima$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(france_test$birth_rate[1:5], as.numeric(forecast_france_arima$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(france_test$birth_rate, as.numeric(forecast_france_arima$mean))
))

results <- rbind(results, data.frame(Country = "France", Model = "ETS",
                                     Train_MAPE = mape(france_train$birth_rate, fitted(france_ets)),
                                     Horizon_1_3_MAPE = mape(france_test$birth_rate[1:3], as.numeric(forecast_france_ets$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(france_test$birth_rate[1:5], as.numeric(forecast_france_ets$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(france_test$birth_rate, as.numeric(forecast_france_ets$mean))
))

results <- rbind(results, data.frame(Country = "France", Model = "Theta",
                                     Train_MAPE = mape(france_train$birth_rate, fitted(france_theta)),
                                     Horizon_1_3_MAPE = mape(france_test$birth_rate[1:3], as.numeric(france_theta$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(france_test$birth_rate[1:5], as.numeric(france_theta$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(france_test$birth_rate, as.numeric(france_theta$mean))
))

results <- rbind(results, data.frame(Country = "France", Model = "NNAR",
                                     Train_MAPE = mape(france_train$birth_rate[(france_nnar$p+1):24], fitted(france_nnar)[(france_nnar$p+1):24]),
                                     Horizon_1_3_MAPE = mape(france_test$birth_rate[1:3], as.numeric(forecast_france_nnar$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(france_test$birth_rate[1:5], as.numeric(forecast_france_nnar$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(france_test$birth_rate, as.numeric(forecast_france_nnar$mean))
))

results <- rbind(results, data.frame(Country = "France", Model = "ARFIMA",
                                     Train_MAPE = mape(france_train$birth_rate, fitted(france_arfima)),
                                     Horizon_1_3_MAPE = mape(france_test$birth_rate[1:3], as.numeric(forecast_france_arfima$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(france_test$birth_rate[1:5], as.numeric(forecast_france_arfima$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(france_test$birth_rate, as.numeric(forecast_france_arfima$mean))
))

results <- rbind(results, data.frame(Country = "France", Model = "GP",
                                     Train_MAPE = mape(france_train$birth_rate, france_gp_fitted_train),
                                     Horizon_1_3_MAPE = mape(france_test$birth_rate[1:3], as.numeric(france_forecast_mean_gp)[1:3]),
                                     Horizon_1_5_MAPE = mape(france_test$birth_rate[1:5], as.numeric(france_forecast_mean_gp)[1:5]),
                                     Horizon_Full_MAPE = mape(france_test$birth_rate, as.numeric(france_forecast_mean_gp))
))

results <- rbind(results, data.frame(Country = "France", Model = "GAM",
                                     Train_MAPE = mape(france_train$birth_rate, fitted(france_gam)),
                                     Horizon_1_3_MAPE = mape(france_test$birth_rate[1:3], as.numeric(forecast_france_gam$fit)[1:3]),
                                     Horizon_1_5_MAPE = mape(france_test$birth_rate[1:5], as.numeric(forecast_france_gam$fit)[1:5]),
                                     Horizon_Full_MAPE = mape(france_test$birth_rate, as.numeric(forecast_france_gam$fit))
))

results <- rbind(results, data.frame(Country = "Germany", Model = "ARIMA",
                                     Train_MAPE = mape(germany_train$birth_rate, fitted(germany_arima)),
                                     Horizon_1_3_MAPE = mape(germany_test$birth_rate[1:3], as.numeric(forecast_germany_arima$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(germany_test$birth_rate[1:5], as.numeric(forecast_germany_arima$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(germany_test$birth_rate, as.numeric(forecast_germany_arima$mean))
))

results <- rbind(results, data.frame(Country = "Germany", Model = "ETS",
                                     Train_MAPE = mape(germany_train$birth_rate, fitted(germany_ets)),
                                     Horizon_1_3_MAPE = mape(germany_test$birth_rate[1:3], as.numeric(forecast_germany_ets$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(germany_test$birth_rate[1:5], as.numeric(forecast_germany_ets$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(germany_test$birth_rate, as.numeric(forecast_germany_ets$mean))
))

results <- rbind(results, data.frame(Country = "Germany", Model = "Theta",
                                     Train_MAPE = mape(germany_train$birth_rate, fitted(germany_theta)),
                                     Horizon_1_3_MAPE = mape(germany_test$birth_rate[1:3], as.numeric(germany_theta$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(germany_test$birth_rate[1:5], as.numeric(germany_theta$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(germany_test$birth_rate, as.numeric(germany_theta$mean))
))

results <- rbind(results, data.frame(Country = "Germany", Model = "NNAR",
                                     Train_MAPE = mape(germany_train$birth_rate[(germany_nnar$p+1):24], fitted(germany_nnar)[(germany_nnar$p+1):24]),
                                     Horizon_1_3_MAPE = mape(germany_test$birth_rate[1:3], as.numeric(forecast_germany_nnar$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(germany_test$birth_rate[1:5], as.numeric(forecast_germany_nnar$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(germany_test$birth_rate, as.numeric(forecast_germany_nnar$mean))
))

results <- rbind(results, data.frame(Country = "Germany", Model = "ARFIMA",
                                     Train_MAPE = mape(germany_train$birth_rate, fitted(germany_arfima)),
                                     Horizon_1_3_MAPE = mape(germany_test$birth_rate[1:3], as.numeric(forecast_germany_arfima$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(germany_test$birth_rate[1:5], as.numeric(forecast_germany_arfima$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(germany_test$birth_rate, as.numeric(forecast_germany_arfima$mean))
))

results <- rbind(results, data.frame(Country = "Germany", Model = "GP",
                                     Train_MAPE = mape(germany_train$birth_rate, germany_gp_fitted_train),
                                     Horizon_1_3_MAPE = mape(germany_test$birth_rate[1:3], as.numeric(germany_forecast_mean_gp)[1:3]),
                                     Horizon_1_5_MAPE = mape(germany_test$birth_rate[1:5], as.numeric(germany_forecast_mean_gp)[1:5]),
                                     Horizon_Full_MAPE = mape(germany_test$birth_rate, as.numeric(germany_forecast_mean_gp))
))

results <- rbind(results, data.frame(Country = "Germany", Model = "GAM",
                                     Train_MAPE = mape(germany_train$birth_rate, fitted(germany_gam)),
                                     Horizon_1_3_MAPE = mape(germany_test$birth_rate[1:3], as.numeric(forecast_germany_gam$fit)[1:3]),
                                     Horizon_1_5_MAPE = mape(germany_test$birth_rate[1:5], as.numeric(forecast_germany_gam$fit)[1:5]),
                                     Horizon_Full_MAPE = mape(germany_test$birth_rate, as.numeric(forecast_germany_gam$fit))
))

results <- rbind(results, data.frame(Country = "Ireland", Model = "ARIMA",
                                     Train_MAPE = mape(ireland_train$birth_rate, fitted(ireland_arima)),
                                     Horizon_1_3_MAPE = mape(ireland_test$birth_rate[1:3], as.numeric(forecast_ireland_arima$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(ireland_test$birth_rate[1:5], as.numeric(forecast_ireland_arima$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(ireland_test$birth_rate, as.numeric(forecast_ireland_arima$mean))
))

results <- rbind(results, data.frame(Country = "Ireland", Model = "ETS",
                                     Train_MAPE = mape(ireland_train$birth_rate, fitted(ireland_ets)),
                                     Horizon_1_3_MAPE = mape(ireland_test$birth_rate[1:3], as.numeric(forecast_ireland_ets$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(ireland_test$birth_rate[1:5], as.numeric(forecast_ireland_ets$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(ireland_test$birth_rate, as.numeric(forecast_ireland_ets$mean))
))

results <- rbind(results, data.frame(Country = "Ireland", Model = "Theta",
                                     Train_MAPE = mape(ireland_train$birth_rate, fitted(ireland_theta)),
                                     Horizon_1_3_MAPE = mape(ireland_test$birth_rate[1:3], as.numeric(ireland_theta$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(ireland_test$birth_rate[1:5], as.numeric(ireland_theta$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(ireland_test$birth_rate, as.numeric(ireland_theta$mean))
))

results <- rbind(results, data.frame(Country = "Ireland", Model = "NNAR",
                                     Train_MAPE = mape(ireland_train$birth_rate[(ireland_nnar$p+1):24], fitted(ireland_nnar)[(ireland_nnar$p+1):24]),
                                     Horizon_1_3_MAPE = mape(ireland_test$birth_rate[1:3], as.numeric(forecast_ireland_nnar$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(ireland_test$birth_rate[1:5], as.numeric(forecast_ireland_nnar$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(ireland_test$birth_rate, as.numeric(forecast_ireland_nnar$mean))
))
results <- rbind(results, data.frame(Country = "Ireland", Model = "ARFIMA",
                                     Train_MAPE = mape(ireland_train$birth_rate, fitted(ireland_arfima)),
                                     Horizon_1_3_MAPE = mape(ireland_test$birth_rate[1:3], as.numeric(forecast_ireland_arfima$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(ireland_test$birth_rate[1:5], as.numeric(forecast_ireland_arfima$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(ireland_test$birth_rate, as.numeric(forecast_ireland_arfima$mean))
))

results <- rbind(results, data.frame(Country = "Ireland", Model = "GP",
                                     Train_MAPE = mape(ireland_train$birth_rate, ireland_gp_fitted_train),
                                     Horizon_1_3_MAPE = mape(ireland_test$birth_rate[1:3], as.numeric(ireland_forecast_mean_gp)[1:3]),
                                     Horizon_1_5_MAPE = mape(ireland_test$birth_rate[1:5], as.numeric(ireland_forecast_mean_gp)[1:5]),
                                     Horizon_Full_MAPE = mape(ireland_test$birth_rate, as.numeric(ireland_forecast_mean_gp))
))

results <- rbind(results, data.frame(Country = "Ireland", Model = "GAM",
                                     Train_MAPE = mape(ireland_train$birth_rate, fitted(ireland_gam)),
                                     Horizon_1_3_MAPE = mape(ireland_test$birth_rate[1:3], as.numeric(forecast_ireland_gam$fit)[1:3]),
                                     Horizon_1_5_MAPE = mape(ireland_test$birth_rate[1:5], as.numeric(forecast_ireland_gam$fit)[1:5]),
                                     Horizon_Full_MAPE = mape(ireland_test$birth_rate, as.numeric(forecast_ireland_gam$fit))
))

results <- rbind(results, data.frame(Country = "Italy", Model = "ARIMA",
                                     Train_MAPE = mape(italy_train$birth_rate, fitted(italy_arima)),
                                     Horizon_1_3_MAPE = mape(italy_test$birth_rate[1:3], as.numeric(forecast_italy_arima$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(italy_test$birth_rate[1:5], as.numeric(forecast_italy_arima$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(italy_test$birth_rate, as.numeric(forecast_italy_arima$mean))
))

results <- rbind(results, data.frame(Country = "Italy", Model = "ETS",
                                     Train_MAPE = mape(italy_train$birth_rate, fitted(italy_ets)),
                                     Horizon_1_3_MAPE = mape(italy_test$birth_rate[1:3], as.numeric(forecast_italy_ets$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(italy_test$birth_rate[1:5], as.numeric(forecast_italy_ets$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(italy_test$birth_rate, as.numeric(forecast_italy_ets$mean))
))

results <- rbind(results, data.frame(Country = "Italy", Model = "Theta",
                                     Train_MAPE = mape(italy_train$birth_rate, fitted(italy_theta)),
                                     Horizon_1_3_MAPE = mape(italy_test$birth_rate[1:3], as.numeric(italy_theta$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(italy_test$birth_rate[1:5], as.numeric(italy_theta$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(italy_test$birth_rate, as.numeric(italy_theta$mean))
))

results <- rbind(results, data.frame(Country = "Italy", Model = "NNAR",
                                     Train_MAPE = mape(italy_train$birth_rate[(italy_nnar$p+1):24], fitted(italy_nnar)[(italy_nnar$p+1):24]),
                                     Horizon_1_3_MAPE = mape(italy_test$birth_rate[1:3], as.numeric(forecast_italy_nnar$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(italy_test$birth_rate[1:5], as.numeric(forecast_italy_nnar$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(italy_test$birth_rate, as.numeric(forecast_italy_nnar$mean))
))

results <- rbind(results, data.frame(Country = "Italy", Model = "ARFIMA",
                                     Train_MAPE = mape(italy_train$birth_rate, fitted(italy_arfima)),
                                     Horizon_1_3_MAPE = mape(italy_test$birth_rate[1:3], as.numeric(forecast_italy_arfima$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(italy_test$birth_rate[1:5], as.numeric(forecast_italy_arfima$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(italy_test$birth_rate, as.numeric(forecast_italy_arfima$mean))
))

results <- rbind(results, data.frame(Country = "Italy", Model = "GP",
                                     Train_MAPE = mape(italy_train$birth_rate, italy_gp_fitted_train),
                                     Horizon_1_3_MAPE = mape(italy_test$birth_rate[1:3], as.numeric(italy_forecast_mean_gp)[1:3]),
                                     Horizon_1_5_MAPE = mape(italy_test$birth_rate[1:5], as.numeric(italy_forecast_mean_gp)[1:5]),
                                     Horizon_Full_MAPE = mape(italy_test$birth_rate, as.numeric(italy_forecast_mean_gp))
))

results <- rbind(results, data.frame(Country = "Italy", Model = "GAM",
                                     Train_MAPE = mape(italy_train$birth_rate, fitted(italy_gam)),
                                     Horizon_1_3_MAPE = mape(italy_test$birth_rate[1:3], as.numeric(forecast_italy_gam$fit)[1:3]),
                                     Horizon_1_5_MAPE = mape(italy_test$birth_rate[1:5], as.numeric(forecast_italy_gam$fit)[1:5]),
                                     Horizon_Full_MAPE = mape(italy_test$birth_rate, as.numeric(forecast_italy_gam$fit))
))
results <- rbind(results, data.frame(Country = "Luxembourg", Model = "ARIMA",
                                     Train_MAPE = mape(luxembourg_train$birth_rate, fitted(luxembourg_arima)),
                                     Horizon_1_3_MAPE = mape(luxembourg_test$birth_rate[1:3], as.numeric(forecast_luxembourg_arima$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(luxembourg_test$birth_rate[1:5], as.numeric(forecast_luxembourg_arima$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(luxembourg_test$birth_rate, as.numeric(forecast_luxembourg_arima$mean))
))

results <- rbind(results, data.frame(Country = "Luxembourg", Model = "ETS",
                                     Train_MAPE = mape(luxembourg_train$birth_rate, fitted(luxembourg_ets)),
                                     Horizon_1_3_MAPE = mape(luxembourg_test$birth_rate[1:3], as.numeric(forecast_luxembourg_ets$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(luxembourg_test$birth_rate[1:5], as.numeric(forecast_luxembourg_ets$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(luxembourg_test$birth_rate, as.numeric(forecast_luxembourg_ets$mean))
))

results <- rbind(results, data.frame(Country = "Luxembourg", Model = "Theta",
                                     Train_MAPE = mape(luxembourg_train$birth_rate, fitted(luxembourg_theta)),
                                     Horizon_1_3_MAPE = mape(luxembourg_test$birth_rate[1:3], as.numeric(luxembourg_theta$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(luxembourg_test$birth_rate[1:5], as.numeric(luxembourg_theta$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(luxembourg_test$birth_rate, as.numeric(luxembourg_theta$mean))
))

results <- rbind(results, data.frame(Country = "Luxembourg", Model = "NNAR",
                                     Train_MAPE = mape(luxembourg_train$birth_rate[(luxembourg_nnar$p+1):24], fitted(luxembourg_nnar)[(luxembourg_nnar$p+1):24]),
                                     Horizon_1_3_MAPE = mape(luxembourg_test$birth_rate[1:3], as.numeric(forecast_luxembourg_nnar$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(luxembourg_test$birth_rate[1:5], as.numeric(forecast_luxembourg_nnar$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(luxembourg_test$birth_rate, as.numeric(forecast_luxembourg_nnar$mean))
))

results <- rbind(results, data.frame(Country = "Luxembourg", Model = "ARFIMA",
                                     Train_MAPE = mape(luxembourg_train$birth_rate, fitted(luxembourg_arfima)),
                                     Horizon_1_3_MAPE = mape(luxembourg_test$birth_rate[1:3], as.numeric(forecast_luxembourg_arfima$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(luxembourg_test$birth_rate[1:5], as.numeric(forecast_luxembourg_arfima$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(luxembourg_test$birth_rate, as.numeric(forecast_luxembourg_arfima$mean))
))

results <- rbind(results, data.frame(Country = "Luxembourg", Model = "GP",
                                     Train_MAPE = mape(luxembourg_train$birth_rate, luxembourg_gp_fitted_train),
                                     Horizon_1_3_MAPE = mape(luxembourg_test$birth_rate[1:3], as.numeric(luxembourg_forecast_mean_gp)[1:3]),
                                     Horizon_1_5_MAPE = mape(luxembourg_test$birth_rate[1:5], as.numeric(luxembourg_forecast_mean_gp)[1:5]),
                                     Horizon_Full_MAPE = mape(luxembourg_test$birth_rate, as.numeric(luxembourg_forecast_mean_gp))
))

results <- rbind(results, data.frame(Country = "Luxembourg", Model = "GAM",
                                     Train_MAPE = mape(luxembourg_train$birth_rate, fitted(luxembourg_gam)),
                                     Horizon_1_3_MAPE = mape(luxembourg_test$birth_rate[1:3], as.numeric(forecast_luxembourg_gam$fit)[1:3]),
                                     Horizon_1_5_MAPE = mape(luxembourg_test$birth_rate[1:5], as.numeric(forecast_luxembourg_gam$fit)[1:5]),
                                     Horizon_Full_MAPE = mape(luxembourg_test$birth_rate, as.numeric(forecast_luxembourg_gam$fit))
))
results <- rbind(results, data.frame(Country = "Netherlands", Model = "ARIMA",
                                     Train_MAPE = mape(netherlands_train$birth_rate, fitted(netherlands_arima)),
                                     Horizon_1_3_MAPE = mape(netherlands_test$birth_rate[1:3], as.numeric(forecast_netherlands_arima$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(netherlands_test$birth_rate[1:5], as.numeric(forecast_netherlands_arima$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(netherlands_test$birth_rate, as.numeric(forecast_netherlands_arima$mean))
))

results <- rbind(results, data.frame(Country = "Netherlands", Model = "ETS",
                                     Train_MAPE = mape(netherlands_train$birth_rate, fitted(netherlands_ets)),
                                     Horizon_1_3_MAPE = mape(netherlands_test$birth_rate[1:3], as.numeric(forecast_netherlands_ets$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(netherlands_test$birth_rate[1:5], as.numeric(forecast_netherlands_ets$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(netherlands_test$birth_rate, as.numeric(forecast_netherlands_ets$mean))
))

results <- rbind(results, data.frame(Country = "Netherlands", Model = "Theta",
                                     Train_MAPE = mape(netherlands_train$birth_rate, fitted(netherlands_theta)),
                                     Horizon_1_3_MAPE = mape(netherlands_test$birth_rate[1:3], as.numeric(netherlands_theta$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(netherlands_test$birth_rate[1:5], as.numeric(netherlands_theta$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(netherlands_test$birth_rate, as.numeric(netherlands_theta$mean))
))

results <- rbind(results, data.frame(Country = "Netherlands", Model = "NNAR",
                                     Train_MAPE = mape(netherlands_train$birth_rate[(netherlands_nnar$p+1):24], fitted(netherlands_nnar)[(netherlands_nnar$p+1):24]),
                                     Horizon_1_3_MAPE = mape(netherlands_test$birth_rate[1:3], as.numeric(forecast_netherlands_nnar$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(netherlands_test$birth_rate[1:5], as.numeric(forecast_netherlands_nnar$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(netherlands_test$birth_rate, as.numeric(forecast_netherlands_nnar$mean))
))

results <- rbind(results, data.frame(Country = "Netherlands", Model = "ARFIMA",
                                     Train_MAPE = mape(netherlands_train$birth_rate, fitted(netherlands_arfima)),
                                     Horizon_1_3_MAPE = mape(netherlands_test$birth_rate[1:3], as.numeric(forecast_netherlands_arfima$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(netherlands_test$birth_rate[1:5], as.numeric(forecast_netherlands_arfima$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(netherlands_test$birth_rate, as.numeric(forecast_netherlands_arfima$mean))
))

results <- rbind(results, data.frame(Country = "Netherlands", Model = "GP",
                                     Train_MAPE = mape(netherlands_train$birth_rate, netherlands_gp_fitted_train),
                                     Horizon_1_3_MAPE = mape(netherlands_test$birth_rate[1:3], as.numeric(netherlands_forecast_mean_gp)[1:3]),
                                     Horizon_1_5_MAPE = mape(netherlands_test$birth_rate[1:5], as.numeric(netherlands_forecast_mean_gp)[1:5]),
                                     Horizon_Full_MAPE = mape(netherlands_test$birth_rate, as.numeric(netherlands_forecast_mean_gp))
))
results <- rbind(results, data.frame(Country = "Netherlands", Model = "GAM",
                                     Train_MAPE = mape(netherlands_train$birth_rate, fitted(netherlands_gam)),
                                     Horizon_1_3_MAPE = mape(netherlands_test$birth_rate[1:3], as.numeric(forecast_netherlands_gam$fit)[1:3]),
                                     Horizon_1_5_MAPE = mape(netherlands_test$birth_rate[1:5], as.numeric(forecast_netherlands_gam$fit)[1:5]),
                                     Horizon_Full_MAPE = mape(netherlands_test$birth_rate, as.numeric(forecast_netherlands_gam$fit))
))

results <- rbind(results, data.frame(Country = "Portugal", Model = "ARIMA",
                                     Train_MAPE = mape(portugal_train$birth_rate, fitted(portugal_arima)),
                                     Horizon_1_3_MAPE = mape(portugal_test$birth_rate[1:3], as.numeric(forecast_portugal_arima$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(portugal_test$birth_rate[1:5], as.numeric(forecast_portugal_arima$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(portugal_test$birth_rate, as.numeric(forecast_portugal_arima$mean))
))

results <- rbind(results, data.frame(Country = "Portugal", Model = "ETS",
                                     Train_MAPE = mape(portugal_train$birth_rate, fitted(portugal_ets)),
                                     Horizon_1_3_MAPE = mape(portugal_test$birth_rate[1:3], as.numeric(forecast_portugal_ets$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(portugal_test$birth_rate[1:5], as.numeric(forecast_portugal_ets$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(portugal_test$birth_rate, as.numeric(forecast_portugal_ets$mean))
))

results <- rbind(results, data.frame(Country = "Portugal", Model = "Theta",
                                     Train_MAPE = mape(portugal_train$birth_rate, fitted(portugal_theta)),
                                     Horizon_1_3_MAPE = mape(portugal_test$birth_rate[1:3], as.numeric(portugal_theta$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(portugal_test$birth_rate[1:5], as.numeric(portugal_theta$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(portugal_test$birth_rate, as.numeric(portugal_theta$mean))
))

results <- rbind(results, data.frame(Country = "Portugal", Model = "NNAR",
                                     Train_MAPE = mape(portugal_train$birth_rate[(portugal_nnar$p+1):24], fitted(portugal_nnar)[(portugal_nnar$p+1):24]),
                                     Horizon_1_3_MAPE = mape(portugal_test$birth_rate[1:3], as.numeric(forecast_portugal_nnar$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(portugal_test$birth_rate[1:5], as.numeric(forecast_portugal_nnar$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(portugal_test$birth_rate, as.numeric(forecast_portugal_nnar$mean))
))

results <- rbind(results, data.frame(Country = "Portugal", Model = "ARFIMA",
                                     Train_MAPE = mape(portugal_train$birth_rate, fitted(portugal_arfima)),
                                     Horizon_1_3_MAPE = mape(portugal_test$birth_rate[1:3], as.numeric(forecast_portugal_arfima$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(portugal_test$birth_rate[1:5], as.numeric(forecast_portugal_arfima$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(portugal_test$birth_rate, as.numeric(forecast_portugal_arfima$mean))
))

results <- rbind(results, data.frame(Country = "Portugal", Model = "GP",
                                     Train_MAPE = mape(portugal_train$birth_rate, portugal_gp_fitted_train),
                                     Horizon_1_3_MAPE = mape(portugal_test$birth_rate[1:3], as.numeric(portugal_forecast_mean_gp)[1:3]),
                                     Horizon_1_5_MAPE = mape(portugal_test$birth_rate[1:5], as.numeric(portugal_forecast_mean_gp)[1:5]),
                                     Horizon_Full_MAPE = mape(portugal_test$birth_rate, as.numeric(portugal_forecast_mean_gp))
))
results <- rbind(results, data.frame(Country = "Portugal", Model = "GAM",
                                     Train_MAPE = mape(portugal_train$birth_rate, fitted(portugal_gam)),
                                     Horizon_1_3_MAPE = mape(portugal_test$birth_rate[1:3], as.numeric(forecast_portugal_gam$fit)[1:3]),
                                     Horizon_1_5_MAPE = mape(portugal_test$birth_rate[1:5], as.numeric(forecast_portugal_gam$fit)[1:5]),
                                     Horizon_Full_MAPE = mape(portugal_test$birth_rate, as.numeric(forecast_portugal_gam$fit))
))

results <- rbind(results, data.frame(Country = "Spain", Model = "ARIMA",
                                     Train_MAPE = mape(spain_train$birth_rate, fitted(spain_arima)),
                                     Horizon_1_3_MAPE = mape(spain_test$birth_rate[1:3], as.numeric(forecast_spain_arima$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(spain_test$birth_rate[1:5], as.numeric(forecast_spain_arima$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(spain_test$birth_rate, as.numeric(forecast_spain_arima$mean))
))

results <- rbind(results, data.frame(Country = "Spain", Model = "ETS",
                                     Train_MAPE = mape(spain_train$birth_rate, fitted(spain_ets)),
                                     Horizon_1_3_MAPE = mape(spain_test$birth_rate[1:3], as.numeric(forecast_spain_ets$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(spain_test$birth_rate[1:5], as.numeric(forecast_spain_ets$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(spain_test$birth_rate, as.numeric(forecast_spain_ets$mean))
))

results <- rbind(results, data.frame(Country = "Spain", Model = "Theta",
                                     Train_MAPE = mape(spain_train$birth_rate, fitted(spain_theta)),
                                     Horizon_1_3_MAPE = mape(spain_test$birth_rate[1:3], as.numeric(spain_theta$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(spain_test$birth_rate[1:5], as.numeric(spain_theta$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(spain_test$birth_rate, as.numeric(spain_theta$mean))
))

results <- rbind(results, data.frame(Country = "Spain", Model = "NNAR",
                                     Train_MAPE = mape(spain_train$birth_rate[(spain_nnar$p+1):24], fitted(spain_nnar)[(spain_nnar$p+1):24]),
                                     Horizon_1_3_MAPE = mape(spain_test$birth_rate[1:3], as.numeric(forecast_spain_nnar$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(spain_test$birth_rate[1:5], as.numeric(forecast_spain_nnar$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(spain_test$birth_rate, as.numeric(forecast_spain_nnar$mean))
))

results <- rbind(results, data.frame(Country = "Spain", Model = "ARFIMA",
                                     Train_MAPE = mape(spain_train$birth_rate, fitted(spain_arfima)),
                                     Horizon_1_3_MAPE = mape(spain_test$birth_rate[1:3], as.numeric(forecast_spain_arfima$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(spain_test$birth_rate[1:5], as.numeric(forecast_spain_arfima$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(spain_test$birth_rate, as.numeric(forecast_spain_arfima$mean))
))

results <- rbind(results, data.frame(Country = "Spain", Model = "GP",
                                     Train_MAPE = mape(spain_train$birth_rate, spain_gp_fitted_train),
                                     Horizon_1_3_MAPE = mape(spain_test$birth_rate[1:3], as.numeric(spain_forecast_mean_gp)[1:3]),
                                     Horizon_1_5_MAPE = mape(spain_test$birth_rate[1:5], as.numeric(spain_forecast_mean_gp)[1:5]),
                                     Horizon_Full_MAPE = mape(spain_test$birth_rate, as.numeric(spain_forecast_mean_gp))
))

results <- rbind(results, data.frame(Country = "Spain", Model = "GAM",
                                     Train_MAPE = mape(spain_train$birth_rate, fitted(spain_gam)),
                                     Horizon_1_3_MAPE = mape(spain_test$birth_rate[1:3], as.numeric(forecast_spain_gam$fit)[1:3]),
                                     Horizon_1_5_MAPE = mape(spain_test$birth_rate[1:5], as.numeric(forecast_spain_gam$fit)[1:5]),
                                     Horizon_Full_MAPE = mape(spain_test$birth_rate, as.numeric(forecast_spain_gam$fit))
))

results <- rbind(results, data.frame(Country = "Sweden", Model = "ARIMA",
                                     Train_MAPE = mape(sweden_train$birth_rate, fitted(sweden_arima)),
                                     Horizon_1_3_MAPE = mape(sweden_test$birth_rate[1:3], as.numeric(forecast_sweden_arima$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(sweden_test$birth_rate[1:5], as.numeric(forecast_sweden_arima$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(sweden_test$birth_rate, as.numeric(forecast_sweden_arima$mean))
))

results <- rbind(results, data.frame(Country = "Sweden", Model = "ETS",
                                     Train_MAPE = mape(sweden_train$birth_rate, fitted(sweden_ets)),
                                     Horizon_1_3_MAPE = mape(sweden_test$birth_rate[1:3], as.numeric(forecast_sweden_ets$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(sweden_test$birth_rate[1:5], as.numeric(forecast_sweden_ets$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(sweden_test$birth_rate, as.numeric(forecast_sweden_ets$mean))
))

results <- rbind(results, data.frame(Country = "Sweden", Model = "Theta",
                                     Train_MAPE = mape(sweden_train$birth_rate, fitted(sweden_theta)),
                                     Horizon_1_3_MAPE = mape(sweden_test$birth_rate[1:3], as.numeric(sweden_theta$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(sweden_test$birth_rate[1:5], as.numeric(sweden_theta$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(sweden_test$birth_rate, as.numeric(sweden_theta$mean))
))

results <- rbind(results, data.frame(Country = "Sweden", Model = "NNAR",
                                     Train_MAPE = mape(sweden_train$birth_rate[(sweden_nnar$p+1):24], fitted(sweden_nnar)[(sweden_nnar$p+1):24]),
                                     Horizon_1_3_MAPE = mape(sweden_test$birth_rate[1:3], as.numeric(forecast_sweden_nnar$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(sweden_test$birth_rate[1:5], as.numeric(forecast_sweden_nnar$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(sweden_test$birth_rate, as.numeric(forecast_sweden_nnar$mean))
))

results <- rbind(results, data.frame(Country = "Sweden", Model = "ARFIMA",
                                     Train_MAPE = mape(sweden_train$birth_rate, fitted(sweden_arfima)),
                                     Horizon_1_3_MAPE = mape(sweden_test$birth_rate[1:3], as.numeric(forecast_sweden_arfima$mean)[1:3]),
                                     Horizon_1_5_MAPE = mape(sweden_test$birth_rate[1:5], as.numeric(forecast_sweden_arfima$mean)[1:5]),
                                     Horizon_Full_MAPE = mape(sweden_test$birth_rate, as.numeric(forecast_sweden_arfima$mean))
))

results <- rbind(results, data.frame(Country = "Sweden", Model = "GP",
                                     Train_MAPE = mape(sweden_train$birth_rate, sweden_gp_fitted_train),
                                     Horizon_1_3_MAPE = mape(sweden_test$birth_rate[1:3], as.numeric(sweden_forecast_mean_gp)[1:3]),
                                     Horizon_1_5_MAPE = mape(sweden_test$birth_rate[1:5], as.numeric(sweden_forecast_mean_gp)[1:5]),
                                     Horizon_Full_MAPE = mape(sweden_test$birth_rate, as.numeric(sweden_forecast_mean_gp))
))

results <- rbind(results, data.frame(Country = "Sweden", Model = "GAM",
                                     Train_MAPE = mape(sweden_train$birth_rate, fitted(sweden_gam)),
                                     Horizon_1_3_MAPE = mape(sweden_test$birth_rate[1:3], as.numeric(forecast_sweden_gam$fit)[1:3]),
                                     Horizon_1_5_MAPE = mape(sweden_test$birth_rate[1:5], as.numeric(forecast_sweden_gam$fit)[1:5]),
                                     Horizon_Full_MAPE = mape(sweden_test$birth_rate, as.numeric(forecast_sweden_gam$fit))
))

write.csv(results, "birth_rate_evaluation.csv", row.names = FALSE)

#analysing the results----
median(results$Train_MAPE[results$Model=='ARIMA'])
median(results$Train_MAPE[results$Model=='ETS'])
median(results$Train_MAPE[results$Model=='Theta'])
median(results$Train_MAPE[results$Model=='NNAR'])
median(results$Train_MAPE[results$Model=='ARFIMA'])
median(results$Train_MAPE[results$Model=='GP'])
median(results$Train_MAPE[results$Model=='GAM'])

median(results$Horizon_1_3_MAPE[results$Model=='ARIMA'])
median(results$Horizon_1_3_MAPE[results$Model=='ETS'])
median(results$Horizon_1_3_MAPE[results$Model=='Theta'])
median(results$Horizon_1_3_MAPE[results$Model=='NNAR'])
median(results$Horizon_1_3_MAPE[results$Model=='ARFIMA'])
median(results$Horizon_1_3_MAPE[results$Model=='GP'])
median(results$Horizon_1_3_MAPE[results$Model=='GAM'])

median(results$Horizon_1_5_MAPE[results$Model=='ARIMA'])
median(results$Horizon_1_5_MAPE[results$Model=='ETS'])
median(results$Horizon_1_5_MAPE[results$Model=='Theta'])
median(results$Horizon_1_5_MAPE[results$Model=='NNAR'])
median(results$Horizon_1_5_MAPE[results$Model=='ARFIMA'])
median(results$Horizon_1_5_MAPE[results$Model=='GP'])
median(results$Horizon_1_5_MAPE[results$Model=='GAM'])

median(results$Horizon_Full_MAPE[results$Model=='ARIMA'])
median(results$Horizon_Full_MAPE[results$Model=='ETS'])
median(results$Horizon_Full_MAPE[results$Model=='Theta'])
median(results$Horizon_Full_MAPE[results$Model=='NNAR'])
median(results$Horizon_Full_MAPE[results$Model=='ARFIMA'])
median(results$Horizon_Full_MAPE[results$Model=='GP'])
median(results$Horizon_Full_MAPE[results$Model=='GAM'])
