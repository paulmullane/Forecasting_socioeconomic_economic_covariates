#load packages----
library(readxl)
library(forecast)
library(e1071)
library(dplyr)
library(ggplot2)
library(cowplot)


#load and merge data----
msw_raw <- read_excel("Italian_MSW.xlsx")          
cov_raw <- read_excel("Italy.xlsx")              

msw <- msw_raw %>%
  rename(Year=TIME, MSW=Italy)
msw$Year <- as.double(msw$Year)

covariates <- cov_raw %>%
  select(Year, Population, GDP, energy_consumption, co2)

#match covariate and waste years
data_full <- msw %>%
  inner_join(covariates, by="Year") %>%
  filter(Year>=1995, Year<=2022) %>%
  arrange(Year)

#Fit and tune SVM (polynomial kernel)----
test_years  <- (max(data_full$Year)-5):max(data_full$Year)
train_data  <- data_full%>%filter(!(Year %in% test_years))
test_data   <- data_full%>%filter(Year %in% test_years)

expanding_window_mape <- function(data_ordered, kernel, cost, gamma, coef0,
                                  degree, epsilon, min_train=15){
  n <- nrow(data_ordered)
  errors <- numeric(0)
  
  for (i in min_train:(n - 1)){
    fold_train <- data_ordered[1:i, ]
    fold_test  <- data_ordered[i+1, , drop = FALSE]
    
    fold_model <- svm(MSW~Population+GDP+energy_consumption+co2,
                      data=fold_train, kernel=kernel, cost=cost, gamma=gamma,
                      coef0=coef0, degree=degree, epsilon=epsilon)
    fold_pred  <- predict(fold_model, newdata = fold_test)
    
    errors <- c(errors, abs((fold_test$MSW - fold_pred) / fold_test$MSW))
  }
  
  mean(errors)
}

#Grid search over cost/gamma/coef0/degree/epsilon (polynomial kernel)
tuning_grid <- expand.grid(cost=2^(-2:6), gamma=2^(-8:2), coef0=c(0, 1, 2),
                           degree=c(2, 3), epsilon=c(0.01, 0.05, 0.1, 0.2))

tuning_grid$mape <- mapply(
  function(cost, gamma, coef0, degree, epsilon) {
    expanding_window_mape(train_data, kernel="polynomial", cost=cost, gamma=gamma,
                          coef0=coef0, degree=degree, epsilon=epsilon, min_train = 15)
  },
  tuning_grid$cost, tuning_grid$gamma, tuning_grid$coef0,
  tuning_grid$degree, tuning_grid$epsilon
)

best_params <- tuning_grid[which.min(tuning_grid$mape), ]
print(best_params)

train_pred <- predict(svm_test_fit, newdata = train_data)
train_mape <- mean(abs((train_data$MSW - train_pred) / train_data$MSW))
cat("Training MAPE:", round(train_mape * 100, 2), "%\n")


#test metric----
svm_test_fit <- svm(MSW~Population+GDP+energy_consumption+co2, 
                    data=train_data, kernel="polynomial", cost=best_params$cost,
                    gamma=best_params$gamma, coef0=best_params$coef0,
                    degree=best_params$degree, epsilon=best_params$epsilon)

test_pred <- predict(svm_test_fit, newdata = test_data)
test_mape <- mean(abs((test_data$MSW - test_pred) / test_data$MSW))
cat("Held-out test MAPE:", round(test_mape * 100, 2), "%\n")

#Refit SVM on the full 1995-2022 sample using train-tuned hyperparameters-----
svm_model <- svm(MSW ~ Population + GDP + energy_consumption + co2,
                 data = data_full, kernel = "polynomial",
                 cost = best_params$cost, gamma = best_params$gamma,
                 coef0 = best_params$coef0, degree = best_params$degree,
                 epsilon = best_params$epsilon)

#sim covariate paths via manual Theta-equivalent recursion----
h <- 6     
n_sims <- 1000
set.seed(20229798)

covariate_names <- c("Population", "GDP", "energy_consumption", "co2")

cov_history <- cov_raw %>%
  select(Year, Population, GDP, energy_consumption, co2) %>%
  filter(Year<=2022)

simulate_covariate_paths <- function(var_name){
  series_df <- cov_history %>%
    select(Year, value=all_of(var_name)) %>%
    filter(!is.na(value)) %>%
    arrange(Year)
  
  ts_obj <- ts(series_df$value, start=min(series_df$Year), frequency=1)
  fc <- thetaf(ts_obj, h=h, level=95)
  
  sigma <- sd(fc$residuals, na.rm=TRUE)
  alpha <- fc$model$alpha
  drift <- fc$model$drift
  
  sims <- matrix(NA, nrow=h, ncol=n_sims)
  last_level <- tail(series_df$value, 1)
  
  for (i in seq_len(n_sims)){
    ell <- last_level
    path <- numeric(h)
    for (t in seq_len(h)){
      eps <- rnorm(1, 0, sigma)
      path[t] <- ell+drift*t+eps
      ell <- alpha*path[t]+(1-alpha)*ell
    }
    sims[, i] <- path
  }
  
  list(sims=sims, start_year=max(series_df$Year)+1)
}

covariate_sims <- lapply(covariate_names, simulate_covariate_paths)
names(covariate_sims) <- covariate_names

#Combine simulated paths across covariates into coherent MSW trajectories----
years <- covariate_sims$Population$start_year+0:(h-1)

msw_paths <- matrix(NA, nrow = h, ncol = n_sims)

for (i in seq_len(n_sims)) {
  path_input <- data.frame(Population=covariate_sims$Population$sims[, i],
                           GDP=covariate_sims$GDP$sims[, i],
                           energy_consumption=covariate_sims$energy_consumption$sims[, i],
                           co2=covariate_sims$co2$sims[, i])
  msw_paths[, i] <- predict(svm_model, newdata = path_input)
}


# summarise plausible MSW paths and compare to naive point-forecast scenario----
scenario_a_input <- data.frame(Year=years,
                               Population=sapply(seq_len(h), function(t) mean(covariate_sims$Population$sims[t, ])),
                               GDP=sapply(seq_len(h), function(t) mean(covariate_sims$GDP$sims[t, ])),
                               energy_consumption=sapply(seq_len(h), function(t) mean(covariate_sims$energy_consumption$sims[t, ])),
                               co2=sapply(seq_len(h), function(t) mean(covariate_sims$co2$sims[t, ])))
scenario_a_input$MSW_forecast <- predict(svm_model, newdata = scenario_a_input)

scenario_b_summary <- data.frame(Year=years, MSW_mean=apply(msw_paths, 1, mean),
                                 MSW_min=apply(msw_paths, 1, min), 
                                 MSW_max=apply(msw_paths, 1, max))

#Compare Scenario A vs Scenario B----

comparison <- scenario_a_input %>%
  select(Year, MSW_point_forecast = MSW_forecast) %>%
  left_join(scenario_b_summary, by="Year") %>%
  mutate(propagated_width=MSW_max-MSW_min,
         relative_width=propagated_width/abs(MSW_point_forecast), 
         naive_uncertainty=0)

cat("\nComparison: naive vs propagated uncertainty on downstream MSW forecast\n")
print(comparison[, c("Year", "MSW_point_forecast", "MSW_mean",
                     "MSW_min", "MSW_max", "relative_width")])

#plot-----
historical_plot_data <- data_full %>% select(Year, MSW)

p <- ggplot() +
  geom_line(data = historical_plot_data, 
            aes(x = Year, y = MSW, color = "Observed"),
            linewidth = 0.5) +
  geom_point(data = historical_plot_data, 
             aes(x = Year, y = MSW, color = "Observed"),
             size = 1.5) +
  geom_ribbon(data = comparison,
              aes(x = Year, ymin = MSW_min, ymax = MSW_max, fill = "Simulated MSW Range"),
              alpha = 0.3) +
  geom_line(data = comparison, 
            aes(x = Year, y = MSW_point_forecast, color = "Naive Forecast"),
            linewidth = 1, linetype = "dashed") +
  scale_color_manual(name = NULL,
                     values = c("Observed" = "black",
                                "Naive Forecast" = "firebrick")) +
  scale_fill_manual(name = NULL,
                    values = c("Simulated MSW Range" = "steelblue")) +
  labs(x = "Year", y = "Municipal solid waste (thousand tonnes)") +
  theme_cowplot() +
  theme(legend.position = "bottom")

ggsave("Italy_msw_forecast_comparison_polynomial.pdf", plot = p, width = 8, height = 5, dpi = 300)
