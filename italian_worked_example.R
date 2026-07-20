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

#Fit and tune SVR----
test_years  <- (max(data_full$Year)-5):max(data_full$Year)
train_data  <- data_full%>%filter(!(Year %in% test_years))
test_data   <- data_full%>%filter(Year %in% test_years)

expanding_window_mape <- function(data_ordered, kernel, cost, gamma=NULL,
                                  epsilon, min_train=15){
  n <- nrow(data_ordered)
  errors <- numeric(0)
  
  for (i in min_train:(n - 1)){
    fold_train <- data_ordered[1:i, ]
    fold_test  <- data_ordered[i+1, , drop = FALSE]
    
    svm_args <- list(formula = MSW~Population+GDP+energy_consumption+co2,
                     data=fold_train, kernel=kernel, cost=cost, epsilon=epsilon)
    if (kernel != "linear") svm_args$gamma <- gamma  # RBF needs gamma; linear ignores it
    
    fold_model <- do.call(svm, svm_args)
    fold_pred  <- predict(fold_model, newdata = fold_test)
    
    errors <- c(errors, abs((fold_test$MSW - fold_pred) / fold_test$MSW))
  }
  
  mean(errors)
}

#Grid search over cost/gamma/epsilon (RBF kernel - gamma now matters and is tuned)
tuning_grid <- expand.grid(cost=2^(-2:6), gamma=2^(-8:2), epsilon=c(0.01, 0.05, 0.1, 0.2))

tuning_grid$mape <- mapply(
  function(cost, gamma, epsilon) {
    expanding_window_mape(train_data, kernel="radial", cost=cost, gamma=gamma,
                          epsilon=epsilon, min_train = 15)
  },
  tuning_grid$cost, tuning_grid$gamma, tuning_grid$epsilon
)

best_params <- tuning_grid[which.min(tuning_grid$mape), ]
cat("Best SVR hyperparameters (expanding-window tuning, train-only):\n")
print(best_params)

if (best_params$cost %in% range(tuning_grid$cost) ||
    best_params$gamma %in% range(tuning_grid$gamma) ||
    best_params$epsilon %in% range(tuning_grid$epsilon)) {
  warning("Best hyperparameters landed on a grid boundary - widen the cost/",
          "gamma/epsilon search range and re-tune before trusting this model.")
}

#test metric----
svr_test_fit <- svm(MSW~Population+GDP+energy_consumption+co2, 
                    data=train_data, kernel="radial", cost=best_params$cost,
                    gamma=best_params$gamma, epsilon=best_params$epsilon)

test_pred <- predict(svr_test_fit, newdata = test_data)
test_mape <- mean(abs((test_data$MSW - test_pred) / test_data$MSW))
cat("Held-out test MAPE:", round(test_mape * 100, 2), "%\n")

# ------------------------------------------------------------------------------
# 4. Refit SVR on the full 1995-2022 sample for downstream forecasting----

svr_model <- svm(MSW~Population+GDP+energy_consumption+co2, 
                 data=data_full, kernel="radial", cost=best_params$cost,
                 gamma=best_params$gamma, epsilon=best_params$epsilon)

# ------------------------------------------------------------------------------
# 5. Fit Theta models to the four covariates (common origin: 2022)
# ------------------------------------------------------------------------------

h <- 6  # forecast horizon: 2023-2028

# Full covariate history up to the common origin year (2022), per variable,
# starting from each variable's earliest available observation.
cov_history <- cov_raw %>%
  select(Year, Population, GDP, energy_consumption, co2) %>%
  filter(Year <= 2022)

fit_theta_forecast <- function(var_name) {
  series_df <- cov_history %>%
    select(Year, value = all_of(var_name)) %>%
    filter(!is.na(value)) %>%
    arrange(Year)
  
  ts_obj <- ts(series_df$value, start = min(series_df$Year), frequency = 1)
  fc <- thetaf(ts_obj, h = h, level = 95)
  
  # Index the interval by position, not by column name ("95%" vs "95 %" vs no
  # dimnames at all varies across forecast package versions/builds) - with a
  # single requested level this is always the first (only) column.
  lower_mat <- as.matrix(fc$lower)
  upper_mat <- as.matrix(fc$upper)
  
  data.frame(
    Year     = (max(series_df$Year) + 1):(max(series_df$Year) + h),
    variable = var_name,
    mean     = as.numeric(fc$mean),
    lower95  = as.numeric(lower_mat[, 1]),
    upper95  = as.numeric(upper_mat[, 1])
  )
}

covariate_names <- c("Population", "GDP", "energy_consumption", "co2")
theta_forecasts <- lapply(covariate_names, fit_theta_forecast)
names(theta_forecasts) <- covariate_names


# Approximate each covariate's per-horizon forecast error as Normal, with the
# standard deviation implied by the 95% PI half-width (upper - mean) / 1.96.
for (v in covariate_names) {
  theta_forecasts[[v]]$sd <- (theta_forecasts[[v]]$upper95 -
                                theta_forecasts[[v]]$mean) / 1.96
}

# ------------------------------------------------------------------------------
# 6. Scenario A: naive propagation (point forecasts treated as known)
# ------------------------------------------------------------------------------

scenario_a_input <- data.frame(
  Year               = theta_forecasts$Population$Year,
  Population         = theta_forecasts$Population$mean,
  GDP                = theta_forecasts$GDP$mean,
  energy_consumption = theta_forecasts$energy_consumption$mean,
  co2                = theta_forecasts$co2$mean
)

scenario_a_input$MSW_forecast <- predict(svr_model, newdata = scenario_a_input)

cat("\nScenario A (naive propagation) - point forecast only:\n")
print(scenario_a_input[, c("Year", "MSW_forecast")])

# ------------------------------------------------------------------------------
# 7. Scenario B: uncertainty-aware propagation (independent Monte Carlo draws)
# ------------------------------------------------------------------------------
# NOTE: draws are independent across the four covariates. In reality their
# forecast errors likely correlate (e.g. GDP, energy consumption and CO2 would
# plausibly move together), which independent sampling does not capture. This
# is a deliberate simplification for illustration - state as a limitation.

n_draws <- 5000

simulate_year <- function(year_idx) {
  pop_draw <- rnorm(n_draws,
                    mean = theta_forecasts$Population$mean[year_idx],
                    sd   = theta_forecasts$Population$sd[year_idx])
  gdp_draw <- rnorm(n_draws,
                    mean = theta_forecasts$GDP$mean[year_idx],
                    sd   = theta_forecasts$GDP$sd[year_idx])
  energy_draw <- rnorm(n_draws,
                       mean = theta_forecasts$energy_consumption$mean[year_idx],
                       sd   = theta_forecasts$energy_consumption$sd[year_idx])
  co2_draw <- rnorm(n_draws,
                    mean = theta_forecasts$co2$mean[year_idx],
                    sd   = theta_forecasts$co2$sd[year_idx])
  
  draw_df <- data.frame(
    Population         = pop_draw,
    GDP                = gdp_draw,
    energy_consumption = energy_draw,
    co2                = co2_draw
  )
  
  predict(svr_model, newdata = draw_df)
}

mc_results <- lapply(seq_len(h), simulate_year)
names(mc_results) <- theta_forecasts$Population$Year

scenario_b_summary <- data.frame(
  Year         = theta_forecasts$Population$Year,
  MSW_mean     = sapply(mc_results, mean),
  MSW_lower95  = sapply(mc_results, quantile, probs = 0.025),
  MSW_upper95  = sapply(mc_results, quantile, probs = 0.975)
)

cat("\nScenario B (uncertainty-aware propagation) - empirical 95% interval:\n")
print(scenario_b_summary)

# ------------------------------------------------------------------------------
# 8. Compare Scenario A vs Scenario B
# ------------------------------------------------------------------------------

comparison <- scenario_a_input %>%
  select(Year, MSW_point_forecast = MSW_forecast) %>%
  left_join(scenario_b_summary, by = "Year") %>%
  mutate(
    propagated_width      = MSW_upper95 - MSW_lower95,
    relative_width        = propagated_width / abs(MSW_point_forecast),
    naive_uncertainty     = 0  # Scenario A has no output uncertainty by construction
  )

cat("\nComparison: naive vs propagated uncertainty on downstream MSW forecast\n")
print(comparison[, c("Year", "MSW_point_forecast", "MSW_mean",
                     "MSW_lower95", "MSW_upper95", "relative_width")])

#plot-----
historical_plot_data <- data_full %>% select(Year, MSW)

p <- ggplot() +
  geom_line(data = historical_plot_data, aes(x = Year, y = MSW),
            color = "black") +
  geom_point(data = historical_plot_data, aes(x = Year, y = MSW),
             color = "black", size = 1.5) +
  geom_ribbon(data = comparison,
              aes(x = Year, ymin = MSW_lower95, ymax = MSW_upper95),
              fill = "steelblue", alpha = 0.3) +
  geom_line(data = comparison, aes(x = Year, y = MSW_point_forecast),
            color = "firebrick", linewidth = 1, linetype = "dashed") +
  labs(x = "Year", y = "Municipal solid waste (thousand tonnes)")+
  theme_cowplot()

ggsave("Italy_msw_forecast_comparison_rbf.pdf", plot = p, width = 8, height = 5, dpi = 300)
