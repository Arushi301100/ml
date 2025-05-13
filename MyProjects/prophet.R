library(prophet)
library(dplyr)



# Step 1: Create Dataframe: _________________________
# Set seed for reproducibility
set.seed(123)

# Generate dates for 1 year (365 days)
ds <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day")
head(ds)
ds
# Simulate seasonality and noise in sales data
y <- 200 +
  30 * sin(2 * pi * as.numeric(format(ds, "%j")) / 365) + # yearly seasonality
  10 * rnorm(length(ds)) # Random noise

# Create dataframe
df <- data.frame(ds, y)

# Preview data
head(df)



# Step 2: Fit Prophet Model: ______________________________________
m <- prophet(df)
m <- prophet(df, yearly.seasonality = TRUE, weekly.seasonality = TRUE)
?prophet
m

# Step 3: Forecast Future Sales (Next 30 days)
future <- make_future_dataframe (m, periods = 30)
forecast <- predict(m, future)
head(forecast)


# View predictions
tail(df, 15)
head(forecast[c("ds", "yhat", "yhat_lower", "yhat_upper")], 15)


# Step 4: Plot Forecast and Components
plot(m, forecast)
prophet_plot_components(m , forecast)


# Check Accuracy: ____________________________________
#Step 1: Filter Forecasted data for training period only

df$ds <- as.Date(df$ds)
forecast$ds <- as.Date(forecast$ds)

# keep only training data portion from the forecast
forecast_train <- forecast[forecast$ds <= max(df$ds), c("ds", "yhat")]
head(forecast_train)

# Merge actual vs predicted data
df_forecast <- merge(df, forecast_train, by = "ds")
head(df_forecast)


# Continue with Accuracy metrics:
library(Metrics)

mae_val <- mae(df_forecast$y, df_forecast$yhat)
rmse_val <- rmse(df_forecast$y, df_forecast$yhat)
mape_val <- mape(df_forecast$y, df_forecast$yhat) * 100

cat("MAE:", round(mae_val, 2), "\n")
cat("RMSE:", round(rmse_val, 2), "\n")
cat("MAPE:", round(mape_val, 2), "%\n")















